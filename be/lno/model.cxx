
/* 
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//                     Machine Models
//                     --------------
//
// Description:
//
//	Model the amount of time it takes to execute one iteration of an 
//	inner loop.  This model is used by lno to select the inner loop
//	and to select blocking factors
//
//	It is impossible to be perfectly accurate.  LNO occurs too early
//	in the compilation process.  As our algorithm is enumerate and
//	evaluate, it's also possible to be very expensive if we're not
//	careful. For the first cut of the algorithm,
//	we use many simplifying assumptions.  As we gain experience on
//	real code, we will enhance the model accordingly.
//	Where possible, we attempt to err in such a way as to minimize
//	unrolling.  It's more likely that we'll do damage if we
//	unroll too much than if we unroll too little.
//
//	Algorithm:
//
//		We first describe the components.  Then we'll discuss
//	how to combine them.
//
//	Step 1. Get resource constraints
//
//	    We need to calculate five things here; OP_rcycles,
//	MEM_rcycles, OP_issue, LOOP_INIT_issue, MEM_issue.  
//	XXX_rcycles is the number of cycles (based on resources for XXX) needed to
//	execute the loop.  XXX_issue is the number of instruction issues
//	needed for XXX.  The number of resource cycles needed overall will be 
//
//	MAX(OP_rcycles,
//          MEM_rcycles,
//          (OP_issue+LOOP_INIT_issue+MEM_issue)/ISSUE_rate)
//
//	We separate issue from XXX_rcycles since issue is a shared resource.
//
//	    1a. Calculate the number of cycles needed based on adding up
//	all the resource requirements.  We walk
//	through the code and add up the resources requirements for all the
//	ops. We pre-compute resource requirements for unroll_prod=1 ..  16.
//	and add in the loop branch and loop var. increment.
//	We try to calculate the number of integer cycles needed based on
//	integer resource requirements.  This is difficult
//	for three reasons.  First, forward substitution makes it seem like
//	there are a lot more integer ops than there really are.  Second,
//	we don't know how array addressing will expand out. Third, integer
//	multiplication will usually be strength reduced.
//      Then we compute the minimum cycles per orig. iteration
//      required based on the resource requirement.
//      We return -1.0 cycles if there is any op we can't handle.
//	For multiply adds, we consider any ADD/SUBTRACT 
//	above a MPY to be a multiply add.  This should underestimate
//	the number of MADDS since it won't catch things like a = b*c; d += a.
//	For complex operators, we treat add/sub as two floating-point adds,
//	we treat neg as two fp negates,
//	we treat multiply as two madds + 2 fp mults.
//	All other complex operators are unhandleable (division generates
//	complex code sequences so we'll never want to unroll for regs).
//	We don't currently handle quads.
//
//	    1b. Calculate the number of ops.  This gives an approximaton
//	to the number of issue resources needed by the ops.  
//	For efficiency this is calculated at the same time as 1a.
//
//	   1.c. Calculate the number of MEM_rcycles.  We assume that each
//	load/store takes 1 cycle.  We look only at array references.
//	Scalars will probably be loop invariant.  We don't count references
//	that are invariant in the inner loop.
//	Group the subexpressions, ie a[i] and a[i]  into equivalence classes.
//	For the inner loop, assume references within a small distance are in 
//	the same class; i.e. assume that a[i][j] and a[i][j+1] are in the same 
//	class. These will generate common-subexpressions when unrolled by the 
//	inner loop If an equivalence class is both loaded and stored and the
//	references are equivalent and the load is lexically before the store,
//	we add two for it.  Otherwise we add one for each equivalence class.  
//	For example given a[i] = a[i] ..., we add two for a[i] since we'll 
//	need to both load and store it.  Given a[i] = ...; ... = a[i] or
//	given a[i] = a[i-1]..., we only add one
//	since we'll keep the value being loaded in a register
//	We don't actually use dependences to find the 
//	cses.  First, we don't have read-read dependences.  Second,
//	it's not sufficient.  Two references can be the same even if they
//	have a star dependence.  Instead, we compare the access vectors.
//	We also ignore intervening dependences.  We figure that either
//	they prevent unrolling (an input to model) or they don't exist.
//
//	Dealing with register blocking.  We assume that 1b scales
//	with the product of the unrolling factors. For 1c, we unroll and count.
//
//	Step 2. Get latency constraints
//
//	    2a. Build a dependence graph for the loop.  Add a vertex for
//	each each fp load and each fp store.  Add an edge to every store from
//	every load in the same statement.  These edges will have a latency
//	equal to the sum of the latencies of the operations going from
//	the load up.  They will have a distance '0'.
//	The latency of the FP ops will be taken from targ_info.
//	We then add an edge for each flow dependence.
//	These edge contains a "distance vector" (we collapse the
//	DEPV array into a single DEPV) and a latency of 0.
//	(this is pretty much true for TFP and T5).  
//	The total latency will be the maxium over all cycles 
//	of (latency/distance).
//
//	We use the SWP algorithm to find this maximum.  
//	This algorithm runs faster with a
//	good lower bound.  We set the lower bound to FP_rcycle
//	We can never do better than this.
//
//	We ignore scalar flow dependences.  So given for example
//	x = a[i] + b[i]
//	c[i] = x + d[i]
//	The total latency will be one add rather than two.  This is a tradeoff.
//	In the above case, we do the wrong thing, but in the example below
//	x = x + a[i]
//	forward substitution/reduction elimination will get rid of the scalar
//	dependence, so we really don't want to count it.  We assume that
//	because of forward substitution, cases like the first example are
//	rarer than cases like the second.
//
//	    2b. Deal with register blocking
//      We assume that unrolling an outer loop does not increase the 
//	recurrence bound for the inner loop.  Carr proved this is true.
//	The one case it's not really true is when there is a reduction
//	and we unroll by changing associativity.  In that case, we assume
//	that the reduction can be broken (using partial sums).
//
//
//	Step 3. Floating point register pressure
//	
//	We want to estimate the number of registers
//	required for the loop.  Actually scheduling and allocating
//	registers is impossible.  So, we guesstimate using the following
//	formula.  We add together the following terms.
//
//	    3a.  Take the latency from load-fp_use-store of the target 
//	machine; usually 5 on tfp and on the r4k, usually 3 on t5. 
//	Add a fudge factor of 1.  Multiply this number by the number
//	of fp units on the machine.  This gives an estimate for the number
//	of registers needed to keep the pipeline of the machine humming;
//	12 on tfp, 6 on the r4k and 8 on t5.
//
//	    3b. Count the number of invariant locations being read/written.
//	Add one for each.  We assume each location is register allocated
//	for the whole loop.  For arrays, we count all the invariant ones.
//	For scalars we only count those that are not stored or are not reductions.  
//	We assume the other ones are just temporaries which do not live 
//	across loop iterations.
//      If most of the stores in the loop are invariant, we don't need
//      registers to keep the pipeline full.  I.e. the registers needed
//      for the pipeline are there for the invariants.  So if > 80% of
//      the stores are invariant, we return 2/3 of the registers needed
//      by the pipeline.
//
//	    3c. Count the number of loop variant array common subexpressions;
//	ie a[i] and a[i].  Group the subexpressions into equivalence classes.
//	Assume that we need to store the value of the equivalence class
//	the whole iteration (in reality we might be storing it for less).
//	So count the number of equivalence classes.  For the inner loop, 
//	assume references within a small distance are in the same class;
//	i.e. assume that a[i][j] and a[i][j+1] are in the same class.
//	These will generate common-subexpressions when unrolled by the
//	inner loop.  For exact duplicates, only count as duplicates
//	things that are loaded multiple times.  Given a[i] = a[i],
//	we don't need to store a[i] in a register.
//	For small distance duplicates, keep track of the min/max offsets.
//	The number of registers needed is the span.  I.e., given
//	a[i][j] and a[i][j+3], add three registers.
//
//	    3d. Add a fuge factor of 6 for fp.
//	Looking at a few codes and the SWP, the above estimate for fp
//	seems to be a bit low.
//
//	    3e. Dealing with register blocking.  3a, 3d are automatic.
//	For 3b and 3c, we unroll the references and count.
//
//	Step 4. Integer register pressure
//
//	    4a. Calculate integer registers used for addressing.
//	We examine all array references after removing potential cse and
//      invariants. If the addresses difference of two references are
//	a known constant and is <256 (typical load offset), 
//	they will use the same register
//	for addressing. For example,
//		for i
//		  for j
//			a[i][j],a[i+1][j],b[i][j]-- different base registers
//			a[i][j],a[i][j+1]		-- same registers
//
//	    4b. Dealing with register blocking. We
//	unroll the references and count.
//
//      Register allocation penalties (both fp and int) 
//
//	Register penalty.  We never unroll when that creates a 
//	schedule that uses more than the maximum number of registers.  We add 
//	a 10% penalty if we're in two of the maximum number of registers.
//	This prevents us from trying to use all the registers unless the
//	possible gain is significant.  When we exceed the number of registers
//	(not unrolling), we divide the total number of refs by the number
//	of registers required for loads/stores.  We take this refs/reg
//	ratio and multiply it by the excess number of registers required.
//	We add this to the number of memory references.  This is an attempt
//	to model not using registers.
//
//
//
//	COMBINING THE COMPONENTS
//
//	   The constructor computes all the components that do not
//	vary with choice of inner loop or with blocking factors:
//	OP_rcycles, OP_issue, LOOP_INIT_issue, issue_rate,
//	base_{fp,int}_regs (the number of registers needed for the pipeline)
//	and scalar_{fp,int}_regs (the number of registers needed for scalars).
//	It then creates a latency graph that will later be used to estimate
//	latencies.  This routine also creates an ARRAY_REF: a list of all the
//	array references in the routine.  These will later be unrolled and
//	evaluated.  Finally, the constructor loops over each possible inner 
//	loop, calling Try_Inner.
//
//	   Try_Inner computes a latency bound (using the latency graph 
//	computed above).  It then calls Try_Unroll, which recurses and
//	loops through the possible unrolling factors.
//
//	  Try_Unroll is a recursive routine with recursion variable "l".
//	If we are allowed to unroll "l", we loop through possible unrolling
//	factors u.  For each u, we unroll the list of array references by 'u'.
//	u starts at 1 and is incremented until we find a reason
//	to stop.  We stop incrementing if the product of the unrolling factors
//	gets too big (> 16), if we can't register allocate or if we get
//	a perfect schedule (bound by floating point resource requirements).
//	If we get a perfect schedule, we pass the information up the 
//	recursion chain so that we don't try any different unrolling
//	factors.  If u=1 and we can't register allocate, then we also
//	pass the information up.  So, if u(l) = 1 and u(l-1) = 2, we
//	won't try to unroll "l-1" any further.  If u!=1, we don't pass
//	the register information up the recursion chain.  If u(l) = 2 and
//	u(l-1) = 1, we WILL still try the case where u(l-1) = 2 and u(l) = 1.
//	If 'l' is greater than the number of loops, we've reached our base
//	case and call evaluate to evaluate a particular unrolling.
//
//	  Evaluate evaluates a particular choice of unrolling.  It counts
//	the number of register needed and array memory references as 
//	described above.  If we can't register allocate, it sets 
//	*can_reg_allocate = FALSE and return.  It computes the resource
//	and latency limits as described above.  If we've found a perfect
//	schedule (bound by resource limits), we set
//	*try_further_unroll = FALSE.  This will prevent us from trying 
//	further unrolling factors.  If we're within two registers of the
//	maximum, we add a 10% penalty to the schedule.  We compare this
//	schedule with the current best, if it's atleast 1% better we reset 
//	the best.  In case of a tie, we chose the schedule with the lowest 
//	unrolling product.  The 1% is to avoid unrolling 16 times in cases 
//	with diminishing returns.
//
//
// 	Max_Unroll_Prod=16
//
//		The maximum amount we allow the product of the unrolling
//		factors to be.
//
//	Max_Cse_Dist=4
//
//		Given 'i' as an inner loop and two references
//		a[i] and a[i+-c], we'll assume that the SWP will
//		cse the two references if 'c' <= 4
//
//      Reserved_Tie_Base_Regs = 0.5
//
//              Percentage of base registers reserved in each TIE register
//              file for estimation purposes. If there's no special
//              knowledge about a TIE register file, it is better to
//              set this number higher to prevent too much outer unroll.

/* ====================================================================
 * ====================================================================
 *
 * Module: model.cxx
 * $Revision: 1.166 $
 * $Date: 2000/05/19 20:23:06 $
 * $Author: murthy $
 * $Source: /isms/cmplrs.src/osprey1.0/be/lno/RCS/model.cxx,v $
 *
 * Revision history:
 *  dd-mmm-94 - Original Version
 *
 * Description: Model time taken by inner loops
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/be/lno/RCS/model.cxx,v $ $Revision: 1.166 $";

#include <sys/types.h>
#include <alloca.h>
#include <math.h>

#include "at_trace.h"
#include "model.h"
#include "cxx_memory.h"
#include "lnopt_main.h"
#include "stab.h"
#include "lnotarget.h"
#include "dep_graph.h"
#include "lwn_util.h"
#include "cache_model.h"
#include "reduc.h"
#include "config.h"
#include "config_targ_options.h"
#include "config_cache.h"
#include "config_lno.h"
#include "config_opt.h"
#include "tlog.h"
#include "simd.h"
#include "glob.h"
#include "at_xcc.h"
#include "simd_at.h"
#include "simd_loop_at.h"
#include "simd_loop_v2.h"
#include "snl_utils.h"
#include "tie.h"
#include "tietypes.h"
#include "wintrinsic.h"
#include "intrn_info.h"
#include "simd_model.h"

typedef HASH_TABLE<WN*,INT> WN2INT;
typedef STACK<ACCESS_ARRAY *> STACK_OF_ACCESS_ARRAY;

#define MTYPE_is_double_size(m)	(MTYPE_size_reg(m)==MTYPE_size_reg(MTYPE_I8))

static MEM_POOL Model_Local_Pool;
static MEM_POOL Model_Lat_Pool;
static BOOL model_mempool_initialized;

#if 0
INT Max_Unroll_Prod=16;
#else
INT Max_Unroll_Prod=4;
#endif

INT Max_Cse_Dist = 4; 
float Reserved_Tie_Base_Regs = 0.5; // 50% reserved

INT LOOP_MODEL::_model_no = 0;

// how many good DO loops (counting possibly this one, surround wn)
static INT Num_Good(WN *wn) 
{
  if (!wn) return(0);
  if (WN_operator(wn) == OPR_DO_LOOP) {
    if (Do_Loop_Is_Good(wn)) {
      return(1+Num_Good(LWN_Get_Parent(wn)));
    } else {
      return(0);
    }
  } else {
    return(Num_Good(LWN_Get_Parent(wn)));
  }
}

// Find the step of loop i given that wn is the inner loop
// return 0 on error or if it's not constant
static INT Find_Step(WN *wn, INT i)
{
  if (!wn) return(0);
  if (WN_operator(wn) == OPR_DO_LOOP) {
    DO_LOOP_INFO *dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (dli->Depth < i) {
      return(0);
    } else if (dli->Depth > i) {
      return(Find_Step(LWN_Get_Parent(wn),i));
    } else {
      ACCESS_VECTOR *Step = dli->Step;
      if (Step->Is_Const()) {
	return(Step->Const_Offset);
      } else {
	return 0;
      }
    }
  } else {
    return(Find_Step(LWN_Get_Parent(wn),i));
  }
}

REG_REF_COUNT::REG_REF_COUNT(LNO_REGCLASS_INFO *regclass_info, MEM_POOL *pool) :
  regs(regclass_info,pool),
  refs(regclass_info,pool),
  inv_stores(regclass_info,pool),
  var_stores(regclass_info,pool),
  unused_regs(regclass_info,pool) {
}

void
REG_REF_COUNT::Clear() {
  regs.Clear();
  refs.Clear();
  inv_stores.Clear();
  var_stores.Clear();
  unused_regs.Clear();
}

void
REG_REF_COUNT::Print(FILE *f, INT indent, bool verbose) {
  fprintf(f,"%*sRegs/refs counts {\n",indent, "");
  {
    indent+=2;
    
    fprintf(f,"%*sRegisters  : ",indent,"");
    regs.Print(f);
    fprintf(f,"\n");
    
    fprintf(f,"%*sReferences : ",indent,"");
    refs.Print(f);
    fprintf(f,"\n");
    
    fprintf(f,"%*sInv_stores : ",indent,"");
    inv_stores.Print(f);
    fprintf(f,"\n");
    
    fprintf(f,"%*sVar_stores : ",indent,"");
    var_stores.Print(f);
    fprintf(f,"\n");
    
    fprintf(f,"%*sUnused_regs: ",indent,"");
    unused_regs.Print(f);
    fprintf(f,"\n");

    
    indent-=2;
  }
  fprintf(f,"%*s}\n",indent, "");
}

// Start off with routines for ARRAY_REF_LIST and ARRAY_REF
// We store a list of all array references to help us estimate how
// many loads and stores are in the loop
void ARRAY_REF_NODE::Print(FILE *fp) const
{
  fprintf(fp, "(size=%d) ", _element_size);
  fprintf(fp, "Wn = %p ", Wn);
  if (_vec != LNO_REGS_IDX_UNDEFINED) {
    fprintf(fp, "VEC (%d x [%d]) ", _vec_regs, _vec);
  }
  if (_align != LNO_REGS_IDX_UNDEFINED) {
    fprintf(fp, "ALIGN [%d] ", _align);
  }
  if (_sel != LNO_REGS_IDX_UNDEFINED) {
    fprintf(fp, "SEL [%d] ", _sel);
  }
  Array->Print(fp);
}

LOOP_MODEL::~LOOP_MODEL()
{
  CXX_DELETE_ARRAY(_block_number,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_block_number_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_iloop,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_iloop_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_stripsz,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_stripsz_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_striplevel,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_striplevel_inner,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_new_order,Malloc_Mem_Pool);
  CXX_DELETE_ARRAY(_new_order_inner,Malloc_Mem_Pool);
}

static bool debug_model;

static INT Num_Complex_Mpy;

extern INT Debug_Cache_Model; 

extern SIMD_INFO* Simd_Info;
extern SIMD_LOOP* Cur_Simd_Loop;
extern MEM_POOL SIMD_default_pool;

void 
LOOP_MODEL::Model(WN* wn, 
                  BOOL* can_be_inner, 
                  BOOL* can_be_unrolled,
                  INT outermost_can_be_tiled, 
                  BOOL trying_invariant,
                  ARRAY_DIRECTED_GRAPH16* array_graph,
                  SX_INFO* pi,
                  INT SNL_Depth,
                  INVAR_TABLE *invar_table) {
  if (!model_mempool_initialized) {
    MEM_POOL_Initialize(&Model_Local_Pool,"Model_Local_Pool",FALSE);
    MEM_POOL_Initialize(&Model_Lat_Pool,"Model_Local_Pool",FALSE);
    model_mempool_initialized = TRUE;
  }
  
  MEM_POOL_Push(&Model_Local_Pool);
  
  _model_no++;
  
  Is_True(WN_operator(wn)==OPR_DO_LOOP, ("non-DO loop passed to LOOP_MODEL"));
  Is_True(Do_Loop_Is_Inner(wn),("non-inner loop passed to LOOP_MODEL"));
  
  debug_model = Get_Trace(TP_LNOPT, TT_LNO_MODEL);
  if (debug_model) {
    fprintf(TFile,"\n"
	    "-----------------------------------------------------------\n"
	    "LOOP_MODEL %d: modeling an SNL, inner loop line %d\n"
	    "-----------------------------------------------------------\n",
	    Model_No(), Srcpos_To_Line(LWN_Get_Linenum(wn)));
  }
  
  INT wndepth = Do_Loop_Depth(wn);
  _num_loops = wndepth+1;
  _num_good = ::Num_Good(wn);
  INT num_bad = _num_loops-_num_good;
  
  if (debug_model) {
    fprintf(TFile,"Loops %d, good %d, bad %d, SNL depth %d\n",
	    _num_loops,_num_good,num_bad,SNL_Depth);
  }

  _invar_table = invar_table;
  _array_graph = array_graph;
  _pi = pi;
  
  _can_be_unrolled = can_be_unrolled;
  _outermost_can_be_tiled = outermost_can_be_tiled;
  _trying_invariant = trying_invariant;
  
  _block_number = CXX_NEW_ARRAY(INT, _num_loops,Malloc_Mem_Pool);
  _block_number_inner = CXX_NEW_ARRAY(INT, _num_loops,Malloc_Mem_Pool);
  _iloop = CXX_NEW_ARRAY(INT, _num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _iloop_inner = CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _stripsz = CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _stripsz_inner = CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _striplevel = CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _striplevel_inner = CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,Malloc_Mem_Pool);
  _new_order = CXX_NEW_ARRAY(INT,_num_loops,Malloc_Mem_Pool);
  _new_order_inner = CXX_NEW_ARRAY(INT,_num_loops,Malloc_Mem_Pool);
  
  /* Make a copy of the can_be_inner array so we have the original values. */
  _can_be_inner_orig = can_be_inner;
  _can_be_inner = CXX_NEW_ARRAY(BOOL, _num_loops, &Model_Local_Pool);
  memcpy(_can_be_inner, can_be_inner, _num_loops * sizeof(BOOL));
  
  _num_cycles = -1.0;
  _wn = wn;
  _snl_depth = SNL_Depth;

  _num_evaluations = 0;
  _lat_graph = NULL;
  _simd_loop = NULL;
  _all_inv_table = NULL;
  _regclass_info = NULL;

  _target = NULL;
  _base = NULL;
  _non_temp_scalars = NULL;
  _regs = NULL;
  _refs = NULL;
  _array_refs = NULL;
  _scalar_refs = NULL;
  _regs_inner = NULL;
  _refs_inner = NULL;
  
  Num_Complex_Mpy = 0;

  if (debug_model) {
    pi->Print(TFile);
  }
  
  _at_analysis_phase = Simd_Info && Simd_Info->AT_Analysis_Phase();
  
  // setup nest info -- permutation, blocking, etc.
  Setup_Nest_Info();
  
  /*
    Find a vectorizable loop in the nest if possible. If there's no loop
    to SIMD, vectorization is disabled, or the necessary SIMD ISA is not
    available:
      _simd_loop will be NULL.
    Otherwise,
      _simd_loop is non-null,
      _simd_loop->Simd_Loop_Level() points to the loop to be vectorized,
      _simd_loop->Simd_Loop() gives the WN of the loop to be vectorized.
  */
  
  SIMD_Model();
  Is_True(_simd_loop == NULL ||
	  (_simd_loop->Simd_Loop_Level() >= 0 &&
	   _simd_loop->Simd_Loop() != NULL), ("Bad SIMD model result"));
  
  /* Auto TIE analysis. */
  if (_at_analysis_phase) {
    WN *analysis_loop = _simd_loop ? _simd_loop->Simd_Loop() : wn;
    INT analysis_level = _simd_loop ? _simd_loop->Simd_Loop_Level() : (_num_loops - 1);
    Simd_Info->SIMD_At()->Add_Orig_Loop(analysis_loop, _all_inv_table, analysis_level);
    
    if (_simd_loop) {
      /* For AutoTIE SIMD analysis, we prescan the loop so that the required
	 vector types are added to the libauto type table and can be available
	 for LNO register estimation. */
      _simd_loop = Simd_Info->SIMD_At()->Pre_Model_Simd_Loop(_simd_loop);

      /* Disable AutoTIE SIMD analysis if AutoTIE doesn't support the loop. */
      if (Simd_Info->SIMD_At()->Bad_Operator())
	_simd_loop = NULL;
    }
  }
  
  // Setup target specific parameters.
  Setup_Target();
  
  // collect the array references in the inner loop
  _arl = CXX_NEW(ARRAY_REF(WN_do_body(wn),SNL_Depth,&Model_Local_Pool,invar_table),
                 &Model_Local_Pool);

  // base array count
  _num_tlb = _arl->Elements();
  
  /* Collect all symbol references in the inner loop. */
  SYMBOL_TREE *symbol_tree = CXX_NEW(SYMBOL_TREE(&Model_Local_Pool), &Model_Local_Pool);
  INT outer = 0;
  for (; !_can_be_inner_orig[outer]; outer++);
  symbol_tree->Enter_Scalar_Refs(WN_do_body(wn), _arl, pi, _can_be_inner_orig,
				 _num_loops, outer);

  if (_simd_loop) {
    if (_at_analysis_phase) {
      // Auto TIE analisys of a vectorizable loop
      Simd_Info->SIMD_At()->Annotate_Vector_Info(_arl, symbol_tree, _simd_loop);
    } else {
      // normal vectorization
      _simd_loop->Annotate_Vector_Info(_arl, symbol_tree);
    }

    Adjust_Vector_Regfiles(_arl, symbol_tree);
  }

  if (debug_model) {
    fprintf(TFile, "ARRAY_REFs:\n");
    _arl->Print(TFile);
  }

  // Registers from scalars
  _scalar_refs = CXX_NEW(LNO_REGS(_regclass_info,&Model_Local_Pool),&Model_Local_Pool);
  _non_temp_scalars = CXX_NEW(LNO_REGS(_regclass_info,&Model_Local_Pool),&Model_Local_Pool);
  symbol_tree->Num_Refs(_scalar_refs,_non_temp_scalars);
  if (debug_model) {
    fprintf(TFile,"Scalar regs: ");
    _non_temp_scalars->Print(TFile);
    fprintf(TFile,"\n");
  }
  
  _refs = CXX_NEW(LNO_REGS(_regclass_info,&LNO_local_pool),&LNO_local_pool);
  _regs = CXX_NEW(LNO_REGS(_target,&LNO_local_pool),&LNO_local_pool);
  _regs->Add(_target);
  
  _array_refs = CXX_NEW(LNO_REGS(_regclass_info,&Model_Local_Pool),&Model_Local_Pool);
  _arl->Num_Refs(_array_refs);
  if (debug_model) {
    fprintf(TFile,"Array refs: ");
    _array_refs->Print(TFile);
    fprintf(TFile,"\n");
  }
  
  // Setup loop resource usage
  _OP_issue = 0.0;
  _OP_resource_count = OP_Resources(WN_do_body(wn), &_OP_issue, invar_table);
  if (debug_model) {
    if (_OP_resource_count) {
      fprintf(TFile, "OP_Resource_Count:\n");
      TI_RES_COUNT_Print(TFile, _OP_resource_count);
      fprintf(TFile, "\n");
    } else {
      fprintf(TFile,"Can't handle loop resources (OP_resource_count is NULL)\n");
    }
  }
  
  if (_OP_resource_count) {
    // resource_count -- OP_resource_count unrolled by the unroll factor product
    TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
    _loop_rcycles_unroll_by = CXX_NEW_ARRAY(double, Max_Unroll_Prod, &Model_Local_Pool);

    // Rcycles
    // for each unroll factor product, add the necessary loop end test resource usage
    // and obtain the minimum execution cycles per iteration based on the resource usage
    for (INT u = 0; u < Max_Unroll_Prod; u++) {
      TI_RES_COUNT_Add(resource_count, resource_count, _OP_resource_count);
      TI_RES_COUNT *tmp_resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
      TI_RES_COUNT_Add(tmp_resource_count, tmp_resource_count, resource_count);
      LNOTARGET_Loop_Inc_Test_Res(tmp_resource_count);
      _loop_rcycles_unroll_by[u] = TI_RES_COUNT_Min_Cycles(tmp_resource_count)/(u+1);
    }
    
    if (debug_model) {
      fprintf(TFile, "_loop_rcycles_unroll_by:\n  (");
      for (INT u=0; u<Max_Unroll_Prod; u++) {
	fprintf(TFile," %0.1f",_loop_rcycles_unroll_by[u]);
      }
      fprintf(TFile, " )\n");
    }
    
    // Latency
    MEM_POOL_Push(&Model_Lat_Pool);
    _lat_graph = CXX_NEW(LAT_DIRECTED_GRAPH16(50,200,_num_good,num_bad,
                                              &Model_Lat_Pool,array_graph),
                         &Model_Lat_Pool);
    if (_lat_graph->Add_Vertices_Op_Edges(WN_do_body(wn),invar_table) == -1)
      _OP_resource_count = NULL;
    if (_lat_graph->Add_Flow_Edges() == -1)
      _OP_resource_count = NULL;
    if (debug_model) {
      if (!_OP_resource_count) {
	fprintf(TFile,"Can't build latency graph; giving up on resource usage...\n");
      } else {
	_lat_graph->Print(TFile);
      }
    }
  }

  // _invariant_ref_coeff coefficient will be used in estimating potential benefit
  // of unrolling beyond the "ideal" schedule due to the hoisting of a larger number
  // of invariant memory references
  INT total_refs = _array_refs->Total();
  if (total_refs > 0) {
    _invariant_ref_coeff = 1 / (double)(total_refs * Max_Unroll_Prod);
  } else {
    _invariant_ref_coeff = 0.0;
  }
  if (debug_model) {
    fprintf(TFile, "_invariant_ref_coeff = %0.2f\n", _invariant_ref_coeff);
  }
  
  /* Try each possible inner loop.
     Check with the vectorizer if it is ok to make a loop inner. */
  for (INT loop = wndepth; loop >= 0; loop--) {
    if (_can_be_inner[loop] &&
	(_simd_loop==NULL || _simd_loop->Is_Inner_Ok(loop))) {
      Try_Inner(can_be_unrolled, outermost_can_be_tiled, loop, _num_loops, pi);
    }
  }
  
  if (_at_analysis_phase) {
    if (_simd_loop) {
      Simd_Info->SIMD_At()->Add_Simd_Loop(_simd_loop);
    }

    if (Simd_Info->SIMD_At()->Trace()) {
      Simd_Info->SIMD_At()->AT_Pu()->Print(TFile, 0, true);
    }
  }

  // setup simd with the final loop model
  SIMD_Post_Model();
  
  if (_lat_graph) {
    // the cxx_delete call is necessary because DIRECTED_GRAPH16 uses its own
    // memory pools which need to be cleaned up
    CXX_DELETE(_lat_graph, &Model_Lat_Pool);
    MEM_POOL_Pop(&Model_Lat_Pool);
    _lat_graph = NULL;
  }
  
  MEM_POOL_Pop(&Model_Local_Pool);
  
  // various reports
  if (debug_model || LNO_Verbose) {
    Model_Report();
  }
  if ( LNO_Tlog ) {
    Model_Performance_Trace();
  }
  if (LNO_Analysis) {
    fprintf(LNO_Analysis,"    (INNER_LOOP %d)\n",
	    Srcpos_To_Line(_snl_line_numbers[_new_order[wndepth]]));
    CXX_DELETE_ARRAY(_snl_line_numbers, Malloc_Mem_Pool);
  }
  
  if (_num_tlb > Mhd.L[0].TLB_Entries) {
    _num_cycles += Mhd.L[0].TLB_Miss_Penalty * (Mhd.L[0].TLB_Entries-_num_tlb);
  }
} // LOOP_MODEL::Model


/*-----------------------------------------------------------------------*
 * Model and set up the SIMD transformation class                        *
 *-----------------------------------------------------------------------*/
void
LOOP_MODEL::SIMD_Model ()
{
  // If vectorization is enabled, this function will consume a lot of memory
  // because it doesn't free any of the SIMD_LOOPs created for non-vectorizable
  // loops. This should be ok, because the SIMD pools are pushed/popped for each
  // SNL in SNL_Phase()
  
  _simd_loop = NULL;
  
  if (Simd_Target == SIMD_TARG_UNKNOWN)
    return;

  Is_True(Simd_Target == SIMD_TARG_VECTRA_II ||
          Simd_Target == SIMD_TARG_AT_ANALYSIS ||
          Simd_Target == SIMD_TARG_GENERIC,
          ("Unexpected simd target."));
  
  _all_inv_table = CXX_NEW(INVAR_TABLE(500,&Model_Local_Pool),
                           &Model_Local_Pool);
  Mark_Invar(_do_stack->Bottom_nth(_num_loops-_snl_depth),
             _num_loops,_do_stack,_all_inv_table,
             &Model_Local_Pool,false,/* all_expr */true);
  
  MEM_POOL_Push(&Model_Local_Pool);
  
  _simd_loop = NULL;
  Is_True(_snl_depth <= _num_good,
          ("Number of good loops %d should be greater than SNL depth %d",
           _num_good, _snl_depth));

  SIMD_LOOP  *best_simd_loop = NULL;
  SIMD_MODEL *best_simd_model = NULL;
  WN         *best_simd_wn = NULL;

  for (INT loop = _snl_depth; loop>0; loop--) {
    INT simd_depth = _num_loops-loop;
    if (debug_model) {
      fprintf(TFile,"SIMD model loop %d\n",simd_depth);
    }
    WN *simd_wn = _do_stack->Bottom_nth(simd_depth);
    
    // output a verbose line
    SIMD_Msg(AT_MSG_SIMD_LOOP_BEGIN, simd_wn);

    /* When doing "generic" vectorization, we start with a bit vector
       with all possible vector lengths enabled and then disallow
       vector lengths for which there's no available vector ctype
       or operator. Then we select the maximum common vector length
       and proceed with mostly vector-length-dependendent checks.
       If some of these checks fail, we decrease the maximum allowed
       vector length to just below the selected one and repeat the
       screening process. */
    INT max_vl = -1;
    bool done_vls = false;
    while (!done_vls) {
      done_vls = true;
      
      /* Create a new SIMD_LOOP structure for the current loop. */
      _simd_loop = SIMD_LOOP_New(this, simd_wn, simd_depth);
      Cur_Simd_Loop = _simd_loop;
      
      if (max_vl > 0)
        _simd_loop->Set_Max_Vector_Length(max_vl);
      
      /* Innvariant table is used everywhere in the SIMD_LOOP. */
      _simd_loop->Set_Invar_Table(_all_inv_table);
      
      /* Screen the operations for matching SIMD operators and compute
         expression size. */
      _simd_loop->Screen_Operator_Compute_Size(WN_do_body(simd_wn), NULL);
      
      /* No ctype or operator. */
      if (_simd_loop->Bad_Operator()) {
        _simd_loop = NULL;
        SIMD_Msg(AT_MSG_SIMD_LOOP_NON_VECTORIZABLE, simd_wn);
        continue;
      }
      
      /* Vectorizability test and set vectorizable loop. */
      if (!_simd_loop->Test_Vectorization(_can_be_unrolled, _outermost_can_be_tiled,
                                          _array_graph, _do_stack)) {

        /* If we're doing "generic" vectorization and we don't find
           vector-length-dependent operation, we need to repeat the
           screening process for other vector lengths. */
        if (Simd_Target == SIMD_TARG_GENERIC) {
          Is_True(max_vl < 0 || _simd_loop->V_Unroll_Factor() <= max_vl,
                  ("Unexpected vector unroll factor %d > %d",
                   _simd_loop->V_Unroll_Factor(), max_vl));
          
          /* Set the maximum allowed vector length to just below the selected
             one and repeat the screening process. This way we may find a
             smaller common vector length that passes all vectorization checks. */
          max_vl = _simd_loop->V_Unroll_Factor() - 1;
          if (max_vl > 1) {
            done_vls = false;
            continue;
          }
        }
        
        _simd_loop = NULL;
        SIMD_Msg(AT_MSG_SIMD_LOOP_NON_VECTORIZABLE, simd_wn);
        continue;
      }
      
      /* The loop is vectorizable. Check if it makes sense from performance
         standpoint to vectorize the loop and how it compares to other
         vectorizable loops in the same loop nest. */
      Is_True(_simd_loop->Simd_Loop_Level() == simd_depth,
              ("Bad SIMD loop level set %d, should be %d",
               _simd_loop->Simd_Loop_Level(), simd_depth));
      Is_True(_simd_loop->Simd_Loop() == simd_wn,("Wrong loop to SIMD"));
      
      SIMD_MODEL *simd_model = CXX_NEW(SIMD_MODEL(&SIMD_default_pool),
                                       &SIMD_default_pool);
      simd_model->Model_Simd_Loop(_simd_loop);
      if (simd_debug) {
        fprintf(TFile, "SIMD cost model:\n");
        simd_model->Print(TFile);
      }
      
      if (simd_model->Beneficial() &&
          (!best_simd_model || simd_model->Min_II() < best_simd_model->Min_II())) {
        if (best_simd_wn) {
          SIMD_Msg(AT_MSG_SIMD_LOOP_COST_MODEL, best_simd_wn);
        }
        
        best_simd_model = simd_model;
        best_simd_loop = _simd_loop;
        best_simd_wn = simd_wn;
      } else {
        SIMD_Msg(AT_MSG_SIMD_LOOP_COST_MODEL, simd_wn);
      }
    }
    
    _simd_loop    = best_simd_loop;
    Cur_Simd_Loop = _simd_loop;
  } /* for each loop in the nest */
  
  if (_simd_loop) {
    Is_True(_simd_loop->Simd_Loop_Level()>=0 &&
            _simd_loop->Simd_Loop()!=NULL,("Wrong SIMD loop level setup"));
    if (Simd_Target==SIMD_TARG_AT_ANALYSIS ||
        Simd_Target==SIMD_TARG_GENERIC) {
      ((SIMD_LOOP_AT *)_simd_loop)->Set_Default_Vl();
    }
    Setup_With_Simd();
    if (debug_model) {
      fprintf(TFile,"SIMD to vectorize loop %d\n",_simd_loop->Simd_Loop_Level());
    }
  } else {
    if (debug_model) {
      fprintf(TFile,"SIMD will not vectorize this SNL\n");
    }
  }
  
  MEM_POOL_Pop(&Model_Local_Pool);
} // LOOP_MODEL::SIMD_Model



void
LOOP_MODEL::SIMD_Post_Model() {
  /* Set up the SIMD with the final loop model. Note that in
     this step we can decide not to SIMD after all. */
  
  if (_at_analysis_phase) {
    Simd_Info->SIMD_At()->Output_Register_Usage(_regs);
    Simd_Info->SIMD_At()->Finalize_Region();
    _simd_loop = NULL; // no SIMD transformation if Auto TIE analysis only
  }
  
  if (_simd_loop && !_simd_loop->Setup_With_Loop_Model()) {
    _simd_loop = NULL; /* The memory will be freed later anyway. */
  }
  
  // by this point we have decided whether to SIMD the loop or not
  if (LNO_SIMD_Verbose && _simd_loop) {
    static INT simd_loop_counter = 0;
    simd_loop_counter++;
    SIMD_Msg(AT_MSG_SIMD_LOOP_VECTORIZED, _simd_loop->Simd_Loop(),
             _simd_loop->V_Unroll_Factor());
    if (LNO_SIMD_Loop_Before || LNO_SIMD_Loop_After) {
      if (LNO_SIMD_Loop_Before<LNO_SIMD_Loop_After &&
	  simd_loop_counter>LNO_SIMD_Loop_Before &&
	  simd_loop_counter<LNO_SIMD_Loop_After) {
	_simd_loop=NULL;
      } else if (LNO_SIMD_Loop_After<=LNO_SIMD_Loop_Before &&
		 (simd_loop_counter<LNO_SIMD_Loop_After ||
		  simd_loop_counter>LNO_SIMD_Loop_Before)) {
	_simd_loop=NULL;
      }
    }
    if (simd_debug) {
      if (_simd_loop) {
	fprintf(stdout,"vectorize loop (#%d)\n",simd_loop_counter);
      } else {
	fprintf(stdout,"skip loop (#%d)\n",simd_loop_counter);
      }
    }
  }
  Cur_Simd_Loop = _simd_loop;
} // LOOP_MODEL::SIMD_Post_Model

void
LOOP_MODEL::Setup_With_Simd() {
  Is_True(_simd_loop && _simd_loop->Simd_Loop_Level()>=0,
	  ("SIMD loop expected."));

  for (INT loop = 0; loop < _num_loops; loop++) {
    bool se_needed_interchange = false;
    bool se_needed_unroll = false;
    bool is_inside = false;
    if (loop>=_simd_loop->Simd_Loop_Level()) {
      is_inside = true;
    }
    if (_pi->Must_Finalize() || _pi->Lcd_Depth()!=-1) {
      se_needed_unroll=true;
    }
    SX_PITER ii(&_pi->Plist);
    SX_PNODE *sn;
    for (SX_PNODE *sn = ii.First(); sn != NULL; sn = ii.Next()) {
      SX_PNODE::STATUS status = sn->Transformable(loop);
      if (status == SX_PNODE::SE_REQD) {
	se_needed_interchange = true;
	break;
      }
    }

    if (debug_model) {
      fprintf(TFile, "Vectorize %d, loop %d: %s %s %s\n",
	      _simd_loop->Simd_Loop_Level(), loop,
	      is_inside ? "INSIDE" : "OUTSIDE",
	      se_needed_interchange ? "SE_NEEDED_INTERCHANGE" : "",
	      se_needed_unroll ? "SE_NEEDED_UNROLL" : "");
    }

    if (loop!=_num_loops-1 && (is_inside || se_needed_interchange)) {
      _can_be_inner[loop] = false;
      if (debug_model) {
	fprintf(TFile,"Because of SIMD, can't be inner %d\n",loop);
      }
    }
    if (is_inside || se_needed_unroll) {
      _can_be_unrolled[loop] = false;
      if (debug_model) {
	fprintf(TFile,"Because of SIMD, can't be unrolled %d\n",loop);
      }
    }
  }
} // LOOP_MODEL::Setup_With_Simd

void
LOOP_MODEL::Setup_Nest_Info() {

  if (LNO_Analysis || LNO_Tlog) {
    _snl_line_numbers = CXX_NEW_ARRAY(INT64,_num_loops,Malloc_Mem_Pool);
  } else {
    _snl_line_numbers = NULL;
  }
  
  _est_num_iterations=CXX_NEW_ARRAY(INT64, _num_loops,&Model_Local_Pool);
  _required_unroll=CXX_NEW_ARRAY(INT, _num_loops,&Model_Local_Pool);
  _required_blocksize=CXX_NEW_ARRAY(INT,_num_loops*MHD_MAX_LEVELS,&Model_Local_Pool);
  _required_permutation=CXX_NEW_ARRAY(INT, _num_loops,&Model_Local_Pool);
  for (INT i = 0; i < _num_loops; i++)
    _required_permutation[i] = -1;
  
  // Initialize results to default case
  _inner_loop = _num_loops-1;
  for (INT i = 0; i < _num_loops; i++) {
    _block_number[i] = 1; // no unrolling
    _new_order[i] = i; // default order
  }
  _nstrips = 0;
  
  _blocking_disabled = LNO_Blocking == 0;
  // disable interchange if -LNO:interchange=off or
  // doing autotie analysis
  if (LNO_Interchange == FALSE || _at_analysis_phase) {
    for (INT j = 0; j < _num_loops; j++)
      _required_permutation[j] = j;
  }
  
  // setup user specified permutation/blocking/unrolling
  // TODO: these pragmas are not yet implemented and the rest of the code
  // in LNO may ignore any user specifications (DP)
  INT depth = _num_loops-1;
  INT loop_count = 0;
  for (WN *tmp = _wn; tmp; tmp = LWN_Get_Parent(tmp)) {
    if (WN_operator(tmp) == OPR_DO_LOOP) {
      loop_count++;
      if (LNO_Analysis || LNO_Tlog) {
	_snl_line_numbers[depth] = WN_Get_Linenum(tmp);
      }
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(tmp);
#if 0
      if (_at_analysis_phase) { // no unrolling if autotie analysis
	_required_unroll[depth] = 1;
      } else {
	_required_unroll[depth] = dli->Required_Unroll;
      }
#else 
      _required_unroll[depth] = dli->Required_Unroll;
#endif
      for (INT ll = 0; ll < MHD_MAX_LEVELS; ll++) {
	Is_True(dli->Required_Blocksize[ll] >= -1 &&
		dli->Required_Blocksize[ll] <= 10000,
		("Suspicious required blocksize %d for loopno=%d level=%d",
		 dli->Required_Blocksize[ll], depth, ll));
	_required_blocksize[depth*MHD_MAX_LEVELS+ll] =
	  dli->Required_Blocksize[ll];
      }
      if (_blocking_disabled == FALSE && loop_count <= _snl_depth)
	_blocking_disabled = dli->Cannot_Block;    // conservative but okay
      if (dli->Permutation_Spec_Count > 0) {
	for (INT j = 0; j < dli->Permutation_Spec_Count; j++) {
	  _required_permutation[depth+j] = dli->Permutation_Spec_Array[j] + depth;
	}
      }
      _est_num_iterations[depth--] = dli->Est_Num_Iterations;
    }
  }
  Is_True(depth == -1,
	  ("Bad loop depth (inner loop %d, outer loop %d)",_inner_loop,_num_loops));
  
  // verify that the loop permutations has been setup correctly
  bool error = false;
  
  // one loop not mapped to several levels
  for (INT i = 0; !error && i < _num_loops; i++) {
    if (_required_permutation[i] >= 0) {
      for (INT j = 0; j < i; j++)
	if (_required_permutation[i] == _required_permutation[j]) {
	  error = true;
	  break;
	}
      if (!LNO_Apply_Illegal_Transformation_Directives) {
	if (i < _outermost_can_be_tiled ||
	    _required_permutation[i] < _outermost_can_be_tiled)
	  error = true;
      }
    }
  }
  
  // for SIMD modeling purposes, assume for now that required permutation
  // is either not set, or specifies the default order
  // this should be fixed later when we handle the interchange pragmas
  for (INT i=0; i<_num_loops; i++) {
    if (_required_permutation[i]!=-1 &&
	_required_permutation[i]!=i) {
      error = true;
      break;
    }
  }
    
  if (error) {
    DevWarn("Required permutation settings ignored");
    for (INT i = 0; i < _num_loops; i++)
      _required_permutation[i] = -1;
  }
  
  for (INT i = 0; i < _num_loops; i++)
    if (_required_permutation[i] != -1)
      _can_be_inner[_required_permutation[i]] = (i == _inner_loop);

  _do_stack = CXX_NEW(DOLOOP_STACK(&Model_Local_Pool),&Model_Local_Pool);
  Build_Doloop_Stack(_wn, _do_stack);
  Is_True(_do_stack->Elements()==_num_loops,
	  ("Inconsistent number of loops (stack %d != num_loops %d)",
	   _do_stack->Elements(),_num_loops));
}


// Setup target specific parameters.
void
LOOP_MODEL::Setup_Target (void)
{
  AT_TY_TAB *at_ty_tab =
    _at_analysis_phase ? &Simd_Info->SIMD_At()->AT_Pu()->Ty_Tab() : NULL;
  _regclass_info = CXX_NEW(LNO_REGCLASS_INFO(&LNO_local_pool, at_ty_tab), &LNO_local_pool);
  if (debug_model) {
    _regclass_info->Print(TFile);
  }

  _target = _regclass_info->Target();
  
  _base = CXX_NEW(LNO_REGS(_regclass_info, &Model_Local_Pool),&Model_Local_Pool);
  _vec_regfiles = CXX_NEW(LNO_REGS(_regclass_info, &Model_Local_Pool),&Model_Local_Pool);
  _align_regfiles = CXX_NEW(LNO_REGS(_regclass_info, &Model_Local_Pool),&Model_Local_Pool);
  
  _LOOP_INIT_issue = 2.0;
  
  if (Is_Target_R8K()) {
    _issue_rate = 4.0;
    _base->Count_Of_Float() = 18;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    _num_mem_units = 2.0;
  } else if (Is_Target_R10K()) {
    _issue_rate = 4.0;
    _base->Count_Of_Float() = 14;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    _num_mem_units = 1.0;
  } else if (Is_Target_R4K()) {
    _issue_rate = 1.0;
    _base->Count_Of_Float() = 12;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    _num_mem_units = 1.0;
  } else if (Is_Target_R5K()) {
    _issue_rate = 2.0;
    _base->Count_Of_Float() = 14;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    _num_mem_units = 1.0;
  } else if (Is_Target_Itanium()) {
    _issue_rate = 6.0;  // 2 bundles with 3 instructions each
    _base->Count_Of_Float() = 32; // (8+1)*2+6
    _base->Count_Of_Int() = 10; // r0, r1, r12, r13, loop ub, fudge (5)
    _num_mem_units = 2.0;
  } else {
    if (!Is_Target_Xtensa()) {
      Lmt_DevWarn(1, ("Warning, LNO machine model parameters are defaulted to Xtensa"));
    }
    if (xt_zero_cost_loop && Enable_ZCL) {
      _LOOP_INIT_issue = 0.0;
    } else {
      _LOOP_INIT_issue = 2.0;
    }

    int max_slots=TI_ISA_Num_Slots(0);
    for (int i=1; i<TI_ISA_Num_Bundles(); i++) {
      max_slots = MAX(max_slots, TI_ISA_Num_Slots(i));
    }
    _issue_rate = max_slots;
    if (_regclass_info->Index_Of_Type(MTYPE_F4) != 
	_regclass_info->Index_Of_Type(MTYPE_I4)) { // HW FP
      _base->Count_Of_Float() = 4;
    }
    _base->Count_Of_Int() = 3; // $sp, $ra, fudge(1)
    _num_mem_units = 1.0;
    for (LNO_REGS_IDX idx = LNO_REGS_IDX_FIRST; idx < _base->Regs_Count(); idx++) {
      // No base registers for autotie types.
      if (_regclass_info->Idx_Source(idx) == LNO_REGCLASS_INFO::SRC_ISA_REGCLASS &&
	  _base->Count_Of_Idx(idx) == 0) {
	_base->Count_Of_Idx(idx) = (INT)(_target->Count_Of_Idx(idx) * Reserved_Tie_Base_Regs);
      }
    }
  }
}


INT
LOOP_MODEL::Old_To_New_Order(INT old_i) {
  for (INT new_i=0; new_i<_num_loops; new_i++)
    if (New_Order(new_i)==old_i)
      return new_i;
  return -1;
}

INT
LOOP_MODEL::Old_To_New_Order(INT old_i, INT inner) {
  Is_True(old_i>=0 && old_i<_num_loops,("Bad loop depth"));
  Is_True(inner>=0 && inner<_num_loops,("Bad inner loop depth"));
  
  if (old_i==inner) {
    return _num_loops-1;
  } else if (old_i>inner) {
    return old_i-1;
  } else {
    return old_i;
  }
}


void
LOOP_MODEL::Adjust_Vector_Regfiles (SYMBOL_TREE_NODE *sym_node)
{
  if (!sym_node) {
    return;
  }

  LNO_REGS_IDX vec = sym_node->Vec_Idx();
  if (vec != LNO_REGS_IDX_UNDEFINED && !_vec_regfiles->Count_Of_Idx(vec)) {
    _vec_regfiles->Count_Of_Idx(vec) = 1;
    
    /* Set the base count to 2 for all vector symbols. We do that only if we're
       the first to mark the register file as vector. Otherwise, we honor the
       default (or the ARRAY_REF) base counts. */
    if (_regclass_info->Idx_Source(vec) == LNO_REGCLASS_INFO::SRC_ISA_REGCLASS) {
      _base->Count_Of_Idx(vec) = 2;
    }
  }
  
  Adjust_Vector_Regfiles(sym_node->Left());
  Adjust_Vector_Regfiles(sym_node->Right());
}


void
LOOP_MODEL::Adjust_Vector_Regfiles (ARRAY_REF *arl_orig, SYMBOL_TREE *sym_tree)
{
  Is_True(_simd_loop != NULL, ("No SIMD loop."));

  INT inner = Num_Loops() - 1;

  /* Make a copy of 'arl_orig' so we can properly count the variant references. */
  ARRAY_REF *arl = CXX_NEW(ARRAY_REF(arl_orig, &Model_Local_Pool), &Model_Local_Pool);
  arl->Remove_Cse(inner, 0, 1);
  arl->Mark_Invariants(inner);
  
  /* Count the number of memory references for each (vector) type. */
  REG_REF_COUNT *rr = CXX_NEW(REG_REF_COUNT(_regclass_info, &Model_Local_Pool),
			      &Model_Local_Pool);
  BOOL outer_vec = (_simd_loop->Simd_Loop_Level() != inner);
  arl->Calc_Regs_And_Refs(rr, outer_vec);
  
  if (debug_model) {
    fprintf(TFile,"Adjust vector regfiles (array references):\n");
    rr->Print(TFile, 2);
  }
  
  /* Adjust the base registers to not count selection and alignment regs, they're 
     handled separately. Set Vector registers to 3 * memory units for aligned regs
     and 4 * mem otherwise. If a vector register file has no memory references
     inside the loop, set the base usage to 2. */
  for (INT i = 0; i < arl->Elements(); i++) {
    ARRAY_REF_ITER iter(arl->Array_Ref_List(i));
    for (ARRAY_REF_NODE* node = iter.First(); node; node = iter.Next()) {
      LNO_REGS_IDX align = node->Align();
      if (align != LNO_REGS_IDX_UNDEFINED) {
	_base->Count_Of_Idx(align) = 0;
	_align_regfiles->Count_Of_Idx(align) = 1;
      }
      
      LNO_REGS_IDX sel = node->Sel();
      if (sel != LNO_REGS_IDX_UNDEFINED) {
	_base->Count_Of_Idx(sel) = 0;
      }

      LNO_REGS_IDX vec = node->Vec();
      if (vec != LNO_REGS_IDX_UNDEFINED) {
	bool first_visit = !_vec_regfiles->Count_Of_Idx(vec);
	_vec_regfiles->Count_Of_Idx(vec) = 1;
	
	/* No base registers for autotie types. */
	if (_regclass_info->Idx_Source(vec) == LNO_REGCLASS_INFO::SRC_ISA_REGCLASS) {
	  INT new_base = 2;
	  
	  if (rr->refs.Count_Of_Idx(vec) > 0) {
	    new_base = ((align == LNO_REGS_IDX_UNDEFINED) ? 3 : 4) * (INT)_num_mem_units;
	    if (!first_visit) {
	      new_base = MAX(new_base, _base->Count_Of_Idx(vec));
	    }
	  }
	  
	  _base->Count_Of_Idx(vec) = new_base;
	}
      }
    }
  }

  Adjust_Vector_Regfiles(sym_tree->Symbol_Node());
  
  if (debug_model) {
    fprintf(TFile, "Adjusted base: ");
    _base->Print(TFile);
    fprintf(TFile, "\n");
    
    fprintf(TFile, "Vector regfiles: ");
    _vec_regfiles->Print(TFile);
    fprintf(TFile, "\n");
    
    fprintf(TFile, "Alignment regfiles: ");
    _align_regfiles->Print(TFile);
    fprintf(TFile, "\n");
  }
}


// ------------------------------------------------------------
// Count the number of references that are likely to be hoisted
// out of the inner loop by minvar algorithm. A reference that 
// has both a load and a store is counted double.
// ------------------------------------------------------------
static INT
Num_Invariant_Refs(ARRAY_REF_LIST* arl, INT loop)
{
  if (debug_model) {
    arl->Base_Array->Print(TFile); 
    fprintf(TFile,"\n");
  }
  INT num_invar_refs = 0;
  arl->Remove_Cse(loop, 0, 1);
  arl->Mark_Invariants(loop);
  ARRAY_REF_ITER iter(arl);
  for (ARRAY_REF_NODE* node = iter.First(); node; node = iter.Next()) {
    if (node->_is_invariant) {
      INT regs_per_ref = 1;
      if (node->_has_store) {
        num_invar_refs += regs_per_ref;
      }
      if (node->_has_load) {
        num_invar_refs += regs_per_ref;
      }
      if (debug_model) {
        fprintf(TFile, "  INV");
        if (node->_has_load)
	  fprintf(TFile, " LOAD");
        if (node->_has_store)
	  fprintf(TFile, " STORE");
	if (node->Vec() != LNO_REGS_IDX_UNDEFINED) {
	  fprintf(TFile, " VEC (%d x [%d])", node->Vec_Regs(), node->Vec());
	  if (node->Align() != LNO_REGS_IDX_UNDEFINED) {
	    fprintf(TFile, " ALIGN");
	  }
	  if (node->Sel() != LNO_REGS_IDX_UNDEFINED) {
	    fprintf(TFile, " SEL");
	  }
	}
	fprintf(TFile, " ");
        node->Print(TFile);
      }
    }
    else {
      if (debug_model) {
        fprintf(TFile, "  NOINV\n");
      }
      return 0;
    }
  }
  return num_invar_refs;
}

static INT
Num_Invariant_Refs(ARRAY_REF* ar, INT loop)
{
  INT num_invar_refs = 0;
  ARRAY_REF local_ar(ar, &Model_Local_Pool);
  for (INT i = 0; i < local_ar.Elements(); i++) {
    num_invar_refs += Num_Invariant_Refs(local_ar.Array_Ref_List(i), loop);
  }
  return num_invar_refs;
}

/*---------------------------------------------------------------------------*
 * Scalar expansion overhead for 'inner' to be the innermost candidate       *
 *                                                                           *
 * for i                                                                     *
 *     for j                                                                 *
 *         x = ...                                                           *
 *         for k                                                             *
 *             for l                                                         *
 *                 ... = x                                                   *
 *                                                                           *
 *                                                                           *
 * If we pick 'i' to be the inner most, scalar expansion and loop            *
 * distribution are required:                                                *
 *                                                                           *
 * for i                                                                     *
 *     for j                                                                 *
 *         se_x[i][j] = ...                                                  *
 *                                                                           *
 * for j                                                                     *
 *     for k                                                                 *
 *         for l                                                             *
 *             for i                                                         *
 *                 ... = se_x[i][j]                                          *
 *                                                                           *
 * We estimate the overhead as 4 (store 1, load 2, other 1) per SE.          *
 * For the whole loop nest, the overhead per iteration is hence:             *
 *    4/(iter_k * iter_l)                                                    *
 *                                                                           *
 *---------------------------------------------------------------------------*/
#define OVERHEAD_PER_SE  4.0
static double
SE_Overhead(INT inner, INT num_loops, SX_INFO* pi, INT64 *est_num_iter)
{
  double   overhead = 0.0;
  SX_PITER ii(&pi->Plist);
  SX_PNODE *sn;
  for (sn = ii.First(); sn != NULL; sn = ii.Next()) {
    SX_PNODE::STATUS status = sn->Transformable(inner);
    if (status == SX_PNODE::SE_REQD) {
      INT    e_depth = sn->Expansion_Depth();
      double inner_iters = 1.0;
      for (INT i = e_depth + 1; i < num_loops; ++i) {
	inner_iters = inner_iters * est_num_iter[i];
      }
      overhead = overhead + OVERHEAD_PER_SE/inner_iters;
    }
  }
  return overhead;
}

// model the nest assumming inner is the inner loop
void 
LOOP_MODEL::Try_Inner(BOOL* can_be_unrolled, 
                      INT outermost_can_be_tiled,
                      INT inner, 
                      INT num_loops,
		      SX_INFO *pi) {
  INT i, j;
  
  MEM_POOL_Push(&Model_Local_Pool);
  
  if (debug_model) {
    fprintf(TFile,"\n-- Trying LOOP %d for inner\n",inner);
  }
  
  _model_limit = MODEL_LIMIT_UNSET;
  
  INT* unroll_factors = CXX_NEW_ARRAY(INT, num_loops, &Model_Local_Pool);
  for (i = 0; i < num_loops; i++) {
    unroll_factors[i] = 1;
    _block_number_inner[i] = 1;
  }
  
  _regs_inner = CXX_NEW(LNO_REGS(_regclass_info,&Model_Local_Pool),
			&Model_Local_Pool);
  _refs_inner = CXX_NEW(LNO_REGS(_target,&Model_Local_Pool),
			&Model_Local_Pool);
  _refs_inner->Add(_target);
  
  ARRAY_REF* arl_for_cache =
    CXX_NEW(ARRAY_REF(_arl, &Model_Local_Pool), &Model_Local_Pool);
  
  // Estimate any scalar expansion overhead necessary for the interchange
  // This is done again within the ARRAY_REF class but the bias doesn't hurt
  // TODO: fix it
  _se_overhead = SE_Overhead(inner, num_loops, pi, _est_num_iterations);
  if (debug_model) {
    fprintf(TFile,"SE overhead for inner loop %d: %0.2f\n",inner,_se_overhead);
  }

  /* Try unrolling only if it is possible to register allocate
     and all operations have been included in the operation count. */
  bool try_unroll = (_OP_resource_count != NULL);
  if (try_unroll && !_at_analysis_phase) {
    /* Check if register allocatable. Don't perform the check
       during autotie analysis so we can get the correct register
       usage. */
    LNO_REGS *extra = CXX_NEW(LNO_REGS(_base, &Model_Local_Pool), &Model_Local_Pool);
    extra->Add(_non_temp_scalars);
    extra->Sub(_target);
    for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx < extra->Regs_Count(); idx++) {
      if (extra->Count_Of_Idx(idx) > 0) {
	try_unroll = false;
	break;
      }
    }
  }
  
  if (try_unroll) {
    _latency_cycles = 
      _lat_graph->Max_Cycle(inner, _loop_rcycles_unroll_by[Max_Unroll_Prod-1]);
    if (debug_model) {
      fprintf(TFile,"Latency cycles for inner loop %d: %0.2f\n",inner,_latency_cycles);
    }
    
    BOOL can_reg_allocate;
    BOOL try_further_unroll = TRUE;
    
    _num_cycles_inner = -1.0;
    
    _invariant_refs_inner = Num_Invariant_Refs(_arl, inner);
    if (debug_model) {
      fprintf(TFile, "Invariant refs for inner loop %d: %d\n",
	      inner, _invariant_refs_inner);
    }
    
    ARRAY_REF* new_arl =
      CXX_NEW(ARRAY_REF(_arl, &Model_Local_Pool), &Model_Local_Pool);
    
    new_arl->Remove_Cse(inner, Max_Cse_Dist, Find_Step(_wn, inner));
    new_arl->Mark_Invariants(inner);
    
    Try_Unroll(can_be_unrolled, inner, num_loops, unroll_factors,
	       /* start loop */ 0, /* unroll product */ 1,
	       &can_reg_allocate, &try_further_unroll, new_arl);
    
    for (i = 0; i < num_loops; i++)
      if (_block_number_inner[i] > 1)
	arl_for_cache->Unroll(i, _block_number_inner[i]);
  } else {
    // any greater than 0 value should be fine
    _num_cycles_inner = 0.01;
    _latency_cycles = 0;
  }
  
  arl_for_cache->Remove_Cse(inner, /* max_dist */ 0, /* step */ 1);
  
  _num_cycles_inner += _se_overhead;
  
  double cycles_per_iter;
  double overhead_cycles;
  
  double machine_cycles = _num_cycles_inner;
  
  FmtAssert(num_loops < 64, ("Impossibly large number of loops %d", num_loops));

  INT	legal_inners[64];
  INT	legal_tiles[64];
  INT*	pinners = legal_inners;
  INT*	ptiles = legal_tiles;
  
  // backwards or cache model gets confused
  for (i = num_loops - 1; i >= 0; i--) {
    if (_can_be_inner[i])
      *pinners++ = i;
    else if (i >= outermost_can_be_tiled)
      *ptiles++ = i;
    // FIXME: this code simply fixes the required permutation
    // to no permutation except the inner
    if (i==num_loops-1) {
      _new_order_inner[i]=inner;
    } else if (i>=inner) {
      _new_order_inner[i]=i+1;
    } else {
      _new_order_inner[i]=i;
    }
    //    _new_order_inner[i] = _required_permutation[i];
  }
  if (debug_model) {
    fprintf(TFile,("\nCACHE_MODEL (fixme): requesting permutation ("));
    for (i=0; i<num_loops; i++) {
      fprintf(TFile," %d",_new_order_inner[i]);
    }
    fprintf(TFile," )\n");
  }
  Cache_Model(arl_for_cache, num_loops-1, inner,
              legal_inners, pinners - legal_inners,
              legal_tiles, ptiles - legal_tiles,
              _block_number_inner, _do_stack,
              _blocking_disabled, _required_blocksize,
              _num_cycles_inner, _refs_inner->Total(),
	      _new_order_inner,
              &_nstrips_inner, &_stripdepth_inner,
              _iloop_inner, _stripsz_inner, _striplevel_inner,
              &cycles_per_iter, &overhead_cycles);
  
  if (LNO_Verbose || Debug_Cache_Model)
    printf("\nINNER %d -> %.2f cycles before cache ",
           inner, _num_cycles_inner);
  if (debug_model)
    fprintf(TFile,"\nINNER %d -> %.2f cycles before cache ",
	    inner, _num_cycles_inner);
  
  _num_cycles_inner += cycles_per_iter;
  
  double cache_cycles = _num_cycles_inner - machine_cycles - overhead_cycles;
  
  if (LNO_Verbose || Debug_Cache_Model)
    printf(" %.2f after cache\n", _num_cycles_inner);
  if (debug_model)
    fprintf(TFile," %.2f after cache\n", _num_cycles_inner);
  
  if (LNO_Verbose || Debug_Cache_Model) 
    printf("   %.2f cache cycles  %.2f overhead cycles\n", cache_cycles, 
	   overhead_cycles); 
  if (debug_model)
    fprintf(TFile, "   %.2f cache cycles  %.2f overhead cycles\n", cache_cycles, 
	    overhead_cycles); 
  
  if (_num_cycles == -1.0 || 
      (_num_cycles_inner >= 0.0 && _num_cycles_inner < _num_cycles) || 
      (_num_cycles_inner == _num_cycles &&
       (_regs_inner->Total() < _regs->Total() ||
        _unroll_prod_inner < _unroll_prod))) {
    // a new overall best
    _num_cycles = _num_cycles_inner;
    *_regs = *_regs_inner;
    *_refs = *_refs_inner;
    _unroll_prod = _unroll_prod_inner;
    _nstrips = _nstrips_inner;
    _stripdepth = _stripdepth_inner;
    _inner_loop = _inner_loop_inner;
    
    for (i = 0; i < num_loops; i++) {
      _new_order[i] = _new_order_inner[i];
      _block_number[i] = _block_number_inner[i];
    }
    
    for (j = 0; j < _nstrips; j++) {
      _iloop[j] = _iloop_inner[j];
      _stripsz[j] = _stripsz_inner[j];
      _striplevel[j] = _striplevel_inner[j];
    }
    
    if (debug_model) {
      fprintf(TFile,"OVERALL BEST\n");
    }
  }

  if (LNO_Analysis) {
    Model_Results_Analysis(inner, num_loops, outermost_can_be_tiled,
			   machine_cycles, cache_cycles, overhead_cycles);
  }

  MEM_POOL_Pop(&Model_Local_Pool);
}

// try different unrolling factors for loop 'l'
// unroll_factors gives the unrolling factors for the outermore loops
// unroll product gives the product of the unrolling factors for
// the outermore loops (we stop trying if the product is too large)
// on output set *can_reg_allocate to true if it was possible to
// register allocate given the outer unrolling factors
// on output set try_further_unroll to TRUE if there is a possible reason
// to try unrolling further (this gives us a short circuit)
// arl gives a list of all the array references unrolled by the outer 
// unrolling factors
void 
LOOP_MODEL::Try_Unroll(BOOL* can_be_unrolled, INT inner, INT num_loops,
                       INT* unroll_factors, INT l, INT unroll_product, 
                       BOOL* can_reg_allocate, BOOL* try_further_unroll,
                       ARRAY_REF* arl) {
  if (l >= num_loops) {
    // base case of the recursion -- traversed all loops
    Evaluate(inner, num_loops, unroll_factors, unroll_product,
             can_reg_allocate, try_further_unroll, arl, can_be_unrolled);
  } else if (l==inner || !can_be_unrolled[l]) {
    // don't unroll the inner loop or loops marked as non-unrollable --
    // go to the next loop
    Try_Unroll(can_be_unrolled, inner, num_loops, unroll_factors, l+1,
               unroll_product, can_reg_allocate, try_further_unroll, arl);
  } else {
    Is_True((l!=inner) && can_be_unrolled[l],("Loop %d can't be unrolled",l));
    INT known_unroll = 
      _required_unroll[l] ? _required_unroll[l] : LNO_Outer_Unroll;
    INT u = known_unroll ? known_unroll : 1;
    double prod = unroll_product;
    
    // Loop through all possible unrolling values as long as the product
    // stays small enough, we can still allocate registers and we haven't
    // reached a good enough schedule.
    // If we can't allocate with u=1, pass that up so that we won't further
    // unroll any outer loops
    BOOL can_allocate = TRUE;
    INT  max_unroll_prod = Max_Unroll_Prod;
    INT  max_unroll      = Max_Unroll_Prod;
    if (known_unroll) {
      max_unroll = MIN(Max_Unroll_Prod,known_unroll);
    } else {
      if (LNO_Outer_Unroll_Max)
        max_unroll = MIN(max_unroll,LNO_Outer_Unroll_Max);
      if (arl->Num_Bad()!=0 && _simd_loop && !LNO_Aligned_Accesses) {
	max_unroll = 1;
	if (debug_model) {
	  fprintf(TFile,"%d bad references, disabling unroll because of SIMD\n",
		  arl->Num_Bad());
	}
      }
      if (LNO_Outer_Unroll_Prod_Max)
        max_unroll_prod = MIN(max_unroll_prod,LNO_Outer_Unroll_Prod_Max);
      if (_est_num_iterations[l] > 0)
        max_unroll = MIN(max_unroll,_est_num_iterations[l]);
    }
    
    while (can_allocate &&
           *try_further_unroll &&
	   (unroll_product * u) <= max_unroll_prod &&
           u <= max_unroll) {
      if (!known_unroll &&
          _est_num_iterations[l] > 0 &&
          u != max_unroll &&
          u*2 > _est_num_iterations[l]) {
        // If the loop has 6 iterations, silly to unroll by 4 or 5.
        // TODO: However, we don't particularly want to do this unless
        // est_num_iters is exact.  Likewise, the setting of max_unroll
        // above should probably be based not on _est_num_iters but
        // some sort of max.
        u = max_unroll;
        continue;
      }
      {
	MEM_POOL_Push(&Model_Local_Pool);
	ARRAY_REF* new_arl;
	unroll_factors[l] = u;
	if (u > 1) {
	  new_arl = CXX_NEW(ARRAY_REF(arl,&Model_Local_Pool),&Model_Local_Pool);
	  new_arl->Unroll(l,u);
	  prod = unroll_product*u;
	} else {
	  new_arl = arl;
	}
	new_arl->Remove_Cse(inner, Max_Cse_Dist, Find_Step(_wn,inner));
	new_arl->Mark_Invariants(inner);
	Try_Unroll(can_be_unrolled, inner, num_loops, unroll_factors, l+1,
		   (int) prod, &can_allocate, try_further_unroll, new_arl);
	if ((u == 1) && !can_allocate) {
	  *can_reg_allocate = FALSE;  // don't let people above unroll further
	}
	MEM_POOL_Pop(&Model_Local_Pool);
      }
      u++;
    }
    unroll_factors[l] = 1;
  } 
}

// This coefficient is used to preference unrolling of multiple
// loops by factors that are similar in size. I don't have a good
// explanation for it, except for the empirical evidence that, e.g.,
// unrolling 4x4 is usually better that unroling 2x8 or 8x2.
double
LOOP_MODEL::Unequal_Unroll_Penalty_Coeff(INT num_loops, INT *unroll_factors) {
  double unequal_unroll_penalty = 1.0;
  if (num_loops >= 2) {
    INT un1, un2;
    if (unroll_factors[0] > unroll_factors[1]) {
      un1 = unroll_factors[0], un2 = unroll_factors[1];
    } else {
      un1 = unroll_factors[1], un2 = unroll_factors[0];
    }
    for (INT i = 2; i < num_loops; i++) {
      if (unroll_factors[i] > un1) {
        un2 = un1;
        un1 = unroll_factors[i];
      } else if (unroll_factors[i] > un2) {
        un2 = unroll_factors[i];
      }
    }
    unequal_unroll_penalty = pow(((double)un2)/un1, 0.3);
  }
  return unequal_unroll_penalty;
}

//  Given that we've unrolled the loop all we will (a factor of unroll
//  product), model this nest, setting inner_loop, num_cycles and
//  block_number if we've found a new best candidate.  Set *can_reg_allocate
//  to true if we don't use too many registers.  Set *try_further_unroll
//  to false if further unrolling won't help (we're doing good enough)
//  on input, arl gives all the array references after unrolling
void 
LOOP_MODEL::Evaluate(INT inner, INT num_loops, INT* unroll_factors,
                     INT unroll_product, BOOL* can_reg_allocate, 
                     BOOL* try_further_unroll, ARRAY_REF* arl,
                     BOOL* can_be_unrolled) {
  _num_evaluations++;
  
  if (debug_model) {
    fprintf(TFile, "\nEVALUATION %d, unroll factors (",_num_evaluations);
    for (INT b = 0; b < num_loops; b++) {
      fprintf(TFile, " %d", unroll_factors[b]);
    }
    fprintf(TFile, " )\n");
  }
  
  bool did_unroll = false;
  for (INT b = 0; b < num_loops && !did_unroll; b++) {
    if (unroll_factors[b] > 1 &&
        unroll_factors[b] != _required_unroll[b] &&
        !(_required_unroll[b] <= 0 && unroll_factors[b] == LNO_Outer_Unroll)) {
      did_unroll = true;
      break;
    }
  }

  MODEL_LIMIT this_limit = MODEL_LIMIT_UNSET;
  
  // Count the number of registers 
  REG_REF_COUNT *rr = CXX_NEW(REG_REF_COUNT(_regclass_info,&Model_Local_Pool),
			      &Model_Local_Pool);
  BOOL outer_vec = _simd_loop && (_simd_loop->Simd_Loop_Level() != inner);
  arl->Calc_Regs_And_Refs(rr,outer_vec);
  
  if (debug_model) {
    fprintf(TFile,"Array reference based stats:\n");
    rr->Print(TFile,2);
  }

  double unequal_unroll_penalty = Unequal_Unroll_Penalty_Coeff(num_loops,unroll_factors);
  
  // Here's what goes into estimating minvar benefit:
  //
  // _invariant_refs_inner:
  //   number of hoistable memory references for the current inner loop
  // unroll_product
  //   because Inner_Invariant_Refs is computed prior to unrolling
  // unequal_unroll_penalty
  //   to favor "square/cube" unrolling factors
  // _invariant_ref_coeff
  //   based on the total number of array references and Max_Unroll_Prod
  // MINVAR_MAGIC_COEFF
  //   limit the potential benefit to 20% of the ideal cycle count
  
#define MINVAR_MAGIC_COEFF (0.20 * _loop_rcycles_unroll_by[Max_Unroll_Prod-1])
  
  double minvar_benefit = MINVAR_MAGIC_COEFF *
    _invariant_refs_inner *
    unroll_product *
    unequal_unroll_penalty *
    _invariant_ref_coeff;
  
  if (debug_model) {
    fprintf(TFile,"Memory invariant removal benefit: %.3f\n",minvar_benefit);
    fprintf(TFile,
	    "  coeff %.3f * inv_refs %d * unr_prod %d * "
	    "uneq_unr_penalty %.3f * inv_ref_coeff %.3f\n",
	    MINVAR_MAGIC_COEFF,
            _invariant_refs_inner,
	    unroll_product,
	    unequal_unroll_penalty,
	    _invariant_ref_coeff);
  }
  
  LNO_REGS *usage = CXX_NEW(LNO_REGS(_base,&Model_Local_Pool),&Model_Local_Pool);

  
  // don't count invariants both as invariants and as pipelines
  if (_regclass_info->Index_Of_Type(MTYPE_F4) != 
		    _regclass_info->Index_Of_Type(MTYPE_I4)) { // HW FP
    if (rr->inv_stores.Count_Of_Float() > 4*rr->var_stores.Count_Of_Float()) {
      usage->Count_Of_Float() /= 3;  
    }
    // complex multiplies will use additional temporary FP registers 
    rr->regs.Count_Of_Float() += (Num_Complex_Mpy * unroll_product);
  }  
  
  if (_est_num_iterations[inner] <= LNO_Small_Trip_Count) {
    // loops with small trip counts will not be pipelined
    usage->Clear();
  }
  
  *can_reg_allocate = TRUE;
  
  usage->Add(rr->regs);
  usage->Add(_non_temp_scalars);
  
  if (debug_model) {
    fprintf(TFile,"Register usage: ");
    usage->Print(TFile);
    fprintf(TFile,"\n");
  }
  
  INT spills = 0;
  bool too_close = false;
  for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx < usage->Regs_Count(); idx++) {
    INT extra = usage->Count_Of_Idx(idx) - _target->Count_Of_Idx(idx);
    if (extra > 0) {
      if (!did_unroll) {
	// FIXME: this should be smarter
	/*
	  double fp_refs_per_reg = (_array_refs.fp_regs + _scalar_refs.fp_regs) /
	  (rr.regs.fp_regs + _scalar.fp_regs);
	  spills.fp_regs = (INT)(extra.fp_regs * fp_refs_per_reg);
	*/
	INT extra_refs = extra * 2;
	rr->refs.Count_Of_Idx(idx) += extra_refs;
	spills += extra_refs;
      }
      *can_reg_allocate = FALSE;
    }

    /* Don't penalize purely alignment register files for being too close
       to unallocatable. */
    if (extra > 0 ||
	_align_regfiles->Count_Of_Idx(idx) == 0 ||
	_vec_regfiles->Count_Of_Idx(idx) > 0)
    {
      double ratio = ((double)extra) / _target->Count_Of_Idx(idx);
      if (ratio > -0.125) {
	too_close = true;
      }
    }
  }
  
  // count memory references
  INT total_refs = rr->refs.Total();
  double MEM_issue = ((double)total_refs) / unroll_product;
  double MEM_rcycles = MEM_issue/_num_mem_units;
  if (debug_model) {
    fprintf(TFile, "MEM_rcycles: %.2f (refs %d / unroll_product %d / mem_units %.0f)\n",
	    MEM_rcycles, total_refs, unroll_product, _num_mem_units);
    fprintf(TFile, "Issue: (op) %.0f + (loop) %.0f + (mem) %.0f = %.0f\n", 
            _OP_issue * unroll_product, 
            _LOOP_INIT_issue, 
            MEM_issue * unroll_product,
            _OP_issue * unroll_product +
            _LOOP_INIT_issue +
            MEM_issue * unroll_product);
  }
  
  double MEM_issue_minus_spills = 
    ((double)(total_refs - spills)) / unroll_product;
  double MEM_rcycles_minus_spills = MEM_issue_minus_spills/_num_mem_units;
  
  double issue_limit =
    (_OP_issue + _LOOP_INIT_issue/unroll_product + MEM_issue) / _issue_rate;
  double issue_limit_minus_spills = 
    (_OP_issue + _LOOP_INIT_issue/unroll_product + MEM_issue_minus_spills) /
    _issue_rate;
  double ideal_resource_cycles = _loop_rcycles_unroll_by[Max_Unroll_Prod-1];
  double resource_cycles = 
    MAX(_loop_rcycles_unroll_by[unroll_product-1],
        MAX(MEM_rcycles, issue_limit)) - minvar_benefit;
  double resource_cycles_minus_spills =
    MAX(_loop_rcycles_unroll_by[unroll_product-1],
        MAX(MEM_rcycles_minus_spills, issue_limit_minus_spills));
  
  // is memory or int still a problem, if not, we shouldn't unroll anymore
  if (!(*can_reg_allocate) ||
      (resource_cycles > ideal_resource_cycles) ||
      (minvar_benefit > 0.0) ||
      (_latency_cycles/unroll_product > resource_cycles)) {
    *try_further_unroll = TRUE;  // latency bound so unrolling can help
  } else {
    *try_further_unroll = FALSE; // doing well enough
    this_limit = MODEL_LIMIT_IDEAL;
  }
  
#define SWP_INSTR_LIMIT 50
  
  // Unrolling should not create loops that are too big for SWP
  if (unroll_product>1 &&
      issue_limit * _issue_rate * unroll_product > SWP_INSTR_LIMIT) {
    if (debug_model) {
      fprintf(TFile, "Unrolled loop would be too large for SWP\n");
    }
    *try_further_unroll = FALSE;
    return;
  }
  
  double cycles = MAX(resource_cycles, _latency_cycles/unroll_product);
  double cycles_minus_spills = MAX(resource_cycles_minus_spills,
                                   _latency_cycles/unroll_product);
  
  if (debug_model) {
    fprintf(TFile, "Resource cycles: %.2f\n", resource_cycles);
    fprintf(TFile, "Latency cycles: %.2f\n", _latency_cycles/unroll_product);
    fprintf(TFile, "Cycles: %.2f\n", cycles);
  }
  
  if (!(*can_reg_allocate) && 
      !did_unroll && 
      cycles == cycles_minus_spills) { 
    // spilling is free so set the number of registers to target
    *usage = *_target;
    too_close = true;
  }
  
  if (!*can_reg_allocate) {
    if (did_unroll) {
      if (debug_model) {
	fprintf(TFile,"CAN'T register allocate\n");
      }
      return;
    }
  } else if (too_close) {
    cycles *= 1.1;  // penalty for being close to unallocatable
    if (debug_model) {
      fprintf(TFile, "Cycles (too close +10%): %.2f\n", cycles);
    }
  }
  
  // Penalty for unrolling something that's not
  // a factor of the estimated number of iterations
  // TODO: the right thing to do is to average in 
  // the percentage of time spent in cleanup
  double worst_mod = 0.0;
  for (INT l = 0; l < num_loops; l++) {
    if(unroll_factors[l] &&
       _est_num_iterations[l] > 0 &&
       _est_num_iterations[l] < LNO_Outer_Unroll_Max) {
      double this_mod = (_est_num_iterations[l] % unroll_factors[l]);
      if (this_mod > _est_num_iterations[l]*worst_mod) {
	worst_mod = this_mod/_est_num_iterations[l];
      }
    }
  }
  if (worst_mod > 0.1) {
    cycles *= 1.05;

    if (debug_model) {
      fprintf(TFile, "Cycles (unroll factor +5%): %.2f\n", cycles);
    }
  }
  
#ifdef TARG_IA64
  // For IA-64 we prefer an even number of FP ops,
  // but we don't penalize small loops that can be unrolled in CG
  INT num_fp_ops = _OP_issue * unroll_product;
  if ((num_fp_ops & 1) &&
      issue_limit * _issue_rate * unroll_product > SWP_INSTR_LIMIT / 2) {
    cycles *= 1.05;
  }
#endif
  
  if (debug_model) {
    fprintf(TFile,"Before cache model, %.2f cycles, ", cycles);
    usage->Print(TFile);
    fprintf(TFile,"\n");
  }

  if (this_limit == MODEL_LIMIT_IDEAL) {
    if (debug_model) {
      fprintf(TFile,"  ** ideal schedule **\n");
    }
  } else if (resource_cycles > _latency_cycles/unroll_product) {
    this_limit = MODEL_LIMIT_RES;
    if (debug_model) {
      fprintf(TFile,"  ** cycles are resource bound\n");
    }
  } else {
    this_limit = MODEL_LIMIT_LAT;
    if (debug_model) {
      fprintf(TFile,"  ** cycles are latency bound\n");
    }
  }
  
  // new best for this inner loop
  if (_num_cycles_inner == -1.0 || // no solution yet
      1.01*cycles < _num_cycles_inner || // 10% faster
      (cycles <= _num_cycles_inner && // somewhat faster and better unroll or regs
       (unroll_product < _unroll_prod_inner ||
        usage->Total()<_regs_inner->Total()))) {
    _inner_loop_inner = inner;
    for (INT i=0; i<num_loops; i++) {
      _block_number_inner[i] = unroll_factors[i];
    }
    _num_cycles_inner = cycles;
    *_regs_inner = *usage;
    *_refs_inner = rr->refs;
    _unroll_prod_inner = unroll_product;
    _model_limit = this_limit;
    
    if (debug_model) {
      fprintf(TFile,"NEW BEST for inner loop %d\n",inner);
    }
  }
}

// Walk through the inner loop 
// return how many cycles the loop takes based on the resource requirements 
// of the floating point instructions
// also set num_instr to the number of floating point instructions
// return -1.0 on error (something unmodelable)
TI_RES_COUNT* 
LOOP_MODEL::OP_Resources(WN* wn,
                         double* num_instr,
                         INVAR_TABLE *invar_table)
{
  TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(&Model_Local_Pool);
  if (OP_Resources_R(wn, resource_count, num_instr, invar_table) == -1) {
    return(NULL);
  }
  return(resource_count);
}

// Similar to above but apply it to the statements in the REGISTER_MODEL
// Another difference is that the loop init instruction is already
// included because we are not going to unroll anymore
double 
LOOP_MODEL::OP_Cycles(REGISTER_MODEL* rmodel,
                      double* num_instr,
                      INVAR_TABLE *invar_table,
                      MEM_POOL* pool)
{
  TI_RES_COUNT* resource_count = TI_RES_COUNT_Alloc(pool);
  for (INT i = 0; i < rmodel->Num_Statements(); i++) {
    if (OP_Resources_R(rmodel->Statement(i),
                       resource_count,
                       num_instr,
                       invar_table) == -1) {
      return -1.0;
    }
  }
  LNOTARGET_Loop_Inc_Test_Res(resource_count);
  return TI_RES_COUNT_Min_Cycles(resource_count);
}


// Assume that INTCONST * INDEX_VARIABLE will be strength reduced
static BOOL
Multiply_Will_Be_Strength_Reduced(WN* wn)
{
  Is_True(WN_operator(wn) == OPR_MPY, ("Expected an OPR_MPY node"));
  WN* ldid;
  if (WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
    ldid = WN_kid1(wn);
  }
  else if (WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
    ldid = WN_kid0(wn);
  }
  else {
    return FALSE;
  }
  if (WN_operator(ldid) != OPR_LDID) {
    return FALSE;
  }
  for (WN* parent = ldid; parent; parent = LWN_Get_Parent(parent)) {
    if (WN_operator(parent) == OPR_DO_LOOP) {
      WN* idx_var = WN_index(parent);
      if (WN_st_idx(idx_var) == WN_st_idx(ldid)) {
        return TRUE;
      }
    }
  }
  return FALSE;
}


// mips4+ and IA-64 ISAs have multiply-adds
static inline BOOL
Target_ISA_Has_Madd()
{
  return (Is_Target_ISA_M4Plus() || Is_Target_ISA_I1Plus());
}


// walk the code, update resource_count and num_instr 
// for every floating point opcode
// par_v is the vertex number of the parent of wn, if any
// return -1 on error
INT 
LOOP_MODEL::OP_Resources_R(WN* wn,
                           TI_RES_COUNT* resource_count, 
                           double* num_instr,
                           INVAR_TABLE *invar_table)
{
  TOP top;
  OPERATOR oper = WN_operator(wn);
  TYPE_ID rtype = WN_rtype(wn);
  TYPE_ID  desc = WN_desc(wn);
  
  if (OPERATOR_is_leaf(oper)) {
    return (1);
  } 

  if (oper == OPR_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      if (OP_Resources_R(kid,resource_count,num_instr,invar_table) == -1) {
	return -1;
      }
      kid = WN_next(kid);
    }
    return 1;
  } 

  if (invar_table) { // no cost to invariant expressions
    BIT_VECTOR* bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      return 1;
    }
  }

  if (Is_Target_Xtensa()) {
    // FIXME: for now, only optimize loops with integer or hw fp types
    // For hw fp, just do a very simple thing of counting each op as 1
    if (xt_hard_float && (desc == MTYPE_F4 || rtype == MTYPE_F4)) {
      if (oper == OPR_ADD || oper == OPR_SUB || oper == OPR_MPY ||
		 oper == OPR_TRUNC || oper == OPR_FLOOR ||
		 oper == OPR_CEIL || oper == OPR_ABS || oper == OPR_NEG ) { 
        *num_instr += 1.0;
      } else if (OPERATOR_is_load(oper) || OPERATOR_is_store(oper) ||
		      oper == OPR_CONST) {
      } else {
        return -1;
      }
      for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
        WN* kid = WN_kid(wn,kidno);
        if (OP_Resources_R(kid, resource_count, num_instr, invar_table) == -1) {
          return -1;
	}
      }
      return 1;
    } else if (desc != MTYPE_V &&
	!MTYPE_is_integral(desc) &&
	!MTYPE_is_pointer(desc) &&
	!MTYPE_is_boolean(desc) &&
	!MTYPE_is_tie(desc)) {
      return -1;
    }
    if (rtype != MTYPE_V &&
	!MTYPE_is_integral(rtype) &&
	!MTYPE_is_pointer(rtype) &&
	!MTYPE_is_boolean(rtype) &&
	!MTYPE_is_tie(rtype)) {
      return -1;
    }
  }
    
  if (oper == OPR_CVT   || 
      oper == OPR_RND   ||
      oper == OPR_CEIL  || 
      oper == OPR_TRUNC || 
      oper == OPR_FLOOR) {
    if (OP_Cycles_Cvt(WN_opcode(wn), resource_count, num_instr) == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_INTRINSIC_OP || oper == OPR_INTRINSIC_CALL) {
    INTRINSIC intrn = (INTRINSIC) WN_intrinsic(wn);
    if (INTRN_is_tie_intrinsic(WN_intrinsic(wn))) {
      // TIE intrinsics
      double inst = LNOTARGET_Tie_Intrinsic_Res (resource_count,intrn);
      if (inst<0.0) {
	return -1;
      }
      *num_instr += inst;
    } else if (oper == OPR_INTRINSIC_OP &&
	       FP_Cycles_Intrinsic(wn, resource_count, num_instr) == -1) {
      return -1;
    }
  } else if (oper == OPR_REALPART || 
	     oper == OPR_IMAGPART ||
	     oper == OPR_PAREN    || 
	     oper == OPR_PARM     ||
	     oper == OPR_OUTPART) {
    // no-ops
  } 
  else if (OPERATOR_is_expression(oper) && 
           !OPERATOR_is_load(oper) &&
           oper != OPR_CONST) {
    // an fp expression
    if (desc  == MTYPE_FQ || 
        desc  == MTYPE_CQ || 
        rtype == MTYPE_FQ ||
        rtype == MTYPE_CQ) {
      return -1;
    } 
    // regular floating point
    else if (desc  == MTYPE_F4 || 
             desc  == MTYPE_F8 ||
             rtype == MTYPE_F4 ||
             rtype == MTYPE_F8) {
      // multiply-adds
      if (Target_ISA_Has_Madd() && 
          (oper == OPR_ADD || oper == OPR_SUB) && 
          (WN_operator(WN_kid0(wn)) == OPR_MPY || 
           WN_operator(WN_kid1(wn)) == OPR_MPY)) { 
        return FP_Cycles_Madd(wn, resource_count, num_instr, invar_table);
      } 
      else if (oper == OPR_MAX || oper == OPR_MIN) {
        *num_instr += LNOTARGET_FP_Min_Max_Res(resource_count, rtype);
      } 
      else if (oper == OPR_SQRT) {
        *num_instr += LNOTARGET_FP_Sqrt_Res(resource_count, rtype);
      } 
      else if ((top = LNOTARGET_Whirl_To_Top(wn)) != TOP_UNDEFINED) {
        *num_instr += 1.0;
	TI_RES_COUNT_Add_Op_Resources(resource_count, top);
      }
      else if (oper == OPR_DIV) {
        *num_instr += LNOTARGET_FP_Div_Res(resource_count, rtype);
      } 
      else if (oper == OPR_RECIP) {
        *num_instr += LNOTARGET_FP_Recip_Res(resource_count, rtype);
      } 
      else if (oper == OPR_RSQRT) {
        *num_instr += LNOTARGET_FP_Rsqrt_Res(resource_count, rtype);
      } 
      else {
        return -1;
      }
    } 
    else if (desc  == MTYPE_C4 || 
             desc  == MTYPE_C8 ||
             rtype == MTYPE_C4 ||
             rtype == MTYPE_C8) {
      if (oper == OPR_ADD || oper== OPR_SUB) {
        *num_instr += LNOTARGET_Complex_Add_Res(resource_count, rtype);
      } 
      else if (oper == OPR_MPY)  {
        *num_instr += LNOTARGET_Complex_Mult_Res(resource_count, rtype);

        // Count the number of "true" complex multiplies (no zeros)
        // because they require additional FP temp registers
        WN* kid0 = WN_kid0(wn);
        WN* kid1 = WN_kid1(wn);
        if ((WN_operator(kid0) != OPR_COMPLEX 
             || ((WN_operator(WN_kid0(kid0)) != OPR_CONST 
                  || !Targ_Is_Zero(STC_val(WN_st(WN_kid0(kid0))))) 
                 && (WN_operator(WN_kid1(kid0)) != OPR_CONST 
                     || !Targ_Is_Zero(STC_val(WN_st(WN_kid1(kid0)))))))
            &&
            (WN_operator(kid1) != OPR_COMPLEX 
             || ((WN_operator(WN_kid0(kid1)) != OPR_CONST 
                  || !Targ_Is_Zero(STC_val(WN_st(WN_kid0(kid1))))) 
                 && (WN_operator(WN_kid1(kid1)) != OPR_CONST 
                     || !Targ_Is_Zero(STC_val(WN_st(WN_kid1(kid1))))))))
        {
          Num_Complex_Mpy++;
        }
      } 
      else if (oper == OPR_NEG) {
        *num_instr += LNOTARGET_Complex_Neg_Res(resource_count, rtype);
      }
      else if (oper == OPR_COMPLEX)  {
	; // not really a floating-point op
      } 
      else {
	return -1;
      }

    } 
    else if (desc  == MTYPE_B  || 
             desc  == MTYPE_I1 ||
             desc  == MTYPE_I2 ||
             desc  == MTYPE_I4 || 
             desc  == MTYPE_I8 || 
             desc  == MTYPE_U1 || 
             desc  == MTYPE_U2 || 
             desc  == MTYPE_U4 || 
             desc  == MTYPE_U8 ||
             rtype == MTYPE_B  || 
             rtype == MTYPE_I1 || 
             rtype == MTYPE_I2 ||
             rtype == MTYPE_I4 || 
             rtype == MTYPE_I8 || 
             rtype == MTYPE_U1 || 
             rtype == MTYPE_U2 || 
             rtype == MTYPE_U4 || 
             rtype == MTYPE_U8) {

      BOOL double_word = (MTYPE_is_double_size(desc) || MTYPE_is_double_size(rtype));

      switch (oper) {
        case OPR_ARRAY: 
          return 1;
        case OPR_INTRINSIC_OP: 
          return -1;
        case OPR_TAS: 
          (*num_instr)++; 
          break;
        case OPR_SELECT:
          *num_instr += LNOTARGET_Int_Select_Res(resource_count);
          break;
        case OPR_CVTL:
          *num_instr += LNOTARGET_Int_Cvtl_Res(resource_count);
          break;
        case OPR_NEG: 
          *num_instr += LNOTARGET_Int_Neg_Res(resource_count, double_word);
          break;
        case OPR_ABS:
          *num_instr += LNOTARGET_Int_Abs_Res(resource_count, double_word);
          break;
        case OPR_PAREN: 
          break;
        case OPR_BNOT: 
          *num_instr += LNOTARGET_Int_Bnot_Res(resource_count);
          break;
        case OPR_LNOT: 
          *num_instr += LNOTARGET_Int_Lnot_Res(resource_count);
          break;
        case OPR_MPY: 
          if (!Multiply_Will_Be_Strength_Reduced(wn)) {
            *num_instr += LNOTARGET_Int_Mult_Res(resource_count, wn);
            break;
          }
          // if multiply will be strength reduced
          // fall through to OPR_ADD
        case OPR_ADD: 
          *num_instr += LNOTARGET_Int_Add_Res(resource_count, double_word);
          break;
        case OPR_SUB:  
          *num_instr += LNOTARGET_Int_Sub_Res(resource_count, double_word);
          break;
        case OPR_DIV: 
          *num_instr += LNOTARGET_Int_Div_Res(resource_count, double_word);
          break;
        case OPR_MOD: 
          *num_instr += LNOTARGET_Int_Mod_Res(resource_count, double_word);
          break;
        case OPR_REM: 
          *num_instr += LNOTARGET_Int_Rem_Res(resource_count, double_word);
          break;
        case OPR_DIVREM: 
          *num_instr += LNOTARGET_Int_DivRem_Res(resource_count, double_word);
          break;
        case OPR_MAX:
        case OPR_MIN:
        case OPR_MINMAX:
          *num_instr += LNOTARGET_Int_Min_Max_Res(resource_count,
                                                  oper == OPR_MINMAX);
          break;
        case OPR_BAND: 
          *num_instr += LNOTARGET_Int_Band_Res(resource_count);
          break;
        case OPR_BIOR: 
          *num_instr += LNOTARGET_Int_Bior_Res(resource_count);
          break;
        case OPR_BNOR: 
          *num_instr += LNOTARGET_Int_Bnor_Res(resource_count);
          break;
        case OPR_BXOR: 
          *num_instr += LNOTARGET_Int_Bxor_Res(resource_count);
          break;
        case OPR_LAND: 
          *num_instr += LNOTARGET_Int_Land_Res(resource_count);
          break;
        case OPR_CAND: 
          *num_instr += LNOTARGET_Int_Cand_Res(resource_count);
          break;
        case OPR_LIOR: 
          *num_instr += LNOTARGET_Int_Lior_Res(resource_count);
          break;
        case OPR_CIOR: 
          *num_instr += LNOTARGET_Int_Cior_Res(resource_count);
          break;
        case OPR_SHL: 
          *num_instr += LNOTARGET_Int_Shl_Res(resource_count, double_word); 
          break;
        case OPR_ASHR: 
          *num_instr += LNOTARGET_Int_Ashr_Res(resource_count, double_word); 
          break;
        case OPR_LSHR: 
          *num_instr += LNOTARGET_Int_Lshr_Res(resource_count, double_word); 
          break;
        case OPR_EQ:  
          *num_instr += LNOTARGET_Int_Eq_Res(resource_count); 
          break;
        case OPR_NE:  
          *num_instr += LNOTARGET_Int_Ne_Res(resource_count); 
          break;
        case OPR_GT:  
          *num_instr += LNOTARGET_Int_Gt_Res(resource_count); 
          break;
        case OPR_GE: 
          *num_instr += LNOTARGET_Int_Ge_Res(resource_count); 
          break;
        case OPR_LT: 
          *num_instr += LNOTARGET_Int_Lt_Res(resource_count); 
          break;
        case OPR_LE: 
          *num_instr += LNOTARGET_Int_Le_Res(resource_count); 
          break;
        case OPR_LDA:
          *num_instr += LNOTARGET_Int_Lda_Res(resource_count); 
          break;
      case OPR_INTCONST: 
        break;
      default: 
        DevWarn("Unknown whirl in LOOP_MODEL::OP_Resources_R");
        return -1;
      }
    }
  }

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    WN* kid = WN_kid(wn,kidno);
    if (OP_Resources_R(kid, resource_count, num_instr, invar_table) == -1) {
      return -1;
    }
  }
  return 1;
}

// deal with a floating point add/subtract with a multiply kid
// we're assuming this will later be turned in to a madd
// on entry, wn points to the fp add or subtract
// return -1 on error
INT 
LOOP_MODEL::FP_Cycles_Madd(WN* wn, 
                           TI_RES_COUNT* resource_count, 
                           double* num_fp_instr,
                           INVAR_TABLE *invar_table)
{
  // first add the resources of the madd
  *num_fp_instr += LNOTARGET_FP_Madd_Res(resource_count, WN_rtype(wn));

  WN *kid0 = WN_kid0(wn);
  WN *kid1 = WN_kid1(wn);

  WN *non_mult_kid;
  WN *mult_kid0;
  WN *mult_kid1;

  // now find the resources of the three kids (the non-multiply kid
  // of the add/subtract and the two kids of the multiply
  if (WN_operator(kid0) == OPR_MPY) {
    non_mult_kid = kid1;
    mult_kid0 = WN_kid0(kid0);
    mult_kid1 = WN_kid1(kid0);
  } 
  else {
    non_mult_kid = kid0;
    mult_kid0 = WN_kid0(kid1);
    mult_kid1 = WN_kid1(kid1);
  }

  if (OP_Resources_R(non_mult_kid,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }

  if (OP_Resources_R(mult_kid0,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }
  if (OP_Resources_R(mult_kid1,
                     resource_count,
                     num_fp_instr,
                     invar_table) == -1) {
    return -1;
  }
  return 1;
}

// Deal with a cvt
// return -1 on error (something we can't handle)
INT 
LOOP_MODEL::OP_Cycles_Cvt(OPCODE opcode, 
                          TI_RES_COUNT* resource_count, 
                          double* num_instr)
{
  double instr = LNOTARGET_Cvt_Res(resource_count, opcode);
  if (instr < 0.0) {
    return -1;
  }
  *num_instr += instr;
  return 1;
}

// Deal with an intrinsic
// Currently we only handle exponentiation by a small number
// return -1 on error (something we can't handle)
INT 
LOOP_MODEL::FP_Cycles_Intrinsic(WN* wn, 
                                TI_RES_COUNT* resource_count, 
                                double* num_fp_instr)
{
  if (WN_kid_count(wn) != 2) {
    return -1;
  }
  WN *const_kid = WN_kid1(wn);
  if (WN_operator(const_kid) == OPR_PARM) {
    const_kid = WN_kid0(const_kid);
  }
  if (WN_operator(const_kid) != OPR_INTCONST) {
    return -1;
  }
  INT num_multiplies = WN_const_val(const_kid) - 1;
  if (num_multiplies == 0) {
    return 1; // noop
  }
  if (num_multiplies < 0 || num_multiplies > 3) {
    return -1;
  }
  double instr = LNOTARGET_FP_Exp_Res(resource_count,
                                      (INTRINSIC) WN_intrinsic(wn),
                                      num_multiplies);
  if (instr < 0.1) {
    return -1;
  }
  *num_fp_instr += instr;
  return 1;
}


// Is this node lexically before node2
BOOL 
ARRAY_REF_NODE::Lexically_Before(ARRAY_REF_NODE *node2)
{
  INT num_loops = Array->Dim(0)->Nest_Depth();
  for (INT i=0; i<num_loops; i++) {
    if (_unroll_copy[i] < node2->_unroll_copy[i]) return TRUE;
  }
  return (_lex_number < node2->_lex_number);
}

void 
ARRAY_REF_LIST::Print(FILE *fp) const
{
  fprintf(fp,"The base array is \"");
  Base_Array->Print(fp);
  if (_is_scalar_expanded) fprintf(fp," (scalar expanded) ");
  fprintf(fp,"\" and the references are \n");
  ARRAY_REF_CONST_ITER iter(this);
  const ARRAY_REF_NODE *first = iter.First();
  for (const ARRAY_REF_NODE *n = first; !iter.Is_Empty(); n = iter.Next()) {
    fprintf(fp, "    ");
    n->Print(fp);
  }
}


// delete a list, including every node on it
// this assumes that all elements are from the same mempool
ARRAY_REF_LIST::~ARRAY_REF_LIST()
{
  MEM_POOL_Set_Default(_pool);
  while (!Is_Empty())
    CXX_DELETE(Remove_Headnode(),_pool);
}


ARRAY_REF_LIST::ARRAY_REF_LIST(ARRAY_REF_LIST *orig, MEM_POOL *pool)
{
  _pool = pool;
  _is_scalar_expanded = orig->_is_scalar_expanded;
  Base_Array = orig->Base_Array;
  ARRAY_REF_ITER iter(orig);
  for (ARRAY_REF_NODE *node = iter.First(); !iter.Is_Empty(); 
       node=iter.Next()) {
    Append(CXX_NEW(ARRAY_REF_NODE(node,pool),pool));
  }
}

void 
ARRAY_REF_LIST::Remove_Invariants(INT loopno)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *first = iter.First();
  ARRAY_REF_NODE *prev_node = NULL;
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=first; node; node = next_node) {
    next_node = iter.Next();
    ACCESS_ARRAY *array = node->Array;
    BOOL is_invar = TRUE;
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if ((av->Non_Const_Loops() > loopno) || (av->Loop_Coeff(loopno) != 0)) {
	is_invar = FALSE;
      }
    }
    if (is_invar) {
      Remove(prev_node,node);
    } else {
      prev_node = node;
    }
  }
}

// are array and array2 cses or duplicates given that loop inner is inner and
// that we consider a cse anything within a distance max_dist
// step is the step size of the loop.  Zero implies its not constant
// if they are exact duplicates, set *is_dup to TRUE
// if we return TRUE, *min_inner_offset and *max_inner_offset are set
// to the min/max offset in dimensions that use the inner loop variable
// these are normalized by the step and inner loop mulitplier
// if we don't return TRUE, they are undefined
static BOOL 
Cse_Or_Dup(ACCESS_ARRAY* array, ACCESS_ARRAY* array2,
           INT inner, INT max_dist, INT step, 
           BOOL* is_dup, mINT16* max_inner_offset, mINT16* min_inner_offset) 
{
  *min_inner_offset = INT16_MAX;
  *max_inner_offset = INT16_MIN;
  if(!step) return((*array) == (*array2));

  if (array->Too_Messy || array2->Too_Messy) return(FALSE);
  if (array->Num_Vec() != array2->Num_Vec()) return(FALSE);
  BOOL seen_mult = FALSE;
  INT diff=0;
  for (INT i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av1 = array->Dim(i);
    ACCESS_VECTOR *av2 = array2->Dim(i);
    if (av1->Too_Messy || av2->Too_Messy) return(FALSE);
    if (av1->Nest_Depth() != av2->Nest_Depth()) return(FALSE);

    INT dist = av1->Const_Offset - av2->Const_Offset;

    // short circuit to get const reference case quickly, ie a[0] vrs a[1]
    if (!av1->Has_Loop_Coeff() || !av2->Has_Loop_Coeff()) {
      if (dist) return(FALSE);
    }

    // the symbols must be equal
    if (av1->Lin_Symb != NULL && !av1->Lin_Symb->Is_Empty()) { // av1 has a symb
      if (av2->Lin_Symb == NULL || av2->Lin_Symb->Is_Empty() ||
		  !(*av1->Lin_Symb == *av2->Lin_Symb)) {
	return(FALSE);
      }
    } else if (av2->Lin_Symb != NULL && !av2->Lin_Symb->Is_Empty()) {
      return(FALSE);
    }
    if (av1->Non_Lin_Symb != NULL && !av1->Non_Lin_Symb->Is_Empty()) {
      if (av2->Non_Lin_Symb == NULL || av2->Non_Lin_Symb->Is_Empty() ||
	!(*av1->Non_Lin_Symb == *av2->Non_Lin_Symb)) {
	return(FALSE);
      }
    } else if (av2->Non_Lin_Symb != NULL && !av2->Non_Lin_Symb->Is_Empty()) {
      return(FALSE);
    }

    // Now check the induction variables
    for (INT i=0; i<av1->Nest_Depth(); i++) {
      if (av1->Loop_Coeff(i) != av2->Loop_Coeff(i)) {
	return(FALSE);
      }
    }
    INT mult = av1->Loop_Coeff(inner);
    if (mult) {
      if (abs(dist) > abs(step*mult*max_dist)) {
	return(FALSE); // too far to be a cse
      }
      if ((dist % (step*mult)) != 0) {
	return(FALSE); // independent
      } 
      INT this_diff = (dist / (step*mult));
      if (seen_mult && (this_diff != diff)) {  // contradictory coupling
	return(FALSE);
      }
      seen_mult = TRUE;
      diff = this_diff;
      *max_inner_offset = MAX(*max_inner_offset,
		MAX(av1->Const_Offset,av2->Const_Offset)/(step*mult));
      *min_inner_offset = MIN(*min_inner_offset,
		MIN(av1->Const_Offset,av2->Const_Offset)/(step*mult));
    } else if (dist != 0) return(FALSE);
  }
  *is_dup = (diff == 0);
  return(TRUE);
}


// Remove the cses and duplicates 
// Mark the remaining copy as a cse or duplicate 
// Union _has_store  and _has_load
// _first_ref_store is set to the _first_ref_store of the earlier ref
void 
ARRAY_REF_LIST::Remove_Cse(INT inner, INT max_dist, INT step)
{
  mINT16 max_inner_offset, min_inner_offset;
  ARRAY_REF_ITER iter(this);
  for (ARRAY_REF_NODE *node=iter.First(); node; node = iter.Next()) {
    ACCESS_ARRAY *array = node->Array;

    // loop through all possible later nodes, removing duplicates
    ARRAY_REF_ITER iter2(node);
    ARRAY_REF_NODE *first = iter2.First();
    first = iter2.Next(); // first is one after node
    ARRAY_REF_NODE *prev_node = node;
    ARRAY_REF_NODE *next_node = NULL;
    for (ARRAY_REF_NODE *node2=first; node2; node2 = next_node) {
      next_node = iter2.Next();
      ACCESS_ARRAY *array2 = node2->Array;
      BOOL is_dup;
      if (Cse_Or_Dup(array,array2,inner,max_dist,step,&is_dup,
                     &max_inner_offset,&min_inner_offset)) {
	bool is_cse = false;
        
	if (is_dup) {
	  node->_is_dup = TRUE;
	  if (node2->_has_dup_loads || 
	      (node->_has_load && node2->_has_load)) {
	    node->_has_dup_loads = TRUE;
          }
	} else {
	  /* Don't CSE accesses that require alignment registers. */
	  is_cse = (node2->Align() == LNO_REGS_IDX_UNDEFINED &&
		    node2->Sel() == LNO_REGS_IDX_UNDEFINED);
	}
        
	if (is_cse) {
	  node->_is_cse = TRUE;
	  node->_max_inner_offset = 
	    MAX(node->_max_inner_offset,max_inner_offset);
	  node->_min_inner_offset = 
	    MIN(node->_min_inner_offset,min_inner_offset);
	}
        
	if (is_dup || is_cse) {
	  node->_has_store |= node2->_has_store;
	  node->_has_load |= node2->_has_load;
	  if (node->_first_ref_store != node2->_first_ref_store) {
	    if (node2->Lexically_Before(node)) {
	      node->_first_ref_store = node2->_first_ref_store;
	    }
	  }

	  Remove(prev_node, node2);
          continue;
	}
      }
      
      prev_node = node2;
    }
  }
}

void 
ARRAY_REF_LIST::Unroll(INT loop_no, INT num_copies)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=iter.First(); node; node = next_node) {
    next_node = node->Next();
    ACCESS_ARRAY *array = node->Array;

    // Does this reference vary with loop_no
    // Note that we ignore symbolics, if they vary in a weird way
    // we won't be able to unroll anyway
    BOOL varies = FALSE;
    if (array->Too_Messy) varies = TRUE;
    for (INT i=0; i<array->Num_Vec() && !varies; i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if (av->Too_Messy || (av->Loop_Coeff(loop_no) != 0)) {
	varies = TRUE;
      }
    }

    if (!varies) {
      node->_is_dup = TRUE;
      if (node->_has_load) node->_has_dup_loads = TRUE;
    } else {  // do the duplication
      INT orig_copy_num = node->_unroll_copy[loop_no];
      for (INT i=num_copies-1; i>=0; i--) {
	if (i != 0) { // create a copy
	  ARRAY_REF_NODE *new_node=CXX_NEW(ARRAY_REF_NODE(node,_pool),_pool);
	  if (orig_copy_num) {
	    new_node->_unroll_copy[loop_no] = orig_copy_num*num_copies+i;
          } else {
	    new_node->_unroll_copy[loop_no] = i;
          }
	  array = new_node->Array;
	  Prepend(new_node,node);
	} else {
	  array = node->Array;
	  if (orig_copy_num) {
	    node->_unroll_copy[loop_no] = orig_copy_num*num_copies+i;
          } else {
	    node->_unroll_copy[loop_no] = i;
          }
	}
	for (INT j=0; j<array->Num_Vec(); j++) {
	  ACCESS_VECTOR *av = array->Dim(j);
	  if (!av->Too_Messy) {
	    INT mult = av->Loop_Coeff(loop_no);
	    if (mult) {
	      av->Const_Offset += mult*i;
	      av->Set_Loop_Coeff(loop_no,num_copies*mult);
            }
          }
	}
      }
    }
  }
}


void 
ARRAY_REF_LIST::Mark_Invariants(INT loopno)
{
  ARRAY_REF_ITER iter(this);
  ARRAY_REF_NODE *first = iter.First();
  ARRAY_REF_NODE *next_node = NULL;
  for (ARRAY_REF_NODE *node=first; node; node = next_node) {
    next_node = iter.Next();
    ACCESS_ARRAY *array = node->Array;
    BOOL is_invar = TRUE;
    for (INT i=0; i<array->Num_Vec(); i++) {
      ACCESS_VECTOR *av = array->Dim(i);
      if ((av->Non_Const_Loops() > loopno) || (av->Loop_Coeff(loopno) != 0)) {
	is_invar = FALSE;
      }
    }
    node->_is_invariant = is_invar;
  }
}

// How many invariants are there
INT 
ARRAY_REF_LIST::Num_Invariants(INT loopno)
{
  INT result=0;
  Mark_Invariants(loopno);

  ARRAY_REF_ITER iter(this);
  for (ARRAY_REF_NODE *node=iter.First(); node; node = iter.Next()) {
    if (node->_is_invariant) result++;
  }
  return result;
}

// How many references are there (add them to refs)
void
ARRAY_REF_LIST::Num_Refs(LNO_REGS *refs) const {
  if (_is_scalar_expanded) {
    TYPE_ID type = Base_Array->Type;
    refs->Inc_Type_Usage(type,this->Len());
    return;
  }
  
  ARRAY_REF_CONST_ITER iter(this);
  for (const ARRAY_REF_NODE *node=iter.First(); node; node=iter.Next()) {
    LNO_REGS_IDX vec = node->Vec();
    if (vec!=LNO_REGS_IDX_UNDEFINED) {
      refs->Count_Of_Idx(vec) += node->Vec_Regs();
      LNO_REGS_IDX align = node->Align();
      if (align!=LNO_REGS_IDX_UNDEFINED) {
	refs->Count_Of_Idx(align)++;
      }
      LNO_REGS_IDX sel = node->Sel();
      if (sel!=LNO_REGS_IDX_UNDEFINED) {
	refs->Count_Of_Idx(sel)++;
      }
    } else {
      TYPE_ID type = node->Type();
      refs->Inc_Type_Usage(type,1);
    }
  }
}

void 
ARRAY_REF_LIST::Calc_Regs_And_Refs(REG_REF_COUNT *rr, BOOL outer_vectorization) {
  ARRAY_REF_ITER iter(this);
  STACK_OF_ACCESS_ARRAY *aa_stack = CXX_NEW(STACK_OF_ACCESS_ARRAY(_pool),_pool);
  for (ARRAY_REF_NODE* node=iter.First(); node; node = iter.Next()) {

    if (debug_model) {
      fprintf(TFile,"calc_regs_and_refs: ");
      node->Print(TFile);
      fprintf(TFile,"\n");
    }

    INT tmp_regs = 0;
    INT tmp_refs = 0;
    INT tmp_variant_stores = 0;
    INT tmp_invariant_stores = 0;
    INT tmp_base_regs = 0;
    
    // First check whether we need a new base register to hold the address
    // Assuming (we should specialize for type) core types, we need a new base
    // register every 256 elements (within 256 one base and different offsets suffice)
    if (!node->_is_invariant) {
      // assuming invariant will be moved out
      bool found = false;
      for (int i=0; i<aa_stack->Elements(); i++) {
        ACCESS_ARRAY* ar  = node->Array;
        ACCESS_ARRAY* ar1 = aa_stack->Bottom_nth(i);
        if (ar->Too_Messy  || ar1->Too_Messy || 
            ar->Num_Vec() != ar1->Num_Vec()) {
          continue;
        }
        bool need_diff_base = false;
        for (INT i = 0; i < ar->Num_Vec(); i++) {
          ACCESS_VECTOR* av  = ar->Dim(i);
          ACCESS_VECTOR* av1 = ar1->Dim(i);
          if (av->Too_Messy || av1->Too_Messy) {
            need_diff_base=true;
            break;
          }
          ACCESS_VECTOR* diff=Subtract(av, av1, _pool);
          if (!diff->Is_Const()) {
            need_diff_base=true;
            break;
          } else if (i < ar->Num_Vec()-1) {
            if (diff->Const_Offset != 0) {
              need_diff_base=true;
              break;
            } else {
              continue;
            }
          } else if (diff->Const_Offset < 0x100 &&
		     -diff->Const_Offset < 0x100) {
            need_diff_base=false;
            break;
          } else {
            need_diff_base=true;
            break;
          }
        }
        if (!need_diff_base) {
          found = true;
          break;
        }
      }
      if (!found) {
        aa_stack->Push(node->Array);
        tmp_base_regs++;
      }
    }
    TYPE_ID type = node->Type();
    
    bool is_fp = false;
    INT regs_per_ref = 1;
    if (node->_is_invariant) {
      tmp_regs += regs_per_ref;
    } else if (node->_is_cse) {  
      if (node->_max_inner_offset > node->_min_inner_offset) {
        tmp_regs += 
          MIN(Max_Cse_Dist,(node->_max_inner_offset-node->_min_inner_offset)) *
	  regs_per_ref;
      } else {
	tmp_regs += regs_per_ref;
      }
      tmp_refs += regs_per_ref;
    } else if (node->_is_dup) {  // a pure duplicate
      if (node->_has_dup_loads) {
        tmp_regs += regs_per_ref;
      }
      if (node->_has_store) {
	tmp_refs += regs_per_ref;
      }
      if (node->_has_load && !node->_first_ref_store) {
	tmp_refs += regs_per_ref;
      }
    } else {
      tmp_refs += regs_per_ref;
    }
    if (node->_has_store) {
      if (node->_is_invariant) {
	tmp_invariant_stores += regs_per_ref;
      } else {
	tmp_variant_stores += regs_per_ref;
      }
    }
    LNO_REGS_IDX vec = node->Vec();
    if (vec==LNO_REGS_IDX_UNDEFINED) {
      rr->regs.Inc_Type_Usage(type,tmp_regs);
      rr->refs.Inc_Type_Usage(type,tmp_refs);
      rr->inv_stores.Inc_Type_Usage(type,tmp_invariant_stores);
      rr->var_stores.Inc_Type_Usage(type,tmp_variant_stores);
    } else {
      rr->regs.Count_Of_Idx(vec) += tmp_regs*node->Vec_Regs();
      rr->refs.Count_Of_Idx(vec) += tmp_refs*node->Vec_Regs();
      rr->inv_stores.Count_Of_Idx(vec) += tmp_invariant_stores*node->Vec_Regs();
      rr->var_stores.Count_Of_Idx(vec) += tmp_variant_stores*node->Vec_Regs();
      LNO_REGS_IDX align = node->Align();
      if (align!=LNO_REGS_IDX_UNDEFINED) {
	if (node->_is_invariant) {
	  rr->unused_regs.Count_Of_Idx(align)++;
	} else {
	  rr->regs.Count_Of_Idx(align)++;
	  if (outer_vectorization) {
	    rr->refs.Count_Of_Idx(vec)++; // priming is inside
          }
	}
      }
      LNO_REGS_IDX sel = node->Sel();
      if (sel!=LNO_REGS_IDX_UNDEFINED) {
	if (node->_is_invariant) {
	  rr->unused_regs.Count_Of_Idx(sel)++;
	  rr->unused_regs.Count_Of_Idx(vec)++;
	} else {
	  rr->regs.Count_Of_Idx(sel)++;
	  if (outer_vectorization) {
	    rr->refs.Count_Of_Idx(vec)++; // priming is inside
	  }
	}
      }
    }
    rr->regs.Count_Of_Int() += tmp_base_regs;
  }
} // ARRAY_REF_LIST::Calc_Regs_And_Refs

// How many references of the maximal dimensionality (but
// at least 2) are invariant in some outer unrollable loop
INT 
ARRAY_REF_LIST::Conflict_Refs(INT max_dim, 
                              BOOL* can_be_unrolled, 
                              INT num_loops)
{
  INT result = 0;

  MEM_POOL_Push(&LNO_local_pool); 
  BOOL *invar = CXX_NEW_ARRAY(BOOL,num_loops,&LNO_local_pool);
  ARRAY_REF_ITER iter1(this);
  for (ARRAY_REF_NODE *node1=iter1.First(); node1; node1 = iter1.Next()) {
    if (!node1->_is_invariant) { // don't care about inner invariants
      ACCESS_ARRAY *array=node1->Array;
      UINT num_vec = array->Num_Vec();
      if (num_vec == max_dim) {
        INT i;
	for (i=0; i<num_loops; i++) { 
          invar[i] = can_be_unrolled[i];
        }
	BOOL too_messy = FALSE;
        for (i=0; i<num_vec; i++) {
          ACCESS_VECTOR *av = array->Dim(i);
	  if (!av->Too_Messy) {
            for (INT j=0; j<av->Nest_Depth(); j++) {
              if ((av->Non_Const_Loops() > j) || (av->Loop_Coeff(j) != 0)) {
	        invar[j] = FALSE;
              }
            }
          } else {
	    too_messy = TRUE;
          }
        }
	if (!too_messy) {
	  BOOL is_invar = FALSE;
          for (i=0; i<num_loops && !is_invar; i++) {
	    if (invar[i]) {
	      is_invar = TRUE;
            }
          }
	  if (is_invar) {
	    result++;
          }
        }
      }
    }
  }
  MEM_POOL_Pop(&LNO_local_pool); 
  return result;
}



ARRAY_REF::ARRAY_REF(ARRAY_REF *orig, MEM_POOL *pool) :
  _stack(pool), _pool(pool) {
  _num_bad_fp = orig->_num_bad_fp;
  _num_bad_int = orig->_num_bad_int;
  _lex_number = orig->_lex_number;
  for (INT i=0; i<orig->Elements(); i++) {
    Push(CXX_NEW(ARRAY_REF_LIST(orig->Array_Ref_List(i),pool),pool));
  }
}

void 
ARRAY_REF::Print(FILE *fp) const
{
  fprintf(fp,"The number of bad references is %d fp and %d int\n",
    Num_Fp_Bad(), Num_Int_Bad());
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Print(fp);
  }
}

void
ARRAY_REF::Num_Refs(LNO_REGS *refs) const {
  refs->Count_Of_Float() = 0;
  refs->Count_Of_Int() = Num_Int_Bad();
  refs->Count_Of_Float() += Num_Fp_Bad();
  
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Num_Refs(refs);
  }
}

INT 
ARRAY_REF::Num_Invariants(INT loopno)
{
  INT result = 0;
  for (INT i=0; i<Elements(); i++) {
    result += Array_Ref_List(i)->Num_Invariants(loopno);
  }
  return result;
}

INT 
ARRAY_REF::Conflict_Refs(BOOL *can_be_unrolled, INT num_loops) 
{
  INT max_dim = 0;
  INT result = 0;
  for (INT i=0; i<Elements(); i++) {
    ARRAY_REF_ITER iter(Array_Ref_List(i));
    ARRAY_REF_NODE *node = iter.First();
    ACCESS_ARRAY *ar=node->Array;
    max_dim = MAX(max_dim,ar->Num_Vec());
  }
  if (max_dim >= 2) {
    for (INT i=0; i<Elements(); i++) {
      result += 
	Array_Ref_List(i)->Conflict_Refs(max_dim,can_be_unrolled,num_loops);
    }
  }
  return result;
}

void 
ARRAY_REF::Calc_Regs_And_Refs(REG_REF_COUNT *rr, BOOL outer_vectorization) {
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Calc_Regs_And_Refs(rr, outer_vectorization);
  }
  // assuming each bad ref need one address reg
  rr->regs.Count_Of_Int() += (_num_bad_fp+_num_bad_int);
  rr->refs.Count_Of_Float() += Num_Fp_Bad();
  rr->refs.Count_Of_Int() += Num_Int_Bad();
}

void 
ARRAY_REF::Remove_Invariants(INT loopno)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Remove_Invariants(loopno);
  }
}

void 
ARRAY_REF::Remove_Cse(INT inner, INT max_dist, INT step)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Remove_Cse(inner, max_dist, step);
  }
}

void 
ARRAY_REF::Mark_Invariants(INT loopno)
{
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Mark_Invariants(loopno);
  }
}

void 
ARRAY_REF::Unroll(INT loop_no, INT num_copies)
{
  _num_bad_fp *= num_copies;
  _num_bad_int *= num_copies;
  for (INT i=0; i<Elements(); i++) {
    Array_Ref_List(i)->Unroll(loop_no,num_copies);
  }
}

extern void 
Build_DLI_Stack(WN *wn, DLI_STACK *stack)
{
  if (wn) {
    Build_DLI_Stack(LWN_Get_Parent(wn), stack);
    if (WN_opcode(wn) == OPC_DO_LOOP) {
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
      stack->Push(dli);
    }
  }
}

// Is this access vector a weird triangular vector
// i.e. is there a term a[i] where i's bounds are triangular
// with a multiple > 5, ie.  do i = 6j,...
BOOL 
Weird_Triangular(ACCESS_VECTOR* av, DLI_STACK* dli_stack, INT SNL_Depth)
{
  for (INT i=0; i<av->Nest_Depth(); i++) {
    if (av->Loop_Coeff(i)) {
      ACCESS_ARRAY *array = dli_stack->Bottom_nth(i)->LB;
      INT j;
      for (j=0; j<array->Num_Vec(); j++) {
        ACCESS_VECTOR *bound = array->Dim(j);
        INT lb = av->Nest_Depth() - SNL_Depth; 
        for (INT k=lb; k<bound->Nest_Depth()-1; k++) {
	  if (abs(bound->Loop_Coeff(k)) > 5) {
	    return TRUE;
          }
        }
      }
      array = dli_stack->Bottom_nth(i)->UB;
      for (j=0; j<array->Num_Vec(); j++) {
        ACCESS_VECTOR *bound = array->Dim(j);
        INT lb = av->Nest_Depth() - SNL_Depth; 
        for (INT k=lb; k<bound->Nest_Depth()-1; k++) {
	  if (abs(bound->Loop_Coeff(k)) > 5) {
	    return TRUE;
          }
        }
      }
    }
  }
  return FALSE;
}

extern BOOL 
Is_Bad_Array(WN* wn_ref, INT nloops)
{
  OPCODE op = WN_opcode(wn_ref);
  OPERATOR opr = OPCODE_operator(op);
  if (!OPCODE_is_load(op) && !OPCODE_is_store(op))
    return FALSE;
  if (opr == OPR_LDID || opr == OPR_STID)
    return FALSE;
  WN* wn_array = OPCODE_is_load(op) ? WN_kid0(wn_ref) : WN_kid1(wn_ref);
  if (WN_operator(wn_array) != OPR_ARRAY)
    return FALSE;
  ACCESS_ARRAY* aa = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, wn_array);
  WN* wn_base = WN_array_base(wn_array);
  if (WN_operator(wn_base) != OPR_LDA
      && WN_operator(wn_base) != OPR_LDID)
    return TRUE;
  if (aa == NULL || aa->Too_Messy
      || Do_Depth(wn_array) + 1 - aa->Non_Const_Loops() < nloops)
    return TRUE;
  DLI_STACK *dli_stack =
    CXX_NEW(DLI_STACK(&LNO_local_pool), &LNO_local_pool);   
  Build_DLI_Stack(wn_ref, dli_stack);
  for (INT i = 0; i < aa->Num_Vec(); i++) {
    ACCESS_VECTOR *av = aa->Dim(i);
    if (av->Too_Messy || av->Non_Lin_Symb
        || Weird_Triangular(av, dli_stack, nloops))
      return TRUE;
  }
  return FALSE;
}

void 
ARRAY_REF::Build(WN* wn, INT SNL_Depth, INVAR_TABLE *invar_table) {
  DLI_STACK *dli_stack = CXX_NEW(DLI_STACK(_pool),_pool);
  Build_DLI_Stack(wn,dli_stack);
  Build_Rec(wn,dli_stack,SNL_Depth,invar_table);
  CXX_DELETE(dli_stack,_pool);
}

void 
ARRAY_REF::Build_Rec(WN* wn, DLI_STACK* dli_stack, INT SNL_Depth,
		     INVAR_TABLE *invar_table) {
  if (!wn) return;
  
  OPCODE opcode = WN_opcode(wn);
  OPERATOR opr = OPCODE_operator(opcode);

  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Build(kid, SNL_Depth,invar_table);
      kid = WN_next(kid);
    }
    return;
  } else if (opcode == OPC_DO_LOOP) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
    dli_stack->Push(dli);
  }

  if (invar_table && !OPCODE_is_load(opcode) && !OPCODE_is_store(opcode)) {
    BIT_VECTOR *bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      Enter_Scalar_Expand(bv,wn);
      return;
    }
  }

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    WN *kid = WN_kid(wn,kidno);
    Build(kid, SNL_Depth,invar_table);
  }


  if (OPCODE_is_load(opcode) && opr != OPR_LDID) {
    if (WN_operator(WN_kid0(wn)) == OPR_ARRAY) {
      Build_Array(WN_kid0(wn),FALSE,dli_stack,SNL_Depth);
    } else {
      if (MTYPE_is_float(WN_desc(wn)))
        _num_bad_fp++;
      else
        _num_bad_int++;
    }
  } else if (OPCODE_is_store(opcode) && opr != OPR_STID) {
    if (WN_operator(WN_kid1(wn)) == OPR_ARRAY) {
      Build_Array(WN_kid1(wn),TRUE,dli_stack,SNL_Depth);
    } else {
      if (MTYPE_is_float(WN_desc(wn)))
        _num_bad_fp++;
      else
        _num_bad_int++;
    }
  } else if (opcode == OPC_DO_LOOP) {
    dli_stack->Pop();
  }
}

void 
ARRAY_REF::Build_Array(WN* wn_array, 
                       BOOL is_store, 
                       DLI_STACK* dli_stack, 
                       INT SNL_Depth)
{
  TYPE_ID type = WN_desc(LWN_Get_Parent(wn_array));
  INT esz = MTYPE_size_min(type) >> 3;

  ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,wn_array);
  if (!array || array->Too_Messy 
      || Do_Depth(wn_array) + 1 - array->Non_Const_Loops() < SNL_Depth) {
    if (MTYPE_is_float(type))
      _num_bad_fp++;
    else
      _num_bad_int++;
    return;
  }

  INT i;
  for (i=0; i<array->Num_Vec(); i++) {
    ACCESS_VECTOR *av = array->Dim(i);
    if (av->Too_Messy // || av->Non_Lin_Symb 
	|| Weird_Triangular(av,dli_stack,SNL_Depth)) {
      if (MTYPE_is_float(type))
        _num_bad_fp++;
      else
        _num_bad_int++;
      return;
    }
  }

  // Find which element in the stack contains our base array
  WN *base = WN_array_base(wn_array);
  if ((WN_operator(base) != OPR_LDA) &&
      (WN_operator(base) != OPR_LDID)) {
    if (MTYPE_is_float(type))
      _num_bad_fp++;
    else
      _num_bad_int++;
    return;
  }
  SYMBOL symb(base);

  ARRAY_REF_NODE* arn = CXX_NEW(ARRAY_REF_NODE(array, wn_array, is_store, esz,
					       _lex_number++,type),
				_pool);
  for (i=0; i<Elements(); i++) {
    if (symb == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(&symb),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
}

// Enter a scalar expanded references
// If we have to expand if 'i' and 'j' are inner, then create an
// ACCESS_ARRAY [i][j]
void 
ARRAY_REF::Enter_Scalar_Expand(WN* wn,
                               SX_PNODE* pnode,
                               BOOL* can_be_inner, 
                               INT num_loops)
{
  BOOL is_store = OPCODE_is_store(WN_opcode(wn));
  TYPE_ID type = WN_desc(wn);
  INT esz = MTYPE_size_min(type) >> 3;

  INT count=0;
  INT i;
  for (i=0; i<num_loops; i++) {
    if (can_be_inner[i] && pnode->Transformable(i)==SX_PNODE::SE_REQD) {
      count++;
    }
  }

  ACCESS_ARRAY *a = CXX_NEW(ACCESS_ARRAY(MAX(1,count),num_loops,_pool),_pool);
  a->Too_Messy = FALSE;
  if (count == 0) {
    a->Dim(0)->Too_Messy = FALSE;
  } else {
    INT c=0;
    for (INT i=0; i<num_loops; i++) {
      if (can_be_inner[i]&&pnode->Transformable(i)==SX_PNODE::SE_REQD) {
	a->Dim(c)->Too_Messy = FALSE;
	a->Dim(c)->Set_Loop_Coeff(i,1);
        c++;
      }
    }
  }

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,NULL,is_store,esz,_lex_number++,type),_pool);
  for (i=0; i<Elements(); i++) {
    if (pnode->Symbol() == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      Array_Ref_List(i)->_is_scalar_expanded = TRUE;
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(pnode->Symbol()),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}

// We have an invariant expression, treat it like a load
// that varies in all the dimensions that the expression varies
void 
ARRAY_REF::Enter_Scalar_Expand(BIT_VECTOR* bv, WN* wn)
{
  INT num_loops = bv->Size();
  INT num_invar = bv->Pop_Count();
  INT num_var = num_loops - num_invar;

  ACCESS_ARRAY* a=CXX_NEW(ACCESS_ARRAY(MAX(1,num_var),num_loops,_pool),_pool);
  a->Too_Messy = FALSE;
  if (num_var == 0) {
    a->Dim(0)->Too_Messy = FALSE;
  } else {
    INT count=0;
    for (INT i=0; i<num_loops; i++) {
      if (!bv->Test(i)) {
	a->Dim(count)->Too_Messy = FALSE;
	a->Dim(count)->Set_Loop_Coeff(i,1);
        count++;
      }
    }
  }

  TYPE_ID type = WN_rtype(wn);
  INT esz = MTYPE_size_min(type) >> 3;

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,wn,FALSE,esz,_lex_number++,type),_pool);
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(),_pool);
  tmp_symb->Type = type;
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}

// Enter a scalar expanded references
// ACCESS_ARRAY [i]
void 
ARRAY_REF::Enter_Innermost_Scalar_Expand(WN* wn)
{
  BOOL is_store = OPCODE_is_store(WN_opcode(wn));
  TYPE_ID type = WN_desc(wn);
  INT esz = MTYPE_size_min(type) >> 3;
  SYMBOL sym(wn);

  WN *tmp=wn;
  while (WN_opcode(tmp) != OPC_DO_LOOP) {
    tmp = LWN_Get_Parent(tmp);
    Is_True(tmp,("Enter_Innermost_Scalar_Expand not in a loop \n"));
  }
  INT depth = Do_Loop_Depth(tmp);
  ACCESS_ARRAY *a = CXX_NEW(ACCESS_ARRAY(1,depth+1,_pool),_pool);
  a->Too_Messy = FALSE;
  a->Dim(0)->Too_Messy = FALSE;
  a->Dim(0)->Const_Offset=0;
  a->Dim(0)->Set_Non_Const_Loops(0);
  for (INT i=0; i<depth; i++) {
    a->Dim(0)->Set_Loop_Coeff(i,0);
  }
  a->Dim(0)->Set_Loop_Coeff(depth,1);

  ARRAY_REF_NODE *arn = 
    CXX_NEW(ARRAY_REF_NODE(a,NULL,is_store,esz,_lex_number++,type),_pool);
  for (INT i=0; i<Elements(); i++) {
    if (sym == *Array_Ref_List(i)->Base_Array) {
      Array_Ref_List(i)->Append(arn);
      Array_Ref_List(i)->_is_scalar_expanded = TRUE;
      return;
    }
  }
  SYMBOL* tmp_symb = CXX_NEW (SYMBOL(sym),_pool);
  Push(CXX_NEW(ARRAY_REF_LIST(_pool,tmp_symb), _pool));
  Array_Ref_List(Elements()-1)->Append(arn);
  Array_Ref_List(Elements()-1)->_is_scalar_expanded = TRUE;
}


//
//
//
//
//
// LATENCY ROUTINES
//
//
//
//
//

// Walk the code
// Add a vertex to the graph for each fp array store/load
// put into the hash table a mapping from the vertex in the array dependence 
// graph to the vertex in the latency graph
// Add an edge from each load to its parent store with the latency = sum
// of the latencies from the store down to the load
// Return -1 on failure
INT LAT_DIRECTED_GRAPH16::Add_Vertices_Op_Edges(WN *wn,
						INVAR_TABLE *invar_table) {
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      if (Add_Vertices_Op_Edges(kid,invar_table) == -1)
	return -1;
      kid = WN_next(kid);
    }
    return 1;
  }
  
  VINDEX16 v;
  if (OPCODE_is_store(opcode) && (v=_array_graph->Get_Vertex(wn))) {
    VINDEX16 this_v = Add_Vertex(wn);
    if (!this_v)
      return -1;
    Map_Vertex(v,this_v);
    
    if (Add_Vertices_Op_Edges_Rec(this_v,WN_kid0(wn),0,invar_table) == -1)
      return -1;
  } else if (!OPCODE_is_stmt(opcode)) {
    for (INT kidno=0; kidno < WN_kid_count(wn); kidno++) {
      if (Add_Vertices_Op_Edges(WN_kid(wn,kidno),invar_table) == -1)
	return -1;
    }
  }
  return 1;
}


// Deal recursively with the things under the store
INT 
LAT_DIRECTED_GRAPH16::Add_Vertices_Op_Edges_Rec(VINDEX16 store,
                                                WN *wn,
                                                INT latency,
                                                INVAR_TABLE *invar_table)
{
  TOP top;
  OPERATOR oper = WN_operator(wn);
  TYPE_ID rtype = WN_rtype(wn);
  TYPE_ID  desc = WN_desc(wn);
  
  VINDEX16 v;
  if (OPERATOR_is_load(oper) && (v = _array_graph->Get_Vertex(wn))) {
    VINDEX16 this_v = Add_Vertex(wn);
    if (!this_v) {
      return -1;
    }
    Map_Vertex(v,this_v);
    EINDEX16 e = Add_Edge(this_v,store,0,latency,0);
    if (!e) {
      return -1;
    }
  } 
  
  if (invar_table) { // no cost to invariant expressions
    BIT_VECTOR *bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
      return 1;
    }
  }
  
  INT op_latency = 0;
  
  if (Is_Target_Xtensa()) {
    // FIXME: this should get the real operation latency
    if (OPERATOR_is_expression(oper) && 
	!OPERATOR_is_load(oper) &&
	oper != OPR_CONST) {
      op_latency = 1;
    }
  }
  else if (oper == OPR_CVT   || 
      oper == OPR_RND   ||
      oper == OPR_CEIL  || 
      oper == OPR_TRUNC || 
      oper == OPR_FLOOR) {
    op_latency = LNOTARGET_Cvt_Lat(WN_opcode(wn));
    if (op_latency == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_INTRINSIC_OP) {
    op_latency = FP_Latency_Intrinsic(wn);
    if (op_latency == -1) {
      return -1;
    }
  } 
  else if (oper == OPR_REALPART || 
           oper == OPR_IMAGPART ||
           oper == OPR_PAREN    || 
           oper == OPR_PARM) {  // no-ops
    op_latency = 0;
  } 
  else if (OPERATOR_is_expression(oper) && 
           !OPERATOR_is_load(oper) &&
           oper != OPR_CONST) {
    // an fp expression
    if (desc == MTYPE_FQ || rtype == MTYPE_FQ) {
      return -1;
    } 
    // an fp expression
    if (desc  == MTYPE_FQ || 
        desc  == MTYPE_CQ || 
        rtype == MTYPE_FQ ||
        rtype == MTYPE_CQ) {
      return -1;
    } 
    // regular floating point
    else if (desc  == MTYPE_F4 || 
             desc  == MTYPE_F8 ||
             rtype == MTYPE_F4 ||
             rtype == MTYPE_F8) {
      // multiply-adds
      if (Target_ISA_Has_Madd() && 
          (oper == OPR_ADD || oper == OPR_SUB) && 
          (WN_operator(WN_kid0(wn)) == OPR_MPY || 
           WN_operator(WN_kid1(wn)) == OPR_MPY)) { 
        return FP_Latency_Madd(store, wn, latency, invar_table);
      } 
      else if (oper == OPR_MAX || oper == OPR_MIN) {
        op_latency = LNOTARGET_FP_Min_Max_Lat(rtype);
      } 
      else if (oper == OPR_SQRT) {
        op_latency = LNOTARGET_FP_Sqrt_Lat(rtype);
      } 
      else if ((top = LNOTARGET_Whirl_To_Top(wn)) != TOP_UNDEFINED) {
	op_latency = LNOTARGET_Top_Latency(top);
      }
      else if (oper == OPR_DIV) {
        op_latency = LNOTARGET_FP_Div_Lat(rtype);
      } 
      else if (oper == OPR_RECIP) {
        op_latency = LNOTARGET_FP_Recip_Lat(rtype);
      } 
      else if (oper == OPR_RSQRT) {
        op_latency = LNOTARGET_FP_Rsqrt_Lat(rtype);
      } 
      else {
        return -1;
      }
    }
    else if (desc  == MTYPE_C4 || 
             desc  == MTYPE_C8 ||
             rtype == MTYPE_C4 ||
             rtype == MTYPE_C8) {
      if (oper == OPR_ADD || oper == OPR_SUB) {
        op_latency = LNOTARGET_Complex_Add_Lat(rtype);
      } 
      else if (oper == OPR_MPY)  {
        op_latency = LNOTARGET_Complex_Mult_Lat(rtype);
      }
      else if (oper == OPR_NEG)  {
        op_latency = LNOTARGET_Complex_Neg_Lat(rtype);
      } 
    }
  } 

  for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++) {
    if (Add_Vertices_Op_Edges_Rec(store,
                                  WN_kid(wn,kidno),
                                  latency+op_latency,
                                  invar_table) == -1) {
      return -1;
    } 
  }
  return 1;
}

// deal with a madd
INT 
LAT_DIRECTED_GRAPH16::FP_Latency_Madd(VINDEX16 store,
                                      WN *wn, 
                                      INT latency,
                                      INVAR_TABLE *invar_table)
{
  TYPE_ID rtype = WN_rtype(wn);
  INT add_op_latency = LNOTARGET_FP_Madd_Add_Lat(rtype);
  INT mult_op_latency = LNOTARGET_FP_Madd_Mult_Lat(rtype);

  WN *kid0 = WN_kid0(wn);
  WN *kid1 = WN_kid1(wn);

  WN *non_mult_kid;
  WN *mult_kid0;
  WN *mult_kid1;

  // now do the appropriate kids
  if (WN_operator(kid0) == OPR_MPY) {
    non_mult_kid = kid1;
    mult_kid0 = WN_kid0(kid0);
    mult_kid1 = WN_kid1(kid0);
  } else {
    non_mult_kid = kid0;
    mult_kid0 = WN_kid0(kid1);
    mult_kid1 = WN_kid1(kid1);
  }

  if (Add_Vertices_Op_Edges_Rec(store,
                                non_mult_kid,
                                latency+add_op_latency,
                                invar_table) == -1) {
    return -1;
  }
  if (Add_Vertices_Op_Edges_Rec(store,
                                mult_kid0,
                                latency+mult_op_latency,
                                invar_table) == -1) {
    return -1;
  }
  if (Add_Vertices_Op_Edges_Rec(store,
                                mult_kid1,
                                latency+mult_op_latency,
                                invar_table) == -1) {
    return -1;
  }

  return 1;
}

// What is the latency of an intrinsic op (currently only handle 
// exponentiation by a small number)
// return -1 on error
INT LAT_DIRECTED_GRAPH16::FP_Latency_Intrinsic(WN *wn)
{
  if (WN_kid_count(wn) != 2) {
    return -1;
  }
  WN *const_kid = WN_kid1(wn);
  if (WN_operator(const_kid) == OPR_PARM) {
    const_kid = WN_kid0(const_kid);
  }
  if (WN_operator(const_kid) != OPR_INTCONST) {
    return -1;
  }
  INT num_multiplies = WN_const_val(const_kid)-1;
  if (num_multiplies == 0) {
    return 0;
  }
  if (num_multiplies < 0 || num_multiplies > 3) {
    return -1;
  }
  return LNOTARGET_FP_Exp_Lat((INTRINSIC) WN_intrinsic(wn), num_multiplies);
}



// use the array_graph to add into the latency graph all the edges from 
// stores to loads.  We assume that these all have latency 0  (We add
// them in because of the transitivity)
// return -1 on error
INT LAT_DIRECTED_GRAPH16::Add_Flow_Edges()
{
  for (VINDEX16 i = Get_Vertex(); i; i=Get_Next_Vertex(i)) {
    WN *wn = _v[i]._wn;
    if (OPCODE_is_store(WN_opcode(wn))) { // go through every store in LAT 
      REDUCTION_TYPE source_red = 
	red_manager == NULL? RED_NONE : red_manager->Which_Reduction(wn);
      VINDEX16 array_source = _array_graph->Get_Vertex(wn);
      EINDEX16 e = _array_graph->Get_Out_Edge(array_source);
      while (e) { // go through every edge from this store in array graph
        REDUCTION_TYPE sink_red = 
	  red_manager == NULL? RED_NONE : red_manager->Which_Reduction(wn);
        if ((source_red != sink_red) || (source_red == RED_NONE)) {
	  VINDEX16 array_sink = _array_graph->Get_Sink(e);
	  // is the edge to a load in this loop (ie a load in the hash table)
          VINDEX16 sink =  _hash_table.Find(array_sink);
	  if (sink && OPCODE_is_load(WN_opcode(_v[sink]._wn))) { 
	    EINDEX16 newe=
	      Add_Edge(i,sink,_array_graph->Depv_Array(e)->Union(_pool),0,
		       _array_graph->Depv_Array(e)->Num_Unused_Dim());
	    if (!newe)
	      return -1;
	  }
	}
	e = _array_graph->Get_Next_Out_Edge(e);
      }
    }
  }
  return 1;
}

// Find the latency bound assuming loop number "inner" is the inner loop
// Find the max over all cycles of sum(latencies)/sum(distances) for
// distances > 0
//
// We use the same algorithm as the SWP taken from Lam taken from Floyd.
// We really should just call the SWP code, but ...
double LAT_DIRECTED_GRAPH16::Max_Cycle(INT inner, double lower_bound)
{
  double result=0.0;

  MEM_POOL_Popper popper(_pool);

  // First build and find the SCC_GRAPH
  // We initialize it with v vertices and edges (the init size has no effect
  // on correctness) rather than v vertices and e edges because many of the
  // edges might not be valid for this choice of inner loop
  SCC_DIRECTED_GRAPH16 *scc_graph = CXX_NEW(SCC_DIRECTED_GRAPH16(
			Get_Vertex_Count(),Get_Vertex_Count()),_pool);
  Set_Scc_Graph(scc_graph,inner);
  INT num_scc = scc_graph->Get_Scc_Count();

  // how many elements of each scc 
  INT *scc_counts = CXX_NEW_ARRAY(INT,num_scc+1,_pool);

  // map each vertex to its ordering of the vertices in the same scc
  // i.e. if vertex 'v' is the x'th vertex in scc[i], set scc_pos[v] = i-1
  INT *scc_pos = CXX_NEW_ARRAY(INT,scc_graph->Get_Vertex_Count()+1,_pool);
  INT i;
  for (i=1; i<=num_scc; i++) {
    scc_counts[i] = 0;
    scc_pos[i] = 0;
  }
  for (i=1; i<=scc_graph->Get_Vertex_Count(); i++) {
    INT id = scc_graph->Get_Scc_Id(i);
    scc_pos[i] = scc_counts[id];
    scc_counts[id]++;
  }

  COST_TABLE *ct = NULL;
  // Find the maximum cycle of each scc
  for (i=1; i<=num_scc; i++) {
    if (scc_counts[i] > 1) { // because of what we put in the graph,
			     // the maximum can't be a self cycle
      // create a cost table
      if (!ct) {
        ct = CXX_NEW(COST_TABLE(scc_counts[i],_pool),_pool);
      } else {
	ct->Realloc(scc_counts[i]);
      }

      // initialize the cost table with the edges in the graph
      double upper_bound = ct->Init(inner,this,scc_graph,i,scc_pos);
      if (upper_bound > result) {
	// solve
	double solve = ct->Solve(lower_bound);
        result = MAX(result,solve);
      }
    }
  }
  
  CXX_DELETE(scc_graph,_pool); // the graph uses its own memory pools... so free them
  Scc_lat_vertex_map = NULL;

  return result;
}

// Initialize the scc graph to have the same vertices as this.
// Add an edge for each valid dependence.  A dependence is
// valid if it has an equal direction in every loop except possibly inner.
// The scc graph cannot overflow since it's strictly smaller than this
void LAT_DIRECTED_GRAPH16::Set_Scc_Graph(SCC_DIRECTED_GRAPH16 *scc_graph,
								INT inner)
{
  Lat_scc_vertex_map= CXX_NEW_ARRAY(VINDEX16,_v.Lastidx()+1,_pool); 
  Scc_lat_vertex_map= CXX_NEW_ARRAY(VINDEX16,_v.Lastidx()+1,_pool); 
  for (INT i=Get_Vertex(); i; i = Get_Next_Vertex(i)) {
    VINDEX16 scc_vertex = scc_graph->Add_Vertex();
    Is_True(scc_vertex,("Impossible overflow in Set_Scc_Graph"));
    Lat_scc_vertex_map[i] = scc_vertex;
    Scc_lat_vertex_map[scc_vertex] = i;
  }
  EINDEX16 e = Get_Edge();
  while (e) {
    if (Is_Valid(inner,e)) {
      scc_graph->Add_Edge(Lat_scc_vertex_map[Get_Source(e)],
			Lat_scc_vertex_map[Get_Sink(e)]);
    } 
    e = Get_Next_Edge(e);
  }
  CXX_DELETE_ARRAY(Lat_scc_vertex_map,_pool);
  Lat_scc_vertex_map = NULL;
}

// Is edge e valid given this inner loop
// an edge is valid if all the non-inner dependences have equal dependences
BOOL LAT_DIRECTED_GRAPH16::Is_Valid(INT inner,EINDEX16 e)
{
  DEPV *depv = _e[e].Depv;
  if (!depv) return TRUE;  // an all equals dependence
  for (INT i=0; i<_num_dim; i++) {
    if ((i!=(inner-_num_bad)) && 
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_EQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_POSEQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_NEGEQ) &&
	(DEP_Direction(DEPV_Dep(depv,i)) != DIR_STAR)) {  
      return(FALSE);
    }
  }
  return(TRUE);
}


void LAT_DIRECTED_GRAPH16::Print(FILE *fp)
{
  VINDEX16 i;
  EINDEX16 e;
  fprintf(fp,"LAT_DIRECTED_GRAPH16:\n");
  for (i=Get_Vertex(); i; i = Get_Next_Vertex(i)) {
    fprintf(fp,"  Vertex %d, WN = %s\n",i,OPCODE_name(WN_opcode(_v[i]._wn)));
    e = _v[i].Get_Out_Edge();
    while (e) {
      fprintf(fp,"    Edge to vertex %d, latency = %d",_e[e].Get_Sink(),_e[e].Latency);
      if (_e[e].Depv) {
	fprintf(fp,", dependence ");
	DEPV_Print(_e[e].Depv,fp,_num_dim);
	fprintf(fp,"\n");
      } else {
	fprintf(fp,", all equals dependence\n");
      } 
      e = _e[e].Get_Next_Out_Edge();
    }
  }
}

COST_V::COST_V() {
  _alloc_length = 4;
  _length = 0;
  _costs = CXX_NEW_ARRAY(COST,(INT) _alloc_length,Default_Mem_Pool);
}


void COST_V::Push(UINT16 latency, UINT16 distance, MEM_POOL *pool) {
  if (_length == _alloc_length) {
    COST *tmp = CXX_NEW_ARRAY(COST,((INT) 2*_alloc_length),pool);
    memcpy(tmp,_costs,_length*sizeof(COST));
    CXX_DELETE_ARRAY(_costs,pool);
    _costs = tmp;
    _alloc_length *= 2;
  }
  _costs[_length].Distance = distance;
  _costs[_length++].Latency = latency;
}



COST_TABLE::COST_TABLE(UINT16 num_vertex, MEM_POOL *pool)
{
  _pool = pool;
  MEM_POOL_Set_Default(_pool);
  _data = CXX_NEW_ARRAY(COST_V,((INT) num_vertex*num_vertex),pool);
  _n = num_vertex;
  _maxn = _n;
}

// Reinit an array (use the same space if possible)
void COST_TABLE::Realloc(UINT16 num_vertex)
{
  if (num_vertex <= _maxn) {
    for (INT i=0; i<num_vertex; i++) {
      for (INT j=0; j<num_vertex; j++) {
	_data[num_vertex*i+j].Init();
      }
    }
    _n = num_vertex;
  } else {
    MEM_POOL_Set_Default(_pool);
    CXX_DELETE_ARRAY(_data,_pool);
    _data = CXX_NEW_ARRAY(COST_V,((INT) num_vertex*num_vertex),_pool);
    _n = _maxn = num_vertex;
  }
}

  
// Initialize the table with the edges from the scc graph (these are
// all the valid edges)
// Use the latency graph to get the latencies of these edges
// scc_pos maps from vertex num to position among vertices of the same
//  scc.  We use it for the vertex number in the Cost Table
// Return the sum of all the latencies.  This is an upper bound on
// the value of the cycle
double COST_TABLE::Init(INT inner, LAT_DIRECTED_GRAPH16 *graph, 
	SCC_DIRECTED_GRAPH16 *scc_graph, INT scc_id, INT *scc_pos)
{
  double result = 0.0;
  BOOL pos_distance = TRUE;
  EINDEX16 scc_e = scc_graph->Get_Edge();
  while (scc_e) {
    VINDEX16 source = scc_graph->Get_Source(scc_e);
    INT source_id = scc_graph->Get_Scc_Id(source);
    if (source_id == scc_id) {
      VINDEX16 sink = scc_graph->Get_Sink(scc_e);
      INT sink_id = scc_graph->Get_Scc_Id(sink);
      if (sink_id == scc_id) {
        EINDEX16 e = graph->Get_Edge(graph->Scc_lat_vertex_map[source],
				     graph->Scc_lat_vertex_map[sink]);
        UINT latency = graph->Latency(e);
	result = result + latency;
        UINT distance=0;
        DEPV *depv = graph->Depv(e);
        if (depv) {  // is this distance positive or zero (we don't care
		     // about lexicographically negative dependences)
	  DEP dep = DEPV_Dep(depv,inner-graph->Num_Unused_Dim(e));
	  if (DEP_IsDistance(dep)) {
	    if (DEP_Distance(dep) >= 0) {
	      distance = DEP_Distance(dep);
	    } else {
	      pos_distance = FALSE;
	    }
          } else {
	    DIRECTION dir = DEP_Direction(dep);
	    if ((dir == DIR_POS) || (dir == DIR_POSEQ) || (dir == DIR_POSNEG) 
	      ||(dir == DIR_STAR)) {
	      distance = 1;
	    } else if ((dir == DIR_NEGEQ) || (dir == DIR_EQ)) {
	      distance = 0;
	    } else {
	      pos_distance = FALSE;
	    }
	  }
        }
        if (pos_distance) {
	  Push(scc_pos[source],scc_pos[sink],latency,distance);
	}
      }
    }
    scc_e = scc_graph->Get_Next_Edge(scc_e);
  }
  return (double) result;
}


// Do the algorithm
double COST_TABLE::Solve(double init_min_ii)
{
  _min_ii = init_min_ii;

  for (INT k=0; k<_n; k++) {
    for (INT i=0; i<_n; i++) {
      for (INT j=0; j<_n; j++) {
        // Consider the existing costs of paths from i to j and the
        // costs of paths from i to j via k.  Filter out those that
        // are cannot be maximal (given a min II) and replace the
        // costs of paths from i to j with the result.
	Add_Maximal_Costs(Cost_V(i,j),Cost_V(i,k),Cost_V(k,j));

	// updatin _min_ii if need be
	Update_Min_II(Cost_V(i,j),Cost_V(j,i));
      }
    }
  }
  return(_min_ii);
}

//  Add_Maximal_Costs
//
//  'Cvij', 'cvik', 'cvkj' are existing entries in the cost table.
//  set cvij U (cvik + cvkj).  In other words, we want to consider
//  paths from i to j via k as well as all the paths from i to j we
//  have already considered.  Only those that we can be maximal (given
//  the min_ii) are included in the resulting 'cvij'.
void COST_TABLE::Add_Maximal_Costs(COST_V *cvij, COST_V *cvik, COST_V *cvkj)
{
  COST *cvik_costs = cvik->Costs();
  COST *cvkj_costs = cvkj->Costs();
  UINT16 cvik_length = cvik->Length();
  UINT16 cvkj_length = cvkj->Length();

  // First consider costs in the cross product of cjik and cvkj.
  // We'll add a cost if it is maximal relative to the current costs
  // in cvij.
  INT i;
  for ( i = 0; i < cvik_length; ++i ) {
    COST *cpik      = cvik_costs + i;
    INT   ikdist   = cpik->Distance;
    INT   iklatency = cpik->Latency;
    for (INT  j = 0; j < cvkj_length; ++j ) {
      COST *cpkj       =  cvkj_costs + j;
      INT   kjdist    = cpkj->Distance;
      INT   kjlatency  = cpkj->Latency;
      INT   ikjdist   = ikdist + kjdist;
      INT   ikjlatency = iklatency + kjlatency;
      if ( Is_Max_Cost(ikjdist,ikjlatency,cvij,0)) {
        cvij->Push(ikjlatency,ikjdist,_pool);
      }
    }
  }

  // each cost in cvij is maximal relative to the preceeding
  // costs, but not necessarily relative to the succeeding costs.
  // We'll compare each cost to the succeeding costs, and delete it if
  // it is not (possibly) maximal.  Deletion is accompilshed by
  // copying the last element of cvij into the element to be deleted
  // and decrementing the length of cvij.  We consider the elements in
  // reverse order so that will not disturb the part of the vector not
  // yet processed.
  //
  COST *cvij_costs  = cvij->Costs();    /* Possibly side-effected */
  UINT16 cvij_length = cvij->Length();  /* ..by previous loop.    */
  for ( i = cvij_length - 1; i >= 0; --i ) {
    COST *cpij = cvij_costs + i;
    INT   dist   = cpij->Distance;
    INT   latency = cpij->Latency;
    if (!Is_Max_Cost(dist,latency,cvij,i+1)) {
      // Delete by replacing with last element...
      if ( i != cvij_length - 1 ) {
        COST *ij_last = cvij_costs + (cvij_length - 1);
        *cpij = *ij_last;           /* Structure copy. */
      }
      --cvij_length;
    }
  }
  cvij->Set_Length(cvij_length);
}

//  Is_Max_Cost
//
//  Is the cost <'distance','latency'> maximal relative to the elements
//  in 'cv' given 'min_ii'?   Offset gives the index of the first element
//  to check
//
BOOL COST_TABLE::Is_Max_Cost(INT dist, INT latency, COST_V *cv, INT offset)
{
  INT   len = cv->Length();
  COST *cp  = cv->Costs();
  for (INT i = offset; i < len; ++i ) {
    INT cvdist   = cp[i].Distance;
    INT cvlatency = cp[i].Latency;
    // We can reject things with duplicates, since never check a cost
    // against itself.
    if ((dist == cvdist && latency <= cvlatency) || 
	dist > cvdist && (latency - cvlatency) <= ((dist-cvdist) * _min_ii)) {
      return FALSE;
    }
  }
  return TRUE;
}


// Update min_ii give that cv1 and cv2 are cost vectors from one vertex
// to another and back
void COST_TABLE::Update_Min_II(COST_V *cv1, COST_V *cv2)
{
  COST *cp1 = cv1->Costs();
  COST *cp2 = cv2->Costs();
  INT   len1 = cv1->Length();
  INT   len2 = cv2->Length();
  for (INT i = 0; i < len1; ++i ) {
    INT dist1   = cp1[i].Distance;
    INT latency1   = cp1[i].Latency;
    for (INT j = 0; j < len2; ++j) {
      INT dist2   = cp2[j].Distance;
      INT latency2   = cp2[j].Latency;
      if ((dist1 + dist2) != 0) {
        INT path_mii = (latency1+latency2)/(dist1+dist2);
        _min_ii = MAX(_min_ii,path_mii);
      }
    }
  }
}

void COST_TABLE::Print(FILE *fp)
{
  fprintf(fp,"Printing a table \n");
  for (INT i = 0; i<_n; i++) {
    for (INT j = 0; j<_n; j++) {
      COST_V cv = _data[_n*i+j];
      if (cv.Length()) {
	fprintf(fp,"Point[%d][%d]: ",i,j);
        for (INT l = 0; l<cv.Length(); l++) {
	  fprintf(fp," (L:%d, D:%d) ",cv.Costs()[l].Latency, 
		cv.Costs()[l].Distance);
	}
	fprintf(fp,"\n");
      }
    }
  }
}


void
SYMBOL_TREE::Initialize_Innermost_Loop_Var_Symbol(WN* wn) {
  WN* loop=Enclosing_Do_Loop(wn);
  _innermost_loop_var_symb.Init(WN_index(loop));
}

bool
SYMBOL_TREE::Integer_Ref_Needs_Reg(WN* wn) {
  
  SYMBOL symb(wn);
  WN* wn1=wn;
  WN* parent=LWN_Get_Parent(wn1);
  while (WN_operator(parent)!=OPR_ARRAY &&
	 OPCODE_is_expression(WN_opcode(parent))) {
    wn1=LWN_Get_Parent(wn1);
    parent=LWN_Get_Parent(wn1);
  }
  
  if (WN_operator(parent)==OPR_ARRAY) {
    INT kid_id=0;
    INT num_dim=WN_num_dim(parent);
    while (WN_kid(parent,kid_id)!=wn1) kid_id++;
    
    // ignore scalar ref to array bases or bound of 1st dim
    
    if (1<kid_id && kid_id<=num_dim) { // appear in dim expr
      ACCESS_ARRAY *ar=(ACCESS_ARRAY*)WN_MAP_Get(LNO_Info_Map,parent);
      for (INT i=kid_id+1; i<=num_dim; i++) {
	ACCESS_VECTOR *av=ar->Dim(i-1);
	// see if innermost loop var appear in outer dim
	if (av->Loop_Coeff(av->Nest_Depth()-1)!=0) {
	  return true;
	}
      }
    } else if (num_dim<kid_id) { // appear in index expr
      if (symb!=_innermost_loop_var_symb) {
	return false;
      }
    }
  } else if (symb!=_innermost_loop_var_symb) {
    return true;
  }
  return false;
}

// Enter unique scalar refs are in the loop
// Count OPR_CONST as well.
//
// Don't count scalars that will be scalar expanded
// Instead pretend they are arrays and add them to ar
// Don't count things inside loads, since we won't need to
// store both the ILOAD and the address expression

void 
SYMBOL_TREE::Enter_Scalar_Refs(WN *wn, WN2INT *se_needed, ARRAY_REF *ar) {
  OPCODE opcode = WN_opcode(wn);
  bool is_store = false;
  bool is_reduction = false;
  
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Enter_Scalar_Refs(kid,se_needed,ar);
      kid = WN_next(kid);
    }
    return;
  } 
  
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID)  || (oper == OPR_CONST) ||
      (is_store=(OPCODE_operator(opcode) == OPR_STID))) {
    
    if (se_needed && se_needed->Find(wn)==1)
      ar->Enter_Innermost_Scalar_Expand(wn);
    else {
      SYMBOL symb(wn);
      is_reduction = red_manager && (red_manager->Which_Reduction(wn) != RED_NONE);
      if (!MTYPE_is_integral(symb.Type) ||
	  Integer_Ref_Needs_Reg(wn)) {
	Enter(&symb, is_store, /* weight */ 1, is_reduction);
      }
    }
    return;
  }

  if (OPCODE_is_store(opcode)) {
    Enter_Scalar_Refs(WN_kid0(wn),se_needed,ar);
    return;
  }
  
  if (!OPCODE_is_load(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Enter_Scalar_Refs(kid,se_needed,ar);
    }
  }
}

void
SYMBOL_TREE::Enter_Scalar_Refs(WN *wn, ARRAY_REF *ar, SX_INFO *pi, BOOL *can_be_inner,
			       INT num_loops, INT outer) {
  OPCODE opcode = WN_opcode(wn);
  bool is_store = false;
  bool is_reduction = false;
  
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Enter_Scalar_Refs(kid,ar,pi,can_be_inner,num_loops,outer);
      kid = WN_next(kid);
    }
    return;
  } 
  
  if (OPCODE_is_store(opcode)) {
    Enter_Scalar_Refs(WN_kid0(wn),ar,pi,can_be_inner,num_loops,outer);
  } else if (!OPCODE_is_load(opcode)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Enter_Scalar_Refs(kid,ar,pi,can_be_inner,num_loops,outer);
    }
  }
  
  OPERATOR oper = OPCODE_operator(opcode);
  if ((oper == OPR_LDID)  || (oper == OPR_CONST) ||
      (is_store=(OPCODE_operator(opcode) == OPR_STID))) {
    
    SYMBOL symb(wn);
    SX_PITER ii(&pi->Plist);
    SX_PNODE *n, *found_n=NULL;
    for (n = ii.First(); n; n = ii.Next()) {
      if (n->Symbol() == symb) {
	if (n->Has_Reduction()) {
	  is_reduction=TRUE;
	}
	SX_PNODE::STATUS status = n->Transformable(outer);
	if (status != SX_PNODE::SE_NOT_REQD) {
	  found_n = n;
	}
	break;
      }
    }
    
    if (found_n) {
      ar->Enter_Scalar_Expand(wn,found_n, can_be_inner, num_loops);
    } else if (!MTYPE_is_integral(symb.Type) ||
	       Integer_Ref_Needs_Reg(wn)) {
      Enter(&symb, is_store, /* weight */ 1, is_reduction);
    }
  }
}

void
SYMBOL_TREE::Enter(SYMBOL *symbol, bool is_store, INT weight, bool is_reduction) {
  if (!_symbol_node) {
    _symbol_node = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight,is_reduction),
		    _pool);
  } else {
    _symbol_node->Enter(symbol,_pool,is_store,weight, is_reduction); 
  }
};

void
SYMBOL_TREE::Num_Refs(LNO_REGS *refs, LNO_REGS *non_temp_refs) const {
  if (_symbol_node) {
    _symbol_node->Num_Refs(refs,non_temp_refs);
  }
}

// Enter a symbol into the binary true if it's a new symbol
// If is_store, set is_store in the tree
// If it's a new symbol set the weight
void
SYMBOL_TREE_NODE::Enter(SYMBOL *symbol, MEM_POOL *pool, bool is_store, INT weight,
				INT is_reduction) {
  INT result = Symbol_Compare(symbol);
  if (result == 0) {
    _is_store = _is_store || is_store;
    _is_reduction = _is_reduction || is_reduction;
  } else if (result < 0) {
    if (_left) {
      _left->Enter(symbol,pool,is_store,weight,is_reduction);
    } else {
      _left = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight,is_reduction),pool);
    }
  } else {
    if (_right) {
      _right->Enter(symbol,pool,is_store,weight,is_reduction);
    } else {
      _right = CXX_NEW(SYMBOL_TREE_NODE(*symbol,is_store,weight,is_reduction),pool);
    }
  }
}

// Return the sum of each non_temp node times its weight 
void
SYMBOL_TREE_NODE::Num_Refs(LNO_REGS *refs, LNO_REGS *non_temp_refs) const {
  
  if (_vec_idx!=LNO_REGS_IDX_UNDEFINED) {
    refs->Count_Of_Idx(_vec_idx)+=_weight;
    if (!_is_store || _is_reduction) {
      non_temp_refs->Count_Of_Idx(_vec_idx)+=_weight;
    }
  } else {
    refs->Inc_Type_Usage(_symbol.Type,_weight);
    if (!_is_store || _is_reduction) {
      non_temp_refs->Inc_Type_Usage(_symbol.Type,_weight);
    }
  }
  
  // recurse into the branches
  if (_left) {
    _left->Num_Refs(refs,non_temp_refs);
  }
  if (_right) {
    _right->Num_Refs(refs,non_temp_refs);
  }
}

INT
SYMBOL_TREE_NODE::Symbol_Compare(SYMBOL *s) { // is this <,= or > s
  if (_symbol.ST_Base() < s->ST_Base())
    return(-1);
  else if (_symbol.ST_Base() > s->ST_Base())
    return(1);
  else if (_symbol.ST_Offset() < s->ST_Offset())
    return(-1);
  else if (_symbol.ST_Offset() > s->ST_Offset())
    return(1);
  else if (_symbol.WN_Offset() < s->WN_Offset())
    return(-1);
  else if (_symbol.WN_Offset() > s->WN_Offset())
    return(1);
  else
    return(0);
}

// REGISTER_MODEL

void 
REGISTER_MODEL::Calculate_Register_Usage(WN* inner, LNO_REGS *regs_used_out) 
{
  Evaluate(inner,NULL,NULL,NULL,regs_used_out);
}

void 
REGISTER_MODEL::Evaluate(WN* inner, 
                         WN2INT* se_needed,
                         INVAR_TABLE *invar_table,
                         double* loop_cycles, 
			 LNO_REGS *regs_used_out)
{
  INT32 fp_regs_used;
  INT32 int_regs_used;
  double issue_rate;
  double num_mem_units;
  BOOL can_reg_allocate;

  BOOL register_only = (se_needed == NULL && loop_cycles == NULL);
  
  MEM_POOL_Push(_pool);
  
  _regclass_info = CXX_NEW(LNO_REGCLASS_INFO(_pool),_pool);
  _base = CXX_NEW(LNO_REGS(_regclass_info, _pool), _pool);

  bool hw_fp =  (_regclass_info->Index_Of_Type(MTYPE_F4) != 
		    _regclass_info->Index_Of_Type(MTYPE_I4));
  bool for_xtensa = false;

  if (debug_model) {
    fprintf(TFile,"Register model doing an evaluation\n");
  }
  
  // registers required for pipeline
  if (Is_Target_R8K()) {
    issue_rate = 4.0;
    _base->Count_Of_Float() = 18;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    num_mem_units = 2.0;
  } else if (Is_Target_R10K()) {
    issue_rate = 4.0;
    _base->Count_Of_Float() = 14;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    num_mem_units = 1.0;
  } else if (Is_Target_R4K()) {
    issue_rate = 1.0;
    _base->Count_Of_Float() = 12;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    num_mem_units = 1.0;
  } else if (Is_Target_R5K()) {
    issue_rate = 2.0;
    _base->Count_Of_Float() = 14;
    _base->Count_Of_Int() = 9; // $0, $26-$29, loop ub, fudge (3)
    num_mem_units = 1.0;
  } else if (Is_Target_Itanium()) {
    issue_rate = 6.0;  // 2 bundles with 3 instructions each
    _base->Count_Of_Float() = 32; // (8+1)*2+6
    _base->Count_Of_Int() = 10; // r0, r1, r12, r13, loop ub, fudge (5)
    num_mem_units = 2.0;
  } else {
    if (!Is_Target_Xtensa()) {
      Lmt_DevWarn(1, ("Warning, LNO machine model parameters are defaulted to Xtensa"));
    }
    for_xtensa = true;
    int max_slots=TI_ISA_Num_Slots(0);
    for (int i=1; i<TI_ISA_Num_Bundles(); i++) {
      max_slots = MAX(max_slots, TI_ISA_Num_Slots(i));
    }
    issue_rate = max_slots;
    if (hw_fp) _base->Count_Of_Float() = 4;
    _base->Count_Of_Int() = 3; // $sp, $ra, fudge(1)
    num_mem_units = 1.0;
    for (LNO_REGS_IDX idx = LNO_REGS_IDX_FIRST; idx<_base->Regs_Count(); idx++) {
      if (_regclass_info->Idx_Source(idx)!=LNO_REGCLASS_INFO::SRC_ISA_REGCLASS) {
	// no base registers for autotie types
	continue;
      }
      if (_base->Count_Of_Idx(idx)==0) {
	_base->Count_Of_Idx(idx)=(INT)(_base->Limit_Of_Idx(idx)*Reserved_Tie_Base_Regs);
      }
     }
  }

  // registers required for array refs
  ARRAY_REF *array_ref = CXX_NEW(ARRAY_REF(_pool),_pool);
  INT i;
  for (i=0; i<_statement_stack->Elements(); i++) {
    array_ref->Add_References(_statement_stack->Bottom_nth(i), 1,NULL);
  }

  // registers required for scalars
  SYMBOL_TREE *symbol_tree = CXX_NEW(SYMBOL_TREE(_pool),_pool);
  for (i=0; i<_statement_stack->Elements(); i++) {
    symbol_tree->Enter_Scalar_Refs(_statement_stack->Bottom_nth(i),
				   se_needed,array_ref);
  }
  LNO_REGS *scalar_refs = CXX_NEW(LNO_REGS(_regclass_info,_pool),_pool);
  LNO_REGS *scalar_regs = CXX_NEW(LNO_REGS(_regclass_info,_pool),_pool);
  symbol_tree->Num_Refs(scalar_refs,scalar_regs);  
  
  LNO_REGS *array_refs = CXX_NEW(LNO_REGS(_regclass_info,_pool),_pool);
  array_ref->Num_Refs(array_refs);

  INT inner_number = Do_Loop_Depth(inner);
  array_ref->Remove_Cse(inner_number, Max_Cse_Dist,Find_Step(inner,inner_number));
  array_ref->Mark_Invariants(inner_number);
  
  double num_fp_spills=0;
  double num_int_spills=0;
  
  REG_REF_COUNT *rr = CXX_NEW(REG_REF_COUNT(_regclass_info,_pool),_pool);
  array_ref->Calc_Regs_And_Refs(rr, FALSE);


  if (hw_fp) {
    if (rr->inv_stores.Count_Of_Float() > 4*rr->var_stores.Count_Of_Float()) {
      if (hw_fp) {
        // don't count invariants both as invariants and
        // as pipelines
	_base->Count_Of_Float() /= 3;
      }
    }
  }

  
  can_reg_allocate = TRUE;
  
  for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx<_base->Regs_Count(); idx++) {
    INT regs_used = _base->Count_Of_Idx(idx) + 
		    scalar_regs->Count_Of_Idx(idx) + 
		    rr->regs.Count_Of_Idx(idx);
    regs_used_out->Count_Of_Idx(idx) = regs_used;
    INT extra = regs_used - _base->Limit_Of_Idx(idx);
    INT extra_refs = extra*2;
    if (extra>0) {
      rr->refs.Count_Of_Idx(idx) += extra_refs;
      can_reg_allocate = FALSE;
    }
  }
  if (debug_model) {
    fprintf(TFile,"can_reg_allocate is %d \n",can_reg_allocate);
  }

  if (register_only && can_reg_allocate) {
    CXX_DELETE(array_ref,_pool);
    MEM_POOL_Pop(_pool);
    return;
  }


  double OP_issue = 0.0, op_cycles;
  op_cycles = LOOP_MODEL::OP_Cycles(this, &OP_issue, invar_table, _pool);
  if (op_cycles == -1.0) {
    OP_issue = Count_Op();
  }
  if (debug_model) {
    fprintf(TFile,"op_cycles is %lf \n",op_cycles);
  }

  double LOOP_INIT_issue;
  if (xt_zero_cost_loop && Enable_ZCL) {
      LOOP_INIT_issue = 0.0;
  } else {
      LOOP_INIT_issue = 2.0;
  }

  // count memory references
  double MEM_issue = 0.0;
  double MEM_issue_minus_spills = 0.0;
  for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx<rr->refs.Regs_Count(); idx++) {
    MEM_issue += (double) rr->refs.Count_Of_Idx(idx);
    MEM_issue_minus_spills +=
    	(double) (rr->refs.Count_Of_Idx(idx)-regs_used_out->Count_Of_Idx(idx)); 
  }
  if (debug_model) {
    fprintf(TFile,"MEM_issue is %lf \n",MEM_issue);
  }
  double MEM_rcycles = MEM_issue/num_mem_units;
  
  double MEM_rcycles_minus_spills = MEM_issue_minus_spills/num_mem_units;
  
  double issue_limit =
	(OP_issue+LOOP_INIT_issue+MEM_issue) / issue_rate;
  double issue_limit_minus_spills = 
    (OP_issue+LOOP_INIT_issue+MEM_issue_minus_spills)/ issue_rate;
  double resource_cycles = MAX(op_cycles,
			       MAX(MEM_rcycles,issue_limit));
  double resource_cycles_minus_spills =
	MAX(op_cycles,
	    MAX(MEM_rcycles_minus_spills,issue_limit_minus_spills));
  
  double cycles = resource_cycles;
  double cycles_minus_spills = resource_cycles_minus_spills;

  if (can_reg_allocate==FALSE &&
      (cycles == cycles_minus_spills)) { // spilling is free so set the 
					 // number of registers
			                 // to target limit
    can_reg_allocate = TRUE;
    for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx<regs_used_out->Regs_Count(); idx++) {
      regs_used_out->Count_Of_Idx(idx) = regs_used_out->Limit_Of_Idx(idx);
    }
  }
  
  if (can_reg_allocate) {
    for (LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED; idx<regs_used_out->Regs_Count(); idx++) {
      if (regs_used_out->Limit_Of_Idx(idx)>8) {
        if (regs_used_out->Count_Of_Idx(idx) > (regs_used_out->Limit_Of_Idx(idx)-2)) {
          cycles *= 1.1;  // penalty for being close to unallocatable
        }
      }
    }
  }
  
  
  CXX_DELETE(array_ref,_pool);
  MEM_POOL_Pop(_pool);
  
  if (!register_only)
    *loop_cycles=cycles;
  
}

// How many fp/complex/quad ops in the system
// complex and quad count as two
// This is an approximation for FP_issue and INT_issue
// in the cases where we can't model
double 
REGISTER_MODEL::Count_Op()
{
  double result = 0.0;
  for (INT i = 0; i < _statement_stack->Elements(); i++) {
    result += Count_Op(_statement_stack->Bottom_nth(0));
  }
  return result;
}

double 
REGISTER_MODEL::Count_Op(WN* wn)
{
  double result = 0.0;
  OPCODE opcode = WN_opcode(wn);
  if (opcode == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      result += Count_Op(kid);
      kid = WN_next(kid);
    }
  } else if (!OPCODE_is_leaf(opcode)) {
    TYPE_ID ti = OPCODE_rtype(opcode);
    TYPE_ID ti2 = OPCODE_desc(opcode);
    if ((ti == MTYPE_C4) || (ti == MTYPE_C8) || (ti == MTYPE_CQ) ||
        (ti2 == MTYPE_C4) || (ti2 == MTYPE_C8) || (ti2 == MTYPE_CQ) ||
        (ti == MTYPE_FQ) || (ti2 == MTYPE_FQ)) {
        result = 2.0;
    } else if ((ti == MTYPE_F4) || (ti == MTYPE_F8) || 
        (ti2 == MTYPE_F4) || (ti2 == MTYPE_F8)) { 
        result = 1.0;
    } else if ((ti == MTYPE_B) || (ti == MTYPE_I1) || (ti == MTYPE_I2) ||
               (ti == MTYPE_I4) || (ti == MTYPE_I8) || (ti == MTYPE_U1) ||
	       (ti == MTYPE_U2) || (ti == MTYPE_U4) || (ti == MTYPE_U8) ||
               (ti2 == MTYPE_B) || (ti2 == MTYPE_I1) || (ti2 == MTYPE_I2) ||
               (ti2 == MTYPE_I4) || (ti2 == MTYPE_I8) || (ti2 == MTYPE_U1) ||
	       (ti2 == MTYPE_U2) || (ti2 == MTYPE_U4) || (ti2 == MTYPE_U8)) {
        result = 1.0;
    }
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      result += Count_Op(kid);
    }
 }
 return result;
}

void
LOOP_MODEL::Model_Performance_Trace() {
#define TLOG_MSG_LENGTH 640	// 30 chars * 16 loops + misc.
  char message[TLOG_MSG_LENGTH];
  char buffer[32];
  UINT16 char_count=0;
  message[0]='\0';
  
  sprintf(buffer,"comb=%d ",_num_evaluations);
  if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
    DevWarn("Tlog message buffer overflow");
    char_count=0;
    message[0]='\0';
  } else {
    strcpy(message+char_count, buffer);
    char_count+=strlen(buffer);
  }
  
  for (INT b=0; b<_num_loops; b++) {
    Is_True(_new_order[b]>=0 && _new_order[b]<_num_loops,("Bad new order"));
    sprintf(buffer,"<%d,%d>", _new_order[b], _block_number[_new_order[b]]);
    if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
      DevWarn("Tlog message buffer overflow");
      char_count=0;
      message[0]='\0';
    } else {
      strcpy(message+char_count, buffer);
      char_count+=strlen(buffer);
    }
  }
  if (_nstrips > 0) {
    sprintf(buffer," sd=%d ",_stripdepth);
    if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
      DevWarn("Tlog message buffer overflow");
      char_count=0;
      message[0]='\0';
    } else {
      strcpy(message+char_count, buffer);
      char_count+=strlen(buffer);
    }
    for (INT s=0; s<_nstrips; s++) {
      sprintf(buffer,"(%d,%d)",_iloop[s],_stripsz[s]);
      if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
	DevWarn("Tlog message buffer overflow");
	char_count=0;
	message[0]='\0';
      } else {
	strcpy(message+char_count, buffer);
	char_count+=strlen(buffer);
      }
    }
  }
  sprintf(buffer," cycles=%f fpreg=%d", _num_cycles, _regs->Count_Of_Float());
  if (char_count+strlen(buffer)>TLOG_MSG_LENGTH) {
    DevWarn("Tlog message buffer overflow");
    char_count=0;
    message[0]='\0';
  } else {
    strcpy(message+char_count, buffer);
    char_count+=strlen(buffer);
  }
  sprintf(buffer,"INNER_LOOP %d",
	  Srcpos_To_Line(_snl_line_numbers[_new_order[_num_loops-1]]));
  
  SRCPOS srcpos=WN_Get_Linenum(_wn);
  Generate_Tlog("LNO","snl", Srcpos_To_Line(srcpos),
                ST_name(WN_st(WN_index(_wn))), message, buffer, "");
  
}

// Output results to analysis file
void 
LOOP_MODEL::Model_Results_Analysis(INT inner, 
                                   INT num_loops, 
                                   INT outermost_can_be_tiled,
                                   double machine_cycles, 
                                   double cache_cycles, 
                                   double overhead_cycles)
{
  fprintf(LNO_Analysis,"    (IF_INNER %d \n",
      Srcpos_To_Line(_snl_line_numbers[inner]));
  fprintf(LNO_Analysis,"        (CYCLES %g \n",_num_cycles_inner);
  if (machine_cycles < 0.0) {
    if (_OP_resource_count==NULL) {
      fprintf(LNO_Analysis,"            (0 \"Can't model fp resources\")");
    } else {
      fprintf(LNO_Analysis,"            (0 \"Requires too many registers\")");
    }
  } else {
    switch (_model_limit)  {
      case MODEL_LIMIT_UNSET: 
        fprintf(LNO_Analysis,"            (%g \"\")\n",machine_cycles);
	break;
      case MODEL_LIMIT_IDEAL: 
        fprintf(LNO_Analysis,"            (%g \"Ideal Schedule\")\n",
							machine_cycles);
	break;
      case MODEL_LIMIT_RES: 
        fprintf(LNO_Analysis,"            ");
	fprintf(LNO_Analysis,"(%g \"Resource Limited Schedule\")\n",
							machine_cycles);
	break;
      case MODEL_LIMIT_LAT: 
        fprintf(LNO_Analysis,"            ");
	fprintf(LNO_Analysis,"(%g \"Latency Limited Schedule\")\n",
							machine_cycles);
	break;
    }
  }
  fprintf(LNO_Analysis,"            %g\n",cache_cycles);
  fprintf(LNO_Analysis,"            %g)\n",overhead_cycles);
  fprintf(LNO_Analysis,"        (FP_REGISTERS %d) \n",_regs_inner->Count_Of_Float());

  fprintf(LNO_Analysis,"        (TRANSFORMATIONS\n");

  fprintf(LNO_Analysis,"            (UNTILED_ORDER");
  INT i;
  for (i=outermost_can_be_tiled; i<num_loops; i++) {
    fprintf(LNO_Analysis," %d", 
	Srcpos_To_Line(_snl_line_numbers[_new_order_inner[i]]));
  }
  fprintf(LNO_Analysis,")");

  INT unroll_entries = 0;
  for (i=outermost_can_be_tiled; i<num_loops; i++) {
    if (_block_number_inner[i] > 1)
      unroll_entries++;
  }
  if (unroll_entries) {
    fprintf(LNO_Analysis,"\n            (UNROLL");
    for (i=outermost_can_be_tiled; i<num_loops; i++) {
      if (_block_number_inner[i] > 1)
        fprintf(LNO_Analysis," (%d %d)",
                Srcpos_To_Line(_snl_line_numbers[i]),_block_number_inner[i]);
    }
    fprintf(LNO_Analysis,")");
  }
  if (_nstrips_inner) {
    fprintf(LNO_Analysis,"\n            (BLOCKING");
    for (INT s = 0; s < _nstrips_inner; s++) {
      INT i = _iloop_inner[s];
      fprintf(LNO_Analysis," (%d %d L%d %d)", 
              Srcpos_To_Line(_snl_line_numbers[_new_order_inner[i]]),
              _stripsz_inner[s],
              _striplevel_inner[s],
              Srcpos_To_Line(_snl_line_numbers[_new_order_inner[_stripdepth_inner]]));
    }
    fprintf(LNO_Analysis,")");
  }
  fprintf(LNO_Analysis,"))\n");
}

void
LOOP_MODEL::Model_Report() {
  for (INT report=0; report<2; report++) {
    FILE *file = NULL;
    if (report==0 && debug_model) {
      file = TFile;
    } else if (report==1 && LNO_Verbose) {
      file = stdout;
    }
    if (file) {
      fprintf(file,"\nLOOP_MODEL evaluated %d combinations. RESULTS:\n",_num_evaluations);
      fprintf(file,"  Transform<loop,reg> = (");
      for (INT b=0; b<_num_loops; b++) {
	INT no = _new_order[b];
	Is_True(no>=0 && no<_num_loops,("Bad new order."));
	fprintf(file,"<%d,%d", no, _block_number[no]);
	if (_simd_loop && _simd_loop->Simd_Loop_Level()==no) {
	  fprintf(file,",simd(%d)",_simd_loop->Simd_Unroll()[no]);
	}
	fprintf(file,">");
      }
      if (_nstrips > 0) {
	fprintf(file," cblk[stripdepth=%d]=",_stripdepth);
	for (INT s=0; s<_nstrips; s++)
	  fprintf(file,"(%d,%d)",_iloop[s],_stripsz[s]);
      }
      fprintf(file,")\n");
      if (_OP_resource_count==NULL)
	fprintf(file,"  COULDN'T evaluate ops\n");
      else {
	fprintf(file,"  Cycles %0.2f, regs: ",_num_cycles);
	_regs->Print(file);
	fprintf(file,"\n");
      }
    }
  }
}

LNO_REGS::LNO_REGS(const LNO_REGCLASS_INFO *regclass_info, MEM_POOL *pool) : 
  _pool(pool), _regclass_info(regclass_info),
  _regs_count(regclass_info->Regs_Count()) {
  
  _cnt = CXX_NEW_ARRAY(INT,_regs_count,_pool);
  _limit = CXX_NEW_ARRAY(INT,_regs_count,_pool);
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<Regs_Count(); idx++) {
    if (regclass_info->Idx_Source(idx) == LNO_REGCLASS_INFO::SRC_ISA_REGCLASS) {
      _limit[idx] = regclass_info->Regclass_Reg_Count(regclass_info->Regclass_Of_Idx(idx));
    } else {
      _limit[idx] = INT32_MAX;
    }
  }
  memset(_cnt,0,sizeof(INT)*_regs_count);
}

LNO_REGS::LNO_REGS(const LNO_REGS *regs, MEM_POOL *pool) :
  _pool(pool), _regclass_info(regs->_regclass_info),
  _regs_count(regs->_regs_count) {
  
  _cnt = CXX_NEW_ARRAY(INT,_regs_count,_pool);
  _limit = CXX_NEW_ARRAY(INT,_regs_count,_pool);
  memcpy(_cnt,regs->_cnt,sizeof(INT)*_regs_count);
  memcpy(_limit,regs->_limit,sizeof(INT)*_regs_count);
}

LNO_REGS::~LNO_REGS() {
}

LNO_REGS &
LNO_REGS::operator = (const LNO_REGS &regs) {
  _regclass_info = regs._regclass_info;
  if (_regs_count != regs._regs_count) {
    _regs_count = regs._regs_count;
    _cnt = CXX_NEW_ARRAY(INT,_regs_count,_pool);
  }
  memcpy(_cnt,regs._cnt,sizeof(INT)*_regs_count);
  return *this;
}

void
LNO_REGS::Clear() {
  memset(_cnt,0,sizeof(INT)*_regs_count);
}

void
LNO_REGS::Add(const LNO_REGS *regs) {
  Is_True(Regs_Count()==regs->Regs_Count(),("Unequal register class count."));
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<Regs_Count(); idx++) {
    Count_Of_Idx(idx)+=regs->Count_Of_Idx(idx);
  }
}

void
LNO_REGS::Sub(const LNO_REGS *regs) {
  Is_True(Regs_Count()==regs->Regs_Count(),("Unequal register class count."));
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<Regs_Count(); idx++) {
    Count_Of_Idx(idx)-=regs->Count_Of_Idx(idx);
  }
}

INT
LNO_REGS::Total() {
  INT total = 0;
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<Regs_Count(); idx++) {
    total+=Count_Of_Idx(idx);
  }
  return total;
}

INT &
LNO_REGS::Limit_Of_Idx(LNO_REGS_IDX idx) {
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _limit[idx];
}

INT
LNO_REGS::Limit_Of_Idx(LNO_REGS_IDX idx) const {
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _limit[idx];
}

INT &
LNO_REGS::Count_Of_Idx(LNO_REGS_IDX idx) {
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

INT
LNO_REGS::Count_Of_Idx(LNO_REGS_IDX idx) const {
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

INT &
LNO_REGS::Count_Of_Type(TYPE_ID type) {
  LNO_REGS_IDX idx = _regclass_info->Index_Of_Type(type);
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

INT
LNO_REGS::Count_Of_Type(TYPE_ID type) const {
  LNO_REGS_IDX idx = _regclass_info->Index_Of_Type(type);
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

INT &
LNO_REGS::Count_Of_At_Ty_Id(AT_TY_ID ty_id) {
  LNO_REGS_IDX idx = _regclass_info->Index_Of_At_Ty_Id(ty_id);
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

INT
LNO_REGS::Count_Of_At_Ty_Id(AT_TY_ID ty_id) const {
  LNO_REGS_IDX idx = _regclass_info->Index_Of_At_Ty_Id(ty_id);
  Is_True(idx>=LNO_REGS_IDX_UNDEFINED && idx<Regs_Count(),
	  ("Invalid register class index"));
  return _cnt[idx];
}

void
LNO_REGS::Inc_Type_Usage(TYPE_ID type, INT uses) {
  INT regs_per_use = LNO_REGCLASS_INFO::Reg_Count_Of_Type(type);
  Count_Of_Type(type)+=uses*regs_per_use;
}

void
LNO_REGS::Print(FILE *f, INT indent, bool verbose) {
  fprintf(f,"%*s{",indent,"");
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<Regs_Count(); idx++) {
    fprintf(f," [%d] %d",idx,Count_Of_Idx(idx));
  }
  fprintf(f," }");
}

LNO_REGCLASS_INFO::LNO_REGCLASS_INFO(MEM_POOL *pool, AT_TY_TAB *at_ty_tab) :
  _pool(pool), _last_idx(0),
  _idx_regclass_size(0), _idx_regclass(NULL),
  _idx_at_ty_size(0), _idx_at_ty(NULL),
  _type_id_idx_size(0), _type_id_idx(NULL),
  _at_ty_tab(at_ty_tab), _at_ty_idx_size(0), _at_ty_idx(NULL),
  _target(NULL) {
  
  // initialize a map from a TYPE_ID to the index of its related register
  // class
  _type_id_idx_size = Mtype_Last+1;
  _type_id_idx = CXX_NEW_ARRAY(LNO_REGS_IDX,_type_id_idx_size,_pool);
  _type_id_idx[MTYPE_UNKNOWN] = LNO_REGS_IDX_UNDEFINED;

  // for each TYPE_ID (mtype)
  for (TYPE_ID type = MTYPE_FIRST; type<=Mtype_Last; type++) {
    ISA_REGCLASS type_regclass = TI_ISA_Regclass_For_Mtype(type);
    
    if (type_regclass == ISA_REGCLASS_UNDEFINED) {
      _type_id_idx[type] = LNO_REGS_IDX_UNDEFINED;
      continue;
    }
    
    LNO_REGS_IDX idx = LNO_REGS_IDX_UNDEFINED;
    for (TYPE_ID prev_type = MTYPE_FIRST; prev_type<type; prev_type++) {
      ISA_REGCLASS prev_type_regclass = TI_ISA_Regclass_For_Mtype(prev_type);
      if (prev_type_regclass == type_regclass) {
	idx = _type_id_idx[prev_type];
	break;
      }
    }
    if (idx==LNO_REGS_IDX_UNDEFINED) {
      idx = ++_last_idx;
    }
    _type_id_idx[type] = idx;
  }
  
  // initialize the index to ISA_REGCLASS map
  _idx_regclass_size = _last_idx+1;
  _idx_regclass = CXX_NEW_ARRAY(ISA_REGCLASS,_idx_regclass_size,_pool);
  
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_UNDEFINED; idx<=_last_idx; idx++) {
    _idx_regclass[idx]=ISA_REGCLASS_UNDEFINED;
  }
  
  for (TYPE_ID type = MTYPE_FIRST; type<=Mtype_Last; type++) {
    LNO_REGS_IDX type_idx = _type_id_idx[type];
    if (type_idx!=LNO_REGS_IDX_UNDEFINED) {
      Is_True(type_idx<=_last_idx,("Bad type index"));
      ISA_REGCLASS type_regclass = TI_ISA_Regclass_For_Mtype(type);
      Is_True(type_regclass!=ISA_REGCLASS_UNDEFINED,("Undefined register class"));
      _idx_regclass[type_idx] = type_regclass;
    }    
  }
  
  if (at_ty_tab!=NULL) {
    _at_ty_idx_size = at_ty_tab->Last_Ty_Id() + 1;
    
    // initialize a map from a AT_TY_ID to the index of its related register
    // class
    _at_ty_idx = CXX_NEW_ARRAY(LNO_REGS_IDX,_at_ty_idx_size,_pool);
    _at_ty_idx[AT_TY_ID_UNKNOWN] = LNO_REGS_IDX_UNDEFINED;
    
    for (AT_TY_ID ty_id = AT_TY_ID_FIRST; ty_id <= at_ty_tab->Last_Ty_Id(); ty_id++) {
      AT_TY *at_ty = at_ty_tab->Get_Type(ty_id);
      
      if (at_ty->Vector_Length() == 1 || at_ty->Pointer()) {
	// Original scalar type -- check its index with ISA_REGCLASS maps
	TYPE_ID mtype = MTYPE_UNKNOWN;
	if (at_ty->Pointer()) {
	  // FIXME: get pointer type (MTYPE_A4)?
	  mtype = MTYPE_U4;
	} else if (at_ty->Kind()==AT_TY_KIND_ATYPE) {
	  mtype = AT_FACTORY::atype_to_mtype(at_ty->Atype());
	} else if (at_ty->Kind()==AT_TY_KIND_TIE_TYPE) {
	  AT_TIE_TYPE *tie_type = at_ty->Tie_Type();
	  mtype = tie_info->mtype_id(tie_type->name());
	} else {
	  FmtAssert(0,("Unknown AT_TY kind %d.",(INT)at_ty->Kind()));
	}
	LNO_REGS_IDX idx = Index_Of_Type(mtype);
	_at_ty_idx[ty_id] = idx;
	continue;
      }
      
      // allocate a new index for each non-scalar (modified) Auto TIE type
      LNO_REGS_IDX idx = ++_last_idx;
      _at_ty_idx[ty_id] = idx;
    }
    
    // initialize the index to AT_TY_ID map
    _idx_at_ty_size = _last_idx-_idx_regclass_size+1;
    if (_idx_at_ty_size>0) {
      _idx_at_ty = CXX_NEW_ARRAY(AT_TY_ID,_idx_at_ty_size,_pool);
      for (LNO_REGS_IDX idx=_idx_regclass_size; idx<=_last_idx; idx++) {
	_idx_at_ty[idx-_idx_regclass_size]=AT_TY_ID_UNKNOWN;
      }
      for (AT_TY_ID ty_id = AT_TY_ID_FIRST; ty_id <= at_ty_tab->Last_Ty_Id(); ty_id++) {
	LNO_REGS_IDX type_idx = _at_ty_idx[ty_id];
	if (type_idx>=_idx_regclass_size) {
	  Is_True(type_idx<=_last_idx,("Bad type index"));
	  _idx_at_ty[type_idx-_idx_regclass_size] = ty_id;
	}
      }
    }
  }

  // initialize target machine registers
  Init_Target_Regs();
}

LNO_REGCLASS_INFO::~LNO_REGCLASS_INFO() {
}

LNO_REGS_IDX
LNO_REGCLASS_INFO::Index_Of_Type(TYPE_ID type) const {
  Is_True(type<_type_id_idx_size,("Type out of range"));
  return _type_id_idx[type];
}

LNO_REGS_IDX
LNO_REGCLASS_INFO::Index_Of_At_Ty_Id(AT_TY_ID ty_id) const {
  if (ty_id < _at_ty_idx_size)
    return _at_ty_idx[ty_id];
  
  return LNO_REGS_IDX_UNDEFINED;
}

LNO_REGCLASS_INFO::SRC
LNO_REGCLASS_INFO::Idx_Source(LNO_REGS_IDX idx) const {
  if (idx<=LNO_REGS_IDX_UNDEFINED) {
    return SRC_UNKNOWN;
  } else if (idx<_idx_regclass_size) {
    return SRC_ISA_REGCLASS;
  } else if (idx<=_last_idx) {
    return SRC_AT_TY_ID;
  }
  return SRC_UNKNOWN;
}

ISA_REGCLASS
LNO_REGCLASS_INFO::Regclass_Of_Idx(LNO_REGS_IDX idx) const {
  if (idx==LNO_REGS_IDX_UNDEFINED) {
    return ISA_REGCLASS_UNDEFINED;
  }
  Is_True(Idx_Source(idx)==SRC_ISA_REGCLASS,("Index out of range"));
  return _idx_regclass[idx];
}

AT_TY_ID
LNO_REGCLASS_INFO::At_Ty_Id_Of_Idx(LNO_REGS_IDX idx) const {
  if (idx==LNO_REGS_IDX_UNDEFINED) {
    return AT_TY_ID_UNKNOWN;
  }
  Is_True(Idx_Source(idx)==SRC_AT_TY_ID,("Index out of range"));
  return _idx_at_ty[idx-_idx_regclass_size];
}

AT_TY *
LNO_REGCLASS_INFO::At_Ty_Of_Idx(LNO_REGS_IDX idx) const {
  if (idx==LNO_REGS_IDX_UNDEFINED) {
    return NULL;
  }
  Is_True(Idx_Source(idx)==SRC_AT_TY_ID,("Index out of range"));
  return _at_ty_tab->Get_Type(At_Ty_Id_Of_Idx(idx));
}

const char *
LNO_REGCLASS_INFO::Regclass_Name_Of_Idx (LNO_REGS_IDX idx)
{
  SRC src = Idx_Source(idx);
  if (src == SRC_ISA_REGCLASS) {
    return Regclass_Name(Regclass_Of_Idx(idx));
  }

  if (src == SRC_AT_TY_ID) {
    AT_TY *ty = At_Ty_Of_Idx(idx);
    Is_True(ty, ("Null AT_TY"));
    
    return ty->Tie_Full_Name(true);
  }
  
  return "<unknown>";
}

/* we don't want unroll to generate too big register file */
#define MAX_AT_REG_SIZE 12

void
LNO_REGCLASS_INFO::Init_Target_Regs() {
  _target = CXX_NEW(LNO_REGS(this,Pool()),Pool());
  for (LNO_REGS_IDX idx=LNO_REGS_IDX_FIRST; idx<Regs_Count(); idx++) {
    SRC src = Idx_Source(idx);
    if (src == SRC_ISA_REGCLASS) {
      ISA_REGCLASS regclass = Regclass_Of_Idx(idx);
      Is_True(regclass != ISA_REGCLASS_UNDEFINED, ("Wrong index to register class map"));
      _target->Count_Of_Idx(idx)=Regclass_Reg_Count(regclass);
    } else if (src == SRC_AT_TY_ID) {
      // just some big number should be ok
      _target->Count_Of_Idx(idx)=MAX_AT_REG_SIZE;  
    }
  }
}

const char *
LNO_REGCLASS_INFO::Regclass_Name(ISA_REGCLASS regclass) {
  const ISA_REGCLASS_INFO *info = TI_ISA_Regclass_Info(regclass);
  const char *name = TI_ISA_Regclass_Name(info);
  return name;
}

INT
LNO_REGCLASS_INFO::Regclass_Reg_Count(ISA_REGCLASS regclass) {
  const ISA_REGCLASS_INFO *info = TI_ISA_Regclass_Info(regclass);
  Is_True(info,("Null register class info"));
  INT reg_count = TI_ISA_Regclass_Last_Reg(info)-TI_ISA_Regclass_First_Reg(info)+1;
  return reg_count;
}

INT
LNO_REGCLASS_INFO::Reg_Count_Of_Type(TYPE_ID type) {
  INT reg_count = TI_ISA_Regsize_For_Mtype (type);
  return reg_count;
}

void
LNO_REGCLASS_INFO::Print(FILE *f, INT indent, bool verbose) {
  fprintf(f,"%*sLNO_REGCLASS_INFO {\n",indent,"");
  {
    indent+=2;

    fprintf(f,"%*sIDX_TO_REGCLASS_MAP {\n",indent,"");
    {
      indent+=2;
      for (INT idx=0; idx<Regs_Count(); idx++) {
	fprintf(f,"%*s[%d] %s\n",indent,"",idx,Regclass_Name_Of_Idx(idx));
      }
      indent-=2;
    }
    fprintf(f,"%*s}\n",indent,"");
    
    fprintf(f,"%*sTARGET ",indent,"");
    _target->Print(f);
    fprintf(f,"\n");
    
    indent-=2;
  }
  fprintf(f,"%*s}\n",indent,"");
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

