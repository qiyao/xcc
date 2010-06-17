/*

  Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.

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

*/
// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/targ_info/libti/xtmicroarch.cxx#1 $

#include "libti.h"
#include "ti_si.h"
#include "trace.h"
#include "xtarch.h"
#include "xtmicroarch.h"
#include "xtmap.h"
#include "util.h"

/* in common/com/xtensa/config_targ_options.cxx */
extern DLL_SHARED BOOL xt_density;
extern DLL_SHARED BOOL xt_flix;

/* We include the generator objects that are used to generate the targ_info
   scheduling information. We feed these objects scheduling info based on
   libisa. Then, instead of outputing a C file describing the scheduling
   data structures, we simply create the data structiures dynamically. */
#define SI_GEN_DYNAMIC
#include "si_gen.cxx"

static SLOT_BIT_VECTOR_t* opcode_slot_table = NULL;
static SLOT_BIT_VECTOR_t* format_slot_table = NULL;
static INT num_format_resources = 0;

class SLOT_RESOURCE {

  // this class represents a resource for an issue constraint
  // for a format

  // _bv is a bit vector of the set of slots represented by the resource
  // _res is the resource created
  // _name is the name of the resource
  // _fmt is the format

  SLOT_BIT_VECTOR_t _bv;
  RESOURCE _res;
  const char* _name;
  xtensa_format _fmt;

public:
  SLOT_RESOURCE():_bv(0),_res(0), _name(NULL), _fmt(0) {};
  ~SLOT_RESOURCE() {};
  void init(const SLOT_BIT_VECTOR_t bv, const RESOURCE res, const char* name,
	    const xtensa_format fmt)
	{ _bv = bv; _res = res; _name = name; _fmt = fmt; }
  SLOT_BIT_VECTOR_t slots() const { return _bv; }
  xtensa_format fmt() const { return _fmt; }
  RESOURCE resource() const { return _res; }
  const char* name() const { return _name; }
};

// we could have used TI_TIE_SLOTS_MAX below but we add 1 to make it power of 2
#define Get_Format_Slot_Key(fmt,slot) ((fmt)*(TI_TIE_SLOTS_MAX+1)+(slot))
int slot_res_size = 0;

typedef UTL_Map<SLOT_BIT_VECTOR_t, UINT, UTL_Map_Int64Hash> issueResourceMap;
typedef UTL_Map<INT, UINT, UTL_Map_IntHash> slotMap;

static slotMap* slot_map_p;
static issueResourceMap* issue_resource_map_p;
static SLOT_RESOURCE* slot_res = 0;

XTM_MicroArchitecture::XTM_MicroArchitecture (MEM_POOL * pool, XT_Architecture_p arch) :
  _pool(pool), _arch(arch), _isa(_arch->isa()), _resourceMap(pool, 127),
  _core_slot_resource(NULL), _prefer_mul16(false),
  _core_format(XTENSA_UNDEFINED), _mov_n_initialized(false)
{
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::XTM_MicroArchitecture");

  _initialized = FALSE;

  // we allow at most 64 bit to support all slots
  // and each slot uses 1 bit in SLOT_BIT_VECTOR_t
  if (sizeof(SLOT_BIT_VECTOR_t)!=8)
    return; // _initialized = FALSE

  /* Set the pool to be used in si_gen.cxx. 'sched_pool' is declared there. */
  sched_pool = _pool;

  /* Initialize for scheduling... */
  Machine("xtensa", ISA_SUBSET_xtensa, 0, NULL);
  
  /* Read the formats/slots information and create resources */
  /* this has to be done before parseSchedule() */
  if (!readIssueFormats())
    return; // _initialized = FALSE

  /* Read the function units, creating a resource for each. */
  readFunctionUnits();
    
  /* Parse scheduling information for the core instructions and tie; and for the
     simulated instructions. */
  if (!parseSchedule())
    return;

  genericSchedule();
  simulatedSchedule();

  /* Extract scheduling information from si_gen objects into the tables
     expected by the scheduling interface. Generating the information has
     side-effects on the objects and at trace level 99 we want to output the
     objects using the C print routines, so we don't want to generate then (see
     debugGenerated for more). */
#ifdef Is_True_On
  if (!TRACE_IS_ON(9))
#endif
    if (!Machine_Done(&_resource_count, 
		      &_resources,
		      &_RRW_initializer,
		      &_RRW_overuse_mask,
		      &_RRW_format_resource_overuse_mask,
		      &_issue_slot_count,
		      &_issue_slots,
		      &_top_si,
		      &_ID_count,
		      &_ID_si))
    return;  // _initialized = FALSE

  _initialized = TRUE;
}


// recursively form a member of the power set of the original format resources
//
// the new resources in the power set are used if the slots used by an opcode
// (which is represented as a format resource) partially overlaps with the
// slots in a format resource. This can only happens if the slots are from
// the same format. Therefore we can skip the members of power set if the
// format does not match.
//
// note: the caller need to make sure 'more' is >=1.
//
void
XTM_MicroArchitecture::find_power_set(
		    xtensa_format bv_fmt, SLOT_BIT_VECTOR_t bv_orig,
		    int begin, int end, int more) {

  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::find_power_set");

  xtensa_isa isa = _isa;

  Is_True(more>=1, ("Invalid 'more' value"));

  for (UINT i = begin; i < end; i++) {

      SLOT_BIT_VECTOR_t resource_i = slot_res[i].slots();
      xtensa_format fmt = slot_res[i].fmt();

      if (bv_fmt!= XTENSA_UNDEFINED && bv_fmt!=fmt)
	continue;

      SLOT_BIT_VECTOR_t bv = bv_orig | resource_i;

      if (more>1) {
	find_power_set(fmt, bv, i+1, end, more-1);
      } else {

	constructFormatResource(bv, fmt);
      }
  }

  LEAVE;
}

char*
XTM_MicroArchitecture::constructFormatResourceName (
		SLOT_BIT_VECTOR_t bv, xtensa_format fmt)
{
  xtensa_isa isa = _isa;
  UINT format_num_slots = xtensa_format_num_slots(isa, fmt);
  size_t res_name_buf_len = (strlen("_SLOT_Res") + 8 +
			strlen(xtensa_format_name(isa,fmt)) + 8);
  char* res_name = TYPE_MEM_POOL_ALLOC_N(char, _pool, res_name_buf_len);
  sprintf(res_name, "_SLOT_Res%d_%s",
			num_format_resources, xtensa_format_name(isa,fmt));
  size_t res_name_len = strlen(res_name);
	
  for (UINT slot=0; slot<format_num_slots; slot++) {

    INT format_slot_key = Get_Format_Slot_Key(fmt,slot);
    UINT slot_id;
    slot_map_p->find(format_slot_key, &slot_id);

    if (bv & (1ULL << slot_id)) {
      size_t new_buf_len = (res_name_len +
      strlen(xtensa_format_slot_name(isa,fmt,slot)) + 8);
      if (new_buf_len > res_name_buf_len)
      {
	res_name = TYPE_MEM_POOL_REALLOC_N(char, _pool, res_name,
	res_name_buf_len, new_buf_len);
	res_name_buf_len = new_buf_len;
      }

      sprintf(res_name+res_name_len, "_%s",
		      	xtensa_format_slot_name(isa,fmt,slot));
      res_name_len = strlen(res_name);
      FmtAssert(res_name_len < res_name_buf_len,
			("issue resource name %s too long", res_name));
    }
  }
  return res_name;
}

void
XTM_MicroArchitecture::constructFormatResource (
		SLOT_BIT_VECTOR_t bv, xtensa_format fmt)
{
      M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::constructFormatResource");

      UINT dummy;
      if (bv !=0 && !issue_resource_map_p->find(bv, &dummy)) {

	issue_resource_map_p->insert(bv, num_format_resources);

	char* res_name = constructFormatResourceName(bv, fmt);
	UINT copies = TARG_INT_Pop_Count(bv);

	RESOURCE res;
	FmtAssert(!_resourceMap.find(res_name, &res),
		("Resource name %s conflict", res_name));
	res = RESOURCE_Create(res_name, copies, fmt);
	_resourceMap.insert(res_name, res);

	TRACE(1, "issue " << res_name << ", copies " << copies);
	
	if (num_format_resources >= slot_res_size) {
	  size_t new_size = slot_res_size * 2;
	  slot_res = TYPE_MEM_POOL_REALLOC_N(
			  SLOT_RESOURCE,_pool,slot_res,slot_res_size,new_size);
	  slot_res_size = new_size;
	}
	slot_res[num_format_resources].init(bv,res,res_name,fmt);

	num_format_resources++;
      }

     LEAVE;
}

bool
XTM_MicroArchitecture::readIssueFormats ()
{

  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::readIssueFormats");

  xtensa_isa isa = _isa;
  UINT isa_num_formats = xtensa_isa_num_formats(isa);
  UINT isa_num_slots = 0;
  UINT isa_num_opcodes = xtensa_isa_num_opcodes(isa);

  // step 1. record in a bit vector format_slot_table[fmt] for each format
  // which slots from the global slot definitions are included

  format_slot_table =
	CXX_NEW_ARRAY(SLOT_BIT_VECTOR_t, isa_num_formats, _pool);

  slot_map_p = CXX_NEW(slotMap(_pool), _pool);

  for (xtensa_format fmt=0; fmt<isa_num_formats; fmt++) {

    UINT format_num_slots = xtensa_format_num_slots(isa, fmt);
    SLOT_BIT_VECTOR_t format_bv = 0;

    for (UINT slot=0; slot<format_num_slots; slot++) {

      INT format_slot_key = Get_Format_Slot_Key(fmt,slot);
      UINT dummy;
      if (!slot_map_p->find(format_slot_key, &dummy)) {
	slot_map_p->insert(format_slot_key, (UINT)isa_num_slots);
	format_bv |= (1ULL << isa_num_slots);
	isa_num_slots++;
      }
    }
    format_slot_table[fmt] = format_bv;
  }

#define SLOT_ID_MAX (sizeof(SLOT_BIT_VECTOR_t)*8)

  if (isa_num_slots>SLOT_ID_MAX) {
     fprintf(stderr, "Too many (%d>%d) total ISA slot definitions",
		isa_num_slots, SLOT_ID_MAX);
     return false;
  }

  opcode_slot_table =
	CXX_NEW_ARRAY(SLOT_BIT_VECTOR_t, isa_num_opcodes, _pool);

  for (xtensa_opcode opc = 0; opc < isa_num_opcodes; opc++) {
    opcode_slot_table[opc] = 0;
  }

  // step 2. record in a bit vector opcode_slot_table[opc] for each opcode
  // which slots from the global slot definitions are included

  xtensa_insnbuf slot_buf = xtensa_insnbuf_alloc(isa);
  for (xtensa_format fmt=0; fmt<isa_num_formats; fmt++) {

    // if flix option is turned off, ignore formats longer than
    // 24 bits
    if (!xt_flix) {
      int format_byte_count=xtensa_format_length(isa,fmt);
      if (format_byte_count>3)
        continue;
    }

    UINT format_num_slots = xtensa_format_num_slots(isa, fmt);
    for (UINT slot=0; slot<format_num_slots; slot++) {

      INT format_slot_key = Get_Format_Slot_Key(fmt,slot);
      UINT slot_id;

      slot_map_p->find(format_slot_key, &slot_id);

      for (xtensa_opcode opc=0; opc<isa_num_opcodes; opc++) {

	if (xtensa_opcode_encode(isa, fmt, slot, slot_buf, opc)==0) {
	  // the current slot can encode opc
	  opcode_slot_table[opc] |= (1ULL << slot_id);
	}
      }
    }
  }

  /*  Make mov_n to be the combination of mov.n and or */
  xtensa_opcode opc_mov_n = xtensa_opcode_lookup(isa, TI_TOP_Name(TOP_mov_n));
  xtensa_opcode opc_or = xtensa_opcode_lookup(isa, TI_TOP_Name(TOP_or));
  opcode_slot_table[opc_mov_n] |= opcode_slot_table[opc_or];

  xtensa_insnbuf_free(isa, slot_buf);
  // step 3. find number of unique issue constraints
  // an issue constraint is produced by
  //
  //	format_slot_table[] X opcode_slot_table[]
  //
  // each of them corresponds to a kind of resource to be created

  issue_resource_map_p = CXX_NEW(issueResourceMap(_pool), _pool);

  // a reasonable bound on the number of format resources
  slot_res_size = isa_num_opcodes;

  slot_res = TYPE_MEM_POOL_ALLOC_N(SLOT_RESOURCE, _pool, slot_res_size);

  for (xtensa_opcode opc = 0; opc < isa_num_opcodes; opc++) {
    for (xtensa_format fmt = 0; fmt < isa_num_formats; fmt++) {

      SLOT_BIT_VECTOR_t bv = opcode_slot_table[opc] & format_slot_table[fmt];

      constructFormatResource(bv, fmt);
    }
  }

  UINT top_count = TI_TOP_Count();
  TOP tops[256];
  for (int top_i = TOP_count; top_i < top_count; top_i++) {
    TOP top = (TOP)top_i;
    XT_Instruction_p xt_inst = _arch->find_instruction(top);
    if (xt_inst==NULL || xt_inst->is_generic()==false)
      continue;

    TOP generic_top = top;
    UINT num_tops = _arch->get_special_tops(generic_top, tops, 256);
    if (num_tops==0)
      continue;

    SLOT_BIT_VECTOR_t opcode_slots = 0;
    for (int j=0; j<num_tops; j++) { 
      const char* opcode_name = TI_TOP_Name(tops[j]);
      xtensa_opcode opc = xtensa_opcode_lookup(isa, opcode_name);
      opcode_slots |= opcode_slot_table[opc];
    }

    for (xtensa_format fmt = 0; fmt < isa_num_formats; fmt++) {
      SLOT_BIT_VECTOR_t bv = opcode_slots & format_slot_table[fmt];
      constructFormatResource(bv, fmt);
    }
  }

  // step 4. based on result from step 3, create the power set of the resources
  // set from step 3. Many of the set in the power set may be redundant.
  // In addition, if the issue width is W, then the computation of power set
  // for union of k>=W sets is not needed.
  //
  // This step is needed to remove imprecise resource interference for set
  // of opcodes with overlapping but not subsuming slot requirement.
  //

  int orig_num_format_resources=num_format_resources;
  for (UINT i = 0; i < orig_num_format_resources; i++) {

      SLOT_BIT_VECTOR_t resource_i = slot_res[i].slots();
      xtensa_format fmt = slot_res[i].fmt();

      int level = TI_ISA_Num_Slots(fmt) - 2;
      if (level>0)
        find_power_set(/*bv_fmt*/fmt,
		       /*bv*/resource_i,
		       /*begin*/i+1,
		       /*end*/orig_num_format_resources,
		       /*more*/level);
  }

  // check if mul16 should be prefered since it can be issued in more slots
  // see PR11809
  _prefer_mul16 = false;
  xtensa_opcode mul16s_opc = xtensa_opcode_lookup(_isa,"mul16s");
  xtensa_opcode mac16_opc = xtensa_opcode_lookup(_isa,"mul.aa.ll");
  if (mul16s_opc != XTENSA_UNDEFINED && mac16_opc != XTENSA_UNDEFINED) {
    _prefer_mul16 = (TARG_INT_Pop_Count(opcode_slot_table[mul16s_opc]) > 
		     TARG_INT_Pop_Count(opcode_slot_table[mac16_opc]));
  }

  CXX_DELETE(issue_resource_map_p, _pool);

  // we have created resources corresponding to different issue slot
  // constraints
  // the resource requirements for each opcode is generated in
  // parseSchedule()

  RETURN true;
}

// a structure to represent one entry of resource usage
typedef struct {
	TOP		top;
	int		stage;
	const char*	res_name;
} RES_USAGE;

// a structure to summarize the usage of a resource
typedef struct {
	int		start;
	int		num_usage;
	int		num_copies;
	bool		required;
	const char*	res_name;
} RES_USAGE_SUMMARY;

// sort function for RES_USAGE array
static int res_usage_sort_func(
	const void* res_usage1,
	const void* res_usage2)
{
  const RES_USAGE *pt1= (RES_USAGE*)res_usage1;
  const RES_USAGE *pt2= (RES_USAGE*)res_usage2;

  int cmp=strcmp(pt1->res_name,pt2->res_name);
  if (cmp!=0)
    return cmp;
  if (pt1->top<pt2->top)
    return -1;
  else if (pt1->top>pt2->top)
    return 1;
  else {
    if (pt1->stage<pt2->stage)
      return -1;
    else if (pt1->stage>pt2->stage)
      return 1;
  }

  return 0;

}

// sort function for RES_USAGE_SUMMARY array
static int res_usage_summary_sort_func(
	const void* res_usage_summary1,
	const void* res_usage_summary2)
{
  const RES_USAGE_SUMMARY *pt1= (RES_USAGE_SUMMARY*)res_usage_summary1;
  const RES_USAGE_SUMMARY *pt2= (RES_USAGE_SUMMARY*)res_usage_summary2;

  return pt1->num_usage - pt2->num_usage;

}

void
XTM_MicroArchitecture::readFunctionUnits ()
{
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::readFunctionUnits");

  // we try to discover the covering of resource usage to minimize the
  // number of resources that need to be modeled

  // step 1. create an array of RES_USAGE to record all resource usage
  // step 2. sort the resource usage array according to the following keys
  //	     (resource, topcode, stage) so all usage of the same resource
  //	     are stored in consecutive entries after sorting and these
  //	     entries are sorted again by topcode then by stage
  // step 3. create an array of RES_USAGE_SUMMARY with index into the sorted
  //	     resource usage array
  // step 4. sort the resource usage summary array according to the number of
  //	     usage (i.e., number of entries in resource usage array for each
  //	     resource)
  // step 5. check each resource pair to identify subsuming
  //	     resource usage A subsume resource usage B if
  //	     whenever resource B is used in (top, stage), resource A is also
  //	     used and A has less or equal number of copies as B has
  //	     resource B is not required to be modeled in this case
  // step 6. creatge resource for those required to be modeled

  UTL_Map<const char *, int, UTL_Map_StringNoCaseHash> unit_map(_pool,127);
  int total_res_usage = 0;
  int num_res = 0;
  
  /* Initialize the unit table. */
  for (xtensa_funcUnit f = 0; f < xtensa_isa_num_funcUnits(_isa); f++) {
    const char *fname = xtensa_funcUnit_name(_isa, f);
    char *uname = CXX_NEW_ARRAY(char, strlen(fname) + 1, _pool);
    sprintf(uname, "%s", fname);
    int ucopies = xtensa_funcUnit_num_copies(_isa, f);
    unit_map.insert(uname, ucopies);
    num_res++;
  }

  RES_USAGE_SUMMARY* res_usage_summary =
    CXX_NEW_ARRAY(RES_USAGE_SUMMARY, num_res, _pool);

  /* Determine the total resource usage. */
  for (xtensa_opcode o = 0; o < xtensa_isa_num_opcodes(_isa); o++) {
    total_res_usage += xtensa_opcode_num_funcUnit_uses(_isa, o);
  }
  
  RES_USAGE* res_usage = CXX_NEW_ARRAY(RES_USAGE, total_res_usage, _pool);

  int idx = 0;

  for (xtensa_opcode o = 0; o < xtensa_isa_num_opcodes(_isa); o++) {
    const char *oname = xtensa_opcode_name(_isa, o);
    TOP top = TI_TOP_Topcode(oname);
    Is_True(top != TOP_UNDEFINED, ("Unable to find the topcode for %s", oname));
    
    for (int fu = 0; fu < xtensa_opcode_num_funcUnit_uses(_isa, o); fu++) {
      xtensa_funcUnit_use *u = xtensa_opcode_funcUnit_use(_isa, o, fu);
      res_usage[idx].top = top;
      res_usage[idx].stage = u->stage;
      res_usage[idx].res_name = xtensa_funcUnit_name(_isa, u->unit);
      idx++;
    }
  }
  
  FmtAssert(idx <= total_res_usage, ("Resource usage count error"));
  total_res_usage = idx;
  
  if (total_res_usage > 0) {
    qsort(&res_usage[0], total_res_usage, sizeof(RES_USAGE), res_usage_sort_func);

    const char* res_name=NULL;
    int prev_summary= -1;
    int i,j;

    idx = 0;
    for (i=0; i<total_res_usage; i++) {
      if (res_name==NULL || strcmp(res_usage[i].res_name,res_name)) {
	res_usage_summary[idx].start = i;
	res_usage_summary[idx].res_name = res_usage[i].res_name;
	res_usage_summary[idx].num_copies = 0;
	res_usage_summary[idx].required = false;
	if (prev_summary!= -1) {
	  res_usage_summary[prev_summary].num_usage =
	    i - res_usage_summary[prev_summary].start;
	}
	res_name = res_usage_summary[idx].res_name;
	prev_summary = idx;
	idx++;
      }
    }
    if (prev_summary != -1) {
      res_usage_summary[prev_summary].num_usage =
	    i - res_usage_summary[prev_summary].start;
    }
    FmtAssert(idx<=num_res,("Resource usage count error"));
    num_res = idx;

    for (i=0; i<num_res; i++) {

      int copies;
    
      if (unit_map.find(res_usage_summary[i].res_name, copies)) {
	res_usage_summary[i].num_copies = copies;
	res_usage_summary[i].required=true;
      }
    }

    qsort(&res_usage_summary[0],num_res,sizeof(RES_USAGE_SUMMARY),
	  res_usage_summary_sort_func);

    for (i=0; i<num_res; i++) {
      if (res_usage_summary[i].required) {
	RESOURCE res = RESOURCE_Create(res_usage_summary[i].res_name,
			      res_usage_summary[i].num_copies);
	const char* res_name = res_usage_summary[i].res_name;
	_resourceMap.insert(strdup(res_name), res);
	TRACE(1, "unit " << res_usage_summary[i].res_name <<
		 ", copies " << res_usage_summary[i].num_copies);
	for (j=i+1; j<num_res; j++) {
	  if (res_usage_summary[j].required &&
	      res_usage_summary[i].num_copies<=res_usage_summary[j].num_copies
	      && res_usage_summary[i].num_usage>=res_usage_summary[j].num_usage) {
	    int i_idx=res_usage_summary[i].start;
	    int j_idx=res_usage_summary[j].start;
	    int i_end = i_idx + res_usage_summary[i].num_usage;
	    int j_end = j_idx + res_usage_summary[j].num_usage;
	    while (i_idx<i_end && j_idx<j_end) {
	      while ((res_usage[i_idx].top<res_usage[j_idx].top ||
		  (res_usage[i_idx].top==res_usage[j_idx].top &&
		  res_usage[i_idx].stage<res_usage[j_idx].stage)) &&
		  i_idx<i_end)
		i_idx++;
	      if (res_usage[i_idx].top!=res_usage[j_idx].top ||
		  res_usage[i_idx].stage!=res_usage[j_idx].stage)
		break;
	      i_idx++;
	      j_idx++;
	    }
	    if (j_idx==j_end) {
	      TRACE(1, "unit usage " << res_usage_summary[j].res_name <<
		 " is covered by " << res_usage_summary[i].res_name);
	      res_usage_summary[j].required=false;
	    }
	  }
	}
      }
    }
  }

  LEAVE;
}


bool
XTM_MicroArchitecture::parseSchedule ()
{
#define XTM_NAMEBUF_SZ 256
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::parseSchedule");
  xtensa_isa isa = _isa;

  int num_opcodes = xtensa_isa_num_opcodes(isa);
  xtensa_opcode opc;
  for (opc=0; opc<num_opcodes; opc++) {
  
    /* Create a new instruction group for this topcode and show its resource
       usage and register read and write cycles. */

    const char *name = xtensa_opcode_name(isa,opc);
    
    TOP top = TI_TOP_Topcode(name);
    if (top == TOP_UNDEFINED) {
      continue;
    }
    Is_True(top != TOP_UNDEFINED, ("can't find TOP for instruction %s\n", name));
    TRACE(1, name << " top = " << top);

    Instruction_Group(strdup(name), top, TOP_UNDEFINED);

    /* Scan the isa operands of the instruction, keeping track of the
       equivalent OPCODE result and operand position, and record register def
       and use for the operands. We expect there to be scheduling information
       for every register operand, and we expect that information to be in
       operand order. */

    UINT op = 0, res = 0;
    if (opc == XTENSA_UNDEFINED)
    {
      DevWarn("Can't find isa for %s\n", name);
    }
    else
    {
      const UINT num_isa_operands = xtensa_opcode_num_operands(isa, opc);
      for (UINT i=0; i < num_isa_operands; i++)
      {
	char inout = xtensa_operand_inout(isa, opc, i);

	/* Don't set access/available time for non-register operands. */
	BOOL nonreg = (xtensa_operand_is_register (isa, opc, i) == 0);
	
	int num_regs = 1;
	if (!nonreg)
	  num_regs = xtensa_operand_num_regs(isa, opc, i);
	if (num_regs!=1) {
	  if (!_arch->multireg_need_expansion(opc, i))
	    num_regs = 1;
	}

	switch (inout)
	{
	case 'i':
	case 'm':
	{
	  if (!nonreg)
	  {
	    int use_stage = xtensa_operand_use_stage(isa, opc, i);
	    for (int r=0; r<num_regs; r++) {
	      TRACE(1, "\toperand " << op+r << " stage " << use_stage);
	      Operand_Access_Time(op+r, use_stage);
	    }

	    /* Remember the access time for operands for core instructions, so
               that we can normalize simulated instructions' access times. */
	    
	    if (top == TOP_or)
	      _normalized_op_read = use_stage;
	  }

	  op+=num_regs;
	  if (inout == 'i')
	    break;
	}
	// fall-through for '=' case
	
	case 'o':
	{
	  if (!nonreg)
	  {
	    int def_stage = xtensa_operand_def_stage(isa, opc, i);
	    /* Remember the access time for operands for core instructions, so
               that we can normalize simulated instructions' available times. */

	    if (top == TOP_or)
	      _normalized_op_write = def_stage;

	    for (int r=0; r<num_regs; r++) {
	      TRACE(1, "\tresult " << res+r << " stage " << def_stage + 1);
	      Result_Available_Time(res+r, def_stage + 1);
	    }
	  }

	  res+=num_regs;
	  break;
	}
	}
      }

      const UINT num_isa_states = xtensa_opcode_num_stateOperands(isa, opc);
      int i;
      for (i=0; i < num_isa_states; i++)
      {
	xtensa_state xt_state = xtensa_stateOperand_state(isa, opc, i);
	const char* state_name = xtensa_state_name(isa, xt_state);
	char inout = xtensa_stateOperand_inout(isa, opc, i);
	XT_State_p state = _arch->find_state(state_name);

        /* Add access time for defined states. */
	if (state && (inout=='o' || inout=='m')) {
	  int def_stage = xtensa_stateOperand_def_stage(isa, opc, i);
	  TRACE(1, "\tstate " << state_name << " result " << op <<
			  " stage " << def_stage);
	  Result_Available_Time(res, def_stage + 1);
	  res++;
	}
        /* Add access time for used states. */
	if (state && (inout=='i' || inout=='m' || state->is_volatile())) {
	  int use_stage = state->is_volatile()?
		  xtensa_stateOperand_def_stage(isa, opc, i):
		  xtensa_stateOperand_use_stage(isa, opc, i);
	  TRACE(1, "\tstate " << state_name << " operand " << op <<
			  " stage " << use_stage);
	  Operand_Access_Time(op, use_stage);
	  op++;
	}
	
      }

      /* Record resource usage. */
      for (int fu = 0; fu < xtensa_opcode_num_funcUnit_uses(isa, opc); fu++) {
        xtensa_funcUnit_use *u = xtensa_opcode_funcUnit_use(isa, opc, fu);
        const char *fname = xtensa_funcUnit_name(isa, u->unit);
        RESOURCE res = find_resource(fname);
	if (res)
	{
	  TRACE(1, "\tresource " << fname << " stage " << u->stage);
          
	  Resource_Requirement(res, u->stage);
	}
      }

      /* record stage info for implicit register operand */
      for (i=0; i< num_isa_operands; i++) {
	if (xtensa_operand_is_register(isa, opc, i)) {

	xtensa_regfile reg_file = xtensa_operand_regfile(isa, opc, i);
	const char *reg_file_name = xtensa_regfile_shortname (isa, reg_file);
	XT_RegFile_p rf = _arch->find_regfile(reg_file_name);

	if (rf->not_allocatable()) {

	  /* a non-allocatable register file operand */
          XT_State_p state = _arch->find_narf_state(reg_file_name);

	  FmtAssert(state,
		    ("Missing implicit state for non-allocatable register file"
		     "%s", reg_file_name));
	  char inout = xtensa_operand_inout(isa, opc, i);
	  if (inout=='i' || inout=='m') {
	    int use_stage = xtensa_operand_use_stage(isa, opc, i);
	    TRACE(1, "\timplicit state " << state->name() << " operand " << op <<
			    " stage " << use_stage);
	    Operand_Access_Time(op, use_stage);
	    op++;
	  }
	  if (inout=='o' || inout=='m') {
	    int def_stage = xtensa_operand_def_stage(isa, opc, i);
	    TRACE(1, "\timplicit state " << state->name() << " result " << res <<
			    " stage " << def_stage);
	    Result_Available_Time(res, def_stage + 1);
	    res++;
	  }
	}
	}
      }


      /* record TIE port references usage */
      {
	int numTieWires = xtensa_opcode_num_interfaceOperands(isa, opc);

	if (numTieWires > 0) {

	  // record TIE port references as states

	  for (int i=0; i<numTieWires; i++) {
	    xtensa_interface xt_interface =
		xtensa_interfaceOperand_interface(isa, opc, i);
	    char inout = xtensa_interface_inout(isa, xt_interface);
	    const char* interface_name = xtensa_interface_name(isa, xt_interface);
	    XT_State_p state = _arch->find_state(interface_name);
	    XT_State_p class_state = NULL;
	    int interface_class_id = xtensa_interface_class_id(isa, xt_interface);
	    if (interface_class_id != XTENSA_UNDEFINED)
	      class_state = _arch->find_interface_class_state(interface_class_id);
	    int stage = xtensa_interface_stage(isa, xt_interface);
	    if (inout=='o' || state->is_volatile()) {
	      TRACE(1, "\timplicit intf state " << state->name() << " result " << res <<
			    " stage " << stage);
	      Result_Available_Time(res, stage+1);
	      res++;
	      if (class_state) {
	        TRACE(1, "\timplicit intf class state " << state->name() << " result "
				<< res << " stage " << stage);
	        Result_Available_Time(res, stage+1);
	        res++;
	      }
	    }
	    if (inout=='i' || state->is_volatile()) {
	      TRACE(1, "\timplicit intf state " << state->name() << " operand " << op <<
			    " stage " << stage);
	      Operand_Access_Time(op, stage);
	      op++;
	      if (class_state) {
	        TRACE(1, "\timplicit intf class state " << state->name() << " operand "
				<< op << " stage " << stage);
	        Operand_Access_Time(op, stage);
	        op++;
	      }
	    }
	  }
	}
      }

      if (TI_ISA_Property_Is_Tie(top)) {
        const ISA_OPERAND_INFO* op_info = TI_ISA_Operand_Info (top);
        FmtAssert(op<=TI_ISA_Op_Operands(op_info) &&
		res<=TI_ISA_Op_Results(op_info),
		 ("Extra scheduling information encountered for %s",
		 name));
        FmtAssert(op>=TI_ISA_Op_Operands(op_info) &&
		res>=TI_ISA_Op_Results(op_info),
		 ("Missing scheduling information for %s",
		 name));
      }
    }

    SLOT_BIT_VECTOR_t opcode_slots = opcode_slot_table[opc];
    bool has_format_resource = false;

    for (UINT i = 0; i < num_format_resources; i++) {

      // check the relation between an opcode's allowed slots and a resource's
      // slots. There are four possibilities:
      //
      // case 1 if the two bit vectors do not intersect,
      //	then resource is not needed (this is exact)
      // case 2 if the resource bit vectors is a subset of the opcode bit
      //	vector, then resource is not needed because there will be
      //	a resource correspond to the opcode's slot requirement and
      //	that resource will catch the any conflict with the opcode
      //	(this is exact)
      // case 3 if the opcode bit vector is a subset of the resource bit vector,
      //	then the resource is required by the opcode (this is exact)
      // case 4 if the opcode and the resource bit vector intersect but none
      //	is a subset of the other,
      //	then the resource is required by the opcode
      //	(this would be a conservative approximation)
      //	but since we have created a new union resource in step 4 of
      //	readIssueFormats(), we will eventually find and use that one
      //	so ignore the current resource
      //
      // In summary, we only need to incluse the resources from cases 3.
      //

      xtensa_format fmt = slot_res[i].fmt();
      SLOT_BIT_VECTOR_t format_opcode_slots =
				opcode_slots & format_slot_table[fmt];
      SLOT_BIT_VECTOR_t resource_slots = slot_res[i].slots();
      if ((format_opcode_slots & resource_slots) &&
	  (format_opcode_slots & resource_slots)==format_opcode_slots) {
	Resource_Requirement(slot_res[i].resource(), 0);
	has_format_resource = true;
	TRACE(1, "\tslot_resource " << slot_res[i].name() << " stage 0");
	if (_core_format== XTENSA_UNDEFINED && top == TOP_or) {
	  // remember the issue resource and format to be used for
	  // simulated op and TOP_mov_n
	  Is_True(_core_slot_resource==NULL,("re-define _core_slot_resoruce"));
	  _core_format = fmt;
	  _core_slot_resource = slot_res[i].resource();
	}
      }
    }

    if (has_format_resource==false) {
      // if all format were considered and has no format resource
      // then the initialization has failed
      // otherwise if no flix option is used then some opcode may
      // not have any valid format but us not an error until the
      // opcode is really used
      if (xt_flix) {
        fprintf(stderr,"Internal error: initialization failed.\n");
        RETURN false;
      }
    }

    UINT isa_num_formats = xtensa_isa_num_formats(isa);
    for (xtensa_format fmt = 0; fmt < isa_num_formats; fmt++) {
      SLOT_BIT_VECTOR_t bv = opcode_slots & format_slot_table[fmt];
      if (bv !=0)
	Valid_Issue_Format(fmt);
    }

    if (top == TOP_mov_n)
      _mov_n_initialized = true;

  }
  if (!_mov_n_initialized) {
    /* we need to add scheduling info for mov.n using the info for 'or'
       which was stored in _normalized_op_read/write
    */

    TOP top = TOP_mov_n;
    TRACE(1, (char *)TI_TOP_Name(top) << " top = " << top);

    Instruction_Group((char *)TI_TOP_Name(top), top, TOP_UNDEFINED);

    TRACE(1, "\toperand " << 0 << " stage " << _normalized_op_read);
    Operand_Access_Time(0, _normalized_op_read);

    TRACE(1, "\tresult " << 0 << " stage " << _normalized_op_write + 1);
    Result_Available_Time(0, _normalized_op_write + 1);

    Is_True(_core_slot_resource!=NULL,("_core_slot_resoruce not defined"));

    TRACE(1, "\tslot_resource " << _core_slot_resource->Name() << " stage 0");
    Resource_Requirement(_core_slot_resource, 0);
    Valid_Issue_Format(_core_format);

    _mov_n_initialized = true;
  }

  RETURN true;
}


// generate result/operand scheduling information and function units/format
// resources for generic opcodes
void
XTM_MicroArchitecture::genericSchedule (void)
{
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::generic_Schedule");

  TOP tops[256];
  int top_cycles[256];
  UINT top_count = TI_TOP_Count();
  xtensa_isa isa = _isa;


  // for each generic opcode, we perform the following steps which are very
  // similar to what we do for normal opcodes in parseSchedule():
  // 1. Create a instruction group.
  // 2. Set the result available times to be the max among all specialized
  //    opcodes.
  // 3. Set the operann access times to be the min among all specialized
  //    opcodes.
  // 4. Request functional units resources which are the superset of those
  //    required by the specialized opcodes.
  // 5. Get the union of the allowed formats/slots from the specialized
  //    opcodes and request the format resources. We also set valid formats
  //    for the generic opcode using the union of the special opcode formats.
 
  for (int i = TOP_count; i < top_count; i++) {
    XT_Instruction_p xt_inst = _arch->find_instruction((TOP)i);
    if (xt_inst==NULL || xt_inst->is_generic()==false)
      continue;

    TOP generic_top = (TOP)i;
    UINT num_tops = _arch->get_special_tops(generic_top, tops, 256);
    if (num_tops==0)
      continue;

    TRACE(1, (char *)TI_TOP_Name(generic_top) << " top = " << generic_top);

    Instruction_Group((char *)TI_TOP_Name(generic_top), generic_top,
		    					TOP_UNDEFINED);

    for (int k=0; k<xt_inst->num_operands(); k++) {
      UINT min_access_time = UINT_MAX;
      for (int j=0; j<num_tops; j++) {
        UINT access_time = Any_Operand_Access_Time_Is_Defined(tops[j]) ?
		Get_Any_Operand_Access_Time ( tops[j] ) :
		Operand_Access_Time_Is_Defined(tops[j], k) ?
		Get_Operand_Access_Time( tops[j], k ) : UINT_MAX;
	if (access_time < min_access_time)
	  min_access_time = access_time;
      }

      if (min_access_time!=UINT_MAX) {
        TRACE(1, "\toperand " << k << " stage " << min_access_time);
        Operand_Access_Time(k, min_access_time);
      }
    }

    for (int k=0; k<xt_inst->num_results(); k++) {
      UINT max_available_time = 0;
      for (int j=0; j<num_tops; j++) {
        UINT available_time = Any_Result_Available_Time_Is_Defined(tops[j]) ?
		Get_Any_Result_Available_Time ( tops[j] ) :
		Result_Available_Time_Is_Defined(tops[j], k)?
		Get_Result_Available_Time( tops[j], k ) : 0;
	if (available_time > max_available_time)
	  max_available_time = available_time;
      }

      // be careful not to increment result available time by 1 as in real
      // opcode because we are computing from result available time and they
      // are incremented by 1 already
      if (max_available_time!=0) {
        TRACE(1, "\tresult " << 0 << " stage " << max_available_time);
        Result_Available_Time(k, max_available_time);
      }
    }

    int max_res_cycle = 0;
    for (int j=0; j<num_tops; j++) {
      top_cycles[j] = Get_Max_Resource_Cycle(tops[j]);
      if (max_res_cycle < top_cycles[j])
	max_res_cycle = top_cycles[j];
    }

    resourceNameMap::keyValuePair_p scan;
    resourceNameMap::iter iter(_resourceMap);
    while ((scan = iter.next()) != NULL) {
      RESOURCE res = (RESOURCE)(scan->value());
      for (int cycle =0; cycle < max_res_cycle; cycle++) {
        int max_num_copy = 0;
        for (int j=0; j<num_tops; j++) {
	  int num_copy = Get_Resource_Requirement_Count(tops[j], res, cycle);
	  if (max_num_copy<num_copy)
	    max_num_copy = num_copy;
        }
	for (int j=0; j<max_num_copy; j++)
	  Resource_Requirement(res, cycle);
      }
    }

    SLOT_BIT_VECTOR_t opcode_slots = 0;
    for (int j=0; j<num_tops; j++) {
      const char* opcode_name = TI_TOP_Name(tops[j]);
      xtensa_opcode opc = xtensa_opcode_lookup(isa, opcode_name);
      opcode_slots |= opcode_slot_table[opc];
    }

    for (UINT i = 0; i < num_format_resources; i++) {

      // follow what we do in the real opcode case

      xtensa_format fmt = slot_res[i].fmt();
      SLOT_BIT_VECTOR_t format_opcode_slots =
				opcode_slots & format_slot_table[fmt];

      SLOT_BIT_VECTOR_t resource_slots = slot_res[i].slots();
      if ((format_opcode_slots & resource_slots) &&
	  (format_opcode_slots & resource_slots)==format_opcode_slots) {
	Resource_Requirement(slot_res[i].resource(), 0);
	TRACE(1, "\tslot_resource " << slot_res[i].name() << " stage 0");
      }
    }

    UINT isa_num_formats = xtensa_isa_num_formats(_isa);
    for (xtensa_format fmt = 0; fmt < isa_num_formats; fmt++) {
      SLOT_BIT_VECTOR_t bv = opcode_slots & format_slot_table[fmt];
      if (bv !=0)
	Valid_Issue_Format(fmt);
    }
  }
  LEAVE;
}

void
XTM_MicroArchitecture::simulatedSchedule (void)
{
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::simulatedSchedule");

  for (UINT i = 0; i < TI_TOP_Count(); i++)
  {
    TOP top = (TOP)i;
    if (TI_ISA_Property_Set(PROP_simulated, top) &&
	!TI_ISA_Property_Set(PROP_dummy, top) &&
	!TI_ISA_Property_Set(PROP_generic, top))
    {
      /* We assume all simulated operands have same access time, and all
         results have same available time. */
      Instruction_Group((char *)TI_TOP_Name(top), top, TOP_UNDEFINED);
      if ((TOP)i<TOP_count) {
        Any_Operand_Access_Time(
		TSI_Operand_Access_Time(top, 0) + _normalized_op_read);
        Any_Result_Available_Time(
		TSI_Result_Available_Time(top, 0) + _normalized_op_write);


        TRACE(1, "simulated top " << TI_TOP_Name(top) <<
	    " operand " << TSI_Operand_Access_Time(top, 0) <<
	    ", result " << TSI_Result_Available_Time(top, 0) << endl);
      } else {
        Any_Operand_Access_Time( 0 + _normalized_op_read);
        Any_Result_Available_Time( 1 + _normalized_op_write);

        TRACE(1, "simulated top " << TI_TOP_Name(top) <<
	    " operand " << 0 <<
	    ", result " << 1 << endl);
      }

      if (top==TOP_extw_pseudo || top==TOP_memw_pseudo) {
	// for extw_pseudo and memw_pseudo,
	// there is no format restriction and consumes
	// no resource so it should be able to be bundled with any ops
	UINT isa_num_formats = xtensa_isa_num_formats(_isa);
	for (xtensa_format fmt = 0; fmt < isa_num_formats; fmt++) {
	  // requires no slot resource but need to set the resource table length
	  Resource_Requirement((RESOURCE)NULL, 0);
	  Valid_Issue_Format(fmt);
	}
      } else {
        TRACE(1, "\tslot_resource " << _core_slot_resource->Name() <<
		 " stage 0");
        Resource_Requirement(_core_slot_resource, 0);
        Valid_Issue_Format(_core_format);
      }
    }
  }

  LEAVE;
}


RESOURCE
XTM_MicroArchitecture::find_resource (const char *unit)
{
  RESOURCE res;
    
  if (_resourceMap.find(unit, &res))
    return res;

  return NULL;
}

#ifdef Is_True_On

void
XTM_MicroArchitecture::debugGenerated (void)
{
  M_ENTER(1, "XTARCH", "XTM_MicroArchitecture::debugGenerated");

/* To check that the generated scheduling information is correct, we generate
     the C code to a temporary file, and then output the generated information
     in a (somewaht) similar fasion so we can visually compare them. */

  /* Outputting schedule has side effects on objects, so we can't do both at
     once. Set TRACE_XTARCH to 99 for C dump, set to 88 for generated dump. */
  if (TRACE_IS_ON(9))
  {
    /* Output C code... just like is done for targ_info. */
    Machine_Done("/tmp/xtm_orig");
  }
  else if (TRACE_IS_ON(8))
  {
    /* Now try to output the generated information in a similar format. */

    FILE* fd = fopen("/tmp/xtm_gen", "w");
    Is_True(fd != NULL, (""));

    for (UINT i = 0; i < _resource_count; ++i)
      fprintf(fd,"TI_SI_RESOURCE %x = {\"%s\",%d,%d,%d,%d};\n",
	      _resources[i],
	      _resources[i]->name,
	      _resources[i]->id,
	      _resources[i]->avail_per_cycle,
	      _resources[i]->word_index,
	      _resources[i]->bit_index);

    fprintf(fd,"int TSI_resource_count = %d;\n", _resource_count);
    fprintf(fd,"TI_SI_RESOURCE *TSI_resources[] = {");

    bool is_first = true;
    for (UINT i = 0; i < _resource_count; ++i)
    {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %x", _resources[i]);
    }
    fprintf(fd,"\n};\n");

    fprintf(fd,"TI_SI_RRW TSI_RRW_initializer = ");
    _RRW_initializer->dump_hex(fd);
    fputc('\n', fd);
    fprintf(fd,"TI_SI_RRW TSI_RRW_overuse_mask =");
    _RRW_overuse_mask->dump_hex(fd);
    fputc('\n', fd);
    fprintf(fd,"TI_SI_RRW TI_SI_RRW_all_format_resource_overuse_mask =");
    TI_SI_RRW_all_format_resource_overuse_mask->dump_hex(fd);
    for (UINT i=0; i<TI_ISA_Num_Bundles(); i++) {
      fprintf(fd,"TI_SI_RRW TSI_RRW_format_resource_overuse_mask[%d] = ", i);
      _RRW_format_resource_overuse_mask[i].dump_hex(fd);
      fputc('\n', fd);
    }

    fprintf(fd,"int TSI_issue_slot_count = %d;\n", _issue_slot_count);
    for (UINT i = 0; i < _issue_slot_count; ++i)
    {
      TI_SI_ISSUE_SLOT *is = _issue_slots[i];

      fprintf(fd, "static TI_SI_ISSUE_SLOT %x = { \"%s\",%d,%d};\n",
	      is,
	      is->name,
	      is->skew,
	      is->avail_per_cycle);
    }

    if (_issue_slot_count == 0)
      fprintf(fd,"TI_SI_ISSUE_SLOT * TSI_issue_slots[1] = {0};\n");
    else {
      fprintf(fd,"TI_SI_ISSUE_SLOT * TSI_issue_slots[%d] = {", _issue_slot_count);
    
      bool is_first = true;
      for (UINT i = 0; i < _issue_slot_count; ++i)
	fprintf(fd,"\n  %x", _issue_slots + i);
    }

    for (UINT i = 0; i < _ID_count; i++)
    {
      TI_SI *tsi = _ID_si[i];

      fprintf(fd,"\n/* Instruction group %s */\n",tsi->name);

      // res.requirement.Output
      fprintf(fd,"static TI_SI_RRW %x[] = {\n  %d",
	      tsi->rr, tsi->rr[0].int_value());

      for (UINT j = 0; j < tsi->rr[0].int_value(); j++) {
	fputc('\n',fd);
	tsi->rr[j+1].dump_hex(fd);
      }

      fprintf(fd,"\n};\n");

      if (tsi->resources_used != 0)
      {
	fprintf(fd,"static TI_SI_RESOURCE_ID_SET %x[] = {", tsi->resources_used);

	bool is_first = true;
	for (UINT j = 0; j < tsi->rr[0].int_value(); j++)
	{
	  Maybe_Print_Comma(fd,is_first);
	  fputc('\n',fd);
	  tsi->resources_used[j].dump_hex(fd);
	}

	fprintf(fd,"\n};\n");
      }

      // res_requirement.Compute_Output_Resource_Count_Vec
      fprintf(fd,"static TI_SI_RESOURCE_TOTAL %x[] = {", tsi->resource_total_vector);

      bool is_first = true;
      for (UINT j = 0; j < tsi->resource_total_vector_size; j++)
      {
	Maybe_Print_Comma(fd,is_first);
	fprintf(fd,"\n  {%x,%d}",
		tsi->resource_total_vector[j].resource,
		tsi->resource_total_vector[j].total_used);
      }
      fprintf(fd,"\n};\n");

      // Output_Latency_Info
      fprintf(fd,"static mUINT8 %x[] = {", tsi->operand_access_times);

      is_first = true;
      for (UINT j = 0; j < TI_ISA_Operand_Max(); j++)
      {
	Maybe_Print_Comma(fd,is_first);
	fprintf(fd,"%d", tsi->operand_access_times[j]);
      }

      fprintf(fd,"};\n");

      fprintf(fd,"static mUINT8 %x[] = {", tsi->result_available_times);

      is_first = true;
      for (UINT j = 0; j < TI_ISA_Result_Max(); j++)
      {
	Maybe_Print_Comma(fd,is_first);
	fprintf(fd,"%d", tsi->result_available_times[j]);
      }

      fprintf(fd,"};\n");


      fprintf(fd,"static TI_SI %x = {\n", tsi);
      fprintf(fd,"  \"%s\",\n",tsi->name);
      fprintf(fd,"  %-15d, /* id */\n",tsi->id);
      fprintf(fd,"  %-15x, /* operand latency */\n", tsi->operand_access_times);
      fprintf(fd,"  %-15x, /* result latency */\n", tsi->result_available_times);
      fprintf(fd,"  %-15d, /* load access time */\n", tsi->load_access_time);
      fprintf(fd,"  %-15d, /* last issue cycle */\n", tsi->last_issue_cycle);
      fprintf(fd,"  %-15d, /* store available time */\n", tsi->store_available_time);
      fprintf(fd,"  %-15x, /* resource requirement */\n", tsi->rr);
      fprintf(fd,"  %-15x, /* res id used set vec */\n", tsi->resources_used);
      fprintf(fd,"  %-15d, /* II info size */\n", tsi->ii_info_size);
      fprintf(fd,"  %-15x, /* II resource requirement vec */\n", tsi->ii_rr);
      fprintf(fd,"  %-15x, /* II res id used set vec */\n", tsi->ii_resources_used);
      fprintf(fd,"  {{ 0x0, 0x0 }}, /* Bad IIs */\n");  // fix when ii done
      fprintf(fd,"  %-15d, /* valid issue slots vec size */\n", tsi->valid_issue_slot_count);
      fprintf(fd,"  %-15x, /* valid issue slots vec */\n", tsi->valid_issue_slots);
      fprintf(fd,"  %-15d, /* resource count vec size */\n", tsi->resource_total_vector_size);
      fprintf(fd,"  %-15x, /* resource count vec */\n", tsi->resource_total_vector);
      fprintf(fd,"  %-15d  /* write-write interlock */\n", tsi->write_write_interlock);
      fprintf(fd,"};\n");
    }


    fprintf(fd,"TI_SI * TSI_ID_si[] = {");

    is_first = true;
    for (UINT j = 0; j < _ID_count; j++)
    {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %x", _ID_si[j]);
    }

    fprintf(fd,"\n};\n");
    fprintf(fd,"int TSI_ID_count = %d;\n\n", _ID_count);


    fprintf(fd,"TI_SI * TSI_top_si[%d] = {", TI_TOP_Count());
    is_first = true;
    for (UINT j = 0; j < TI_TOP_Count(); j++)
    {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %-10x  /* %s */", _top_si[j], TI_TOP_Name((TOP)j));
    }

    fprintf(fd,"\n};\n");
  
    fclose(fd);
  }
}
#endif

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
