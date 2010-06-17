/*
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#include <stdlib.h>

#include "defs.h"
#include "config.h"
#include "glob.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "mempool.h"
#include "config_asm.h"
#include "cgir.h"
#include "cg.h"
#include "cg_internal.h"
#include "cgtarget.h"
#include "bb_map.h"
#include "cg_flags.h"
#include "cg_region.h"
#include "freq.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "gra_live.h"
#include "cg_special_op.h"
#include "cflow_common.h"
#include "cflow_layout.h"

static BOOL freqs_computed;

BB_LAYOUT *bblayout_obj = NULL;
static BOOL Entry_BB_Always_First = TRUE;

#define GET_BB_CHAIN(x)		bblayout_obj->get_bb_chain(x)
#define GET_BB_BRANCHINFO(x)	bblayout_obj->get_bb_branchinfo(x)
#define GET_BB_SIZE(x)     	bblayout_obj->get_bb_size(x)
#define GET_BB_OFFSET(x)     	bblayout_obj->get_bb_offset(x)

MEM_POOL *BRANCH_INFO::pool = NULL;
BRANCH_INST_TABLE *BRANCH_INFO::btable = NULL;

static BB *create_jump_bb(BB *bb, BRANCH_INFO *pbinfo, BB *succ, float prob);

typedef struct traverse_edges {
  CHAIN_BB_EDGE **pedges;
  bool          is_out;
  bool          is_forward;
  bool          is_chain1;
} TRAVERSE_EDGES;

static void set_up_traverse_edges (TRAVERSE_EDGES *tedges, CHAIN *chain1, CHAIN *chain2)
{
  tedges[0].pedges     = &(chain1->out_edges);
  tedges[0].is_out     = true;
  tedges[0].is_forward = true;
  tedges[0].is_chain1  = true;
  tedges[1].pedges     = &(chain1->in_edges);
  tedges[1].is_out     = false;
  tedges[1].is_forward = false;
  tedges[1].is_chain1  = true;

  tedges[2].pedges     = &(chain2->out_edges);
  tedges[2].is_out     = true;
  tedges[2].is_forward = false;
  tedges[2].is_chain1  = false;
  tedges[3].pedges     = &(chain2->in_edges);
  tedges[3].is_out     = false;
  tedges[3].is_forward = true;
  tedges[3].is_chain1  = false;
}

static TOP get_invert_branch_top(OP *br_op)
{
  TOP br_top = OP_code(br_op);
  TOP new_top;
  BOOL is_generic = TI_ISA_Property_Set(PROP_generic, br_top);
  if (is_generic) {
    TOP tops[256];
    int num_tops = TI_TOP_Get_Special_Tops(br_top, tops, 256);
    FmtAssert(num_tops > 0, ("Wrong generic op %s", TI_TOP_Name(br_top)));

    new_top = TI_TOP_Invert_Branch (tops[0]);
    if (new_top != TOP_UNDEFINED) {
      Set_OP_code(br_op, new_top);
      CG_Relax_Special_Op(br_op);
      new_top = OP_code(br_op);

      // Restore top
      Set_OP_code(br_op, br_top);
    }
  } else {
    new_top = TI_TOP_Invert_Branch (br_top);
  }
  return new_top;
}

/* 
   Return additional cycles needed for direct relaxation

   Note: at this point, does fall-thru has higher probability
         than target ?
*/
static float get_direct_relaxation_cost(BRANCH_INFO *pbinfo)
{
  BB *bb = pbinfo->get_br_bb();
  float f;
  if (pbinfo->relax_cost1 < -0.01f)
  {
    OP * br_op = pbinfo->get_br_op();
    if ( br_op && 
         (OP_code(br_op) == TOP_j || OP_code(br_op) == TOP_jx)) {
      f = 0.0f;
    } else if (br_op && pbinfo->is_cond_jump()) {
      f = pbinfo->btable->get_branch_taken_delay();
      if (get_invert_branch_top(br_op) != TOP_UNDEFINED) {
        f = f * pbinfo->get_succ_prob(FALL_THRU_IX) +
            pbinfo->get_succ_prob(TARGET_IX);
      } else {
        f = (f + 1.0f) * pbinfo->get_succ_prob(FALL_THRU_IX);
      }
      if (freqs_computed || BB_freq_fb_based(bb)) {
        f *= BB_freq(bb);
      }
    } else {
      /* bb has one fall-thru succ */
      f = pbinfo->btable->get_branch_taken_delay() + 1.0f;
      if (freqs_computed || BB_freq_fb_based(bb)) {
        f *= BB_freq(bb);
      }
    }
    pbinfo->relax_cost1 = f;
  }
  return pbinfo->relax_cost1;
}

static float get_indirect_relaxation_cost(BRANCH_INFO *pbinfo)
{
  BB *bb = pbinfo->get_br_bb();
  float f;
  if (pbinfo->relax_cost2 < -0.01f) {
    OP *br_op = pbinfo->get_br_op();
    if (br_op && pbinfo->is_cond_jump()) {
      f = pbinfo->btable->get_branch_taken_delay();
      f = (f + 1.0f) * pbinfo->get_succ_prob(TARGET_IX);
      if (freqs_computed || BB_freq_fb_based(bb)) {
        f *= BB_freq(bb);
      }
      pbinfo->relax_cost2 = f;
    } else {
      Is_True(FALSE, ("shouldn't happen"));
    }
  }
  return pbinfo->relax_cost2;
}
      

CHAIN_BB_EDGE *CHAIN::find_out_edge (BB *src, BB *dst)
{
  CHAIN_BB_EDGE *e;
  for (e = out_edges; e; e = e->next_out()) {
    if ( (e->get_bb_src() == src) && (e->get_bb_dst() == dst) ) {
      return e;
    }
  }
  return NULL;
}


void CHAIN::print_edges(bool print_in_edges)
{
  CHAIN_BB_EDGE *cedge;

  fprintf(TFile, "  %s\n", print_in_edges ? "in_edges:" : "out_edges:");
  for ( cedge = print_in_edges ? in_edges : out_edges;
        cedge != NULL;
        cedge = print_in_edges ? cedge->next_in() : cedge->next_out())
  {
    BB *from = cedge->get_bb_src();
    BB *to   = cedge->get_bb_dst();
    CHAIN *from_chain = GET_BB_CHAIN(from);
    CHAIN *to_chain   = GET_BB_CHAIN(to);
    BRANCH_INFO *pbinfo = GET_BB_BRANCHINFO(from);
    bool is_fall_thru = (pbinfo->get_succ_fall_thru() == to);

    fprintf(TFile, "    Chain: %d->%d; BB: %d->%d (%c", 
                   from_chain->chain_id, to_chain->chain_id,
                   BB_id(from), BB_id(to),
		   is_fall_thru ? 'f' : 't');
    if (cedge->is_loop_start()) {
      fprintf(TFile, ", loop");
    } 
    if (cedge->is_loop_end()) {
      fprintf(TFile, ", loop_end");
    }
    fprintf(TFile, "),");
      
    int ix = pbinfo->get_succ_index(to);
    if (is_fall_thru) {
      fprintf(TFile, " dist: <%d, %d> (fall-thru)\n",
                     pbinfo->get_min_distance(ix, false),
                     pbinfo->get_min_distance(ix, true),
                     pbinfo->get_range_min(),
                     pbinfo->get_range_max());
    } else {
      fprintf(TFile, " dist: <%d, %d> (range: <%d, %d>)\n",
                     pbinfo->get_min_distance(ix, false),
                     pbinfo->get_min_distance(ix, true),
                     pbinfo->get_range_min(),
                     pbinfo->get_range_max());
    }
  }
}
  

void CHAIN::print (bool detail)
{
  BB *bb;
  int n=0;

  fprintf(TFile, "CHAIN %d: size=%d", chain_id, bytes_size);
  if (is_entry_chain()) {
    fprintf(TFile, " entry");
  }
  fprintf(TFile, "\n  %s\n", "BBs:");
  fprintf(TFile, "    ");
  for (bb = head; bb; bb = BB_next(bb) ) {
    if ( n == 5 ) {
      fprintf(TFile, "\n    ");
      n = 0;
    }
    n++;

    if (BB_for_layout(bb)) {
      fprintf(TFile, " BB:%d(sz=%d, ofst=%d, j) ",
                     BB_id(bb), GET_BB_SIZE(bb), GET_BB_OFFSET(bb));
    } else {
      fprintf(TFile, " BB:%d(sz=%d, ofst=%d) ",
                     BB_id(bb), GET_BB_SIZE(bb), GET_BB_OFFSET(bb));
    }
  }
  fprintf(TFile, "\n");
  if (detail) {
    print_edges();
  }
}

OP * Mk_jump_OP (BB *target)
{
  LABEL_IDX label = Gen_Label_For_BB(target);
  TN *tn_target = Gen_Label_TN(label,0);
  OP *j_op = Mk_OP(TOP_j, tn_target);
  return j_op;
}

/*
   To change branch (two-way) 
   from:

     bb/op->succ
*/
BOOL
OP_Retarget_Branch(OP *br_op, BB *from, BB *to)
{
  Is_True(br_op != NULL, ("OP must be non-zero"));

  INT opnd;
  INT opnd_count;
  CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
  if (opnd_count > 0) {
    TN *br_targ = OP_opnd(br_op, opnd);
    Is_True(opnd_count == 1, ("Branch with multiple target BBs"));
    if (Is_Label_For_BB(TN_label(br_targ), from)) {
      LABEL_IDX label = Gen_Label_For_BB(to);
      Set_OP_opnd(br_op, opnd, Gen_Label_TN(label,0));
      return TRUE;
    }
  } else {
    FmtAssert (FALSE, ("Switch statement (jx) not handled"));
  }
  return FALSE;
}

/*
  Create a jump BB from bb to succ
*/
static BB *create_jump_bb(BB *bb, BRANCH_INFO *pbinfo, BB *succ, float prob)
{
  OP *br_op = pbinfo->get_br_op();
  BB *new_bb = Gen_BB();
  BB_rid(new_bb) = BB_rid(bb);

  Add_Goto (new_bb, succ);

  SRCPOS tmp_srcpos=0;
  OP *tmp_op;
  if (br_op) {
    tmp_srcpos = OP_srcpos(br_op);
  } else {
    tmp_op = BB_last_op(bb);
    if (tmp_op) { 
      tmp_srcpos = OP_srcpos(tmp_op);
    }
  }
  if (tmp_srcpos != 0) {
    FOR_ALL_BB_OPs(new_bb, tmp_op) {
      OP_srcpos(tmp_op) = tmp_srcpos;
    }
  }

  Set_BB_for_layout(new_bb);
  return new_bb;
}

void BRANCH_INST::add_variant_TOP (TOP t)
{
  Is_True (num_variants < MAX_VARIANTS, 
           ("Too many variants for TOP %s : %s", TI_TOP_Name(tops[0]), TI_TOP_Name(t)));

  tops[num_variants] = t;
  ISA_LITCLASS target_lc = TI_TOP_Branch_Target_Litclass (t);

  FmtAssert (target_lc != LC_UNDEFINED, ("Branch instruction must have target!"));

  INT32 min, max;
  BOOL has_range;
  has_range = TI_ISA_LC_Range (target_lc, &min, &max);

  FmtAssert (has_range, ("Branch instruction's target should have range!"));

  if (t == TOP_loop_end) {
    /*  The loop_end uses the same litclass as loop, which is
        forward only (uimm8pc). But for loop_end, it really
        means the backward, so we have to reverse it.
     */
    int t = min;
    min = -max;
    max = -t;
  }
  ranges[num_variants].min = min;
  ranges[num_variants].max = max;

  num_variants++;
}

bool BRANCH_INST::get_min_max (TOP top, int *l, int *h)
{
  for (int i=0; i < num_variants; i++) {
    if (tops[i] == top) {
      *l = ranges[i].min;
      *h = ranges[i].max;
      return true;
    }
  }
  return false;
}

BRANCH_INST_TABLE::BRANCH_INST_TABLE (MEM_POOL *a_pool) :
  _pool (a_pool)
{
  tops_ix = CXX_NEW_ARRAY (int, TI_TOP_Count(), _pool);
  table_size = 0;
  for (int i=0; i < TI_TOP_Count(); i++) {
    if (TOP_is_branch((TOP)i)) {
      table_size++;
    }
    tops_ix[i] = -1;
  }
  FmtAssert(table_size >=0, ("The number of branch instructions must be >= 0"));
  branch_table = CXX_NEW_ARRAY(BRANCH_INST, table_size, _pool);

  // The latency is the same for all branches, including TIE branches.
  taken_delay = xt_i_latency + 1;

  init_table();
}

BRANCH_INST_TABLE::~BRANCH_INST_TABLE()
{
  CXX_DELETE_ARRAY(branch_table, _pool);
  CXX_DELETE_ARRAY(tops_ix, _pool);
}

int BRANCH_INST_TABLE::get_branch_range_min(TOP top)
{
  int ix = tops_ix[top];
  BRANCH_INST *binst = branch_table + ix;
  int min, max;
  binst->get_min_max(top, &min, &max);
  return min;
}

int BRANCH_INST_TABLE::get_branch_range_max(TOP top)
{
  int ix = tops_ix[top];
  BRANCH_INST *binst = branch_table + ix;
  int min, max;
  binst->get_min_max(top, &min, &max);
  return max;
}

/*
   All conditional branches + jump are initialized
   in branch_table[]. Because the algo does not need
   to consider jx for laying out BBs, jx isn't initialized
   in this table.
*/
void BRANCH_INST_TABLE::init_table()
{
  int n=0;
  int i, j;

  // First, set up core branch instructions
  for (i=0; i < TOP_count; i++) {
    if (TOP_is_branch((TOP)i)) {
      if ( (TOP)i == TOP_bnez_n ||
           (TOP)i == TOP_beqz_n ) {
        continue;
      }

      tops_ix[i] = n;
      branch_table[n].add_variant_TOP ((TOP)i);
      n++;
    }
  }

  // set up narrow branches TOP_bnez_n and TOP_beqz_n
  if (xt_density) {
    int table_ix = tops_ix[TOP_bnez];
    FmtAssert(table_ix >= 0, ("table_ix (entry for TOP_bnez) must be >= 0"));
    branch_table[table_ix].add_variant_TOP (TOP_bnez_n);
    tops_ix[TOP_bnez_n] = tops_ix[TOP_bnez];

    table_ix = tops_ix[TOP_beqz];
    FmtAssert(table_ix >= 0, ("table_ix (entry for TOP_beqz) must be >= 0"));
    branch_table[table_ix].add_variant_TOP (TOP_beqz_n);
    tops_ix[TOP_beqz_n] = tops_ix[TOP_beqz];
  }

  // set up wide-branch variants
  for (i=0; i < n; i++) {
    ISA_LITCLASS lc;
    /* If there is a wide branch variant for this core TOP, 
       the following will return its TOP.
     */
    TOP wbtop = TI_TOP_Get_WBranch_Top_Litclass((TOP)branch_table[i].get_top(0), &lc);
    if (wbtop != TOP_UNDEFINED) {
      tops_ix[wbtop] = i;
      branch_table[i].add_variant_TOP (wbtop);
    }
  }

  // Set up other branch instructions
  for (i=TOP_count; i < TI_TOP_Count(); i++) {
    if (tops_ix[i] == -1 &&       // Not processed yet
        TOP_is_branch((TOP)i)) {
      tops_ix[i] = n;
      branch_table[n].add_variant_TOP ((TOP)i);
      n++;
    }
  }

  FmtAssert( n <= table_size, 
             ("Number of branches:%d > table size:%d", n, table_size));
}


BRANCH_INFO::BRANCH_INFO ()
{
  br_bb = NULL;
  br_op = NULL;
  orig_br_op = NULL;
  for (int i=0; i < 2; i++) {
    forward_min_distance[i] = 0;
    backward_min_distance[i] = 0;
  }

  nsuccs = 0;
  succs = NULL;
  offset = 0;

  valid_entry = false;
  no_relax = false;

  relax_cost  = -1.0f;  // -1 means not set yet
  relax_cost1 = -1.0f;  // -1 means not set yet
  relax_cost2 = -1.0f;  // -1 means not set yet
}

BRANCH_INFO::~BRANCH_INFO ()
{
  if (valid_entry && succs != NULL) {
    CXX_DELETE_ARRAY (succs, pool);
    succs = NULL;
  }
}

BRANCH_INFO& BRANCH_INFO::operator= (const BRANCH_INFO &rhs)
{
  if (!rhs.valid_entry) return *this;

  valid_entry = rhs.valid_entry;
  br_bb = rhs.br_bb;
  br_op = rhs.br_op ? Dup_OP(rhs.br_op) : NULL;
  orig_br_op = rhs.orig_br_op;
  cond_jump = rhs.cond_jump;
  layout_done = rhs.layout_done;
  no_relax = rhs.no_relax;

  relax_cost  = rhs.relax_cost;
  relax_cost1 = rhs.relax_cost1; 
  relax_cost2 = rhs.relax_cost2; 

  nsuccs = rhs.nsuccs;
  if (nsuccs > 0) {
    succs = CXX_NEW_ARRAY(BBLIST, nsuccs, pool);
    for (int i=0; i < nsuccs; i++) {
      succs[i].item  = rhs.succs[i].item;
      succs[i].prob  = rhs.succs[i].prob;
      succs[i].flags = rhs.succs[i].flags;
      succs[i].next = NULL; // unused
    }
  } else {
    succs = NULL;
  }

  for (int i=0; i < 2; i++) {
    forward_min_distance[i] = rhs.forward_min_distance[i];
    backward_min_distance[i] = rhs.backward_min_distance[i];
  }

  return *this;
}

int BRANCH_INFO::get_succ_index(BB *a_succ) {
  int i;
  for (i=0; i < nsuccs; i++) {
    if (get_succ_bb(i) == a_succ) {
      return i;
    }
  }
  return -1;
}

void BRANCH_INFO::swap_succs(int succ0, int succ1) {
  BB   *b = get_succ_bb(succ0);
  float p = get_succ_prob(succ0);
  int   f = get_succ_flags(succ0);

  set_succ_bb(succ0, get_succ_bb(succ1));
  set_succ_prob(succ0, get_succ_prob(succ1));
  set_succ_flags(succ0, get_succ_flags(succ1));
  set_succ_bb(succ1, b);
  set_succ_prob(succ1, p);
  set_succ_flags(succ1, f);

  int d = forward_min_distance[succ0];
  forward_min_distance[succ0] = forward_min_distance[succ1];
  forward_min_distance[succ1] = d;

  d = backward_min_distance[succ0];
  backward_min_distance[succ0] = backward_min_distance[succ1];
  backward_min_distance[succ1] = d;
}


bool BRANCH_INFO::is_distance_within_range(int ix, int inc, bool is_forward)
{
  int dist = get_min_distance(ix, is_forward);

  /*
     The distance saved is always positive. So the negative number
     indicates that branch cannot be going that direction.
   */
  if (dist < 0) {
    return false;
  }

  dist += inc;
  int br_range = is_forward ? get_range_max() : -get_range_min();
  return (dist < br_range);
}

bool BRANCH_INFO::chain_distance_okay (
  CHAIN *theChain, 
  int ix, 
  int inc, 
  bool is_forward
)
{
  if (ignore_min_distance(ix, is_forward)) {
     return true;
  }

  int br_offset = offset + GET_BB_SIZE(br_bb);
  int dist = is_forward ? (theChain->bytes_size - br_offset) : br_offset;
  dist += inc;
  int br_range = is_forward ? get_range_max() : -get_range_min();
  return (dist < br_range);
}

int BRANCH_INFO::get_succ_delay (int succ_ix)
{
  int delay;

  if ((succ_ix == 0) && has_fall_thru()) {
    // fall-thru
    delay = 0;
  } else {
    OP *br_op = get_br_op();
    delay = BRANCH_INFO::btable->get_branch_taken_delay(); 
  }
  return delay;
}

/*
  Suppose that we will merge chain1 with chain2, this method
  check if any of TOP in the variant lists can meet min distance
  requirement, if so, set this TOP to br_op.
*/
TOP BRANCH_INFO::select_bigger_variant_top (int ix, int inc, bool is_forward)
{
  if (!br_op) {
    // This is a jump, just return. It shouldn't happen.
    return TOP_UNDEFINED;
  }

  BB  *bb_succ = get_succ_bb(ix);
  TOP top = OP_code(br_op);
  BRANCH_INST *binst = get_top_branch_inst(top);
  int min, max;
  if (!binst->get_min_max(top, &min, &max)) {
    return TOP_UNDEFINED;
  }

  int dist = get_min_distance(ix, is_forward);
  if (dist < 0) {
    return TOP_UNDEFINED;
  }
  dist += inc;

  int n = binst->get_num_variants();
  TOP new_top;
  int new_min, new_max;
  for (int i = 0; i < n; i++) {
    new_top = binst->get_top(i);
    if (new_top == top) {
      continue;
    }
    // the larger the 'i', the bigger the top's range
    if (!binst->get_min_max(new_top, &new_min, &new_max)) {
      return TOP_UNDEFINED;
    }
    if ( (new_min <= min) && (new_max >= max) ) {
      if ( is_forward  && (dist  > new_max) ||
           !is_forward && (-dist < new_min) ) {
        return TOP_UNDEFINED; 
      }
      return new_top;
    }
  }
  return TOP_UNDEFINED;        
}
   
BB_LAYOUT::BB_LAYOUT(MEM_POOL *const a_pool) :
  pool(a_pool),
  num_branchinfo(0),
  num_bbchains(0),
  len_edges(0)
{
  branchinfo = CXX_NEW_ARRAY(BRANCH_INFO, PU_BB_Count + 1, pool);
  bbchains = CXX_NEW_ARRAY(CHAIN, PU_BB_Count + 1, pool);
  num_branchinfo = PU_BB_Count + 1;
  num_bbchains = PU_BB_Count + 1;

  noafter_chains = BS_Create ((BS_ELT)(PU_BB_Count + 1), pool);
  nobefore_chains = BS_Create ((BS_ELT)(PU_BB_Count + 1), pool);
  bbs_affected = CXX_NEW_ARRAY(int, num_branchinfo, pool);
  jbb_after = BB_SET_Create(PU_BB_Count + 1, pool);

  int n = Count_Succ_Edges();
  edges = CXX_NEW_ARRAY(EDGE, n, pool);

  bb_size =  CXX_NEW_ARRAY(int, PU_BB_Count + 1, pool);
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    OP *op = BB_last_op(bb);
    if (op && OP_cond(op)) {
      // Reserve 3 bytes for possibly inserting 'j target'
      bb_size[BB_id(bb)] = BB_size(bb) + 3;
    } else {
      bb_size[BB_id(bb)] = BB_size(bb);
    }
  }

  chain_map = BB_MAP_Create();
}

BB_LAYOUT::~BB_LAYOUT()
{
  BB_MAP_Delete(chain_map);

  CXX_DELETE_ARRAY(bb_size, pool);
  CXX_DELETE_ARRAY(edges, pool);
  CXX_DELETE_ARRAY(bbchains, pool);
  CXX_DELETE_ARRAY(branchinfo, pool);
  CXX_DELETE_ARRAY(bbs_affected, pool);
}

void BB_LAYOUT::print_chain (CHAIN *c, bool detail)
{
  c->print(detail);
}

void BB_LAYOUT::print_chain(int cid, bool detail)
{
  CHAIN *c = bbchains + cid;
  c->print(detail);
}

void BB_LAYOUT::print_chains (bool detail)
{
  CHAIN *c;
  for (c=bbchains; c; c = c->next) {
    print_chain (c, detail);
  }
}

float BB_LAYOUT::calculate_succ_benefit (
  BRANCH_INFO *pbinfo,
  int  succ_ix,
  AFFECTED_BRANCH_STATUS *br_status
)
{
  bool is_forward = br_status->is_forward;
  bool dir_not_allowed = br_status->dir_not_allowed;
  bool dir_out_of_range = br_status->dir_out_of_range;
  bool out_of_range = br_status->out_of_range;
  BB   *bb_after = br_status->insert_after_bb;
  bool is_jbb_fwd = br_status->is_jbb_forward;

  BB *bb = pbinfo->get_br_bb();
  float bc = 0.0f;

  if (br_status->must_resolve) {
    if (!br_status->insert_jump) {
      if ( pbinfo->has_fall_thru() && (succ_ix == FALL_THRU_IX) ) {
        bc = 2.0f * get_direct_relaxation_cost(pbinfo); // benefit
      } else {
        bc = 1.5f * get_direct_relaxation_cost(pbinfo); // benefit
      }
    }
  } else if (pbinfo->is_succ_outofrange(succ_ix)) {
    // this branch has been out of range already.
    // TODO: calculate the cost difference with the previous cost
    ;
  } else if (dir_not_allowed && !out_of_range) {
    bc = -0.3 * get_direct_relaxation_cost(pbinfo);
  } else if (dir_out_of_range && !out_of_range) {
    if (!pbinfo->is_succ_outofrange(succ_ix, is_forward)) {
      bc = -0.2 * get_direct_relaxation_cost(pbinfo);
    } else {
      // Not worse, so, increase benefit
      bc = 0.1 * get_direct_relaxation_cost(pbinfo);
    }
  } else if (out_of_range) {
    if (bb_after == bb) {
      bc = -get_direct_relaxation_cost(pbinfo);
    } else {
      bc = get_indirect_relaxation_cost(pbinfo);
      if (!BB_for_layout(bb_after) && !BB_SET_MemberP(jbb_after, bb_after)) {
        BRANCH_INFO *pi = branchinfo + BB_id(bb_after);
        bc += -get_direct_relaxation_cost(pi);
      }
    }
  } else {
    // this branch has not been made out of range by this merging.
    // Assume no benefit nor cost at this time
    //bc = 0.0f; //get_direct_relaxation_cost(pbinfo);
  }

  return bc;
}

void BB_LAYOUT::relax_cond_branch (CHAIN *pchain, BRANCH_INFO *pbinfo)
{
  BB *bb = pbinfo->get_br_bb();
  BB *fall_thru_bb = pbinfo->get_succ_bb(FALL_THRU_IX);
  BB *target_bb = pbinfo->get_succ_bb(TARGET_IX);
  CHAIN *fall_thru_chain = get_bb_chain(fall_thru_bb);

  TOP invert_top = get_invert_branch_top (pbinfo->get_br_op());
  BB *jump_bb;
  if (invert_top != TOP_UNDEFINED) {
    jump_bb = create_jump_bb(bb, pbinfo, target_bb,
                             pbinfo->get_succ_prob(TARGET_IX));

    Set_OP_code(pbinfo->get_br_op(), invert_top);
    OP_Retarget_Branch(pbinfo->get_br_op(), target_bb, fall_thru_bb);
    pbinfo->swap_succs(FALL_THRU_IX, TARGET_IX);

    pbinfo->set_succ_resolved (TARGET_IX);
    pbinfo->set_succ_forward (TARGET_IX);
    pbinfo->set_min_distance(TARGET_IX, 0, true);
    pbinfo->set_min_distance(TARGET_IX, -1, false);
    pbinfo->reset_succ_outofrange(TARGET_IX);

  } else {
    jump_bb = create_jump_bb(bb, pbinfo, fall_thru_bb,
                             pbinfo->get_succ_prob(FALL_THRU_IX));
  }

  BB *next = BB_next(bb);
  BB_prev(jump_bb) = bb;
  BB_next(jump_bb) = next;
  BB_next(bb) = jump_bb;
  if (next) {
    BB_prev(next) = jump_bb;
  }

  if (pchain->tail == bb) {
    pchain->tail == jump_bb;
  }

  pbinfo->set_succ_bb(FALL_THRU_IX, jump_bb);
  BB_MAP_Set(chain_map, jump_bb, pchain);

  pbinfo->set_succ_resolved (FALL_THRU_IX);
  pbinfo->set_succ_forward (FALL_THRU_IX);
  pbinfo->reset_succ_outofrange(FALL_THRU_IX);
  pbinfo->set_min_distance(FALL_THRU_IX, 0, true);
  pbinfo->set_min_distance(FALL_THRU_IX, -1, false);

  GRA_LIVE_Compute_Liveness_For_BB(jump_bb);

  if ( CFLOW_Trace_Layout ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,
            "<CFLOW_layout> A jump BB:%d (BB:%d-->jump BB-->BB:%d) inserted\n",
            BB_id(jump_bb), BB_id(bb), BB_id(target_bb));
    fprintf(TFile,
            "               right after BB:%d for edge BB:%d->%d(f)\n",
            BB_id(bb), BB_id(bb), BB_id(target_bb));
  }
}

void BB_LAYOUT::relax_fall_thru (CHAIN *pchain, BRANCH_INFO *pbinfo)
{
  BB *bb = pbinfo->get_br_bb();
  BB *fall_thru_bb = pbinfo->get_succ_bb(FALL_THRU_IX);

  BB *jump_bb = create_jump_bb(bb, pbinfo, fall_thru_bb,
                               pbinfo->get_succ_prob(FALL_THRU_IX));
  BB *next = BB_next(bb);
  BB_prev(jump_bb) = bb;
  BB_next(jump_bb) = next;
  BB_next(bb) = jump_bb;
  if (next) {
    BB_prev(next) = jump_bb;
  }
 
  if (pchain->tail == bb) {
    pchain->tail == jump_bb;
  }

  pbinfo->set_succ_bb(FALL_THRU_IX, jump_bb);
  BB_MAP_Set(chain_map, jump_bb, pchain);

  pbinfo->set_succ_resolved (FALL_THRU_IX);
  pbinfo->set_succ_forward (FALL_THRU_IX);
  pbinfo->reset_succ_outofrange(FALL_THRU_IX);
  pbinfo->set_min_distance(FALL_THRU_IX, 0, true);
  pbinfo->set_min_distance(FALL_THRU_IX, -1, false);

  GRA_LIVE_Compute_Liveness_For_BB(jump_bb);

  if ( CFLOW_Trace_Layout ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,
            "<CFLOW_layout> A jump BB:%d (BB:%d-->jump BB-->BB:%d) inserted\n",
            BB_id(jump_bb), BB_id(bb), BB_id(fall_thru_bb));
    fprintf(TFile,
            "               right after BB:%d for edge BB:%d->%d(f)\n",
            BB_id(bb), BB_id(bb), BB_id(fall_thru_bb));
  }
}
     
void BB_LAYOUT::generate_jump_bb (
  CHAIN *aChain,
  BRANCH_INFO *pbinfo,
  int  succ_ix,
  BB  *jump_loc_bb,
  bool is_jbb_forward
)
{
  BB *jump_loc = jump_loc_bb;
  BB *bb = pbinfo->get_br_bb();
  BB *bb_succ = pbinfo->get_succ_bb(succ_ix);
  bool is_fall_thru = (pbinfo->get_succ_fall_thru() == bb_succ);
  float prob = pbinfo->get_succ_prob(succ_ix);
  CHAIN *tchain = get_bb_chain(jump_loc);

  // TODO: don't call generate_jump_bb() again
  // If inserting breaks fall-thru of jump_loc, insert a jump at this point
  if (!BB_for_layout(jump_loc) && !is_fall_thru) {
    BRANCH_INFO *pi = branchinfo + BB_id(jump_loc);
    bool fall_thru_resolved = pi->has_fall_thru() &&
                              pi->is_succ_resolved(FALL_THRU_IX); 
    if ( (pi->get_nsuccs() > 0) && fall_thru_resolved ) {
      FmtAssert(!BB_for_layout(BB_next(jump_loc)),
                ("Fall-thru BB must not be a jump BB"));
      if (pi->is_cond_jump()) {
        relax_cond_branch (aChain, pi);
      } else if (pi->get_nsuccs() == 1) {
        relax_fall_thru (aChain, pi);
      }
      jump_loc = BB_next(jump_loc);
    }
  }
      
  BB *jump_bb = create_jump_bb(bb, pbinfo, bb_succ, prob);
  if (!is_fall_thru) {
    OP_Retarget_Branch(pbinfo->get_br_op(), bb_succ, jump_bb);
  }
  pbinfo->set_succ_bb(succ_ix, jump_bb);
  BB_MAP_Set(chain_map, jump_bb, tchain);

  BB *next = BB_next(jump_loc);
  BB_prev(jump_bb) = jump_loc;
  BB_next(jump_bb) = next;
  BB_next(jump_loc) = jump_bb;
  if (next != NULL) {
    BB_prev(next) = jump_bb;
  }
  if (jump_loc == tchain->tail) {
    tchain->tail = jump_bb;
    tchain->set_tail_jump_bb();
  }

  GRA_LIVE_Compute_Liveness_For_BB(jump_bb);

  pbinfo->set_succ_resolved (succ_ix);
  pbinfo->reset_succ_outofrange(succ_ix);
  if (is_jbb_forward) {
    pbinfo->set_succ_forward (succ_ix);
  } else {
    pbinfo->set_succ_backward (succ_ix);
  }
  int dist;
  if (is_jbb_forward) { 
    if (tchain == aChain) {
      dist = get_bb_offset(jump_bb)
             - (pbinfo->get_offset() + bb_size [BB_id(pbinfo->get_br_bb())]);
    } else {
      dist = get_bb_offset(jump_bb)
             + (aChain->bytes_size  
                - (pbinfo->get_offset() + bb_size [BB_id(pbinfo->get_br_bb())]));
    }
  } else {
    if (tchain == aChain) {
      dist = (pbinfo->get_offset() + bb_size [BB_id(pbinfo->get_br_bb())])
             - get_bb_offset(jump_bb);
    } else {
      dist = pbinfo->get_offset() + bb_size [BB_id(pbinfo->get_br_bb())]
             + (tchain->bytes_size - get_bb_offset(jump_bb));
    }
  }
  pbinfo->set_min_distance(succ_ix, dist, is_jbb_forward);
  pbinfo->set_min_distance(succ_ix, -1, !is_jbb_forward);

  if ( CFLOW_Trace_Layout ) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,
            "<CFLOW_layout> A jump BB:%d (BB:%d-->jump BB-->BB:%d) inserted\n", 
            BB_id(jump_bb), BB_id(bb), BB_id(bb_succ));
    fprintf(TFile,
            "               right after BB:%d for edge BB:%d->%d(%c)\n",
            BB_id(BB_prev(jump_bb)), BB_id(bb), BB_id(bb_succ),
            is_fall_thru ? 'f' : 't');
  }
}

void BB_LAYOUT::set_branchinfo_visited (bool val)
{
  for (int i=0; i < num_branchinfo; i++) {
    branchinfo[i].set_visited(val);
  }
}

void BB_LAYOUT::add_chain_edge (BB *src, BB *dst)
{
  CHAIN *src_chain = get_bb_chain (src);
  CHAIN *dst_chain = get_bb_chain (dst);
  Is_True (src_chain != dst_chain, ("No edge from chain to itself")); 

  CHAIN_BB_EDGE *new_edge = CXX_NEW (CHAIN_BB_EDGE(src, dst), pool);
  CHAIN_BB_EDGE *cedge;

  new_edge->reset_flags();
  OP *br_op = BB_last_op(src);
  if (br_op && OP_loop_start(br_op)) {
    new_edge->set_loop_start();
  } else if (br_op && OP_code(br_op) == TOP_loop_end) {
    new_edge->set_loop_end();
  }

  // src_chain's out_edges
  new_edge->set_next_out(src_chain->out_edges);
  src_chain->out_edges = new_edge;

  // dst_chain's in_edges
  new_edge->set_next_in(dst_chain->in_edges);
  dst_chain->in_edges = new_edge;

#if Is_True_On
  for (cedge = new_edge->next_out(); 
       cedge != NULL; 
       cedge = cedge->next_out()) {
    if (is_chain_edge_same(cedge, new_edge)) {
      FmtAssert(FALSE, ("duplicate out edges"));
    }
  }

  for (cedge = new_edge->next_in();
       cedge != NULL;
       cedge = cedge->next_in()) {
    if (is_chain_edge_same(cedge, new_edge)) {
      FmtAssert (FALSE, (" Duplicate in edges "));
    }
  }
#endif
}

void BB_LAYOUT::delete_chain_edge (CHAIN_BB_EDGE *chain_edge)
{
  CHAIN *src_chain = get_bb_chain (chain_edge->get_bb_src());
  CHAIN *dst_chain = get_bb_chain (chain_edge->get_bb_dst());

  CHAIN_BB_EDGE *cedge, *prev;

  // src_chain's out_edges
  bool found = false;
  for (prev = NULL, cedge = src_chain->out_edges;
       cedge != NULL;
       prev = cedge, cedge = cedge->next_out()) {
    if (is_chain_edge_same(cedge, chain_edge)) {
      found = true;
      break;
    }
  }
  if (found) {
    if (prev) {
      prev->set_next_out(chain_edge->next_out());
    } else {
      src_chain->out_edges = chain_edge->next_out();
    }
    chain_edge->set_next_out(0);
  } else {
    Is_True (FALSE, ("CHAIN_BB_EDGE not found in out_edges"));
  }

  // dst_chain's in_edges
  found = false;
  for (prev = NULL, cedge = dst_chain->in_edges;
       cedge != NULL;
       prev = cedge, cedge = cedge->next_in()) {
    if (is_chain_edge_same(cedge, chain_edge)) {
      found = true;
      break;
    }
  }
  if (found) {
    if (prev) {
      prev->set_next_in(chain_edge->next_in());
    } else {
      dst_chain->in_edges = chain_edge->next_in();
    }
    chain_edge->set_next_in(0);
  } else {
    Is_True (FALSE, ("CHAIN_BB_EDGE not found in in_edges"));
  }

  CXX_DELETE (chain_edge, pool);
}

void BB_LAYOUT::init_branchinfo_distances (BB *from, BB *to, int ix)
{
  CHAIN *fchain = get_bb_chain(from);
  CHAIN *tchain = get_bb_chain(to);

  BB *bb;
  int dist;
  int n = BB_id(from);
  OP  *br_op = BB_last_op(from);

  // forward distance
  dist = 0;
  if (br_op && OP_code(br_op) == TOP_loop_end) {
    // no forward jump for loopend
    dist = -1;
  } else if (fchain == tchain) {
    // branch within the same chain 
    if (from == to) {
      dist = -1;  // no forward
    } else {
      for (bb = BB_next(from); bb && bb != to; bb = BB_next(bb)) {
        dist += bb_size[BB_id(bb)];
      }
      if (bb == NULL) {
        dist = -1;  // no forward
      }
    }
  } else {
    for (bb = BB_next(from); bb; bb = BB_next(bb)) {
      dist += bb_size[BB_id(bb)];
    }
    for (bb = tchain->head; bb && bb != to; bb = BB_next(bb)) {
      dist += bb_size[BB_id(bb)];
    }
  }
  branchinfo[n].set_min_distance(ix, dist, true);
  if (dist > 0) {
    if ( !branchinfo[n].is_distance_within_range(ix, dist, true) ) {
      branchinfo[n].set_ignore_min_distance(ix, true);
    }
    if (fchain == tchain) {
      branchinfo[n].set_succ_resolved(ix);
      branchinfo[n].set_succ_forward(ix);
    }
  }
    

  // backward distance
  dist = 0;
  if (br_op && OP_loop_start(br_op)) {
    // no backward jump for loop
    dist = -1;
  } else if (fchain == tchain) {
    // branch within the same chain 
    for (bb = from; bb && bb != to; bb = BB_prev(bb)) {
      dist += bb_size[BB_id(bb)];
    }
    if (bb == NULL) {
      dist = -1; // no backward
    } else {
      dist += bb_size[BB_id(to)];
    }
  } else {
    for (bb = from; bb; bb = BB_prev(bb)) {
      dist += bb_size[BB_id(bb)];
    }
    for (bb = tchain->tail; bb && bb != to; bb = BB_prev(bb)) {
      dist += bb_size[BB_id(bb)];
    }
    dist += bb_size[BB_id(to)];
  }
  branchinfo[n].set_min_distance(ix, dist, false);
  if (dist > 0) {
    if ( !branchinfo[n].is_distance_within_range(ix, dist, false) ) {
      branchinfo[n].set_ignore_min_distance(ix, false);
    }
    if (fchain == tchain) {
      branchinfo[n].set_succ_resolved(ix);
      branchinfo[n].set_succ_backward(ix);
    }
  }
}

void BB_LAYOUT::init_edges()
{
  // Use the existing Init_Edges()
  len_edges = Init_Edges(edges);
}

// Similar to Init_Chains(), but simpler
void BB_LAYOUT::init_chains()
{
  BB *bb, *next_bb;
  RID *first_rid = BB_rid(REGION_First_BB);
  CHAIN *prev = NULL; 

  CHAIN *chain = bbchains;
  int   cid = 0;
  for (bb = REGION_First_BB; bb; bb = next_bb) {
    Is_True(BB_rid(bb) == first_rid,
            ("region nesting botched at BB:%d", BB_id(bb)));

    /*
       Inner regions will be included in the same chain.
       For exception regions, they will be included in the same chain
       as well.
     */
    BB *tail;
    next_bb = bb;
    bool keep_growing;
    int size = 0;
    do {
      tail = next_bb;
      size += bb_size[BB_id(tail)];
      BB_MAP_Set(chain_map, tail, chain);
      next_bb = BB_next(tail);

      /*
        Some BBs must be in the same chain, such as loop and its succ,
        loop_end and its succ, etc.  We also want to keep a call BB
        and its succ that uses the return value together as well.
      */
      keep_growing = false;
      if (BB_call(tail)) {
        ANNOTATION  *ant = ANNOT_Get(BB_annotations(tail), ANNOT_CALLINFO);
        CALLINFO *my_ant = ANNOT_callinfo(ant);
        Is_True(my_ant != 0, ("No CALLINFO annotation"));

        if ( CALLINFO_ret_value(my_ant) ) { // has return value
          keep_growing = true;
        }
      } else {
        OP *br_op = BB_branch_op(tail);
        if (br_op != NULL) {
          TOP br_top = OP_code(br_op);
          if ( br_top == TOP_loop    || br_top == TOP_loopgtz ||
               br_top == TOP_loopnez || br_top == TOP_loop_end ) {
            keep_growing = true;
          }
        }
      }
    } while (    next_bb 
              &&    ((BB_rid(next_bb) != first_rid)
                 ||  (BBINFO_eh_rgn(tail) && BBINFO_eh_rgn(next_bb))
                 ||  keep_growing) );

    /* Create a new chain */
    BB_prev(bb) = NULL;
    BB_next(tail) = NULL;

    chain->chain_id = cid;
    chain->head = bb;
    chain->tail = tail;
    chain->next = chain + 1;
    chain->prev = prev;
    chain->weight = 0.0;
    chain->bytes_size = size;
    chain->in_edges = NULL;
    chain->out_edges = NULL;
    chain->flags = 0;

    prev = chain;
    ++chain;
    cid++;

    if (CFLOW_Trace_Layout && bb != tail) {
      #pragma mips_frequency_hint NEVER
      BB *bbc;
      fprintf(TFile, "<CFLOW_layout> multi-BB chain created:");
      bbc = bb;
      do {
        fprintf(TFile, "%c%d", bbc == bb ? ' ' : '-', BB_id(bbc));
      } while (bbc = BB_next(bbc));
      fprintf(TFile, "\n");
    }
  }
  prev->next = NULL;

  CHAIN *tmp = get_bb_chain (REGION_First_BB);
  tmp->set_entry_chain();
}

/*
  branchinfo[] has one entry for each BB. 

  The distance info is set up only for entry that corresponds
  to a BB with either conditional-branch or jump. The distance
  info IS NOT set up for any BB with jx, because of no need for
  doing that.
*/
void BB_LAYOUT::init_branchinfo()
{
  CHAIN *chain;
  BB *bb;
  int i;

  for (chain = bbchains; chain; chain = chain->next) 
  {
    int dist_to_top = 0;
    for (bb = chain->head; bb; bb = BB_next(bb))
    {
 
      OP *br_op = BB_last_op(bb);

      int bix = BB_id(bb);
      BRANCH_INFO *pbinfo = branchinfo + bix;
      pbinfo->set_valid_entry();
      pbinfo->set_br_bb(bb);
      pbinfo->set_offset(dist_to_top);
      dist_to_top += bb_size[bix];

      if (BB_next(bb) != NULL) {
        // Never break initial chain
        pbinfo->set_no_relax(true);
      }

      int num_succs = BBINFO_nsuccs(bb);
      pbinfo->set_nsuccs(num_succs);
      if (num_succs > 0) {
        BBLIST *succs_array = CXX_NEW_ARRAY(BBLIST, num_succs, pool);
        pbinfo->set_succs(succs_array);
        if (br_op && OP_cond(br_op)) {
          Is_True (num_succs == 2, ("Conditional jump must have two successors"));

          // fall-thru entry must be the first
          pbinfo->set_succ_bb(FALL_THRU_IX, BBINFO_succ_bb(bb, 1));
          pbinfo->set_succ_prob(FALL_THRU_IX, BBINFO_succ_prob(bb, 1));
          pbinfo->set_succ_flags(FALL_THRU_IX, 0);

          // target entry
          pbinfo->set_succ_bb(TARGET_IX, BBINFO_succ_bb(bb, 0));
          pbinfo->set_succ_prob(TARGET_IX, BBINFO_succ_prob(bb, 0));
          pbinfo->set_succ_flags(TARGET_IX, 0);
        } else {
          for (i=0; i < num_succs; i++) {
            pbinfo->set_succ_bb(i, BBINFO_succ_bb(bb, i));
            pbinfo->set_succ_prob(i, BBINFO_succ_prob(bb, i));
            pbinfo->set_succ_flags(i, 0);
          }
        }
      } else {
        pbinfo->set_succs(0);
      }
        
      pbinfo->set_orig_br_op(NULL);

      if (br_op && OP_cond(br_op)) {
        // two-way conditional jump

        pbinfo->set_orig_br_op(br_op);
        pbinfo->set_cond_jump(true);

        BB *target_bb = BBINFO_succ_bb(bb, 0);
        BB *fall_thru_bb = BBINFO_succ_bb(bb, 1);
        CHAIN *tchain = get_bb_chain(target_bb);
        CHAIN *fchain = get_bb_chain(fall_thru_bb);

        init_branchinfo_distances (bb, target_bb, TARGET_IX);
        if (chain != tchain) {
          add_chain_edge (bb, target_bb);
        }

        init_branchinfo_distances (bb, fall_thru_bb, FALL_THRU_IX);
        if (chain != fchain) {
          add_chain_edge(bb, fall_thru_bb);
        }

#if 0
        if (OP_loop_start(br_op) || OP_code(br_op) == TOP_loop_end) {
          pbinfo->set_no_relax(true);
        } else {
          pbinfo->set_no_relax(false);
        }

        if (chain != tchain || chain != fchain) {
           pbinfo->set_layout_done(false);
        } else {
           pbinfo->set_layout_done(true);
        }
#endif
      } else {
        // having either direct jump or indirect jump
        if (br_op && (OP_jump(br_op) || OP_ijump(br_op))) {
          pbinfo->set_orig_br_op(br_op);
        } else {
          // no jump instruction
          pbinfo->set_orig_br_op(NULL);
        }
        pbinfo->set_cond_jump(false);
        if (br_op && OP_ijump(br_op)) {
          pbinfo->set_layout_done(true);
        } else if (BBINFO_nsuccs(bb) == 1) {
          BB *succ_bb = BBINFO_succ_bb(bb, 0);
          CHAIN *schain = get_bb_chain(succ_bb);
          
          init_branchinfo_distances (bb, succ_bb, 0);

          if (chain != schain) {
            pbinfo->set_layout_done(false);
            add_chain_edge (bb, succ_bb);
          } else {
            pbinfo->set_layout_done(true);
          }
        } else {
          Is_True (BBINFO_nsuccs(bb) == 0,
                   ("Must be return, etc. basic block with no succ."));
        }
      }

      if (pbinfo->get_orig_br_op()) {
        OP *new_op = Dup_OP(pbinfo->get_orig_br_op());
        OP_srcpos(new_op) = OP_srcpos(pbinfo->get_orig_br_op());
        pbinfo->set_br_op(new_op);
      } else {
        pbinfo->set_br_op(NULL);
      }
    }
  }
}

bool BB_LAYOUT::check_edges_dependences (CHAIN *chain1, CHAIN *chain2)
{
  CHAIN *pchain, *schain;
  CHAIN_BB_EDGE *cedge=NULL, *next=NULL;
  BB *bb, *bb_succ;

  TRAVERSE_EDGES all_edges[4];
  set_up_traverse_edges(all_edges, chain1, chain2);

  noafter_chains = BS_ClearD(noafter_chains);
  nobefore_chains = BS_ClearD(nobefore_chains);

  bool no_before_chain = (Entry_BB_Always_First && chain1->is_entry_chain());
  int edge_seq;
  for (edge_seq = 0; edge_seq < 4; edge_seq++)
  {
    CHAIN_BB_EDGE *edges_head = *(all_edges[edge_seq].pedges);
    bool          is_out_edge = all_edges[edge_seq].is_out;

    for ( cedge = edges_head; cedge != NULL; cedge = next )
    {
      next = is_out_edge ? cedge->next_out() : cedge->next_in();

      if ( !cedge->is_dependence_edge() ) {
        continue;
      }

      bb = cedge->get_bb_src();
      bb_succ = cedge->get_bb_dst();
      pchain = get_bb_chain(bb);
      schain = get_bb_chain(bb_succ);

      Is_True(pchain != schain, ("pred-chain shouldn't be the same as succ-chain"));

      if (pchain == chain1 && schain == chain2) {
        if (cedge->is_loop_end()) {
          return false;
        }
      } else if (pchain == chain2 && schain == chain1) {
        if (cedge->is_loop_start()) {
          return false;
        }
      } else { 
        CHAIN *another;
        if (pchain == chain1 || pchain == chain2) {
          another = schain;
          if (cedge->is_loop_start()) {
            if (BS_MemberP (noafter_chains, another->chain_id)) {
              return false;
            } else {
              nobefore_chains = BS_Union1 (nobefore_chains, another->chain_id, pool);
            }
          } else if (cedge->is_loop_end()) {
            if ( no_before_chain ||
                 BS_MemberP (nobefore_chains, another->chain_id) ) {
              return false;
            } else {
              noafter_chains = BS_Union1 (noafter_chains, another->chain_id, pool);
            }
          }
        } else { // schain == chain1 || schain == chain2
          another = pchain;
          if (cedge->is_loop_start()) {
            if ( no_before_chain ||
                 BS_MemberP (nobefore_chains, another->chain_id) ) {
              return false;
            } else {
              noafter_chains = BS_Union1 (noafter_chains, another->chain_id, pool);
            }
          } else if (cedge->is_loop_end()) {
            if (BS_MemberP (noafter_chains, another->chain_id)) {
              return false;
            } else {
              nobefore_chains = BS_Union1 (nobefore_chains, another->chain_id, pool);
            }
          }
        }
      }
    } // cedge
  } // edge_seq

  return true;
}

/*
   Given two chains: chain1 and chain2, this method checks if merging chain1
   with chain2 (chain1-->chain2) causes any of min distances to be out of range.
   If not, return true; otherwise, return false.
 
   A branch is considered to be out of range if both its forward and backward
   min distances are out of range; otherwise, it is within the range.

   If do_update is true, check_update_edges_distances() will update min
   distances, assuming chain1 is merged with chain2.
*/
bool BB_LAYOUT::check_update_edges_distances
(
  CHAIN    *chain1,
  CHAIN    *chain2,
  bool     do_update   // true: doing update only; false: check distance only
)
{
  BB *bb, *bb_succ;
  CHAIN *pchain, *schain;
  BRANCH_INFO *pbinfo;
  CHAIN_BB_EDGE *cedge=NULL, *next=NULL;

  /*
     Need to traverse both chains' in/out edges (4 edges list), using
     TRAVERSE_EDGES is convenient.
  */
  TRAVERSE_EDGES all_edges[4];
  set_up_traverse_edges(all_edges, chain1, chain2);

  if (!do_update) {
    noafter_chains  = BS_ClearD(noafter_chains);
    nobefore_chains = BS_ClearD(nobefore_chains);
  }

  int edge_seq;
  for (edge_seq = 0; edge_seq < 4; edge_seq++)
  {
    CHAIN_BB_EDGE *edges_head = *(all_edges[edge_seq].pedges);
    bool           is_forward = all_edges[edge_seq].is_forward;
    bool          is_out_edge = all_edges[edge_seq].is_out;
    bool            is_chain1 = all_edges[edge_seq].is_chain1;

    int inc = is_chain1 ? chain2->bytes_size : chain1->bytes_size;
    for ( cedge = edges_head; cedge != NULL; cedge = next )
    {
      next = is_out_edge ? cedge->next_out() : cedge->next_in();
      bb = cedge->get_bb_src();
      bb_succ = cedge->get_bb_dst();
      pbinfo = branchinfo + BB_id(bb);

      pchain = get_bb_chain(bb);
      schain = get_bb_chain(bb_succ);

      Is_True(pchain != schain, ("pred-chain shouldn't be the same as succ-chain"));

      if ( !is_out_edge && 
           ((pchain == chain1 && schain == chain2) ||
            (pchain == chain2 && schain == chain1)) ) {
        // This edge should be processed in out-edge
        continue;
      }
  
      int ix = pbinfo->get_succ_index (bb_succ);
  
      FmtAssert(ix != -1, ("Succ isn't in Succ lists!"));

      bool is_fall_thru = (pbinfo->get_succ_fall_thru() == bb_succ);

      if (do_update) {
        if ( (pchain == chain1 || pchain == chain2) &&
             (schain == chain2 || schain == chain1) ) {
          pbinfo->set_succ_resolved(ix);
          pbinfo->set_min_distance(ix, -1, !is_forward);
          if (pchain == chain2) {
            pbinfo->set_succ_backward(ix);
          } else {
            pbinfo->set_succ_forward(ix);
          }
        } else {
          pbinfo->inc_min_distance(ix, inc, is_forward);
          if (is_fall_thru) {
            if (inc > 0) {
              pbinfo->set_succ_outofrange(ix);
            }
          } else if (!pbinfo->is_distance_okay(ix, 0, is_forward)) {
            pbinfo->set_succ_outofrange(ix, is_forward);

            // The other direction must be within the range
            Is_True( pbinfo->is_distance_okay(ix, 0, !is_forward),
                     ("The min distance (%s) shouldn't be out of range",
                      !is_forward ? "backward" : "forward" ));
          }
        }

        continue;
      }
  
      Is_True(!do_update, (" do_update must be false at this point"));

      // Now, check distances
      if (pchain == chain1 && schain == chain2) {
        // edge: chain1 --> chain2
        if ( !pbinfo->is_distance_okay(ix, 0, true) ) { 
          TOP new_top = pbinfo->select_bigger_variant_top(ix, 0, true);
          if (new_top == TOP_UNDEFINED) {
            return false;
          }
          Set_OP_code(pbinfo->get_br_op(), new_top);
        }
      } else if (pchain == chain2 && schain == chain1 ) {
        // edge: chain2 --> chain1
        if (is_fall_thru) {
          return false;
        } else if ( !pbinfo->is_distance_okay(ix, 0, false) ) { 
          TOP new_top = pbinfo->select_bigger_variant_top(ix, 0, false);
          if (new_top == TOP_UNDEFINED) {
            return false;
          }
          Set_OP_code(pbinfo->get_br_op(), new_top);
        }
      } else {
        /*
           Check forward/backward distances between {chain1, chain2} and
           another chain;
  
           Need to check both forward and backward distances and keep track
           of whether forward or backward direction is available with 
           nobefore_chains/noafter_chains.
         */
        CHAIN *another = (pchain == chain1 || pchain == chain2) ? schain : pchain;
        bool can_before = ! BS_MemberP ( nobefore_chains, another->chain_id);
        bool can_after  = ! BS_MemberP ( noafter_chains, another->chain_id);
        bool okay = false;
        if ( is_chain1 && can_after || !is_chain1 && can_before ) {
          if ( !pbinfo->is_distance_okay(ix, inc, is_forward) ) {
            TOP new_top = pbinfo->select_bigger_variant_top(ix, inc, is_forward);
            if (new_top == TOP_UNDEFINED) {
              if (is_chain1) {
                noafter_chains = BS_Union1 (noafter_chains, another->chain_id, pool);
              } else {
                nobefore_chains = BS_Union1 (nobefore_chains, another->chain_id, pool);
              }
            } else {
              Set_OP_code(pbinfo->get_br_op(), new_top);
              okay = true;
            }
          } else {
            okay = true;
          }
        }
  
        if (is_chain1 && can_before || !is_chain1 && can_after) {
          if ( !pbinfo->is_distance_okay(ix, inc, !is_forward) ) {
            TOP new_top = pbinfo->select_bigger_variant_top(ix, inc, !is_forward);
            if (new_top == TOP_UNDEFINED) {
              if (is_chain1) {
                nobefore_chains = BS_Union1 (nobefore_chains, another->chain_id, pool);
              } else {
                noafter_chains = BS_Union1 (noafter_chains, another->chain_id, pool);
              }
            } else {
              Set_OP_code(pbinfo->get_br_op(), new_top);
              okay = true;
            }
          } else {
            okay = true;
          }
        }
  
        if (!okay) {
          return false;
        }
      }
    } // for cedge
  }
  return true;
}
            
void BB_LAYOUT::merge_edges(CHAIN *pchain, CHAIN *schain)
{
  CHAIN_BB_EDGE *cedge, *next_cedge, *prev;
  CHAIN *tmp;

  // First, remove edges from pchain to schain 
  next_cedge = NULL;
  for (cedge = pchain->out_edges; cedge != NULL; cedge = next_cedge) {
    next_cedge = cedge->next_out();
    tmp = get_bb_chain(cedge->get_bb_dst());
    if (tmp == schain) {
      delete_chain_edge (cedge);
    }
  }

  // Second, remove edges from schain to pchain 
  next_cedge = NULL;
  for (cedge = schain->out_edges; cedge != NULL; cedge = next_cedge) {
    next_cedge = cedge->next_out();
    tmp = get_bb_chain(cedge->get_bb_dst());
    if (tmp == pchain) {
      delete_chain_edge (cedge);
    }
  }

  // Third, Merge schain's in_edges into pchain's
  for (prev = NULL, cedge = pchain->in_edges;
       cedge != NULL;
       prev = cedge, cedge = cedge->next_in());
  if (prev == NULL) {
    pchain->in_edges = schain->in_edges;
  } else {
    prev->set_next_in(schain->in_edges);
  }

  // Finally, Merge schain's out_edges into pchain's
  for (prev = NULL, cedge = pchain->out_edges;
       cedge != NULL;
       prev = cedge, cedge = cedge->next_out());
  if (prev == NULL) {
    pchain->out_edges = schain->out_edges;
  } else {
    prev->set_next_out(schain->out_edges);
  }
}

void BB_LAYOUT::merge_chains (BB *pred, BB *succ)
{
  CHAIN *pchain = get_bb_chain(pred);
  CHAIN *schain = get_bb_chain(succ);

  // update branchinfo
  update_min_distances(pchain, schain);

  merge_edges(pchain, schain);

  int dist = pchain->bytes_size;
  pchain->bytes_size = pchain->bytes_size + schain->bytes_size;

  // Keep pchain and discard schain
  BB *bb;
  for (bb = schain->head; bb != NULL; bb = BB_next(bb)) {
    int ix = BB_id(bb);
    if (ix < num_branchinfo) {
      int ofst = branchinfo[ix].get_offset() + dist;
      branchinfo[ix].set_offset(ofst); 
    }
    BB_MAP_Set(chain_map, bb, pchain);
  }
  BB_next(pred) = succ;
  BB_prev(succ) = pred;
  pchain->tail = schain->tail;

  // delete schain from chain list
  CHAIN *prev = schain->prev;
  CHAIN *next = schain->next;
  if (prev == NULL) {
    bbchains = next;
  } else {
    prev->next = next;
  }
  if (next != NULL) {
    next->prev = prev;
  }

  schain->next = schain->prev = NULL;
}

void BB_LAYOUT::save_branchinfo()
{
  int i;
  for (i=0; i < num_branchinfo; i++) {
    if (branchinfo[i].is_valid_entry()) {
      saved_branchinfo[i] = branchinfo[i];
    }
  }
}

void BB_LAYOUT::restore_branchinfo()
{
  int i;
  for (i=0; i < num_branchinfo; i++) {
    if (saved_branchinfo[i].is_valid_entry()) {
      branchinfo[i] = saved_branchinfo[i];
    }
  }
}

void BB_LAYOUT::grow_chains()
{
  INT i;

  INT32 unuse1;
  double unuse2;
  INT fixed_branch_cost;
  INT taken_cycle;
  BRANCH_INFO *pbinfo;

  if (CFLOW_Trace_Layout) {
    fprintf(TFile, "\n%s"
                   " <CFLOW_layout> -- START GROWING CHAINS ...... \n\n",
                   DBar);
  }

  
  saved_branchinfo = CXX_NEW_ARRAY(BRANCH_INFO, num_branchinfo, pool);
  save_branchinfo();

  for (i = 0; i < len_edges; ++i) {
    EDGE *e = edges + i;
    BB *pred = e->pred;
    BB *succ = e->succ;
    CHAIN *pchain = get_bb_chain(pred);
    CHAIN *schain = get_bb_chain(succ);
    int   bix = BB_id(pred);
    pbinfo = branchinfo + bix;
    
    if (pchain != schain && pchain->tail == pred && schain->head == succ) {
      bool can_merge = true;

      if ( pbinfo->is_cond_jump() && pbinfo->get_succ_target() == succ ) {
        TOP invert_top = get_invert_branch_top (pbinfo->get_br_op());
        if (invert_top == TOP_UNDEFINED) {
          can_merge = false;
        } else {
          // change branchinfo[]
          OP_Retarget_Branch (pbinfo->get_br_op(),
                              pbinfo->get_succ_target(),
                              pbinfo->get_succ_fall_thru());
          Set_OP_code(pbinfo->get_br_op(), invert_top);
          pbinfo->swap_succs(FALL_THRU_IX, TARGET_IX);
        }
      }

      if (can_merge) {
        bool min_dist_okay = check_min_distances(pchain, schain);
        if (!min_dist_okay) {
          can_merge = false;
        }
      }

      if (can_merge) {
        merge_chains (pred, succ);

        if (CFLOW_Trace_Layout) {
          fprintf(TFile, " <CFLOW_layout> merging CHAIN:%d with CHAIN:%d\n",
                         pchain->chain_id, schain->chain_id);
        }

        /* If pred has a j instruction to jump to succ, that j
           is no longer needed. Change it to nop.
         */

        /* TODO:  change distance after replacing nop 
                  if pred has no other OPs, pred can be deleted.
         */
        OP *br_op = pbinfo->get_br_op();
        if ( br_op && (OP_code(br_op) == TOP_j) ) {
	  if ( CFLOW_Trace_Layout ) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, "                A jump (BB%d --> BB:%d) changed to nop\n",
                           BB_id(pred), BB_id(succ));
          }

          pbinfo->set_br_op(Mk_OP(TOP_noop));
        }
        save_branchinfo();
      } else {
        restore_branchinfo();
      }
    }
  }

  CXX_DELETE_ARRAY(saved_branchinfo, pool);
  saved_branchinfo = NULL;
}


void BB_LAYOUT::check_branch_target_status
(
  CHAIN       *chain1,
  CHAIN       *chain2,
  CHAIN       *pchain,
  CHAIN       *schain,
  BRANCH_INFO *pbinfo,
  int         succ_ix,
  AFFECTED_BRANCH_STATUS *status_flags
)
{
  FmtAssert(pchain != schain, ("pchain should not be eqaul to schain")); 
    
  bool out_range = false;
  bool dir_out_range = false;
  bool insert = false;
  bool forward = (pchain == chain1 || schain == chain2);
  bool must_resolve = false;
  bool dir_not_allowed = false;
  int  inc;
  if ( (pchain == chain1 || pchain == chain2) &&
       (schain == chain1 || schain == chain2) ) {
    inc = 0;
  } else if (pchain == chain1 || schain == chain1) {
    inc = chain2->bytes_size;
  } else if (pchain == chain2 || schain == chain2) {
    inc = chain1->bytes_size;
  } else {
    inc = 0;
  }

  if ( (pchain == chain1 || pchain == chain2) &&
       (schain == chain1 || schain == chain2) ) {
    dir_out_range = !pbinfo->ignore_min_distance(succ_ix, forward) &&
                     (pbinfo->is_succ_outofrange(succ_ix, forward) ||
                      !pbinfo->is_distance_okay(succ_ix, 0, forward));
    out_range = dir_out_range;
    insert = out_range;
    must_resolve = true;
  } else if ( (pchain == chain1 || pchain == chain2) ||
              (schain == chain1 || schain == chain2) ) {
    dir_out_range = pbinfo->is_succ_outofrange(succ_ix, forward) ||
                    !pbinfo->is_distance_okay(succ_ix, inc, forward);
    out_range = dir_out_range &&
                pbinfo->is_succ_outofrange(succ_ix, !forward);

    if (out_range && (pchain == chain1 || pchain == chain2)) {
      insert = !pbinfo->chain_distance_okay(pchain, succ_ix, inc, forward) &&
               !pbinfo->chain_distance_okay(pchain, succ_ix, 0, !forward);
    }
  } else {
    dir_out_range = false; 
    out_range = false; // pbinfo->is_succ_outofrange(succ_ix);
    insert = false;
  }

  if (chain1->is_entry_chain() && forward && (schain == chain2 || schain == chain1) ||
      schain->is_entry_chain() ||
      !forward && pchain->is_entry_chain() ) {
    dir_not_allowed = true;
  }

  status_flags->inc = inc;
  status_flags->must_resolve = must_resolve;
  status_flags->is_forward = forward;
  status_flags->dir_not_allowed = dir_not_allowed;
  status_flags->dir_out_of_range = dir_out_range;
  status_flags->out_of_range = out_range;
  status_flags->insert_jump = insert;
  return;
}


void BB_LAYOUT::resolve_target
(
  CHAIN       *chain1,
  CHAIN       *chain2,
  BRANCH_INFO *pbinfo,    // branch to be resolved
  float       *benefit    // same as resolve_branches   
)
{
  BB    *bb = pbinfo->get_br_bb();
  CHAIN *pchain = get_bb_chain(bb);

  BB    *target_bb =  pbinfo->get_succ_target();
  CHAIN *target_chain = target_bb ? get_bb_chain(target_bb) : NULL;
  int   target_ix = target_bb ? pbinfo->get_succ_index(target_bb) : -1;
  bool target_resolved = (target_ix == -1) 
                           ? true
                           : pbinfo->is_succ_resolved(target_ix);

  if (target_resolved) {
    return;
  }

  FmtAssert(target_ix >=0, ("the index of target BB must be positive"));

  OP *br_op = BB_last_op(bb);
  bool zcl_loop_br = br_op && 
                     (OP_loop_start(br_op) || (OP_code(br_op) == TOP_loop_end));

  AFFECTED_BRANCH_STATUS status_flags;
  check_branch_target_status(chain1, chain2, pchain, target_chain, 
                             pbinfo, target_ix, &status_flags);
  bool out_of_range = status_flags.out_of_range;
  bool insert_jump_bb = status_flags.insert_jump;
  bool is_forward = status_flags.is_forward;
  bool dir_out_of_range = status_flags.dir_out_of_range;
  bool must_resolve = status_flags.must_resolve;
  int inc = status_flags.inc;

  if (zcl_loop_br) {
    // TODO: special zcl

    if (benefit != NULL) {
      return;
    }

    // Now, update min distance and delete edge if it's resolved.
    if ( (pchain != chain1 && pchain != chain2) ||
         (target_chain != chain1 && target_chain != chain2) ) {
      if (inc != 0) {
        pbinfo->inc_min_distance(target_ix, inc, is_forward);
      }
    } else if ( (pchain == chain1 || pchain == chain2) &&
                (target_chain == chain1 || target_chain == chain2) ) {
      CHAIN_BB_EDGE *e = pchain->find_out_edge(bb, target_bb);
      FmtAssert(e, ("e must not be NULL"));
      delete_chain_edge(e);
    }

  } else {
    /* 
       If merging <chain1, chain2> causes target_ix to be out of range, we
       will see if another variant with bigger range can make target within
       range. 
     */
    TOP  new_top = TOP_UNDEFINED;
    if (out_of_range && !pbinfo->is_succ_outofrange(target_ix)) {
      new_top = pbinfo->select_bigger_variant_top(target_ix, inc, is_forward);
      if (new_top != TOP_UNDEFINED) {
        // found a bigger one
        out_of_range = false;
        dir_out_of_range = false;
        insert_jump_bb = false;

        status_flags.out_of_range = false;
        status_flags.dir_out_of_range = false;
        status_flags.insert_jump = false;
      }
    }

    BB *jbb_cand=NULL;
    bool is_jbb_fwd = is_forward;
    if (out_of_range) {
      jbb_cand = find_best_location(chain1, chain2, pbinfo, target_ix, &is_jbb_fwd, benefit);
      status_flags.insert_after_bb = jbb_cand;
      status_flags.is_jbb_forward = is_jbb_fwd;

      if ( (jbb_cand == bb) && (benefit == NULL) ) {
        // Let xt-as to relax it
        pbinfo->set_ignore_min_distance(target_ix, true);
        pbinfo->set_ignore_min_distance(target_ix, false);
        out_of_range = false;
        dir_out_of_range = false;
        insert_jump_bb = false;

        status_flags.out_of_range = false;
        status_flags.dir_out_of_range = false;
        status_flags.insert_jump = false;
      } 
    }

    if (benefit != NULL) {
      /* 
         the distance of branch bb->target_bb (is_forward) is affected by
         merging chain1 with chain2.
       */
      *benefit += calculate_succ_benefit(pbinfo, target_ix, &status_flags);
      if (insert_jump_bb) {
        jbb_after = BB_SET_Union1(jbb_after, jbb_cand, pool);
      }
    } else {
      if (insert_jump_bb) {
        generate_jump_bb(pchain, pbinfo, target_ix, 
                         status_flags.insert_after_bb, status_flags.is_jbb_forward);
        CHAIN_BB_EDGE *e = pchain->find_out_edge(bb, target_bb);
        FmtAssert(e, ("e must not be NULL"));
        delete_chain_edge(e);
      } else {
        if (new_top != TOP_UNDEFINED) {
          Set_OP_code(pbinfo->get_br_op(), new_top);
        }
        /* Need to update distance no matter if it is out of range !
           No jump is added at this point.
         */
        if ( (pchain != chain1 && pchain != chain2) ||
             (target_chain != chain1 && target_chain != chain2) ) {
          if (inc > 0) {
            pbinfo->inc_min_distance(target_ix, inc, is_forward);
          }
        } else {
          /* The branching is between chain1 and chain2. Need to
             remove edge. No need to update the min distance.
           */
          CHAIN_BB_EDGE *e = pchain->find_out_edge(bb, target_bb);
          FmtAssert(e, ("e must not be NULL"));
          delete_chain_edge(e);

          pbinfo->set_succ_resolved(target_ix);
          pbinfo->set_min_distance(target_ix, -1, !is_forward);
          if (!is_forward) {
            pbinfo->set_succ_backward(target_ix);
          }
        }

        if (dir_out_of_range) {
          pbinfo->set_succ_outofrange(target_ix, is_forward);
        }
      }
    }
  }
}

void BB_LAYOUT::resolve_fall_thru
(
  CHAIN       *chain1,
  CHAIN       *chain2,
  BRANCH_INFO *pbinfo,    // branch to be resolved
  float       *benefit    // same as resolve_branches   
)
{
  BB    *bb = pbinfo->get_br_bb();
  CHAIN *pchain = get_bb_chain(bb);
  
  /* Set boolean fall_thru_resolved/target_resolved. If no fall-thru/target,
     set them to true.
   */
  BB    *fall_thru_bb    = pbinfo->get_succ_fall_thru();
  CHAIN *fall_thru_chain = fall_thru_bb ? get_bb_chain(fall_thru_bb) : NULL;
  bool  fall_thru_resolved = pbinfo->has_fall_thru() ? (fall_thru_chain == pchain)
                                                     : true;
  if (fall_thru_resolved) {
    return;
  }

  AFFECTED_BRANCH_STATUS status_flags;

  bool out_of_range = (BB_next(bb) != NULL) ||
                   (BB_prev(fall_thru_bb) != NULL) ||
                   pchain->has_tail_jump_bb() ||
                   fall_thru_chain->has_head_jump_bb() ||
                   (fall_thru_chain == chain1 && pchain == chain2) ||
                   (fall_thru_chain == chain2 && pchain != chain1) ||
                   (fall_thru_chain != chain2 && pchain == chain1) ||
                   (benefit != NULL &&  BB_SET_MemberP(jbb_after, bb));
  bool insert_jump_bb = out_of_range;

  bool must_resolve = (pchain == chain1 || pchain == chain2) && 
                      (fall_thru_chain == chain1 || fall_thru_chain == chain2);

  status_flags.is_forward = true;
  status_flags.dir_not_allowed = false;
  status_flags.dir_out_of_range = out_of_range;
  status_flags.out_of_range = out_of_range;
  status_flags.must_resolve = must_resolve;
  status_flags.insert_jump = out_of_range;
  status_flags.inc = 0;
  status_flags.is_jbb_forward = true;
  status_flags.insert_after_bb = bb;

  if (benefit != NULL) {
    *benefit += calculate_succ_benefit(pbinfo, 0, &status_flags);
    if (insert_jump_bb) {
      jbb_after = BB_SET_Union1(jbb_after, bb, pool);
    }
  } else if (insert_jump_bb) {
    generate_jump_bb(pchain, pbinfo, 0, bb, true);

    CHAIN_BB_EDGE *e = pchain->find_out_edge(bb, fall_thru_bb);
    delete_chain_edge(e);
  } else {
    if (must_resolve) {
      pbinfo->set_succ_resolved(FALL_THRU_IX);
      pbinfo->set_succ_forward(FALL_THRU_IX);
      pbinfo->set_min_distance(FALL_THRU_IX, -1, false);
    }
  }
}


void BB_LAYOUT::get_list_of_sorted_branches
(
  CHAIN *chain1,
  CHAIN *chain2,
  int   *bbs_array,
  int   *alen
)
{
  BB *bb;
  CHAIN *pchain;
  CHAIN_BB_EDGE *cedge, *next;
  BRANCH_INFO *pbinfo;
  TRAVERSE_EDGES all_edges[4];
  set_up_traverse_edges(all_edges, chain1, chain2);

  // Prevent from multiple visits to any branch
  set_branchinfo_visited(false);

  int for_len=0, back_len=(num_branchinfo-1);
  int edge_seq;
  for (edge_seq = 0; edge_seq < 4; edge_seq++) 
  {
    CHAIN_BB_EDGE *edges_head = *(all_edges[edge_seq].pedges);
    bool          is_out_edge = all_edges[edge_seq].is_out;

    for ( cedge = edges_head; cedge != NULL; cedge = next )
    {
      next = is_out_edge ? cedge->next_out() : cedge->next_in();
      bb = cedge->get_bb_src();
      pbinfo = branchinfo + BB_id(bb);
      pchain = get_bb_chain (bb);

      FmtAssert(pbinfo->get_nsuccs() <= 2,
                ("A branch considered here should not have more than 2 succs!"));

      if (pbinfo->is_visited()) {
         continue;
      }

      BB *fall_thru_bb = pbinfo->get_succ_fall_thru();
      CHAIN *fall_thru_chain = fall_thru_bb ? get_bb_chain(fall_thru_bb) : NULL;
      bool fall_thru_resolved = pbinfo->has_fall_thru() ? (fall_thru_chain == pchain) : true;

      if (fall_thru_resolved || (BB_next(bb) != NULL)) {
        // add in bbs_array's first part
        bbs_array[for_len] = BB_id(bb);
        for_len++;
      } else {
#if 0
        Is_True (BB_next(bb) == NULL, 
                 ("BB with Unresolved fall_thru should have NULL in BB_next()"));
#endif
        bbs_array[back_len] = BB_id(bb);
        back_len--;
      }
      pbinfo->set_visited(true);
    }
  }

  FmtAssert(for_len < back_len, ("<CFLOW_layout> for_len must be less than back_len"));
  int len_bbs, i;
  for (len_bbs=for_len, i=back_len+1; i < num_branchinfo; len_bbs++, i++) {
    bbs_array[len_bbs] = bbs_array[i];
  }

  *alen = len_bbs;
}
  
/*
   resolve_branches() inserts jump-BB for every branch that is
   out of range because of merging chain1 and chain2. 
*/
void BB_LAYOUT::resolve_branches
(
  CHAIN  *chain1,
  CHAIN  *chain2,
  float  *benefit      // if not null, resolve_branches() just calculates the benefit
                       // without actually inserting jump_bb
)
{
  int len_bbs;
  // memset(bbs_affected, 0, sizeof(int) * num_branchinfo);
  get_list_of_sorted_branches(chain1, chain2, bbs_affected, &len_bbs);
  
  /* Now, since we need to handle all target before handling fall-thru,
     resolve target first.
  */
  BRANCH_INFO *pbinfo;
  for (int i=0; i < len_bbs; i++) { 
    pbinfo = branchinfo + bbs_affected[i];
    resolve_target(chain1, chain2, pbinfo, benefit);
  }

  /* Inserting jump_bb does not cause target to be out of range, but
     it may cause fall-thru to be out of range. Here, we resolve fall-thrus
     that are out of range.
   */
  for (int i=0; i < num_branchinfo; i++) 
  {
    pbinfo = branchinfo + i;
    if ( !pbinfo->is_valid_entry() || 
         (pbinfo->get_nsuccs() < 1) ||
         !pbinfo->has_fall_thru()  ||
         pbinfo->is_succ_resolved(FALL_THRU_IX) ) {
      continue;
    }

    resolve_fall_thru(chain1, chain2, pbinfo, benefit);
  }

  /* Process unnecessary jump */
  BB *bb_tail = chain1->tail;
  BB *bb_head = chain2->head;
  if (BB_for_layout(bb_tail) || BB_for_layout(bb_head)) {
     return;
  }

  pbinfo = branchinfo + BB_id(bb_tail);
  if ( (pbinfo->get_nsuccs() == 1) &&      // has a single succ
       (pbinfo->get_br_op() != NULL) &&    // has a jump instruction
       (pbinfo->get_succ_bb(0) == bb_head) // bb_tail's succ == bb_head
     )
  {
    if (benefit != NULL) {
      int delay = pbinfo->get_succ_delay(0) + 1;
      float prob = pbinfo->get_succ_prob(0);
      prob *= delay;
      if (freqs_computed || BB_freq_fb_based(bb_tail)) {
        prob *= BB_freq(bb_tail);
      }
      *benefit = *benefit + prob;
    } else {
      if ( CFLOW_Trace_Layout ) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, "<CFLOW_layout> A jump (BB%d --> BB:%d) changed to noop\n",
                       BB_id(bb_tail), BB_id(bb_head));
      }

      pbinfo->set_br_op(Mk_OP(TOP_noop));
    }
  }
}

void BB_LAYOUT::chain_append(CHAIN *pchain, CHAIN *schain)
{
  BRANCH_INFO *pbinfo;

  // Resolve branches that will be out of range due to this append
  resolve_branches (pchain, schain, NULL);

  merge_edges(pchain, schain);

  int dist = pchain->bytes_size;
  pchain->bytes_size = pchain->bytes_size + schain->bytes_size;
  if (schain->tail) {

    FmtAssert (schain->head != NULL, ("chain's head shouldn't be NULL\n"));

    BB_prev(schain->head) = pchain->tail;
    pchain->tail->next = schain->head;
    pchain->tail = schain->tail;
    for (BB *bb = schain->head; bb != NULL; bb = BB_next(bb)) {
      int ix = BB_id(bb);
      if (ix < num_branchinfo) {
        int ofst = branchinfo[ix].get_offset() + dist;
        branchinfo[ix].set_offset(ofst);
      }
      BB_MAP_Set(chain_map, bb, pchain);
    }
  }

  if (schain->has_tail_jump_bb()) {
    pchain->set_tail_jump_bb();
  } else {
    pchain->reset_tail_jump_bb();
  }
}

/*
  Select the best chain on the list started from chains, to append
  to pchain.
*/
CHAIN *BB_LAYOUT::select_chain (CHAIN *chains, CHAIN *pchain)
{
  CHAIN *s;
  for (s = chains; s; s = s->next) {
    s->save_flags();
  }
  pchain->save_flags();

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, 
      "<CFLOW_layout> Selecting candidate chain to append to CHAIN:%d ......\n",
      pchain->chain_id);
  }

  float benefit_max, benefit;
  CHAIN *chain, *candidate = NULL;
  bool is_first = true;
  for (chain = chains; chain; chain = chain->next) {
    if (can_combine(pchain, chain)) {
      // Need to clear jbb_after first before calling 
      // calculate_benefit()
      jbb_after = BB_SET_ClearD(jbb_after);
      benefit = calculate_benefit (pchain, chain);
      if (is_first) {
        benefit_max = benefit;
        candidate = chain;
        is_first = false;
      } else if (benefit_max < benefit) {
        benefit_max = benefit;
        candidate = chain;
      }

      for (s = chains; s; s = s->next) {
        s->restore_flags();
      }
      pchain->restore_flags();

      if (CFLOW_Trace_Layout) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, 
          "<CFLOW_layout>    CHAIN:%d (benefit=%5.2f)\n",
          chain->chain_id, benefit);
      }
    } else {
      if (CFLOW_Trace_Layout) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, 
          "<CFLOW_layout>    CHAIN:%d cannot be combined into Chain 0\n",
          chain->chain_id);
      }
    }
  }

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, 
      "<CFLOW_layout> CHAIN:%d (benefit=%5.2f) selected\n",
      candidate->chain_id, benefit_max);
  }

  FmtAssert(candidate != NULL, ("candidate must not be NULL"));
  return candidate;
}
        
void BB_LAYOUT::combine_chains()
{
/*
  1. remove entry_chain from bbchains;

  2. 
     assume: 
        unit cost:  
          still pending branch :  1
          out of range:           j + j's delay (3 or 4)
          within range:           0  (benefit)
          using variant's         1.5/2 (perf: always 1, -Os, 1.5)

       cost = SUM of all ( freq * (unit cost))
        
          
     min = MAX_INT;
     for each chain c in bbchains do
       t = min cost of merging entry_chain with c;
       if ( t < min ) {
         candidate = c;
         min = t;
       }
     }

     remove candidate from bbchains;
     merge entry_chain with candidate;

     if bbchains is not empty, goto 2; otherwise 3

  3. done
*/
  bool dead_lock;
  CHAIN_BB_EDGE *current_loop_edge;
  CHAIN *entry_chain = bbchains;
  CHAIN *chain, *next;

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "%s\n", DBar);
    fprintf(TFile, "<CFLOW_layout> Start of combine_chains()\n");
    fprintf(TFile, "%s\n", DBar);
  }

  while (bbchains->next != NULL) 
  {
    dead_lock = true;

#if 1

    chain = select_chain (bbchains->next, entry_chain);
    if (chain != NULL) {
      chain_append(entry_chain, chain);

      CHAIN *prev = chain->prev;
      CHAIN *next = chain->next;

      // entry_chain is the first, so prev cannot be NULL
      Is_True (prev != NULL, ("prev cannot be NULL"));

      prev->next = next;
      if (next != NULL) {
        next->prev = prev;
      }

      dead_lock = false;
    }

#else    

    /* Just pick the next chain in the list and combine */

    for (chain = bbchains->next; chain; chain = next) {
      next = chain->next;

      if (can_combine (entry_chain, chain)) {
        chain_append(entry_chain, chain);

        CHAIN *prev = chain->prev;
        Is_True(prev, ("prev must not be NULL"));
        prev->next = next;
        if (next != NULL) {
          next->prev = prev;
        }

        dead_lock = false;
      }
    }

#endif

    Is_True(!dead_lock, ("Dead lock happends, something wrong !"));
  }

  entry_chain->next = NULL;

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "%s\n", DBar);
    fprintf(TFile, "<CFLOW_layout> End of combine_chains()\n");
    fprintf(TFile, "%s\n", DBar);
  }
}

void BB_LAYOUT::swap_branches()
{
  int i;
  BRANCH_INFO *pbinfo = branchinfo;
  for (int i=0; i < num_branchinfo; i++) {
    pbinfo = branchinfo + i;
    if ( !pbinfo->is_valid_entry() ||
         !pbinfo->is_cond_jump() ) {
      continue;
    }

    BB *bb = pbinfo->get_br_bb();
    BB *fall_thru_bb = pbinfo->get_succ_fall_thru();
    BB *target_bb = pbinfo->get_succ_target();
    if ( BB_for_layout(fall_thru_bb) && !BB_for_layout(target_bb) &&
         (pbinfo->get_succ_prob(FALL_THRU_IX) > pbinfo->get_succ_prob(TARGET_IX)) ) {
      OP *br_op = pbinfo->get_br_op();
      TOP invert_top = get_invert_branch_top (br_op);
      if (invert_top == TOP_UNDEFINED) {
        continue;
      }

      // check if the min dist of fall_thru is within the range
      // of the target.....
      BB *real_fall_thru_bb = BB_First_Succ(fall_thru_bb);
      if (real_fall_thru_bb == NULL) {
        continue;
      }
      BRANCH_INFO *pbinfo1 = branchinfo + BB_id(real_fall_thru_bb);
      int dist = get_bb_offset(fall_thru_bb);
      bool is_forward;
      if (dist <= pbinfo1->get_offset()) {
        // forward
        dist = pbinfo1->get_offset() - dist;
        is_forward = true;
      } else {
        // backward
        dist = dist - pbinfo1->get_offset();
        is_forward = false;
      }

      TOP orig_top = OP_code(br_op);
      Set_OP_code(br_op, invert_top);
      int orig_dist = pbinfo->get_min_distance(TARGET_IX, is_forward);
      if (!pbinfo->is_distance_within_range(TARGET_IX, (dist - orig_dist), is_forward)) {
        Set_OP_code(br_op, orig_top);
        continue;
      }
      pbinfo->set_min_distance(TARGET_IX, dist, is_forward);
      pbinfo->set_min_distance(TARGET_IX, -1, !is_forward);
      if (is_forward) {
        pbinfo->set_succ_forward(TARGET_IX);
      } else {
        pbinfo->set_succ_backward(TARGET_IX);
      }
      float p = pbinfo->get_succ_prob(FALL_THRU_IX);
      pbinfo->set_succ_prob(FALL_THRU_IX, pbinfo->get_succ_prob(TARGET_IX));
      pbinfo->set_succ_prob(TARGET_IX, p);
      pbinfo->set_succ_bb(TARGET_IX, real_fall_thru_bb);

      // fall_thru_bb to target_bb
      OP_Retarget_Branch (BB_last_op(fall_thru_bb), real_fall_thru_bb, target_bb);
      Unlink_Pred_Succ (fall_thru_bb, real_fall_thru_bb);
      Link_Pred_Succ_with_Prob(fall_thru_bb, target_bb, 1.0F);

      OP_Retarget_Branch (br_op, target_bb, real_fall_thru_bb);

      if ( CFLOW_Trace_Layout ) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile, "<CFLOW_layout> the branch of  BB:%d swapped !\n", BB_id(bb));
      }
    }
  }
}

void BB_LAYOUT::adjust_branches()
{
  swap_branches();
}

static bool OP_equivalent (OP *op1, OP *op2);

void BB_LAYOUT::finalize()
{
  BB *bb, *prev;
  BRANCH_INFO *pbinfo;
  for (prev = NULL, bb = bbchains->head; bb; prev = bb, bb = BB_next(bb)) { 
    Is_True (BB_prev(bb) == prev, 
             ("BB_prev(BB:%d) isn't BB:%d",
                   BB_id(bb), 
                   (prev == NULL) ? -1 : BB_id(prev)));
    if (BB_for_layout(bb)) {
      /*
         A jump BB added by layout. Its successor was set up
         when it was created.
      */
      continue;
    }

    // 1. Unlink the previous pred/succ
    BBLIST *slist, *next=NULL;
    for (slist = BB_succs(bb); slist; slist = next) { 
      next = BBLIST_next(slist);
      BB *succ = BBLIST_item(slist);
      Unlink_Pred_Succ (bb, succ);
    }

    // 2. Link the new pred/succ
    pbinfo = branchinfo + BB_id(bb);
    for (int i=0; i < pbinfo->get_nsuccs(); i++) {
      BB *succ = pbinfo->get_succ_bb(i);
      float prob = pbinfo->get_succ_prob(i);
      Link_Pred_Succ_with_Prob (bb, succ, prob);
    }

    // 3. Use the new branch op
    if ( pbinfo->get_br_op() ) {
      if (pbinfo->get_orig_br_op()) {
        BB_Remove_Op(bb, pbinfo->get_orig_br_op());
      }
      if (OP_code(pbinfo->get_br_op()) != TOP_noop) {
        BB_Append_Op(bb, pbinfo->get_br_op());
      }
    }
  }
  
  REGION_First_BB = bbchains->head;
}

void BB_LAYOUT::consistency_verify()
{
  // Do verify if CFLOW_Enable_Layout is 2 or up
  if (CFLOW_Enable_Layout < 2) { 
    return;
  }

  BRANCH_INFO *pbinfo;
  CHAIN *chain;
  BB    *bb;

  // reset flag1
  for (chain = bbchains; chain != NULL; chain = chain->next) {
    for (bb = chain->head; bb != NULL; bb = bb->next) {
      Reset_BB_local_flag1(bb);
    }
  }

  // Verify branchinfo
  for (chain = bbchains; chain != NULL; chain = chain->next)
  {
    int size_in_bytes = 0;
    for (bb = chain->head; bb != NULL; bb = bb->next)
    {
      Set_BB_local_flag1(bb);

      if (BB_id(bb) >= num_branchinfo) {
        FmtAssert(BB_for_layout(bb), ("BB:%d should be jump BB", BB_id(bb)));
        continue;
      }

      pbinfo = branchinfo + BB_id(bb);

      FmtAssert(pbinfo->is_valid_entry(), ("BB:%d shouldn't be invalid", BB_id(bb)));
 
      OP *br_op = pbinfo->get_br_op();
      if ( (br_op != NULL) &&  OP_ijump(br_op)) {
        continue;
      }

      int nsuccs = pbinfo->get_nsuccs();
      if (pbinfo->get_offset() != size_in_bytes) {
#if 0
        DevWarn("branchinfo[BB:%d]'s offset is %d (should be %d)",
                BB_id(bb), pbinfo->get_offset(), size_in_bytes);
#endif
        FmtAssert(FALSE, ("branchinfo[BB:%d]'s offset is %d (should be %d)",
                BB_id(bb), pbinfo->get_offset(), size_in_bytes));
      }

      if (pbinfo->is_cond_jump()) {
        FmtAssert(nsuccs == 2,
                  ("branchinfo[BB:%d] should have two succs", BB_id(bb)));
      }

      size_in_bytes += bb_size[BB_id(bb)];
      for (int j=0; j < nsuccs; j++) {
        if (pbinfo->is_succ_resolved(j)) {
          BB *succ_bb = pbinfo->get_succ_bb(j);
          if (pbinfo->is_succ_backward(j)) {
            FmtAssert(BB_local_flag1(succ_bb), 
                      ("The succ BB:%d of BB:%d isn't backward", 
                       BB_id(succ_bb), BB_id(bb)));

          } else {
            FmtAssert(!BB_local_flag1(succ_bb), 
                      ("The succ BB:%d of BB:%d isn't forward", 
                       BB_id(succ_bb), BB_id(bb)));
          }

          // Verify min distances
          int d;
          if (pbinfo->is_succ_backward(j)) {
            d = size_in_bytes - get_bb_offset(succ_bb);
            if ( d != pbinfo->get_min_distance(j, false) ) {
#if 0
              DevWarn("Backward min_distance of BB:%d to BB:%d is %d (should be %d)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, false), d);
#endif
              FmtAssert(FALSE, ("Backward min_distance of BB:%d to BB:%d is %d (should be %d)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, false), d));
            }
            if (pbinfo->get_min_distance(j, true) != -1) {
#if 0
              DevWarn("forward min_distance of BB:%d to BB:%d is %d (should be -1)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, true));
#endif
              FmtAssert(FALSE, ("forward min_distance of BB:%d to BB:%d is %d (should be -1)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, true)));
            }
          } else {
            d = get_bb_offset(succ_bb) - size_in_bytes;
            if ( d != pbinfo->get_min_distance(j, true) ) {
#if 0
              DevWarn("forward min_distance of BB:%d to BB:%d is %d (should be %d)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, true), d);
#endif
              FmtAssert(FALSE, ("forward min_distance of BB:%d to BB:%d is %d (should be %d)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, true), d));
            }
            if (pbinfo->get_min_distance(j, false) != -1) {
#if 0
              DevWarn("backward min_distance of BB:%d to BB:%d is %d (should be -1)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, false));
#endif
              FmtAssert(FALSE, ("backward min_distance of BB:%d to BB:%d is %d (should be -1)",
                      BB_id(bb), BB_id(succ_bb),  pbinfo->get_min_distance(j, false)));
            }
          }
        }
      }
    }
  }
}

int BB_LAYOUT::get_bb_offset(BB *b)
{
  if (!BB_for_layout(b)) {
    return branchinfo[BB_id(b)].get_offset();
  } else {
    BB *tmp_bb = b;
    // offset of the next non-jump BB is the offset
    // for the current one.
    while ( tmp_bb && BB_for_layout(tmp_bb) ) {
      tmp_bb = BB_next(tmp_bb);
    }
    if (tmp_bb) {
       return branchinfo[BB_id(tmp_bb)].get_offset();
    } else {
      tmp_bb = b;
      while ( tmp_bb && BB_for_layout(tmp_bb) ) {
        tmp_bb = BB_prev(tmp_bb);
      }
      if (tmp_bb) {
        return branchinfo[BB_id(tmp_bb)].get_offset() + bb_size[BB_id(tmp_bb)];
      }
    }
  }

  // No info, just return 0
  return 0;
}

BB *BB_LAYOUT::find_best_location
(
  CHAIN *chain1, 
  CHAIN *chain2, 
  BRANCH_INFO *pbinfo,
  int target_ix,
  bool *is_forward,
  float *benefit
)
{
  BB *bb = pbinfo->get_br_bb();
  BB *bb_succ = pbinfo->get_succ_bb(target_ix);
  CHAIN *pchain = get_bb_chain(bb);
  CHAIN *schain = get_bb_chain(bb_succ);

  Is_True( pchain != schain, 
           ("BB:%d and BB:%d should not be in the same CHAIN") );

  // TODO: what if bb is no_relax bb
  float cst = get_indirect_relaxation_cost(pbinfo);
  float min_cost = get_direct_relaxation_cost(pbinfo);
  BB *min_bb = bb;
  if (pbinfo->is_no_relax()) {
    min_bb = NULL;
    *is_forward = false;
  } else {
    *is_forward = true;
  }

  bool enable_location_trace = (CFLOW_Enable_Layout >= 3) && CFLOW_Trace_Layout &&
                               (benefit != NULL);

  if (enable_location_trace) {
    #pragma mips_frequency_hint NEVER
    OP *br_op = pbinfo->get_br_op();
    FmtAssert(br_op != NULL, ("BB:%d must have branch", BB_id(bb)));
    fprintf(TFile,
            "<CFLOW_layout>       find_best_location() for OP:%s  BB:%d --> BB:%d\n",
            TI_TOP_Name(OP_code(br_op)), BB_id(bb), BB_id(bb_succ));
    if (min_bb != NULL) {
      fprintf(TFile, "<CFLOW_layout>         (initial direct relaxtion cost=%f)\n", min_cost);
    }
  }

  /*
     Since we may need to traverse three chains: pchain, schain,
     and either chain1 or chain2, we use iterator[] to avoid 
     duplicate code. 
  */
  struct {
    CHAIN *sc;
    BB    *start_bb;
    int   offset;
  } iterator[3];

  int t_offset;
  BB *b;
  if (!pbinfo->ignore_min_distance(target_ix, false))
  {
    // backward

    t_offset = -pchain->bytes_size; 
    iterator[0].sc       = pchain;
    iterator[0].start_bb = BB_prev(bb);
    iterator[0].offset   = t_offset;
    t_offset += pchain->bytes_size;
    bool encounter_entry = pchain->is_entry_chain();
    if ((schain == chain1) && (pchain != chain2)) {
      iterator[1].sc       = chain2;
      iterator[1].start_bb = encounter_entry ? NULL : chain2->tail;
      iterator[1].offset   = t_offset;

      t_offset += chain2->bytes_size;
      encounter_entry = encounter_entry  || chain2->is_entry_chain();
    } else {
      // invalid
      iterator[1].start_bb = NULL;
    }
    iterator[2].sc       = schain;
    iterator[2].start_bb = encounter_entry ? NULL : schain->tail;
    iterator[2].offset   = t_offset;
      
    for (int i=0; i < 2; i++) {
      if (iterator[i].start_bb == NULL) {
        continue;
      }
      for ( b=iterator[i].start_bb; 
            b && (b != bb_succ); 
            b = BB_prev(b))
      {
        if (BB_for_layout(b)) {
          *is_forward = false;

          if (enable_location_trace) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, 
              "<CFLOW_layout>         BB:%d no fall-thru succ, selected\n", BB_id(b));
          }
          return b;
        }
        BRANCH_INFO *pi = branchinfo + BB_id(b);
        if (pi->is_no_relax()) {
          continue;
        }
        if ( (pi->get_nsuccs() <= 0) || benefit && BB_SET_MemberP(jbb_after, b) )
        {
          *is_forward = false;

          if (enable_location_trace) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, 
              "<CFLOW_layout>         BB:%d no fall-thru succ, selected\n", BB_id(b));
          }
          return b;
        }

        int o = iterator[i].sc->bytes_size - pi->get_offset();
        o += iterator[i].offset;
        if (pbinfo->chain_distance_okay(pchain, target_ix, o, false)) {
          float f = get_direct_relaxation_cost(pi);
          f += cst;

          if ((min_bb == NULL) || (min_cost > f)) {
            *is_forward = false;
            min_cost = f;
            min_bb = b;

            if (enable_location_trace) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,
                "<CFLOW_layout>         BB:%d cost=%f, selected\n", BB_id(b), f);
            }
          } else {
            if (enable_location_trace) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile, 
                "<CFLOW_layout>         BB:%d cost=%f, not selected\n", BB_id(b), f);
            }
          }
        } else {
          return min_bb;
        }
      }
    }
  }

  if (!pbinfo->ignore_min_distance(target_ix, true))
  {
    // forward 

    t_offset = -pchain->bytes_size;
    iterator[0].sc       = pchain;
    iterator[0].start_bb = BB_next(bb);
    iterator[0].offset   = t_offset;
    t_offset += pchain->bytes_size;
    bool encounter_entry = false;
    if ((schain == chain2) && (pchain != chain1)) {
      encounter_entry = chain1->is_entry_chain();

      iterator[1].sc       = chain1;
      iterator[1].start_bb = encounter_entry ? NULL : chain1->head;
      iterator[1].offset   = t_offset;

      t_offset += chain1->bytes_size;
    } else {
      // invalid
      iterator[1].start_bb = NULL;
    }
    encounter_entry = encounter_entry || schain->is_entry_chain();
    iterator[2].sc       = schain;
    iterator[2].start_bb = encounter_entry ? NULL : schain->head;
    iterator[2].offset   = t_offset;
      
    for (int i=0; i < 2; i++) {
      if (iterator[i].start_bb == NULL) {
        continue;
      }
      for ( BB *b=iterator[i].start_bb; 
            b && (b != bb_succ);
            b = BB_next(b))
      {
        if (BB_for_layout(b)) {
          *is_forward = true;

          if (enable_location_trace) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, 
              "<CFLOW_layout>         BB:%d no fall-thru succ, selected\n", BB_id(b));
          }
          return b;
        }

        BRANCH_INFO *pi = branchinfo + BB_id(b);
        if (pi->is_no_relax()) {
          continue;
        }
        if ( (pi->get_nsuccs() <= 0) || benefit && BB_SET_MemberP(jbb_after, b) )
        {
          *is_forward = true;

          if (enable_location_trace) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, 
              "<CFLOW_layout>         BB:%d no fall-thru succ, selected\n", BB_id(b));
          }

          return b;
        }

        int o = pi->get_offset() + bb_size[BB_id(b)];
        o += iterator[i].offset;
        if (pbinfo->chain_distance_okay(pchain, target_ix, o, true)) {
          float f = get_direct_relaxation_cost(pi);
          f += cst;
          if ((min_bb == NULL) || (min_cost > f)) {
            *is_forward = true;
            min_cost = f;
            min_bb = b;

            if (enable_location_trace) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,
                "<CFLOW_layout>         BB:%d cost=%f, selected\n", BB_id(b), f);
            }
          } else {
            if (enable_location_trace) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile, 
                "<CFLOW_layout>         BB:%d cost=%f, not selected\n", BB_id(b), f);
            }
          }
        } else {
          return min_bb;
        }
      }
    }
  }

  return min_bb;
}

    
BOOL Freq_BB_LAYOUT(void)
{
  BRANCH_INFO::pool = &MEM_local_nz_pool;

  BRANCH_INST_TABLE *a_btable = 
      CXX_NEW(BRANCH_INST_TABLE(&MEM_local_nz_pool), &MEM_local_nz_pool);

  BRANCH_INFO::btable = a_btable;
  bblayout_obj = CXX_NEW(BB_LAYOUT(&MEM_local_nz_pool), &MEM_local_nz_pool);

  freqs_computed = FREQ_Frequencies_Computed();

  // 1.  Set up edges
  bblayout_obj->init_edges();
  
  // 2. Create initial chains & set up branch info
  bblayout_obj->init_chains();
  bblayout_obj->init_branchinfo();

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%s"
                   " <CFLOW_layout> -- All chains: after Chain initialization\n"
                   " PU: \"%s\"\n"
                   "%s\n",
                   DBar, Cur_PU_Name, DBar);
    bblayout_obj->print_chains (true);
  }


  // 3. Grow Chains
  bblayout_obj->grow_chains();

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%s"
                   " <CFLOW_layout> -- All chains: after grow_chain()\n"
                   " PU: \"%s\"\n"
                   "%s\n",
                   DBar, Cur_PU_Name, DBar);
    bblayout_obj->print_chains (true);
  }

  // 4. combine chains into a single chain
  bblayout_obj->combine_chains();

  // 5. Branch adjustment
  bblayout_obj->adjust_branches();

  if (CFLOW_Trace_Layout) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "\n%s"
                   " <CFLOW_layout> -- All chains: after combine_chain()\n"
                   " PU: \"%s\"\n"
                   "%s\n",
                   DBar, Cur_PU_Name, DBar);
    bblayout_obj->print_chains (true);
  }

#if 1
#ifdef Is_True_On
  bblayout_obj->consistency_verify();
#endif
#endif

  // 5. finalize
  bblayout_obj->finalize();

  CXX_DELETE(a_btable, &MEM_local_nz_pool);
  CXX_DELETE(bblayout_obj, &MEM_local_nz_pool);
  bblayout_obj = NULL;

  BRANCH_INFO::pool = NULL;
  BRANCH_INFO::btable = NULL;
  return TRUE;
}

