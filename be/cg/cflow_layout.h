/*
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#ifndef cflow_layout_INCLUDED
#define cflow_layout_INCLUDED


class BB_LAYOUT;
class BRANCH_INFO;

extern BB_LAYOUT *bblayout_obj;

/*
   A branch is resolved if its target and itself has been laid out
   already, i.e. either the target is within the range of branch or
   a jump is inserted to relax the branch.

   During merging two chains, we check all branches whose distances
   are affected by this merging. A branch can be either forward or
   backward. However, merging two chains can only affect at most
   one direction of any unresolved branches. 

   AFFECTED_BRANCH_STATUS keeps track of information related to those
   affected direction of branches, such as whether it is forward or
   backward, whether it is out of range, whether it must be resolved,
   whether a jump BB is needed, and if so, where it should be, etc.
*/
typedef struct affected_branch_status {
  bool is_forward;             // forward branch ?
  bool dir_not_allowed;        // this direction is not allowed ?
  bool dir_out_of_range;       // this direction is out of range ?
  bool out_of_range;           // both forward/backward branch are out of range ?
  bool must_resolve;           // branch will be resolved after merging ?
  bool insert_jump;            // insert jump BB ?
  bool is_jbb_forward;         // if insert jump, jump is forward ?
  BB  *insert_after_bb;        // If not NULL, insert jump BB after this one.
  int  inc;                    // increase amount of min distance of this branch direction 

  affected_branch_status() :
    is_forward (false),
    dir_not_allowed (false),
    dir_out_of_range (false),
    out_of_range (false),
    must_resolve (false),
    insert_jump (false),
    is_jbb_forward (false),
    insert_after_bb (NULL),
    inc (0)
  {}
} AFFECTED_BRANCH_STATUS;


class CHAIN_BB_EDGE {
private:
  /*
    This class represents the edge from bb_src --> bb_dst, 
    where bb_src and bb_dst are in different chains.

    Each chain will have both 'in and 'out' edges. All 'in' edges
    are linked together by next_dst; and all 'out' edges are
    linked together by next_src. 
    
    Don't need to keep chain_src/chain_dst because they can
    be acquired easily from bb_src/bb_dst.
  */
  unsigned flags;
#define CHAIN_BB_EDGE_LOOP_START        0x1  // bb_src is loop
#define CHAIN_BB_EDGE_LOOP_END          0x2  // bb_src is loop_end

  BB *bb_src;
  BB *bb_dst;
  CHAIN_BB_EDGE *next_src;   // next CHAIN_BB_EDGE that is from this chain(src) to other chain
  CHAIN_BB_EDGE *next_dst;   // next CHAIN_BB_EDGE that is from other chain to this chain(dst)

public:
  CHAIN_BB_EDGE (BB *src, BB *dst) :
    bb_src (src),
    bb_dst (dst),
    next_src (0),
    next_dst (0) {
  }

  CHAIN_BB_EDGE (const CHAIN_BB_EDGE&);              // unwanted
  CHAIN_BB_EDGE& operator = (const CHAIN_BB_EDGE&); // unwanted 

    
  BB *get_bb_src() const        { return bb_src; }
  BB *get_bb_dst() const        { return bb_dst; }
  CHAIN_BB_EDGE *next_in  () const { return next_dst; }
  CHAIN_BB_EDGE *next_out () const { return next_src; }
  void set_next_in  (CHAIN_BB_EDGE *t) { next_dst = t; }
  void set_next_out (CHAIN_BB_EDGE *t) { next_src = t; }
  void reset_flags ()   	{ flags = 0; }
  void set_loop_start ()        { flags |= CHAIN_BB_EDGE_LOOP_START; }
  void set_loop_end ()          { flags |= CHAIN_BB_EDGE_LOOP_END; }
  bool is_loop_start ()         { return flags & CHAIN_BB_EDGE_LOOP_START; }
  bool is_loop_end ()           { return flags & CHAIN_BB_EDGE_LOOP_END; }
  bool is_dependence_edge ()	{ return is_loop_start() || is_loop_end(); }
}; 

/* 
  The following data structure is used to generate chains of BBs
  that fall-through to each other.
*/

/* Chain's flags  */
#define CHAIN_FLAGS_ENTRY 		0x1
#define CHAIN_FLAGS_HEAD_JUMP_BB	0x2
#define CHAIN_FLAGS_TAIL_JUMP_BB	0x4
class CHAIN {
public:
  int      chain_id;
  unsigned flags;     
  double   weight;      /* we use this for ordering the chains */
  int      bytes_size;  /* size in bytes of this chain */
  BB       *head;       /* first BB of the chain */
  BB       *tail;       /* last BB of the chain */
  CHAIN_BB_EDGE *in_edges;
  CHAIN_BB_EDGE *out_edges;
  CHAIN *next;          /* next chain */
  CHAIN *prev;          /* previous chain */

  // for temporary use
  unsigned saved_flags;

  bool is_entry_chain() const  { return flags & CHAIN_FLAGS_ENTRY; }
  void set_entry_chain()       { flags |= CHAIN_FLAGS_ENTRY; }
  void reset_entry_chain()     { flags &= ~CHAIN_FLAGS_ENTRY; }
  bool has_head_jump_bb() const { return flags & CHAIN_FLAGS_HEAD_JUMP_BB; }
  void set_head_jump_bb()       { flags |= CHAIN_FLAGS_HEAD_JUMP_BB; }
  void reset_head_jump_bb()     { flags &= ~CHAIN_FLAGS_HEAD_JUMP_BB; }
  bool has_tail_jump_bb() const { return flags & CHAIN_FLAGS_TAIL_JUMP_BB; }
  void set_tail_jump_bb()       { flags |= CHAIN_FLAGS_TAIL_JUMP_BB; }
  void reset_tail_jump_bb()     { flags &= ~CHAIN_FLAGS_TAIL_JUMP_BB; }

  void save_flags() 	        { saved_flags = flags; }
  void restore_flags() 	        { flags = saved_flags; }


  CHAIN_BB_EDGE *find_out_edge (BB *src, BB *dst);

  void print_edges(bool print_in_edges);
  void print (bool =false);

  void print_edges() {
    print_edges(true);
    print_edges(false);
  }
};


/*
   The class BRANCH_INST and class BRANCH_INST_TABLE keeps all
   properties of all branch instructions.
*/
#define MAX_VARIANTS 4 

class BRANCH_INST {

private:
    /*
       tops[] keeps all TOPs that have the same instruction format 
       and semantics except that their target labels has different
       range. For example, bnez and bnez.n has the same instruction
       format except bnez's target range is 12-bit and bnez.n is 6-bit. 

       All core cond branches are in tops[0]. So, bnez will be
       in tops[0], and bnez.n will be tops[1], etc...
     */
    int num_variants;
    TOP tops[MAX_VARIANTS];
    struct {
      int min;
      int max;
    } ranges[MAX_VARIANTS];

public:
    BRANCH_INST() { num_variants = 0; }

    int get_num_variants() { return num_variants; }
    TOP get_top(int ix)    { return tops[ix]; }

    void add_variant_TOP (TOP t);
    bool get_min_max (TOP top, int *l, int *h);
};


class BRANCH_INST_TABLE {
private:
  int *tops_ix;               // index to branch_table[]
  int table_size;
  BRANCH_INST *branch_table;  // branch table

  MEM_POOL *const _pool;

  void init_table();

  int taken_delay;  // all branches have the same branch-taken delay

public:

  BRANCH_INST_TABLE (MEM_POOL *a_pool);
  ~BRANCH_INST_TABLE();

  int get_branch_range_min(TOP top);
  int get_branch_range_max(TOP top);
  int get_branch_taken_delay () const  { return taken_delay; }

  BRANCH_INST *get_top_inst(TOP t) {
    int ix = tops_ix[t];
    return (branch_table + ix);
  }
};
  

/*
   class BRANCH_INFO keeps specific information on each branch instruction
   in the current program unit.

   One instance of this class is created for each BB, and that instance keeps
   branching info for this BB throughout the whole process of the layout.
   When laying out BBs, this specific branching info may need updating. But
   updating is only limited within BRANCH_INFO, for example: if the branch
   inst is inverted, its BRANCH_INFO will be updated to record this change
   and no change to the BB is made until doing final layout (finalize()).
   
   An array of BRANCH_INFO is created at the time BB_LAYOUT instance is
   created. So, later When creating a new BB, there is no entry in the array
   for this new BB.  any control-flow change to this BB goes to the
   real BB (rather than in BRANCH_INFO) by setting its BB_succ(). (Its
   BB_pred() will be set up when its pred is set up at finalize().)
*/
class BRANCH_INFO {
  private:
    
    bool valid_entry;   // Since we create an array of BRANCH_INFO whose size is
                        // larger than the number of BBs, some entries will be invalid.

    /*
       This entry is for br_bb/br_op. The final layout will use this info to
       set up BB_pred/BB_succ accordingly.
     */
    BB  *br_bb;
    OP  *br_op;
    OP  *orig_br_op;

    bool cond_jump;      // true: conditional jump; false: jump
    bool layout_done;    // If both fall-thru & target have been laid out in a chain.
                         // or if br is a jx; set this to true.
    bool no_relax;       // false: a jump bb cannot be inserted after this BB.
                         // true:  a jump bb can be inserted after this BB.

    /* 
       succs[] keeps all succs of br_bb, succs[0] will be fall-thru BB if the BB
       have one (as for conditional branch, etc.). If orig_br_op is ijump (jx), 
       the order of succs[] probably does not matter !
    */
#define FALL_THRU_IX	0
#define TARGET_IX	1
#define FLAGS_RESOLVED_BACKWARD     0x1		// 1: backward; 0: forward
#define FLAGS_SUCC_RESOLVED         0x2		// 1: branch resolved
#define FLAGS_SUCC_FOR_OUTOFRANGE   0x4		// 1: forward branch has been out of range 
#define FLAGS_SUCC_BACK_OUTOFRANGE  0x8		// 1: backward branch has been out of range 
#define FLAGS_SUCC_OUTOFRANGE       (0x4 | 0x8)	

/*
   If branch is out of range originally, we will ignore min distance
   for this branch.
*/
#define FLAGS_SUCC_IGNORE_FOR_DIST	0x10	// ignore forward min distance
#define FLAGS_SUCC_IGNORE_BACK_DIST	0x20	// ignore backward min distance
    int nsuccs;
    BBLIST *succs;

    bool visited;
   
    /*
       Distance's array index ---  0: fall-thru, 1: target
       All branches needs only target's distance except tail, which needs both.
       Note that all distances (either backward or forward) are positive. When
       compares with a branch's range, backward distances need to be negated.

       A distance may be out of range either by chaining a sequence of BBs between
       the source BB and target BB or in the original chain. If a branch is out of
       range in the original chain (initial chain), there is no need to consider
       this branch. And the flags FLAGS_SUCC_IGNORE_FOR/BACK_DIST are used for
       this purpose. 
     */
    int forward_min_distance[2];
    int backward_min_distance[2];
    int offset;    // offset from chain's first BB to this branch

  public:

    static MEM_POOL *pool;
    static BRANCH_INST_TABLE *btable;

    float relax_cost;   // the minimal cost if it's out-of-range, but not resolved yet. 
    float relax_cost1;  // cost for direct relaxation
    float relax_cost2;  // cost for indirect relaxation


    BRANCH_INFO();
    ~BRANCH_INFO ();

    BRANCH_INFO& operator= (const BRANCH_INFO &rhs);

    BRANCH_INST *get_top_branch_inst(TOP t) {
      return btable->get_top_inst(t);
    }

    int get_range_max() {
      TOP t = br_op ? OP_code(br_op) : TOP_j;
      return btable->get_branch_range_max(t);
    }
    int get_range_max(TOP t) {
      return btable->get_branch_range_max(t);
    }
    int get_range_min() {
      TOP t = br_op ? OP_code(br_op) : TOP_j;
      return btable->get_branch_range_min(t);
    }
    int get_range_min(TOP t) {
      return btable->get_branch_range_min(t);
    }

    int get_min_distance (int ix, bool is_forward)
    {
      int *p = is_forward ? forward_min_distance : backward_min_distance;
      return p[ix];
    }

    void set_min_distance (int ix, int val, bool is_forward)
    {
      int *p = is_forward ? forward_min_distance : backward_min_distance;
      p[ix] = val;
    }

    void inc_min_distance (int ix, int val, bool is_forward)
    {
      int *p = is_forward ? forward_min_distance : backward_min_distance;
      if (p[ix] >= 0) {
        p[ix] += val;
      }
    }

    bool has_fall_thru () const   { 
      return ((nsuccs > 0) && ((br_op == NULL) || (OP_code(br_op) == TOP_noop) ||
              (OP_cond(br_op)))); 
    }
      
    void set_valid_entry()	  { valid_entry = true; }
    bool  is_valid_entry() 	  { return valid_entry; }
    void set_succs (BBLIST *s)    { succs = s; }
    int  get_nsuccs() const       { return nsuccs; }
    void set_nsuccs(int d)        { nsuccs = d;    }
    BB   *get_succ_bb(int ix)           { return succs[ix].item; }
    void set_succ_bb(int ix, BB *s)     { succs[ix].item = s;  }
    float get_succ_prob(int ix)         { return succs[ix].prob; }
    int  get_succ_flags(int ix) const   { return succs[ix].flags; }
    void set_succ_prob(int ix, float s) { succs[ix].prob = s;  }
    void set_succ_flags(int ix, int s)  { succs[ix].flags = s;  }
    void set_succ_resolved (int ix)	{ succs[ix].flags |= FLAGS_SUCC_RESOLVED; }
    bool is_succ_resolved (int ix)      { return succs[ix].flags & FLAGS_SUCC_RESOLVED; }
    void set_succ_backward (int ix)	{ succs[ix].flags |= FLAGS_RESOLVED_BACKWARD; }
    void set_succ_forward (int ix)	{ succs[ix].flags &= ~FLAGS_RESOLVED_BACKWARD; }
    bool is_succ_backward (int ix)      { return succs[ix].flags & FLAGS_RESOLVED_BACKWARD; }
    void set_succ_outofrange (int ix, bool is_forward) {
      if (ignore_min_distance(ix, is_forward)) return;
      succs[ix].flags |= 
        is_forward ? FLAGS_SUCC_FOR_OUTOFRANGE : FLAGS_SUCC_BACK_OUTOFRANGE;
    }
    bool is_succ_outofrange (int ix, bool is_forward) {
      return succs[ix].flags & 
             (is_forward ? FLAGS_SUCC_FOR_OUTOFRANGE : FLAGS_SUCC_BACK_OUTOFRANGE);
    }
    void reset_succ_outofrange (int ix, bool is_forward) {
      succs[ix].flags &= 
        (is_forward ? ~FLAGS_SUCC_FOR_OUTOFRANGE : ~FLAGS_SUCC_BACK_OUTOFRANGE);
    }
    void set_succ_outofrange (int ix)	{ succs[ix].flags |= FLAGS_SUCC_OUTOFRANGE; }
    bool is_succ_outofrange (int ix)    { return succs[ix].flags & FLAGS_SUCC_OUTOFRANGE; }
    void reset_succ_outofrange (int ix)	{ succs[ix].flags &= ~FLAGS_SUCC_OUTOFRANGE; }

    void set_ignore_min_distance(int ix, bool is_forward) {
      if (is_forward) {
        succs[ix].flags |= FLAGS_SUCC_IGNORE_FOR_DIST;
      } else {
        succs[ix].flags |= FLAGS_SUCC_IGNORE_BACK_DIST;
      }
      reset_succ_outofrange(ix, is_forward);
    }
    bool ignore_min_distance (int ix, bool is_forward) {
      if (is_forward) {
        return (succs[ix].flags & FLAGS_SUCC_IGNORE_FOR_DIST);
      } else {
        return (succs[ix].flags & FLAGS_SUCC_IGNORE_BACK_DIST);
      }
    }
    void set_offset (int o)		{ offset = o; }
    int  get_offset ()			{ return offset; }

    
    void set_br_bb(BB *b)         { br_bb = b; }
    BB   *get_br_bb() const       { return br_bb; }
    void set_br_op (OP *o)        { br_op = o; }
    OP   *get_br_op () const      { return br_op; }
    void set_orig_br_op (OP *o)   { orig_br_op = o; }
    OP   *get_orig_br_op () const { return orig_br_op; }
    bool is_cond_jump() const     { return cond_jump; }
    void set_cond_jump(bool b)    { cond_jump = b; }
    bool is_layout_done() const   { return layout_done; }
    void set_layout_done(bool b)  { layout_done = b; }
    bool is_no_relax() const      { return no_relax; }
    void set_no_relax(bool b)     { no_relax = b; }
    
    BB *get_succ_fall_thru(void)  {
      return has_fall_thru() ? get_succ_bb(FALL_THRU_IX) : NULL;
    }
    BB *get_succ_target(void)     {
      if (cond_jump) {
        return get_succ_bb(TARGET_IX);
      } else {
        return has_fall_thru() ? NULL : get_succ_bb(0);
      }
    }

    int  get_succ_index(BB *a_succ);
    void swap_succs(int succ0, int succ1);
    bool is_distance_within_range(int ix, int inc, bool is_forward);
    int  get_succ_delay(int ix);
    TOP  select_bigger_variant_top(int ix, int inc, bool is_forward);

    bool chain_distance_okay(CHAIN *theChain, int ix, int inc, bool is_forward);
    bool is_distance_okay(int ix, int inc, bool is_forward) {
      return (ignore_min_distance(ix, is_forward) ||
              is_distance_within_range(ix, inc, is_forward));
    }

    void set_visited(bool _b)	  { visited = _b; }
    bool is_visited()	const	  { return visited; }

};


class BB_LAYOUT {

private:
   MEM_POOL    *const pool;

   BRANCH_INFO *branchinfo;
   BRANCH_INFO *saved_branchinfo;
   int          num_branchinfo;
   CHAIN       *bbchains;
   int          num_bbchains;
   int          len_edges;
   EDGE        *edges;     // all edges b/w BBs
   BB_MAP       chain_map;
   int         *bb_size;   // BB s's size in bytes

   // temporary use of bitset 
   BS  *noafter_chains;                  
   BS  *nobefore_chains;                  
   int *bbs_affected;
   BB_SET *jbb_after;   // record BBs whose next is a jump BB

   bool check_update_edges_distances(CHAIN *chain1, CHAIN *chain2, bool do_update);
   bool check_min_distances(CHAIN *pchain, CHAIN *schain) {
     return check_update_edges_distances(pchain, schain, false);
   }
   void update_min_distances(CHAIN *pchain, CHAIN *schain) {
     check_update_edges_distances(pchain, schain, true);
   }

   void merge_edges(CHAIN *pchain, CHAIN *schain);
   void merge_chains (BB *pred, BB *succ);

   CHAIN *select_chain(CHAIN *chains, CHAIN *pchain);
   bool check_edges_dependences(CHAIN *, CHAIN *);
   bool can_combine(CHAIN *c1, CHAIN *c2) {
     return check_edges_dependences (c1, c2);
   }
   float calculate_benefit (CHAIN *pchain, CHAIN *schain) {
     float benefit = 0.0f;
     resolve_branches (pchain, schain, &benefit);
     return benefit;
   }


   void save_branchinfo();
   void restore_branchinfo();

   void get_list_of_sorted_branches(CHAIN *chain1, CHAIN *chain2,
                                    int *bbs_array, int *alen);
   void check_branch_target_status (CHAIN *, CHAIN *, CHAIN *, CHAIN *,
				    BRANCH_INFO *, int, AFFECTED_BRANCH_STATUS *);
   float calculate_succ_benefit(BRANCH_INFO *, int, AFFECTED_BRANCH_STATUS *);
   void relax_cond_branch (CHAIN *pchain, BRANCH_INFO *pbinfo);
   void relax_fall_thru (CHAIN *pchain, BRANCH_INFO *pbinfo);
   void generate_jump_bb(CHAIN *aChain, BRANCH_INFO *, int, BB *, bool);
   void resolve_target(CHAIN *, CHAIN *, BRANCH_INFO *, float *);
   void resolve_fall_thru(CHAIN *, CHAIN *, BRANCH_INFO *, float *);
   void resolve_branches(CHAIN *, CHAIN *, float *);

   void chain_append(CHAIN *chain, CHAIN *nchain);
   //void chain_insert(CHAIN *chain, CHAIN *nchain);

   void init_branchinfo_distances (BB *from, BB *to, int ix);

   
public:
   BB_LAYOUT(MEM_POOL *const a_pool);
   ~BB_LAYOUT();

   CHAIN *get_bb_chain (BB *b) const { return (CHAIN *)BB_MAP_Get(chain_map, b); }
   CHAIN *get_bbchains() const       { return bbchains; }
   BRANCH_INFO *get_bb_branchinfo(BB *b) { return branchinfo + BB_id(b); }
   int get_bb_offset(BB *b);
   int get_bb_size(BB *b) { 
     // array bb_size[] has the same number of elements as branchinfo[]
     // A jump bb has the size of 3.
     return (BB_id(b) >= num_branchinfo) ? 3 : bb_size[BB_id(b)];
   }

   // utility methods
   void delete_chain_edge (CHAIN_BB_EDGE *chain_edge);
   void add_chain_edge (BB *src, BB *dst);
   void set_branchinfo_visited (bool val);
   BB *find_best_location(CHAIN *, CHAIN *, BRANCH_INFO *, int, bool *, float *);

   // Major methods
   void init_edges();
   void init_branchinfo();
   void init_chains();
   void grow_chains();
   void combine_chains();
   void swap_branches();
   void adjust_branches();
   void finalize();

   // debugging etc.
   void print_chain (CHAIN *c, bool detail=false);
   void print_chain (int cid, bool detail=false);
   void print_chains (bool = false);
   void consistency_verify();

};


inline bool TOP_is_branch(TOP t)
{
  return TI_ISA_Property_Set(PROP_cond, t) || TI_ISA_Property_Set(PROP_jump, t);
}

inline bool is_chain_edge_same (CHAIN_BB_EDGE *c1, CHAIN_BB_EDGE *c2)
{
  return ((c1->get_bb_src() == c2->get_bb_src()) && 
          (c1->get_bb_dst() == c2->get_bb_dst()));
}

#endif /* cflow_layout_INCLUDED */
