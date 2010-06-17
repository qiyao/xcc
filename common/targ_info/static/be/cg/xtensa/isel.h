#ifndef __OLIVE_HEADER_INCLUDED__
#define __OLIVE_HEADER_INCLUDED__

#if !defined(ISEL_INITIAL_MAX_DYNAMIC_RULES)
#define ISEL_INITIAL_MAX_DYNAMIC_RULES 1024
#endif
#if !defined(ISEL_MAX_NT)
#define ISEL_MAX_NT 64
#endif
#define ISEL_NT_BITVECTOR_SIZE (((ISEL_MAX_NT)+31)>>5)
#define ISEL_NUM_PRECOMPILED_RULES 706

class ISEL {
public:
  // typedefs
  class Rule_info;
  class Chain_rule_info;
  class State;
  typedef int Rule;
  typedef unsigned int Nonterm;
  typedef State *(Precompiled_labeller_func)(NODEPTR);
  typedef bool (Labeller_func)(NODEPTR, COST &);
  typedef /* line 2283 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (stmt_action_type)(ISEL::State *, /* line 2283 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2235 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (expr_action_type)(ISEL::State *, /* line 2235 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2236 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (areg_action_type)(ISEL::State *, /* line 2236 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2237 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (breg_action_type)(ISEL::State *, /* line 2237 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2238 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (bareg_action_type)(ISEL::State *, /* line 2238 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2239 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (b1reg_action_type)(ISEL::State *, /* line 2239 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2240 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (freg_action_type)(ISEL::State *, /* line 2240 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2241 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (tie_reg_action_type)(ISEL::State *, /* line 2241 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2242 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (tie_cvt_reg_action_type)(ISEL::State *, /* line 2242 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2243 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (ll_cvt_areg_action_type)(ISEL::State *, /* line 2243 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2244 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void (acc_reg_action_type)(ISEL::State *, /* line 2244 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2245 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (tie_imm_action_type)(ISEL::State *);
  typedef /* line 2246 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (wsar_action_type)(ISEL::State *, /* line 2246 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
  typedef /* line 2247 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (sar_left_action_type)(ISEL::State *, /* line 2247 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
  typedef /* line 2248 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (sar_right_action_type)(ISEL::State *, /* line 2248 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
  typedef /* line 2249 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (areg_or_simm12_action_type)(ISEL::State *, /* line 2249 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2250 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (areg_or_simm8_action_type)(ISEL::State *, /* line 2250 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2251 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (areg_or_b4const_action_type)(ISEL::State *, /* line 2251 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2252 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (areg_or_b4constu_action_type)(ISEL::State *, /* line 2252 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2253 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (extui_mask_action_type)(ISEL::State *);
  typedef /* line 2254 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (bbci_bbsi_imm_action_type)(ISEL::State *);
  typedef /* line 2255 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (logical_and_immed_action_type)(ISEL::State *, /* line 2255 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
  typedef /* line 2256 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (clamps_const_action_type)(ISEL::State *);
  typedef /* line 2257 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (simm8x256_action_type)(ISEL::State *);
  typedef /* line 2260 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (simm8_action_type)(ISEL::State *);
  typedef /* line 2261 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (simm12_action_type)(ISEL::State *);
  typedef /* line 2263 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (simm32_action_type)(ISEL::State *);
  typedef /* line 2266 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (uimm4_action_type)(ISEL::State *);
  typedef /* line 2267 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (uimm5_action_type)(ISEL::State *);
  typedef /* line 2272 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (b4constu_action_type)(ISEL::State *);
  typedef /* line 2273 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (b4const_action_type)(ISEL::State *);
  typedef /* line 2277 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * (msalp32_action_type)(ISEL::State *);
  typedef /* line 2278 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void (zero_action_type)(ISEL::State *);
  typedef /* line 2279 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void (one_action_type)(ISEL::State *);
  typedef /* line 2280 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void (thirtytwo_action_type)(ISEL::State *);
  typedef /* line 2281 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
INT32 (float_const_action_type)(ISEL::State *);
  union Action_type {
    void *none;
    stmt_action_type *_stmt;
    expr_action_type *_expr;
    areg_action_type *_areg;
    breg_action_type *_breg;
    bareg_action_type *_bareg;
    b1reg_action_type *_b1reg;
    freg_action_type *_freg;
    tie_reg_action_type *_tie_reg;
    tie_cvt_reg_action_type *_tie_cvt_reg;
    ll_cvt_areg_action_type *_ll_cvt_areg;
    acc_reg_action_type *_acc_reg;
    tie_imm_action_type *_tie_imm;
    wsar_action_type *_wsar;
    sar_left_action_type *_sar_left;
    sar_right_action_type *_sar_right;
    areg_or_simm12_action_type *_areg_or_simm12;
    areg_or_simm8_action_type *_areg_or_simm8;
    areg_or_b4const_action_type *_areg_or_b4const;
    areg_or_b4constu_action_type *_areg_or_b4constu;
    extui_mask_action_type *_extui_mask;
    bbci_bbsi_imm_action_type *_bbci_bbsi_imm;
    logical_and_immed_action_type *_logical_and_immed;
    clamps_const_action_type *_clamps_const;
    simm8x256_action_type *_simm8x256;
    simm8_action_type *_simm8;
    simm12_action_type *_simm12;
    simm32_action_type *_simm32;
    uimm4_action_type *_uimm4;
    uimm5_action_type *_uimm5;
    b4constu_action_type *_b4constu;
    b4const_action_type *_b4const;
    msalp32_action_type *_msalp32;
    zero_action_type *_zero;
    one_action_type *_one;
    thirtytwo_action_type *_thirtytwo;
    float_const_action_type *_float_const;
  };
  enum {
    stmt_NT=1,
    expr_NT=2,
    areg_NT=3,
    breg_NT=4,
    bareg_NT=5,
    b1reg_NT=6,
    freg_NT=7,
    tie_reg_NT=8,
    tie_cvt_reg_NT=9,
    ll_cvt_areg_NT=10,
    acc_reg_NT=11,
    tie_imm_NT=12,
    wsar_NT=13,
    sar_left_NT=14,
    sar_right_NT=15,
    areg_or_simm12_NT=16,
    areg_or_simm8_NT=17,
    areg_or_b4const_NT=18,
    areg_or_b4constu_NT=19,
    extui_mask_NT=20,
    bbci_bbsi_imm_NT=21,
    logical_and_immed_NT=22,
    clamps_const_NT=23,
    simm8x256_NT=24,
    simm8_NT=25,
    simm12_NT=26,
    simm32_NT=27,
    uimm4_NT=28,
    uimm5_NT=29,
    b4constu_NT=30,
    b4const_NT=31,
    msalp32_NT=32,
    zero_NT=33,
    one_NT=34,
    thirtytwo_NT=35,
    float_const_NT=36,
  };
  static int max_nt;
  static int max_dynamic_rules;
  static int num_dynamic_rules;
  static Rule_info *dynamic_rules;
  static const char *ntname[ISEL_MAX_NT];
  static Rule_info *ntchains[ISEL_MAX_NT];
  struct Match {
    COST cost;
    Rule rule_num;
  };
  struct State {
    int op;
    NODEPTR node;
    int arity;
    State **kids;
    Match *matches;
    unsigned has_match[ISEL_NT_BITVECTOR_SIZE];
    void set_match(Nonterm nt,COST &,Rule r);
    bool has_match_for(Nonterm nt);
  };

  struct Exception {
    char *reason;
    NODEPTR node;
    Exception(char *r,NODEPTR u): reason(r),node(u) {}
  };
  struct Rule_info { // for dynamic rules 
  int op;
    Nonterm lhs;
    Nonterm rhs;
    Labeller_func *f;
    Action_type action;
    Rule_info *next;
    inline bool is_chain_rule(void) const { return rhs!=0; }
  };
  struct Op_info {
    int op;
    Precompiled_labeller_func *labeller;
    Rule_info *dynamic_rules;
  };
  // User-provided static functions
  static Op_info *find_op_info( int opc );
  static void add_op_info( int opc, Op_info *);
  static State *STATE_LABEL(NODEPTR p);
  static void SET_STATE(NODEPTR p, State *s);
  // ALLOC_FOR_TREE for objects whose lifetime same as a tree
  // ALLOC for more persistent objects
  static void *ALLOC_FOR_TREE( int size );
  static void *ALLOC( int size );
  static void *REALLOC(void *, int old_size, int new_size);

  // Generated functions
  static State *label(NODEPTR);
  static bool match(NODEPTR, Nonterm, COST &);
  static State *label1(NODEPTR);
  static void closure(State *s,Nonterm rhs);
  static void initialize(void);
  static Rule rule_no(Rule_info *);
  static State *alloc_state(NODEPTR u,int arity);
  static Rule ISEL::rule(ISEL::State *state,int goalnt);
  static Nonterm add_nonterm( const char *name);
  static Rule_info *alloc_rule(void);
  static Rule add_rule( int opc, Nonterm nt, int arity, Labeller_func f);
  static Rule add_chain_rule( Nonterm lhs, Nonterm rhs, Labeller_func f);
  static Nonterm find_nonterm(const char *n);
  static Rule_info *get_dynamic_rule(Rule r) {
    assert( (int)r >= ISEL_NUM_PRECOMPILED_RULES );
    assert( (int)r-ISEL_NUM_PRECOMPILED_RULES < num_dynamic_rules );
    return &dynamic_rules[(int)r-ISEL_NUM_PRECOMPILED_RULES];
  }

  static void add_stmt_action(Rule r, stmt_action_type *a) {
    get_dynamic_rule(r)->action._stmt = a;
  }
  static void add_expr_action(Rule r, expr_action_type *a) {
    get_dynamic_rule(r)->action._expr = a;
  }
  static void add_areg_action(Rule r, areg_action_type *a) {
    get_dynamic_rule(r)->action._areg = a;
  }
  static void add_breg_action(Rule r, breg_action_type *a) {
    get_dynamic_rule(r)->action._breg = a;
  }
  static void add_bareg_action(Rule r, bareg_action_type *a) {
    get_dynamic_rule(r)->action._bareg = a;
  }
  static void add_b1reg_action(Rule r, b1reg_action_type *a) {
    get_dynamic_rule(r)->action._b1reg = a;
  }
  static void add_freg_action(Rule r, freg_action_type *a) {
    get_dynamic_rule(r)->action._freg = a;
  }
  static void add_tie_reg_action(Rule r, tie_reg_action_type *a) {
    get_dynamic_rule(r)->action._tie_reg = a;
  }
  static void add_tie_cvt_reg_action(Rule r, tie_cvt_reg_action_type *a) {
    get_dynamic_rule(r)->action._tie_cvt_reg = a;
  }
  static void add_ll_cvt_areg_action(Rule r, ll_cvt_areg_action_type *a) {
    get_dynamic_rule(r)->action._ll_cvt_areg = a;
  }
  static void add_acc_reg_action(Rule r, acc_reg_action_type *a) {
    get_dynamic_rule(r)->action._acc_reg = a;
  }
  static void add_tie_imm_action(Rule r, tie_imm_action_type *a) {
    get_dynamic_rule(r)->action._tie_imm = a;
  }
  static void add_wsar_action(Rule r, wsar_action_type *a) {
    get_dynamic_rule(r)->action._wsar = a;
  }
  static void add_sar_left_action(Rule r, sar_left_action_type *a) {
    get_dynamic_rule(r)->action._sar_left = a;
  }
  static void add_sar_right_action(Rule r, sar_right_action_type *a) {
    get_dynamic_rule(r)->action._sar_right = a;
  }
  static void add_areg_or_simm12_action(Rule r, areg_or_simm12_action_type *a) {
    get_dynamic_rule(r)->action._areg_or_simm12 = a;
  }
  static void add_areg_or_simm8_action(Rule r, areg_or_simm8_action_type *a) {
    get_dynamic_rule(r)->action._areg_or_simm8 = a;
  }
  static void add_areg_or_b4const_action(Rule r, areg_or_b4const_action_type *a) {
    get_dynamic_rule(r)->action._areg_or_b4const = a;
  }
  static void add_areg_or_b4constu_action(Rule r, areg_or_b4constu_action_type *a) {
    get_dynamic_rule(r)->action._areg_or_b4constu = a;
  }
  static void add_extui_mask_action(Rule r, extui_mask_action_type *a) {
    get_dynamic_rule(r)->action._extui_mask = a;
  }
  static void add_bbci_bbsi_imm_action(Rule r, bbci_bbsi_imm_action_type *a) {
    get_dynamic_rule(r)->action._bbci_bbsi_imm = a;
  }
  static void add_logical_and_immed_action(Rule r, logical_and_immed_action_type *a) {
    get_dynamic_rule(r)->action._logical_and_immed = a;
  }
  static void add_clamps_const_action(Rule r, clamps_const_action_type *a) {
    get_dynamic_rule(r)->action._clamps_const = a;
  }
  static void add_simm8x256_action(Rule r, simm8x256_action_type *a) {
    get_dynamic_rule(r)->action._simm8x256 = a;
  }
  static void add_simm8_action(Rule r, simm8_action_type *a) {
    get_dynamic_rule(r)->action._simm8 = a;
  }
  static void add_simm12_action(Rule r, simm12_action_type *a) {
    get_dynamic_rule(r)->action._simm12 = a;
  }
  static void add_simm32_action(Rule r, simm32_action_type *a) {
    get_dynamic_rule(r)->action._simm32 = a;
  }
  static void add_uimm4_action(Rule r, uimm4_action_type *a) {
    get_dynamic_rule(r)->action._uimm4 = a;
  }
  static void add_uimm5_action(Rule r, uimm5_action_type *a) {
    get_dynamic_rule(r)->action._uimm5 = a;
  }
  static void add_b4constu_action(Rule r, b4constu_action_type *a) {
    get_dynamic_rule(r)->action._b4constu = a;
  }
  static void add_b4const_action(Rule r, b4const_action_type *a) {
    get_dynamic_rule(r)->action._b4const = a;
  }
  static void add_msalp32_action(Rule r, msalp32_action_type *a) {
    get_dynamic_rule(r)->action._msalp32 = a;
  }
  static void add_zero_action(Rule r, zero_action_type *a) {
    get_dynamic_rule(r)->action._zero = a;
  }
  static void add_one_action(Rule r, one_action_type *a) {
    get_dynamic_rule(r)->action._one = a;
  }
  static void add_thirtytwo_action(Rule r, thirtytwo_action_type *a) {
    get_dynamic_rule(r)->action._thirtytwo = a;
  }
  static void add_float_const_action(Rule r, float_const_action_type *a) {
    get_dynamic_rule(r)->action._float_const = a;
  }
};

/* line 2283 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * stmt_action(ISEL::State *_s, /* line 2283 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2235 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * expr_action(ISEL::State *_s, /* line 2235 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2236 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * areg_action(ISEL::State *_s, /* line 2236 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2237 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * breg_action(ISEL::State *_s, /* line 2237 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2238 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * bareg_action(ISEL::State *_s, /* line 2238 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2239 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * b1reg_action(ISEL::State *_s, /* line 2239 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2240 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * freg_action(ISEL::State *_s, /* line 2240 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2241 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * tie_reg_action(ISEL::State *_s, /* line 2241 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2242 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * tie_cvt_reg_action(ISEL::State *_s, /* line 2242 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2243 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * ll_cvt_areg_action(ISEL::State *_s, /* line 2243 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2244 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void acc_reg_action(ISEL::State *_s, /* line 2244 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2245 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * tie_imm_action(ISEL::State *_s);
/* line 2246 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * wsar_action(ISEL::State *_s, /* line 2246 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
/* line 2247 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * sar_left_action(ISEL::State *_s, /* line 2247 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
/* line 2248 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * sar_right_action(ISEL::State *_s, /* line 2248 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
OPS *ops);
/* line 2249 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * areg_or_simm12_action(ISEL::State *_s, /* line 2249 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2250 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * areg_or_simm8_action(ISEL::State *_s, /* line 2250 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2251 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * areg_or_b4const_action(ISEL::State *_s, /* line 2251 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2252 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * areg_or_b4constu_action(ISEL::State *_s, /* line 2252 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2253 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * extui_mask_action(ISEL::State *_s);
/* line 2254 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * bbci_bbsi_imm_action(ISEL::State *_s);
/* line 2255 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * logical_and_immed_action(ISEL::State *_s, /* line 2255 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN *result,OPS *ops);
/* line 2256 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * clamps_const_action(ISEL::State *_s);
/* line 2257 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * simm8x256_action(ISEL::State *_s);
/* line 2260 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * simm8_action(ISEL::State *_s);
/* line 2261 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * simm12_action(ISEL::State *_s);
/* line 2263 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * simm32_action(ISEL::State *_s);
/* line 2266 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * uimm4_action(ISEL::State *_s);
/* line 2267 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * uimm5_action(ISEL::State *_s);
/* line 2272 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * b4constu_action(ISEL::State *_s);
/* line 2273 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * b4const_action(ISEL::State *_s);
/* line 2277 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
TN * msalp32_action(ISEL::State *_s);
/* line 2278 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void zero_action(ISEL::State *_s);
/* line 2279 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void one_action(ISEL::State *_s);
/* line 2280 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
void thirtytwo_action(ISEL::State *_s);
/* line 2281 /build/tree/RB-2008.4_kuma/p4root/Xtensa/Software/xcalibur/common/targ_info/static/be/cg/xtensa/isel.pat */
INT32 float_const_action(ISEL::State *_s);
extern int ISEL_start;
#endif
