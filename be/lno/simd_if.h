// Copyright (c) 2003 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_if.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_IF: SIMD if conversion transformation                               *
 *                                                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_if.h $

#ifndef __SIMD_IF__
#define __SIMD_IF__

#include "simd.h"

class SIMD_AT;
class SIMD_MODEL;

//
// SIMD_IF
//

/*--------------------------------------------------------------------------*
 * A class to hold the SEL values for if-conversion                         *
 *--------------------------------------------------------------------------*/
class SIMD_SEL {
protected:
    SIMD_PREG *_then_preg;
    SIMD_PREG *_else_preg;
    SIMD_PREG *_res_preg;

public:
    SIMD_SEL() : _then_preg(NULL), _else_preg(NULL), _res_preg(NULL) {}
    ~SIMD_SEL() {}

    SIMD_PREG* Then_Preg() { return _then_preg; }
    SIMD_PREG* Else_Preg() { return _else_preg; }
    SIMD_PREG* Res_Preg()  { return _res_preg;  }

    void       Set_Then_Preg(SIMD_PREG *preg) { _then_preg = preg; }
    void       Set_Else_Preg(SIMD_PREG *preg) { _else_preg = preg; }
    void       Set_Res_Preg(SIMD_PREG *preg)  { _res_preg = preg;  }
};

class SIMD_SEL_S : public SIMD_SEL {
protected:
    SIMD_SCALAR *_s_info;
    WN        *_last_def;
public:
    SIMD_SEL_S(SIMD_SCALAR *s_info) : _s_info(s_info), _last_def(NULL) {}
    SIMD_SCALAR* S_Info() { return _s_info; }
    WN*        Last_Def() { return _last_def; }
    void       Update_Last_Def(WN *last_def);
};

class SIMD_SEL_I : public SIMD_SEL {
protected:
    IMEM_INFO *_imem_info;
    WN        *_last_def;
public:
    SIMD_SEL_I(IMEM_INFO *imem) : _imem_info(imem), _last_def(NULL) {}
    IMEM_INFO* Imem_Info() { return _imem_info; }
    WN*        Last_Def() { return _last_def; }
    void       Update_Last_Def(WN *last_def);
};


class SIMD_IF
{
protected:
    WN *_if_wn;
    WN *_top_if_wn;
    DYN_ARRAY<SIMD_SEL_S*> _sel_s_info;
    DYN_ARRAY<SIMD_SEL_I*> _sel_i_info;
    bool        _if_converted;

    void        Update_Last_Def(WN *last_def);

public:
    SIMD_IF(WN *if_wn, WN *top_if_wn, MEM_POOL *pool) :
      _if_wn(if_wn), _top_if_wn(top_if_wn), _sel_s_info(pool), _sel_i_info(pool), 
      _if_converted(false) { }
    ~SIMD_IF() {}

    WN *Top_If_Wn (void) const { return _top_if_wn; }
    SIMD_SEL_S* Get_Sel_S(SIMD_SCALAR* s_info);
    SIMD_SEL_I* Get_Sel_I(IMEM_INFO* imem_info);
    WN*         Generate_Sel(SIMD_PREG *res, WN *then_val, WN *else_val, 
			     WN *last_def);
    
    bool        If_Converted(void)     { return _if_converted; }
    void        Set_If_Converted(void) { _if_converted = true; }
    void        Generate_Sel(SIMD_IF *p_simd_if, bool is_then);
    void        Update_Sel(SIMD_SCALAR *s_info, SIMD_PREG *preg, WN *last_wn, 
			   bool is_then);
    void        Update_Sel(IMEM_INFO   *imem, SIMD_PREG *preg, WN *last_wn, 
			   bool is_then);
    void        If_Conv_Sym_Pop(void);
    void        Separate_Rhs(WN *wn, SIMD_PREG *preg);
};

class SIMD_IF_CONV {
protected:
    WN         *_sel;       /* first SELECT in the if-converted stmts       */
    INT         _count;     /* total number of SELECT corresponding to a IF */
    INT         _processed; /* total number of SELECT processed             */
    SIMD_EINFO *_cond_info; /* tranformed cond EINFO                        */
    bool        _swap_branch; /* swap 'then' with 'else' if the comparison  
				 is changed from != to == */
    
public:
    SIMD_IF_CONV(WN *sel, INT count) : _sel(sel), _count(count), 
	_processed(0), _cond_info(NULL), _swap_branch(false) {}
    ~SIMD_IF_CONV() {}
    
    INT      Count(void)         { return _count;               }
    INT      Processed(void)     { return _processed;           }
    void     Inc_Processed(void) { _processed++;                }
    void     Reset_Processed(void) { _processed = 0;            }
    INT      Finished(void)      { return _processed == _count; }
    bool     Is_First(void)      { return _processed == 0;      }
    SIMD_EINFO *Cond_Info(void)  { return _cond_info;           }
    void     Set_Cond_Info(SIMD_EINFO *t) { _cond_info = t;     }
    void     Set_Swap_Branch(bool t) { _swap_branch = t;        }
    bool     Swap_Branch(void)   { return _swap_branch;         }
};

typedef HASH_TABLE<IMEM_INFO*, INT>     IMEM_VIS_Map;
typedef HASH_TABLE<SIMD_SCALAR*, INT>   SINFO_VIS_Map;

class SIMD_IF_ANA {
private:
    WN              *_wn;              /* the WN_if node          */
    DYN_ARRAY<SIMD_IF_ANA*> _inner_if_then; /* nested IF inside        */
    DYN_ARRAY<SIMD_IF_ANA*> _inner_if_else; /* nested IF inside        */
    EINFO_Stack      _test_einfo;      /* SIMD_EINFO on test part */
    EINFO_Stack      _then_einfo;      /* SIMD_EINFO on then part */
    EINFO_Stack      _else_einfo;      /* SIMD_EINFO on else part */
    EXPR_Stack       _then_store;      /* stores on the then part */
    EXPR_Stack       _else_store;      /* stores on the else part */
    EINFO_Stack      _sel_einfo;       /* SIMD_EINFO on each CMOV */
    IMEM_Stack       _must;            /* must load/store  */
    IMEM_Stack       _specu;           /* speculative load/store  */
    bool             _is_then;         /* decending on the 'then' */
    bool             _is_test;         /* decending on the if_test*/
    float            _orig_cycle;      /* original cycle          */
    INT              _if_conv_cycle;   /* cycle after if-conversion */
    INT              _cmov_count;      /* number of CMOV            */
    MEM_POOL        *_pool;

    INT   Count_If_Cycle(DYN_ARRAY<SIMD_IF_ANA*> &a);
    INT   Common_Load_Store_Cycle(IMEM_VIS_Map &seen_imem_load,
				  IMEM_VIS_Map &seen_imem_store);
    INT   Common_Load_Store_Cycle(EINFO_Stack &stack, 
				  IMEM_VIS_Map &seen_load,
				  IMEM_VIS_Map &seen_store);
    bool  Has_Mismatched_Vector_Length(DYN_ARRAY<SIMD_IF_ANA*> &a,
				       INT vec_length);
    bool  Has_Mismatched_Vector_Length(EINFO_Stack &stack, INT vec_length);
    void  Enter_Unique_Speculative_LS(IMEM_INFO *imem);
    void  Classify_One_Side(IMEM_VIS_Map &must_ls, IMEM_VIS_Map &specu_ls,
			    EINFO_Stack &einfos, 
			    DYN_ARRAY<SIMD_IF_ANA*> &simd_if_anas);
    void  Classify_Load_Store_Rec(void);
    void  Screen_Unconditional_LS();
    INT   CMove_Cycle(EINFO_Stack &stack, SINFO_VIS_Map &seen_sinfo,
		      IMEM_VIS_Map &seen_imem);
    INT   CMove_Cycle(SINFO_VIS_Map &seen_sinfo, IMEM_VIS_Map &seen_imem);
    bool  Has_CMov(SIMD_EINFO *einfo, bool verbose=true);
    bool  Check_Missing_CMov(EINFO_Stack &stack, SINFO_VIS_Map &seen_sinfo,
			     IMEM_VIS_Map &seen_imem);
    bool  Check_Missing_CMov(SINFO_VIS_Map &seen_sinfo, IMEM_VIS_Map &seen_imem);
    void  Request_AT_CMov(SIMD_EINFO *e_info, SIMD_AT *simd_at);
    void  Request_Model_CMov(SIMD_EINFO *e_info, SIMD_MODEL *simd_m);
    
public:
    
    /* Constructor/Destructor */
    SIMD_IF_ANA(WN *if_wn, MEM_POOL *pool) : 
	_wn(if_wn), _pool(pool), _inner_if_then(pool), _inner_if_else(pool),
	_test_einfo(pool), _then_einfo(pool), _sel_einfo(pool),
	_else_einfo(pool),_then_store(pool), _else_store(pool), 
	_is_test(true), _is_then(true),	_orig_cycle(0.0), _cmov_count(0),
	_if_conv_cycle(0), _must(pool), _specu(pool) {}
    ~SIMD_IF_ANA() {}
    
    /* Member access */
    WN*          If_Wn(void)           { return _wn;             }
    DYN_ARRAY<SIMD_IF_ANA*>& Inner_If_Then(void) { return _inner_if_then;  }
    DYN_ARRAY<SIMD_IF_ANA*>& Inner_If_Else(void) { return _inner_if_else;  }
    void         Add_Inner_If(SIMD_IF_ANA *t);
    EINFO_Stack& Then_EInfo(void)      { return _then_einfo;     }
    EINFO_Stack& Else_EInfo(void)      { return _else_einfo;     }
    EINFO_Stack& Test_EInfo(void)      { return _test_einfo;     }
    EXPR_Stack&  Then_Store(void)      { return _then_store;     }
    EXPR_Stack&  Else_Store(void)      { return _else_store;     }
    EINFO_Stack& Sel_EInfo(void)       { return _sel_einfo;      }
    IMEM_Stack&  Must_IMem(void)       { return _must;           }
    IMEM_Stack&  Specu_IMem(void)      { return _specu;          }
    
    void         Set_Is_Test(void)     { _is_test = true;        }
    void         Set_Is_Then(void)     { _is_test = false;
                                         _is_then = true;        }
    void         Set_Is_Else(void)     { _is_test = false;
                                         _is_then = false;       }
    void         Add_EInfo(SIMD_EINFO *e_info);

    bool         Is_Test(void)         { return _is_test;                     }
    bool         Is_Then(void)         { return !_is_test && _is_then;        }
    bool         Is_Else(void)         { return !_is_test && !_is_then;       }

    float        Original_Cycle(void);
    INT          If_Converted_Cycle(void);
    bool         Has_Mismatched_Vector_Length(INT p_v_length=0);
    bool         Has_Specu(void)       { return Specu_IMem().Elements() >0;   }
    void         Classify_Load_Store(void);
    INT          CMove_Count(void);     
    bool         Check_Missing_CMov(void);
    void         Request_AT_CMov(SIMD_AT *simd_at);
    void         Request_Model_CMov(SIMD_MODEL *simd_m);

    bool         If_Conv_Analysis(void);
}; // SIMD_IF_ANA

#endif /* __SIMD_IF__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

