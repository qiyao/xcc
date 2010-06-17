// Copyright (c) 2004-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD: SIMD transformation related data structures and functions:         *
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

// $Id: simd.h $

#define USE_STANDARD_TYPES
#include "defs.h"
#ifndef access_vector_INCLUDED
#include "access_vector.h"
#endif
#ifndef cxx_graph_INCLUDED
#include "cxx_graph.h"
#endif
#ifndef graph_template_INCLUDED
#include "graph_template.h"
#endif
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif
#ifndef dep_INCLUDED
#include "dep.h"
#endif
#ifndef lno_scc_INCLUDED
#include "lno_scc.h"
#endif
#ifndef lnopt_main_INCLUDED
#include "lnopt_main.h"
#endif
#ifndef snl_nest_INCLUDED
#include "snl_nest.h"
#endif
#include <sys/types.h>
#include "sxlist.h"
#ifndef _lno_bv_INCLUDED
#include "lno_bv.h"
#endif
#ifndef lnoutils_INCLUDED
#include "lnoutils.h"
#endif

#include "tie.h"
#include "wintrinsic.h"
#include "lwn_util.h"
#include <stdarg.h>

#include "model.h"
#include "reduc.h"

#ifndef __SIMD_INFO__
#define __SIMD_INFO__

#include "ir_reader.h"
#include "at_message.h"

extern bool simd_debug; // SIMD tracing through -tt31:0x1000

// forward declarations
class SIMD_LOOP;
class SIMD_EINFO;
class SIMD_PREG;
class SIMD_SCALAR;
class SIMD_TYPE;
class SIMD_INFO;

class IMEM_INFO;                          /* simd_imem.h                  */
class IMEM_OFFSET;                        /* simd_imem.h                  */
class IMEM_GROUP;                         /* simd_imem.h                  */
class IMEM_Map;                           /* simd_imem.h                  */
class SIMD_AT;                            /* simd_at.h                    */
class SIMD_TI;                            /* simd_ti.h                    */
class SIMD_TI_V2;                         /* simd_ti_v2.h                 */
class SIMD_SELECT;                        /* simd_select.h                */
class SIMD_IF_ANA;                        /* simd_if.h                    */

// stack and array types

typedef STACK<WN*> EXPR_Stack;
typedef STACK<SIMD_EINFO*> EINFO_Stack;
typedef STACK<IMEM_INFO*> IMEM_Stack;
typedef STACK<EINDEX16> EDGE_STACK;
typedef STACK<VINDEX16> VERTEX_STACK;
typedef STACK<SIMD_SCALAR*> SINFO_Stack;
typedef STACK<SIMD_PREG *> SIMD_PREG_Stack;

typedef DYN_ARRAY <SIMD_PREG *> SIMD_PREGS;
typedef DYN_ARRAY <WN *> WN_ARRAY;
typedef DYN_ARRAY<VINDEX16> VINDEX16_ARRAY;

// hash tables

struct Symbol_Hash {
    INT operator() (const SYMBOL &symbol) const
    { return (INT)(symbol.ST_Base() + symbol.ST_Offset() + 
		   symbol.WN_Offset()); }
};

struct Symbol_Equal {
    BOOL operator() (const SYMBOL &s1, const SYMBOL &s2) const
    {	return (s1==s2) ; }
};

typedef struct {
    TYPE_ID preg_type;
    INT64 val;
} CONST_MAP_KEY;

struct CONST_MAP_KEY_Hash {
    INT operator() (const CONST_MAP_KEY &key) const {
	return ((INT)key.preg_type)^((INT)key.val);
    }
};

struct CONST_MAP_KEY_Equal {
    BOOL operator() (const CONST_MAP_KEY &k1, const CONST_MAP_KEY &k2) const {
	return (k1.preg_type == k2.preg_type) &&
	    (k1.val == k2.val);
    }
};

// CONSTANT to PREG, i.e. (TYPE_ID,VALUE)->SIMD_PREG
typedef USER_HASH_TABLE <CONST_MAP_KEY, SIMD_PREG *,
    CONST_MAP_KEY_Hash, CONST_MAP_KEY_Equal> SIMD_CONST_Map;
typedef USER_HASH_TABLE<SYMBOL,SIMD_EINFO*,Symbol_Hash,Symbol_Equal> SIMD_SMap;
typedef USER_HASH_TABLE<SYMBOL,SIMD_SCALAR*,Symbol_Hash,Symbol_Equal> SINFO_Map;
typedef HASH_TABLE<TYPE_ID,SIMD_TYPE *> SIMD_TYPE_Map;
typedef HASH_TABLE<WN*, SIMD_EINFO*> SIMD_EMap;
typedef USER_HASH_TABLE<SYMBOL, INT, Symbol_Hash, Symbol_Equal> ALIGNMENT_TABLE;
typedef USER_HASH_TABLE_ITER<SYMBOL, INT, Symbol_Hash, Symbol_Equal> ALIGNMENT_TABLE_ITER;

extern SIMD_PREG *SIMD_CONST_Map_Find(SIMD_CONST_Map *map,
				      TYPE_ID preg_type, INT64 val);
extern void SIMD_CONST_Map_Enter(SIMD_CONST_Map *map,
				 TYPE_ID preg_type, INT64 val, SIMD_PREG *preg);
extern SIMD_PREG *SIMD_CONST_Map_Get(SIMD_CONST_Map *map,
				     TYPE_ID preg_type, INT64 val,
				     TYPE_ID val_type, WN *stmt,
                                     bool is_select,
				     char *name =NULL, MEM_POOL *pool =NULL);

extern void SIMD_PREGS_Copy(SIMD_PREGS *src, SIMD_PREGS *dst);
extern SIMD_PREG *SIMD_PREGS_Get(SIMD_PREGS *pregs, UINT idx);
extern void SIMD_PREGS_Set(SIMD_PREGS *pregs, UINT idx, SIMD_PREG *preg);

extern SIMD_LOOP *Cur_Simd_Loop;    // Current SIMD loop
extern SIMD_INFO *Simd_Info;        // SIMD info
extern SIMD_TI *Simd_Ti;            // SIMD target-info
extern SIMD_TI_V2 *Simd_Ti_V2;      // SIMD Vectra2 target-info

class SIMD_SCALAR {
private:

    enum FLAG {
	NONE          = 0x0000,
	HAS_DEF       = 0x0001,                /* has definition     */
	HAS_USE       = 0x0002,                /* has use            */
	EXP_USE       = 0x0004,                /* exposed use to     *
                                                * outside definition */
	LIVEOUT       = 0x0008,                /* live out           */
	BAD_DEP       = 0x0010,                /* bad dependence     */
	INITIALIZE    = 0x0020,                /* need initialization*/
	REDUCTION     = 0x0040,                /* reduction          */
	LOOP_INDEX    = 0x0080,                /* use loop index     */
	PRIVATE       = 0x0100,                /* private for SE     */
	VISITED       = 0x0200,                /* visited in trans   */
	EXPAND        = 0x0400,                /* need expand        */ 
    };

    SYMBOL     _sym;                         /* scalar symbol      */
    EXPR_Stack _lodsto ;                     /* load/store exprs   */
    UINT       _flag;                        /* flags              */
    REDUCTION_TYPE _red_type;                /* reduction type     */
    ST*        _se_array;                    /* expand array       */

    SIMD_EINFO *_e_info;                     /* most recent SIMD_EINFO */
    SIMD_PREG_Stack _if_conv;                /* for If_Conv            */

public:
    
    /* Constructor/Destructor */
    SIMD_SCALAR(const SYMBOL &sym, WN *expr, MEM_POOL *pool) :
	_sym(sym), _lodsto(pool), _flag(0), _e_info(NULL), _red_type(RED_NONE),
	_se_array(NULL), _if_conv(pool)
    {
	Add_Lod_Sto(expr);
    }
    
    ~SIMD_SCALAR() {}
    
    /* Member access */
    SYMBOL&       Symbol(void)         { return _sym;                      }
    EXPR_Stack&   Lod_Sto(void)        { return _lodsto;                   }
    
    /* Flags */
    void          Set_Has_Def(void)    { _flag |= HAS_DEF;                 }
    bool          Has_Def(void)        { return (_flag & HAS_DEF) != 0;    }
    void          Set_Has_Use(void)    { _flag |= HAS_USE;                 }
    bool          Has_Use(void)        { return (_flag & HAS_USE) != 0;    }
    void          Set_Has_Exp_Use(void){ _flag |= EXP_USE;                 }
    bool          Has_Exp_Use(void)    { return (_flag & EXP_USE) != 0;    }
    void          Set_Live_Out(void)   { _flag |= LIVEOUT;                 }
    bool          Live_Out(void)       { return (_flag & LIVEOUT) != 0;    }
    void          Set_Bad_Dep(void)    { _flag |= BAD_DEP;                 }
    bool          Bad_Dep(void)        { return (_flag & BAD_DEP) != 0;    }
    void          Set_Initialize(void) { _flag |= INITIALIZE;              }
    bool          Initialize(void)     { return (_flag & INITIALIZE) != 0; }
    void          Set_Loop_Index(void) { _flag |= LOOP_INDEX;              }
    bool          Loop_Index(void)     { return (_flag & LOOP_INDEX) != 0; }
    void          Set_Reduction(void)  { _flag |= REDUCTION;               }
    bool          Reduction(void)      { return (_flag & REDUCTION) != 0;  }
    void          Set_Private(void)    { _flag |= PRIVATE;                 }
    bool          Private(void)        { return (_flag & PRIVATE) != 0;    }
    void          Set_Transformed(void){ _flag |= VISITED;                 }
    bool          Transformed(void)    { return (_flag & VISITED) != 0;    }
    void          Set_Expand(void)     { _flag |= EXPAND;                  }
    bool          Expand(void)         { return (_flag & EXPAND) != 0;     }

    REDUCTION_TYPE Red_Type(void)      { return _red_type;                 }
    void          Set_Red_Type(REDUCTION_TYPE red) { _red_type = red;      }

    SIMD_EINFO   *E_Info(void)         { return _e_info;                   }
    void          Set_E_Info(SIMD_EINFO *e)  
                                      { _e_info = e;                       }
    ST*           SE_Array(void)      { return _se_array;                  }
    void          Set_SE_Array(ST* st){ _se_array = st;                    }

    /* Add/remove a LDID/STID to the stack */
    void Add_Lod_Sto (WN *expr);
    
    /* check if it is reduction */
    void Check_Reduction(WN *loop, INT depth, SIMD_LOOP *simd_loop);
    bool Check_Missing_Du(void);
    void Setup_Scalar_Dependence_Info(WN *loop);
    void Check_Expansion(WN *loop);
    void Check_Def_Outside(WN* loop);
    void Check_Loop_Index(WN *loop);

    // TOS of If_Conv symbol stack 
    SIMD_PREG *If_Conv_Sym_Top(void) {
	if (_if_conv.Is_Empty()) {
	    return NULL;
	} else {
	    return _if_conv.Top();
	}
    }

    void If_Conv_Sym_Push(SIMD_PREG *preg) {
	_if_conv.Push(preg);
    }

    void If_Conv_Sym_Pop() {
	_if_conv.Pop();
    }

    WN* Get_Load_Copy(void);
};

// SIMD_TYPE stores a relation between scalar and vector types

class SIMD_TYPE {

private:
    TYPE_ID _simd_reg_type;
    TYPE_ID _simd_mem_type;
    INT _simd_width;
    INT _simd_mem_bit_size;
    INT _simd_reg_bit_size;
    SIMD_SELECT *_simd_select;
    
public:
    SIMD_TYPE() :
	_simd_reg_type(MTYPE_UNKNOWN), _simd_mem_type(MTYPE_UNKNOWN),
	_simd_width(0), _simd_mem_bit_size(0), _simd_reg_bit_size(0),
	_simd_select(NULL) { }
    SIMD_TYPE(TYPE_ID simd_reg_type, TYPE_ID simd_mem_type,
	      INT simd_width, SIMD_SELECT *simd_select) :
	_simd_reg_type(simd_reg_type), _simd_mem_type(simd_mem_type),
	_simd_width(simd_width), _simd_select(simd_select)
    {
	_simd_mem_bit_size = MTYPE_bit_size(simd_mem_type);
	_simd_reg_bit_size = MTYPE_bit_size(simd_reg_type);
    }
    ~SIMD_TYPE();
    
    void Set_SIMD_Reg_Type(TYPE_ID type)   { _simd_reg_type = type; }
    TYPE_ID SIMD_Reg_Type(void) const      { return _simd_reg_type; }
    
    void Set_SIMD_Mem_Type(TYPE_ID type)   { _simd_mem_type = type; }
    TYPE_ID SIMD_Mem_Type(void) const      { return _simd_mem_type; }

    void Set_SIMD_Width(INT width)         { _simd_width = width; }
    INT SIMD_Width(void) const             { return _simd_width;  }
    
    void Set_SIMD_Mem_Byte_Size(INT size)  { _simd_mem_bit_size = size << 3; }
    INT SIMD_Mem_Byte_Size(void) const {
	Is_True((_simd_mem_bit_size&7)==0,
		("Invalid SIMD memory bit size %d for type %s.",
		 _simd_mem_bit_size, MTYPE_name(SIMD_Mem_Type())));
	return _simd_mem_bit_size >> 3;
    }
    
    void Set_SIMD_Mem_Bit_Size(INT size)   { _simd_mem_bit_size = size; }
    INT SIMD_Mem_Bit_Size(void) const      { return _simd_mem_bit_size; }

    void Set_SIMD_Reg_Bit_Size(INT size)   { _simd_reg_bit_size = size; }
    INT SIMD_Reg_Bit_Size(void) const      { return _simd_reg_bit_size; }

    INT Elem_Mem_Bit_Size(void) const {
	Is_True(_simd_width!=0 && _simd_mem_bit_size%_simd_width==0,
		("Inconsistent bit size %d and width %d for type %s.",
		 _simd_mem_bit_size,_simd_width,MTYPE_name(_simd_reg_type)));
	return _simd_mem_bit_size/_simd_width;
    }
    INT Elem_Reg_Bit_Size(void) const {
	Is_True(_simd_width!=0 && _simd_reg_bit_size%_simd_width==0,
		("Inconsistent bit size %d and width %d for type %s.",
		 _simd_reg_bit_size,_simd_width,MTYPE_name(_simd_reg_type)));
	return _simd_reg_bit_size/_simd_width;
    }

    void Set_SIMD_Select (SIMD_SELECT *simd_select) { _simd_select = simd_select; }
    SIMD_SELECT *SIMD_Select (void) const { return _simd_select; }
};

struct VSE_TY {
    TYPE_ID mtype;
    INT     size;
    TY_IDX  ty_idx;
    VSE_TY(TYPE_ID m, INT l, TY_IDX idx) : mtype(m), size(l), ty_idx(idx) {}
};

class SIMD_INFO {

protected:
    MEM_POOL *_pool;
    
    INT   _type1simd;
    INT   _type2simd;
    INT   _type1membits;
    INT   _type1regbits;
    INT   _type2membits;
    INT   _type2regbits;
    INT   _simdratio;
    bool  _aluops;
    bool  _avg;
    bool  _mulvv;
    bool  _mulvc;
    INT   _mulin1bits;
    INT   _mulin2bits;
    INT   _muloutshift;
    INT   _naregs;
    INT   _nsregs;
    INT   _nvregs;
    bool  _mul16;
    bool  _mulabc;
    bool  _mulsbc;
    
    bool  _vectra_ok; // is the Vectra configuration ok to handle
    
    /*-----------------------------------------------------------------------*
     * Predefined Vector Integer types, need to hook up with TIE              *
     *------------------------------------------------------------------------*/
    TYPE_ID  _v8x16type;
    TYPE_ID  _v8x20type;
    TYPE_ID  _v4x32type;
    TYPE_ID  _v4x40type;
    TYPE_ID  _v128x1type;
    TYPE_ID  _v160x1type;
    TYPE_ID  _vsel_type;
    TYPE_ID  _align_type;
    TYPE_ID  _coeff_type;
    
    TYPE_ID      _s16type;
    TYPE_ID      _s32type;
    
    ST*          _store_align;
    
    SIMD_TYPE_Map *_scalar_simd_map; /* scalar TYPE_ID to SIMD_TYPE map */
    SIMD_TYPE_Map *_simd_simd_map;   /* simd TYPE_ID to SIMD_TYPE map */
    
    SIMD_AT       *_simd_at;         /* SIMD AT object */
    
    ALIGNMENT_TABLE *_alignment_table; /* SYMBOL to alignment value map */
    
    STACK<VSE_TY*> _vse_ty;          /* existing scalar expand TY_IDX */
    
  typedef USER_HASH_TABLE<const char *, bool, String_Hash, String_Equal> STR_SET;

  /* Maintain a set of emitted messages to avoid duplicates. */
  STR_SET *_message_set;

public:
    /* constructor/destructor */
    SIMD_INFO(MEM_POOL *pool);
    
    /* Initialize vectra configuration information from libcc */
    void Init_Vectra_Config(void);
    virtual bool Vectra_Ok(void) { return _vectra_ok; }
    virtual bool Is_Vectra1(void){ return true; }
    
    
    void Init_SIMD_Type_Map(void);
    
    /* find existing VSE TY_ID */
    TY_IDX Vse_Ty_Find(TYPE_ID t, INT len) {
	for (INT i = 0; i < _vse_ty.Elements(); i++) {
	    VSE_TY *cur = _vse_ty.Bottom_nth(i);
	    if (cur->mtype == t && cur->size == len) {
		return cur->ty_idx;
	    }
	}
	return TY_IDX_ZERO;
    }
    
    void Add_Vse_Ty(VSE_TY *vse_ty) {
	_vse_ty.Push(vse_ty);
    }
  
    // member access
    MEM_POOL *Mem_Pool(void)  { return _pool;         }
    MEM_POOL *Pool(void)      { return _pool;         }
    
    INT  Type1_Simd(void)     { return _type1simd;    }
    INT  Type2_Simd(void)     { return _type2simd;    }
    INT  Type1_Mem_Bits(void) { return _type1membits; }
    INT  Type1_Mem_Bytes(void){ return _type1membits >> 3; }
    INT  Type1_Reg_Bits(void) { return _type1regbits; }
    INT  Type2_Mem_Bits(void) { return _type2membits; }
    INT  Type2_Mem_Bytes(void){ return _type2membits >> 3; }
    INT  Type2_Reg_Bits(void) { return _type2regbits; }
    INT  Simd_Ratio(void)     { return _simdratio;    }
    bool Alu_Ops(void)        { return _aluops;       }
    bool Avg(void)            { return _avg;          }
    bool Mul_V_V(void)        { return _mulvv;        }
    bool Mul_V_C(void)        { return _mulvc;        }
    INT  Mul_In1_Bits(void)   { return _mulin1bits;   }
    INT  Mul_In2_Bits(void)   { return _mulin2bits;   }
    INT  Mul_Out_Shift(void)  { return _muloutshift;  }
    INT  N_A_Regs(void)       { return _naregs;       }
    INT  N_S_Regs(void)       { return _nsregs;       }
    INT  N_V_Regs(void)       { return _nvregs;       }
    bool Mul16(void)          { return _mul16;        }
    bool Mula_B_C(void)       { return _mulabc;       }
    bool Muls_B_C(void)       { return _mulsbc;       }
    
    SIMD_TYPE_Map *Scalar_SIMD_Map(void) { return _scalar_simd_map;  }
    SIMD_TYPE_Map *SIMD_SIMD_Map(void) { return _simd_simd_map;  }
    
    SIMD_AT* SIMD_At(void) { return _simd_at; }
    bool AT_Analysis_Phase(void);
    
    ALIGNMENT_TABLE *Alignment_Table() { return _alignment_table; }
    
    ST*  Create_Global_Alignment_Select(void);
    ST*  Store_Align(void)   { 
	if (_store_align) {
	    return _store_align;
	} else {
	    return Create_Global_Alignment_Select();
	}
    }
    
    ST*  Create_64bit_Integer_Var(INT64 val);

    /* Print */
    void Print_Vectra_Config(FILE *file);
    
    /* Member access */
    TYPE_ID V8x16type(void)   { return _v8x16type;    }
    TYPE_ID V4x32type(void)   { return _v4x32type;    }
    TYPE_ID V128x1type(void)  { return _v128x1type;   }
    TYPE_ID V8x20type(void)   { return _v8x20type;    }
    TYPE_ID V4x40type(void)   { return _v4x40type;    }
    TYPE_ID V160x1type(void)  { return _v160x1type;   }
    TYPE_ID Vsel_type(void)   { return _vsel_type;    }
    TYPE_ID Align_type(void)  { return _align_type;   }
    TYPE_ID Coeff_type(void)  { return _coeff_type;   }
    TYPE_ID S16type(void) const { return _s16type;    }
    TYPE_ID S32type(void) const { return _s32type;    }
    
    TYPE_ID     Get_Sel_Type(void) { return Vsel_type();  }
    
    /* Bitwidth of the narrow element in register */
    INT         Get_Narrow_Element_Reg_Bits(void) {
	return Type1_Reg_Bits();
    }
    
    /* Multiply element bits */
    INT         Get_Mul_In1_Bits(void) {
	return Mul_In1_Bits();
    }
    
    /* Bitwidth of the wide element in register */
    INT         Get_Wide_Element_Reg_Bits(void) {
	return Type2_Reg_Bits();
    }
    
    /* Bitwidth of the narrow element in memory */
    INT         Get_Narrow_Element_Mem_Bits(void) {
	return Type1_Mem_Bits();
    }
    
    /* Bitwidth of the wide element in memory */
    INT         Get_Wide_Element_Mem_Bits(void) {
	return Type2_Mem_Bits();
    }
    
    /* Bytewidth of the narrow element in memory */
    INT         Get_Narrow_Element_Mem_Bytes(void) {
	return Type1_Mem_Bytes();
    }
    
    /* Bytewidth of the wide element in memory */
    INT         Get_Wide_Element_Mem_Bytes(void) {
	return Type2_Mem_Bytes();
    }
    
    /* Bit width of SIMD register */
    INT         Get_Reg_Bits(void) {
	return Type1_Reg_Bits() * Type1_Simd();
    }
    
    /* Vector length for narrow element */
    INT         Get_Narrow_Reg_Length(void) {
	return Type1_Simd();
    }
    
    /* Vector length for wide element */
    INT         Get_Wide_Reg_Length(void) {
	return Type2_Simd();
    }
    
    INT         Vectra_Alignment(void) {
	Is_True(Type1_Mem_Bytes()*Type1_Simd()==
		Type2_Mem_Bytes()*Type2_Simd(),
		("Inconsistent Vectra Type 1 and Type 2 settings."));
	return (Type1_Mem_Bytes()*Type1_Simd());
    }
    
    /* Map from basic type to SIMD register type */
    virtual TYPE_ID Get_SIMD_Reg_Type_Scalar(TYPE_ID scalar_type);
    virtual TYPE_ID Get_SIMD_Mem_Type_Scalar(TYPE_ID scalar_type);
    virtual TYPE_ID Get_Alignment_Type_Scalar (TYPE_ID scalar_type) {
	return Align_type();
    }
    virtual TYPE_ID Get_SIMD_Xtbool(INT vl);
    
    virtual INT  Guard_Bits(TYPE_ID scalar_type, INT vl = 0);
    
    virtual TIE_MACRO_ID Get_Tie_Set_Vsar(void);
    virtual TIE_MACRO_ID Get_Tie_Set_Round(void);

    /* number of elements in a SIMD register */
    virtual INT Get_SIMD_Width_Scalar(TYPE_ID scalar_type);
    virtual INT Get_SIMD_Width_SIMD(TYPE_ID simd_type);
    
    /* number of memory bytes of a SIMD type corresponding to a scalar type */
    virtual INT Get_SIMD_Mem_Bytes_Scalar(TYPE_ID scalar_type);
    
    // number of element bits in SIMD register/memory
    INT         Get_Elem_Reg_Bits_SIMD(TYPE_ID simd_type);
    INT         Get_Elem_Mem_Bits_SIMD(TYPE_ID simd_type);
    
    // SIMD_SELECT object for the type
    virtual SIMD_SELECT *Get_SIMD_Select_Scalar(TYPE_ID scalar_type);
    virtual SIMD_SELECT *Get_SIMD_Select_SIMD(TYPE_ID simd_type);
    
    /* find a SIMD function in 'type' for 'op' corresponding the 'lohi' */
    virtual TIE_MACRO_ID Search_Vector_Macro(OPERATOR op,
					     TYPE_ID dtype, TYPE_ID rtype =MTYPE_UNKNOWN,
					     INT lohi = 0, bool update = false);
    
    /* find a SIMD function in 'type' for 'op' corresponding the 'lohi' */
    INTRINSIC   Find_Function(OPERATOR op,
			      TYPE_ID dtype, TYPE_ID rtype =MTYPE_UNKNOWN,
			      INT lohi = 0, bool update = false);
    
    virtual INTRINSIC Radd (TYPE_ID res_type, TYPE_ID desc_type);
    virtual INTRINSIC Rmin (TYPE_ID res_type, TYPE_ID desc_type);
    virtual INTRINSIC Rmax (TYPE_ID res_type, TYPE_ID desc_type);
    INTRINSIC   Find_Movar(TYPE_ID mtype);
    virtual INTRINSIC CMov(TYPE_ID rtype, TYPE_ID bool_type);

    virtual bool Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type);
    virtual bool Has_Paired_Mac(OPERATOR op, TYPE_ID out_type, 
				TYPE_ID in_type) { return false; }
    virtual bool Has_Even_Odd_Mac(OPERATOR op, TYPE_ID out_type, 
				  TYPE_ID in_type) { return true; }

    bool Has_Update_Load_Store(IMEM_GROUP *ig, bool load, bool store);
    bool Has_Update_Indexed_Load_Store(IMEM_GROUP *ig, bool load, bool sto);

    /* make intrinsic call according to SIMD type */
    WN         *Generate_Unary_Call(OPERATOR op, TYPE_ID type, WN *operand);
    WN         *Generate_Binary_Call (WN *stmt, OPERATOR op, SIMD_PREG *res_preg,
				      WN *operand1, WN *operand2);
    WN         *Generate_Pack_Call(TYPE_ID out_type,
				   WN *operand1, WN *operand2, WN* result);
    WN         *Generate_SRL_Call(OPERATOR op, WN *operand1, WN* result);
    WN         *Generate_Mac_Call(OPERATOR op, TYPE_ID type, WN *oprn1, 
				  WN *oprn2, INT lohi, WN *oprn3 = NULL, 
				  WN *oprn4 = NULL, WN *imm = NULL);
    WN         *Generate_Mulr_Call(OPERATOR op, TYPE_ID type, WN *oprn1, 
				   WN *oprn2, WN* oprn3, INT lohi);
    WN         *Generate_Load(OPERATOR op, TYPE_ID type, WN *stmt,
			      WN *addr, WN_OFFSET wn_offset, SIMD_PREG *target,
			      TYPE_ID desc);
    WN         *Generate_Store_Back(SIMD_LOOP *doinfo, SIMD_EINFO *einfo, 
				    WN *sto, WN *stmt, SIMD_PREG *valReg, 
				    INT inc = 0);
    WN         *Generate_CMov_Call(WN *operand1, WN *operand2, WN* result);
    
    /* Unaligned load support */
    virtual WN *Generate_LoadA_Prime (TYPE_ID scalar_type, WN *stmt, WN *addr,
				      SIMD_PREG *addr_reg, SIMD_PREG *target,
				      SIMD_PREG *align);
    WN *Generate_LoadA_Update (TYPE_ID scalar_type, WN *stmt, SIMD_PREG *addr,
			       SIMD_PREG *target, SIMD_PREG *align,
			       INT inc);
    WN *Generate_LoadA (TYPE_ID scalar_type, WN *stmt, WN *addr,
			SIMD_PREG *target, SIMD_PREG *align);
    
    /* Unaligned store support */
    virtual void Generate_StoreA_Prime (IMEM_GROUP *ig, WN *stmt);
    virtual void Generate_StoreA_Update (IMEM_GROUP *ig, WN *stmt,
					 SIMD_PREG *val, INT inc);
    virtual void Generate_StoreA_Flush (IMEM_GROUP *ig, WN *stmt, bool ins_after);    
    
    WN         *Generate_Update_Load(OPERATOR op, TYPE_ID rtype, TYPE_ID desc,
				     WN *stmt, SIMD_PREG *addr_preg,
				     SIMD_PREG *target, INT inc, 
				     INT vs = 0);
    WN         *Generate_Update_Store(TYPE_ID reg_type, TYPE_ID mem_type, 
				      WN *stmt, WN* lod_val, SIMD_PREG *addr,
				      INT inc, WN *block = NULL);
    WN         *Generate_Update_Indexed_Store(
	TYPE_ID reg_type, TYPE_ID mem_type, WN *stmt, WN* lod_val, 
	SIMD_PREG *addr, SIMD_PREG *index);
    WN         *Generate_LoadS_X(OPERATOR op, TYPE_ID type, TYPE_ID desc,
				 WN *stmt, SIMD_PREG *addr, SIMD_PREG *target, 
				 SIMD_PREG *idx, bool update);
    WN         *Generate_LoadV_X(OPERATOR op, TYPE_ID type, TYPE_ID desc,
				 WN *stmt, SIMD_PREG *addr, SIMD_PREG *target, 
				 SIMD_PREG *idx, bool update);
    
    void        Insert_Mul_Call (WN *stmt, WN *expr, SIMD_PREG *dest,
				 WN *src1, WN *src2, INT lohi = 0);
    
    void        Initialize_Rounding(WN *expr, WN *stmt);
    WN         *Initialize_VSAR(WN *stmt, WN *shift_amount);
    SIMD_PREG  *Gen_Align_Symbol(TYPE_ID scalar_type, MEM_POOL *pool); 
    SIMD_PREG  *Gen_Sel_Symbol(MEM_POOL *pool); 

  /* Add the message string 'msg' to the message set.
     Return 'false' if the message is already in the message set. */
  bool Add_Message_Str (const char *msg);
    
    /* create a scalar array to save vector scalar used in transforming
       vec = ...
       for (i = 0; i < 4; i++) {
         ... = ... extract(vec, i);
       }
       
       into:
       
       *(vec *)__v_se = ...
       for (i = 0; i < 4; i++) {
           ... = ... __vse[i];
       }
    */
    ST         *Create_SE_Array(TYPE_ID scalar_type, INT length);
    virtual void Shift_Into_Reg(SIMD_PREG *new_in, SIMD_PREG *res,
				WN *block, WN *stmt, INT64 line_num,
				INT shift_amount = 1, SIMD_PREG *target=NULL);

}; // SIMD_INFO

/*-------------------------------------------------------------------------*
 * A class to hold SIMD PREG information                                   *
 *-------------------------------------------------------------------------*/
class SIMD_PREG {
private:
    TYPE_ID     _mtype;
    PREG_NUM    _preg;

public:
    SIMD_PREG(TYPE_ID mtype, PREG_NUM preg);
    ~SIMD_PREG() {}
    
    /* MTYPE */
    TYPE_ID Type(void) { return _mtype;   }

    /* generate LDID off SIMD_PREG, update UD */
    WN *Simd_Preg_Ldid(void);

    WN *Simd_Preg_Ldid_With_EInfo(void);

    /* generate STID off SIMD_PREG */
    WN *Simd_Preg_Stid(WN* rhs);

    void Print(FILE *out_file = stderr, int indent = 0);
};

typedef DYN_ARRAY<SIMD_EINFO *> SIMD_EINFOS;

typedef enum {
    Eprop_Donot_Care  = 0,
    Eprop_Interleaved = 1,
    Eprop_Normal      = 2
} EINFO_PROP;

/*--------------------------------------------------------------------------*
 * SIMD expression information for SIMD code generation                     *
 *--------------------------------------------------------------------------*/
class SIMD_EINFO {
private:
    MEM_POOL *  _pool;                    /* memory pool                    */
    mINT16      _bit_size;                /* bit size of the expression,    */
    TYPE_ID     _res_type;                /* result type of original expression */
    SIMD_PREGS *_simdRegs;                /* SIMD reg of same size          */
    SIMD_PREGS *_simdIRegs;               /* Interleaved value of _simd2Reg */
    WN         *_expr;                    /* pointer back to the expr       */
    IMEM_INFO  *_imem_info;               /* pointer to shared IMEM_INFO    */
    SIMD_PREG  *_load_ahead;              /* load ahead register symbol     */
    SIMD_PREG  *_coef_reg;                /* coefficient register           */
    WN_ARRAY   *_simd_exprs;              /* SIMD expr after transformation */
    SIMD_EINFO *_kid_einfo[2];            /* Kid's SIMD_EINFO               */
    WN         *_pre_load;                /* alignment register preload     */
    INT         _depth_diff;              /* difference in depth for Dgraph */
    INT         _do_depth;                /* Do_Depth(_expr)                */
    bool        _double_size;             /* ever need double size ?        */
    bool        _update;                  /* update the address register    */
    bool        _store_back;              /* store back candidate           */
    bool        _interleaved;             /* is interleaved (e.g. after mul)*/
    bool        _need_interleave;         /* need to apply interleave       */
    bool        _need_deinterleave;       /* need to deinterleave           */
    bool        _round_reg;               /* is rounding register           */
    bool        _load_reuse;              /* is load reuse for load c/pair  */
    bool        _mulabc;                  /* is MULABC candidate            */
    bool        _mula;                    /* is MULA/MULS candidate         */
    bool        _reduction;               /* is reduction                   */
    bool        _is_vector;               /* is vector                      */
    bool        _shift_scale_moved;       /* shift scale moved out          */

    /*  This contains to all the uses for DU chain.  
	The UD chain are obtained from SIMD_EINFO(operand) */
    SIMD_EINFOS *_uses;                    
    SIMD_EINFOS *_defs;

public:
    /* Constructor and destructor */
    SIMD_EINFO(WN *expr, INT s, MEM_POOL *pool) : 
	_bit_size(s), _res_type(MTYPE_UNKNOWN), _expr(expr), _imem_info(NULL),
	_pool(pool), _double_size(false), _pre_load(NULL),
	_load_ahead(NULL), _coef_reg(NULL), _update(false),_store_back(false), 
	_interleaved(false),_need_interleave(false),_need_deinterleave(false),
	_round_reg(false), _load_reuse(false), _mulabc(false), _mula(false),
	_reduction(false), _depth_diff(0),
	_is_vector(false), _shift_scale_moved(false), _do_depth(::Do_Depth(expr))
    {
	_simdRegs  = CXX_NEW(SIMD_PREGS(Pool()),Pool());
	_simdIRegs = CXX_NEW(SIMD_PREGS(Pool()),Pool());
	_simd_exprs = CXX_NEW(WN_ARRAY(Pool()),Pool());
	_kid_einfo[0] = _kid_einfo[1] = NULL;
	_uses = CXX_NEW(SIMD_EINFOS(Pool()), Pool());
	_defs = CXX_NEW(SIMD_EINFOS(Pool()), Pool());

	/* set reduction */
	if (red_manager && red_manager->Which_Reduction(expr) != RED_NONE) {
	    Set_Reduction();
	}
    }

    SIMD_EINFO(WN *expr, SIMD_EINFO *e_info) :
	_bit_size(e_info->_bit_size), _res_type(e_info->_res_type),
	_expr(expr), _imem_info(NULL), _pool(e_info->_pool), 
	_double_size(false), _pre_load(NULL),
	_load_ahead(NULL), _coef_reg(NULL), _update(false),_store_back(false), 
	_interleaved(false),_need_interleave(false),_need_deinterleave(false),
	_round_reg(false), _load_reuse(false), _mulabc(false), _mula(false),
	_reduction(false), _depth_diff(0),
	_is_vector(e_info->_is_vector), _shift_scale_moved(false), 
	_do_depth(::Do_Depth(expr))
    {
	_simdRegs  = CXX_NEW(SIMD_PREGS(Pool()),Pool());
	_simdIRegs = CXX_NEW(SIMD_PREGS(Pool()),Pool());
	_simd_exprs = CXX_NEW(WN_ARRAY(Pool()),Pool());
	_kid_einfo[0] = _kid_einfo[1] = NULL;
	_uses = CXX_NEW(SIMD_EINFOS(Pool()), Pool());
	_defs = CXX_NEW(SIMD_EINFOS(Pool()), Pool());
	if (e_info->Interleaved() && !e_info->Need_Deinterleave()) {
	    Set_Interleaved();
	}
    }

    virtual
    ~SIMD_EINFO() {
	CXX_DELETE(_simdRegs,Pool());
	CXX_DELETE(_simdIRegs,Pool());
	CXX_DELETE(_simd_exprs,Pool());
	CXX_DELETE(_uses, Pool());
	CXX_DELETE(_defs, Pool());
    }
    
    /* Member access functions */
    INT         Bit_Size(void)               { return _bit_size;    }
    void        Set_BitSize(INT s)           { _bit_size = s;       }
    bool        Double_Size(void)            { return _double_size; }
    void        Set_Double_Size(void)        { _double_size = true; }
    void        Reset_Double_Size(void)      { _double_size = false;}

    TYPE_ID     Res_Type(void) const         { return _res_type;    }
    void        Set_Res_Type(TYPE_ID type)   { _res_type = type;    }

    MEM_POOL *  Pool(void)                   { return _pool;        } 

    SIMD_PREGS* Simd_Regs()                  { return _simdRegs;         }
    SIMD_PREG*  Simd_Reg(INT i = 0)          { return SIMD_PREGS_Get(Simd_Regs(),i); }
    void        Add_Simd_Reg(SIMD_PREG* st)  { Simd_Regs()->AddElement(st); }
    void        Set_Simd_Reg(INT i, SIMD_PREG* st)
                                             { SIMD_PREGS_Set(Simd_Regs(),i,st); }
    
    SIMD_PREGS* Simd_IRegs()                 { return _simdIRegs;   }
    SIMD_PREG*  Simd_IReg(INT i = 0)         { return SIMD_PREGS_Get(Simd_IRegs(),i); }
    void        Add_Simd_IReg(SIMD_PREG* st) { Simd_IRegs()->AddElement(st); }
    void        Set_Simd_IReg(INT i, SIMD_PREG* st)
                                             { SIMD_PREGS_Set(Simd_IRegs(),i,st); }

    INT         Reg_Count() 
    {
	return (Simd_Regs()->Elements()) ? 
	    Simd_Regs()->Elements() : Simd_IRegs()->Elements();
    }

    SIMD_PREG*  Load_Ahead(void)             { return _load_ahead;  }
    void        Set_Load_Ahead(SIMD_PREG* e)
                                             { _load_ahead = e;     }

    SIMD_PREG*  Coef_Reg(void)               { return _coef_reg;    }
    void        Set_Coef_Reg(SIMD_PREG* e)
                                             { _coef_reg = e;       }

    WN*         Pre_Load(void)               { return _pre_load;    }
    void        Set_Pre_Load(WN *wn)         { _pre_load = wn;      }

    bool        Updating(void)               { return _update;      }
    void        Set_Updating(void)           { _update = true;      }
    bool        Store_Back(void)             { return _store_back;  }
    void        Set_Store_Back(void)         { _store_back = true;  }
    bool        Is_Round_Reg(void)           { return _round_reg;   }
    void        Set_Is_Round_Reg(void)       { _round_reg = true;   }
    void        Reset_Is_Round_Reg(void)     { _round_reg = false;  }

    bool        Load_Reuse(void)             { return _load_reuse;  }
    void        Set_Load_Reuse(void)         { _load_reuse = true;  }

    bool        Interleaved(void)            { return _interleaved; }
    void        Set_Interleaved(void)        { _interleaved = true; }
    void        Reset_Interleaved(void)      { _interleaved = false; }

    bool        Need_Interleave(void)        { return _need_interleave; }
    void        Set_Need_Interleave(void)    { _need_interleave = true; }
    void        Reset_Need_Interleave(void)  { _need_interleave = false; }

    bool        Need_Deinterleave(void)      { return _need_deinterleave; }
    void        Set_Need_Deinterleave(void)  { _need_deinterleave = true; }
    void        Reset_Need_Deinterleave(void){ _need_deinterleave = false; }

    bool        Is_Vector(void)              { return _is_vector;         }
    void        Set_Is_Vector(void)          { _is_vector = true;         }
    void        Reset_Is_Vector(void)        { _is_vector = false;        }
    bool        Shift_Scale_Moved(void)      { return _shift_scale_moved; }
    void        Set_Shift_Scale_Moved(void)  { _shift_scale_moved = true; }
    void        Reset_Shift_Scale_Moved(void){ _shift_scale_moved = false;}

    bool        Mulabc(void)                 { return _mulabc;      }
    void        Set_Mulabc(void)             { _mulabc = true;      }

    bool        Mula(void)                   { return _mula;        }
    void        Set_Mula(void)               { _mula = true;        }

    bool        Reduction(void)              { return _reduction;   }
    void        Set_Reduction(void)          { _reduction = true;   }
    
    INT         Do_Depth(void)               { return _do_depth;    }

    INT         Depth_Diff(void)             { return _depth_diff;  }
    void        Set_Depth_Diff(INT d)        { _depth_diff = d;     }

    WN*         Expr(void)                   { return _expr;        }

    WN_ARRAY *  Simd_Exprs()                 { return _simd_exprs;  }
    WN*         Simd_Expr(INT i=0)           { return (*Simd_Exprs())[i]; }
    void        Add_Simd_Expr(WN *e)         { Simd_Exprs()->AddElement(e); }
    void        Set_Simd_Expr(INT i, WN *e)  { (*Simd_Exprs())[i]=e; }

    SIMD_EINFO *Kid_EInfo(INT i=0)           { return _kid_einfo[i];}
    void        Set_Kid_EInfo(SIMD_EINFO *e, INT i=0)
                                             { _kid_einfo[i] = e;   }

    SIMD_EINFOS*Uses(void)                   { return _uses;        } 
    void        Add_Use(SIMD_EINFO *e_info)  { Uses()->AddElement(e_info); }
    SIMD_EINFO *Use(INT i)                   { return (*Uses())[i]; }

    SIMD_EINFOS*Defs(void)                   { return _defs;        } 
    void        Add_Def(SIMD_EINFO *e_info)  { Defs()->AddElement(e_info); }
    SIMD_EINFO *Def(INT i)                   { return (*Defs())[i]; }
    
    void        Copy_Interleave_Property(SIMD_EINFO *o)
    {
	_interleaved       = o->Interleaved();
	_need_interleave   = o->Need_Interleave();
	_need_deinterleave = o->Need_Deinterleave();
    }

    IMEM_INFO*  IMem(void)                  { return _imem_info;   }
    void        Set_IMem(IMEM_INFO *im)     { _imem_info = im;     }

    /* single use */
    bool        Single_Use(void);
    /* is over written as in 's = s +/- ...' */
    bool        Is_Over_Written(void);

    /* copy from one SIMD_INFO to 'this' */
    void        Copy_Simd_Reg_Info(SIMD_EINFO *e_old);
    void        Copy_Simd_Reg_Valid_Info(SIMD_EINFO *e_old);

    /* pick SIMD register according to parent's interleave prop requirement */
    SIMD_PREG*   Pick_Simd_Reg(EINFO_PROP prop, INT hilo = 0);
    void         Set_Pick_Reg_Valid(EINFO_PROP prop, INT hilo = 0);

    /* get the result register to save an expression's initial result       */
    SIMD_PREG*   Get_Res_Reg(INT hilo = 0);
    /* set result register value valid bit                                  */
    void         Set_Res_Reg_Valid();

    /* get the register containing the final result of an EINFO             */
    SIMD_PREG*   Get_Final_Reg(INT hilo = 0);
    
    /* Requiring property of operand                                        */
    EINFO_PROP   Get_Required_Operand_Prop(bool is_madd = false);

    /* Interleave/Deinterleave if required */
    void         Simd_Post_Processing(WN* stmt, SIMD_INFO *simd, 
				      SIMD_LOOP *doinfo);
    /* copy simd_reg to all required simd registers so they are the same    */
    void    Duplicate_To_All(INT vec_length, SIMD_PREG *simd_preg);

    /* Initialization into SIMD register of same size */
    void    Init_Simd_Reg_Ldid(WN *stmt, SIMD_LOOP *simd_loop,
			       SIMD_INFO *simd, MEM_POOL *pool);
    
    /* Loop invariant initialization of SIMD register */
    void    Init_Simd_Reg_Invariant(WN *stmt, SIMD_LOOP *simd_loop, 
				    SIMD_INFO *simd, MEM_POOL *pool);
    
    /* Interleave _simdReg[0] to _simdIReg[0] */
    virtual void Simd_Interleave_One(WN *stmt, SIMD_INFO *simd, 
				     SIMD_LOOP *doinfo);
    /* Interleave _simdReg[0], _simdReg[1] to _simdIReg[0], _simdIReg[1],
       or _simdReg[0] to _simdIReg[0]                                       */
    virtual void Simd_Interleave(WN *stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo);

    /* Deinterleave _simdIReg[0] to _simdReg[0] */
    virtual void Simd_Deinterleave_One(WN *stmt, SIMD_INFO *simd, 
				       SIMD_LOOP *doinfo, WN *block = NULL);
    /* Deinterleave _simdIReg[0], _simdIReg[1] to _simdReg[0], _simdReg[1],
       or _simdIReg[0] to _simdReg[0]                                       */
    virtual void Simd_Deinterleave(WN *stmt, SIMD_INFO *simd, 
				   SIMD_LOOP *doinfo, WN *block = NULL);

    void    Fold_Deinterleave(void);
    bool    Better_Move_Deinterleave_Up(void);
    bool    Better_Move_Interleave_Down(void);

    /* Generate load from SIMD register of same type */
    WN     *Gen_Lod_Reg(EINFO_PROP prop, INT idx = 0);
    
    /* Generate expr result into vector register */
    virtual
    void Generate_Simd_Expr(bool unary, WN *expr, WN *stmt, 
			    SIMD_EINFO *c1_info, SIMD_INFO *simd,
			    SIMD_EINFO *c2_info = NULL, INT idx = 0);

    void Generate_Compare_Expr(OPERATOR op, WN *expr, WN *stmt,
			       SIMD_EINFO *c1_info, SIMD_INFO *simd,
			       SIMD_EINFO *c2_info, INT idx, bool &negated);

    /* Generate mula/muls result into vector register */
    virtual WN*  Generate_Mula_Expr(WN *expr, WN *stmt, 
				    SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
				    SIMD_EINFO *s2_info, WN *mul, 
				    SIMD_INFO *simd, INT idx = 0);
    virtual void Generate_Mul_Expr(WN *expr, WN *stmt,
				   SIMD_EINFO *c1_info, SIMD_EINFO *c2_info,
				   SIMD_INFO *simd);
    WN*  Generate_Load_Ahead(WN *simd_expr, SIMD_INFO *simd, MEM_POOL *pool,
			     bool insert_after);
    virtual void Generate_Mula(WN *expr,
			       WN *child2, WN *stmt, 
			       SIMD_EINFO *c1_info, SIMD_LOOP *doinfo,
			       SIMD_INFO *simd);
    void Generate_CMov_Expr(WN *expr, WN *stmt, SIMD_EINFO *cond_info,
			    SIMD_EINFO *t_info, SIMD_EINFO *f_info, 
			    SIMD_INFO *simd, INT idx=0);

    /* Generate WC expression */
    void Generate_Write_Coef(WN *stmt, SIMD_INFO *simd);

  virtual void Init_Sum_Reduction (WN *stmt);
};

extern void SIMD_EINFO_Add_Def_Use(SIMD_EINFO *def_einfo, 
				   SIMD_EINFO *use_einfo);

/*--------------------------------------------------------------------------*
 * A class to help inner loop unroll                                        *
 *--------------------------------------------------------------------------*/
class E_Candidate {
    EINFO_Stack *_candidate;      /* candidate stack */
    WN          *_cur_wn;         /* current wn      */
    SIMD_EINFO  *_cur_einfo;      /* current e_info  */
    INT          _pos;            /* current pointer */

public:
    E_Candidate(EINFO_Stack *can);
    WN         *Get_Next(void);
    WN         *Cur_Wn(void)    { return _cur_wn; }
    SIMD_EINFO *Cur_EInfo(void) { return _cur_einfo; }
};    

/*--------------------------------------------------------------------------*
 * A class to hold pending store motion candidates                          *
 *--------------------------------------------------------------------------*/
class MOVE_RANGE {
    WN *_move_after;                        /* the range of WN* to    */
    WN *_move_end;                          /* (move_after, move_end] */

public:
    MOVE_RANGE(WN *a, WN *e): _move_after(a), _move_end(e) {}
    ~MOVE_RANGE(){}
    WN *Move_After(void)                     { return _move_after; }
    WN *Move_End(void)                       { return _move_end;   }
};

typedef STACK<MOVE_RANGE*> MR_Stack;

class MOVE_CAND {
    WN       *_to_loop;                    /* to which loop          */
    MR_Stack  _cand;                       /* candidates             */

public:
    MOVE_CAND(WN *loop, MEM_POOL *pool) : _to_loop(loop), _cand(pool) {}
    ~MOVE_CAND() {}
    
    WN         *To_Loop(void)                { return _to_loop;    }
    MR_Stack&   Cand(void)                   { return _cand;       }
};

/*---------------------------------------------------------------------------*
 * For moving shift_scale out of a loop                                      *
 *                                                                           *
 * for (...) {                                                               *
 *    x = x + (a * b >> scale);                                              *
 * }                                                                         *
 *                                                                           *
 * x is a vector                                                             *
 *                                                                           *
 * v_x = 0;                                                                  *
 * for (...) {                                                               *
 *   v_x = v_x + a*b;                                                        *
 * }                                                                         *
 * v_x = v_x >> scale;                                                       *
 *---------------------------------------------------------------------------*/
struct SHIFT_SCALE {
    WN  *st_wn;            /* the store wn                              */
    SIMD_EINFO *e_info;    /* the EINFO                                 */
    WN  *ld_wn;            /* load wn                                   */
    WN  *shift;            /* shift wn                                  */
    WN  *shift_scale;      /* the shift_scale                           */
    WN  *after_use;        /* use after loop                            */
    WN  *init_wn;          /* init value of the reduction               */
    bool has_inner_use;    /* has uses in the same loop or further down */
    bool has_multiple_use; /* has multiple uses                         */
    bool has_inner_def;    /* has defs in the same loop or further down */
    bool init_is_zero;     /* initial value is zero                     */
    bool shift_is_ok;      /* shift is not changed in the loop          */

    bool Use_Ok(void)        { return (shift_scale && !has_inner_use); }
    bool Def_Use_Ok(void)    { return (Use_Ok() && !has_inner_def);    }
	
    bool Ok_To_Combine(void) { return (Def_Use_Ok() && shift_is_ok &&
				       after_use && 
				       (WN_operator(after_use) == OPR_LDID) &&
				       !has_multiple_use && init_is_zero); }
    bool Ok_To_Move(void)    { return (Def_Use_Ok() && shift_is_ok);   }

    SHIFT_SCALE(WN *wn_in, SIMD_EINFO *e_info_in) :
	st_wn(wn_in), e_info(e_info_in), ld_wn(NULL), shift(NULL), 
	shift_scale(NULL), after_use(NULL), init_wn(NULL), 
	has_inner_use(false), has_multiple_use(false), has_inner_def(false),
	init_is_zero(false), shift_is_ok(false)
    {}
};

class SIMD_IF;
typedef HASH_TABLE<WN*, SIMD_IF*> SIMD_IF_Map;

class SIMD_IF_CONV;
typedef HASH_TABLE<WN*, SIMD_IF_CONV*> SIMD_IF_CONV_Map;

typedef HASH_TABLE<WN*, SIMD_IF_ANA*> SIMD_IF_ANA_Map;

/*--------------------------------------------------------------------------*
 * A class to hold SIMD transformation related data structures              *
 *--------------------------------------------------------------------------*/
class SIMD_LOOP {

protected:

    enum FLAG {
	NONE        =  0x00,                    
	PEEL_ALIGN  =  0x01,                /* peel to align access  */
	PEELED      =  0x02,                /* loop's been peeled    */
	HAS_S_DEP   =  0x04,                /* scalar dependence     */ 
	HAS_CALL    =  0x08,                /* has call              */
	BAD_MEM     =  0x10,                /* has bad memory access */
	HAS_DEP     =  0x20,                /* has array dependence  */
	BAD_TYPE    =  0x40,                /* has bad type          */
	ELM_SZ_N    =  0x80,                /* seen narrow type      */
	ELM_SZ_W    = 0x100,                /* seen wide type        */
	BAD_OPER    = 0x200,                /* has bad operator      */
	USED_ROUND  = 0x400,                /* used rounding register*/
	D_OVERFLOW  = 0x800,                /* dgraph overflow       */
	OUTER_UNR   = 0x1000,               /* outer loop unrolled   */
	N_TO_W      = 0x2000,               /* narrow to wide        */
	TOO_SMALL   = 0x4000,               /* small trip count      */
	BAD_SHIFT   = 0x8000,               /* loop variant shift    */
	BAD_STRIDE  = 0x10000,              /* bad write stride      */
	STRUCT_GAP  = 0x20000,              /* gaps in struct        */
	TRAPEZOIDAL = 0x40000,              /* has trapezoidal inner */

	/* last    0x80000000  for 32 bits total                     */
    };

    enum LEVEL_FLAG {
	HAS_CYCLE   =  0x01,                /* has cycle             */
	
	/* last 0x8000 for 16 bits total                             */
    };

    MEM_POOL         *_pool;                /* MEM pool              */
    LOOP_MODEL       *_lm;                  /* SNL loop model        */
    INT               _num_loops;           /* number of loops       */
    INT               _num_good;            /* number of good enclosing loop */

    INT               _max_vl;

    INVAR_TABLE      *_invar_table;         /* loop invariant table  */

    EINFO_Stack     **_unroll_candidate;    /* transform candidate   *
                                             * for inner unroll      */
    EINFO_Stack       _scalar_redu;         /* scalar reductions     */

    EINFO_Stack       _all_einfo;           /* all the SIMD_EINFO    */

    STACK<SHIFT_SCALE*>
                      _shift_scale;         /* moved shift scale     */

    STACK<MOVE_CAND*> _move_cand;           /* store motion candidate*/

    INT              *_simd_unroll;         /* vector unroll amount  */
    WN              **_permloop;            /* loop after permutation*/
    mUINT16          *_level_flag;          /* flags for each level  */

    UINT32            _flag;                /* various flags         */

    SIMD_EMap         _eInfo;               /* simd expr info        */
    IMEM_Map         *_imem_map;            /* mem op-> IMEM_INFO    */

    VERTEX_STACK      _orig_ver;            /* Original vertex stack */
    
    WN               *_VSAR;                /* VSAR amount           */
    WN               *_VSAR_expr;           /* VSAR expression       */
    INT               _VSAR_cnt;            /* number of VSAR val    */
    
    WN               *_round_expr;          /* EXPR in roundoff      */
    WN               *_simd_loop;           /* which loop to unroll  */
    INT               _simd_loop_level;     /* depth of simd loop    */

    WN               *_td_rem;              /* 2D remainder loop     */

    /* constant variable map */
    SIMD_CONST_Map   *_const_map;         /* from result_type and value to SIMD_PREG */

    SIMD_SMap         _scalar_map;        /* from scalar to SIMD_EINFO* */
    SINFO_Map         _sinfo_map;         /* from scalar to SCALAR_INFO* */
    SINFO_Stack       _sinfo_stack;       /* all SCALAR_INFO* */

    SIMD_SMap         _cur_scalar;        /* from scalar to SIMD_EINFO* */
    SIMD_SMap         _redu_scalar;       /* reduction scalar           */

    /* old statement to be deleted after transformation */
    STACK<WN*>        _old_stmt;         
    STACK<WN*>        _inv_ldid;          /* invariant LDIDs            */

    /* post loop finalization statements, for trapezoidal transformation */
    STACK<WN*>        _post_loop;         

    PREG_NUM          _first_preg;        /* first SIMD PREG            */
    PREG_NUM          _last_preg;         /* last SIMD PREG             */
    
    bool              WN_Is_Simd_Preg(WN *wn) {
	return (WN_class(wn) == CLASS_PREG && 
		_first_preg <= WN_offset(wn) && WN_offset(wn) <= _last_preg);
    }

    SIMD_IF_Map       _if_map;            /* if conversion map          */
    SIMD_IF_CONV_Map  _if_conv_map;       /* if conversion map          */
    SIMD_IF_ANA_Map   _if_ana_map;        /* if convertion analysis map */
    SIMD_IF_CONV     *_if_conv;           /* current SIMD_IF_CONV       */

    /* Helper functions for SIMD transformation */
    void Generate_Simd_Reg_Info(SIMD_EINFO *e_info, SIMD_INFO *simd);
    void Generate_Simd_IReg_Info(SIMD_EINFO *e_info, SIMD_INFO *simd);

    void Copy_Simd_Reg_Info(SIMD_EINFO *e_old);
    SIMD_EINFO* Simd_Transform_Scalar(WN *, WN *, SIMD_INFO *);
    SIMD_EINFO* Simd_Transform_Indirect(WN *, WN *, SIMD_INFO *);
    SIMD_EINFO* Simd_Transform_Unary(WN *, WN *, SIMD_INFO *);
    SIMD_EINFO* Simd_Transform_Binary(WN *expr, WN *stmt, SIMD_INFO *simd);
    SIMD_EINFO* Simd_Transform_Constant(WN *expr, SIMD_INFO *simd, bool double_size=false);
    
    virtual SIMD_EINFO* Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd);

    SIMD_EINFO* Simd_Transform_Select(WN *, WN*, SIMD_INFO *);
    SIMD_EINFO* Simd_Transform_Compare(WN *, WN*, SIMD_INFO *, bool &);

    void Simd_Transform_Block(WN *blk, SIMD_INFO *simd, WN *result);
    void Simd_Transform_If(WN *orig_if, SIMD_INFO *simd, WN *copy_if);
    void Simd_Transform_Loop_Body(WN *loop, SIMD_INFO *simd, WN *copy_loop);
    WN*  Combine_Load(SIMD_EINFO *e_info, WN *stmt, SIMD_INFO *simd);
    WN*  Combine_Mulabc(SIMD_EINFO *e_info, WN *stmt, SIMD_INFO *simd, INT imm,
			SIMD_EINFO *s1_info, SIMD_EINFO *s2_info);

    void Initialize_Loop_Invariant(void);

    /* Generate load ahead and mulabc */
    void Operator_Combine_Unrolled_Loop(WN **unroll_body, 
					INT u,
					SIMD_INFO *simd,
					INT level);

    void Operator_Combine_Unrolled_Loop(WN **bodies,
					INT u,
					SIMD_INFO *simd, 
					E_Candidate &cand,
					WN **next_bodies = NULL);

    /* preprocessing to setup mula/mulabc */
    void Simd_Find_Mulabc(SIMD_EINFO *e_info, WN *wn, WN *child2, 
			  SIMD_EINFO *c1_info);
    void Simd_Preprocess_Interleave(void);

    /* Update DU routines */
    void Simd_Build_Du_Info_Loop(WN *loop, SCALAR_STACK *reads,
				 SCALAR_STACK *writes);
    void Simd_Build_Du_Info_Block(WN *blk, SCALAR_STACK *reads,
				  SCALAR_STACK *writes);
    void Simd_Exclude_Covered_Read(SCALAR_STACK *local_rw, 
				   SCALAR_STACK *loop_reads_c, 
				   SCALAR_STACK *loop_reads);
    void Simd_Build_Du_Info_Wn(WN *wn, SCALAR_STACK *rw, 
			       SCALAR_STACK *loop_w);
    void Simd_Build_Du_Chain(WN *wn, SCALAR_STACK *rw, SCALAR_STACK *loop_w);
    void Simd_Build_Backedge_Du(SCALAR_STACK *rw, SCALAR_STACK *loop_r,
				SCALAR_STACK *loop_w);
    void Simd_Add_Du(SCALAR_STACK *reads, SCALAR_STACK *writes);
    void Simd_Update_Global_Scalar(SCALAR_STACK *f_st, SCALAR_STACK *t_r,
				   SCALAR_STACK *t_w);

    virtual WN* Generate_Sum_Reduction (TYPE_ID scalar_type, SIMD_PREG *vec_reg,
					WN *block, INT64 line_number);
    virtual WN* Generate_Reduction (TYPE_ID scalar_type, SIMD_PREG *reg, OPERATOR op,
				    WN *stmt, INT64 line_number);
    void Simd_Finalize_Scalar_Reduction(WN *loop, SIMD_INFO *simd, WN *stmt);
    void Simd_Finalize_Scalar_Expansion(WN *loop, SIMD_INFO *simd, WN *stmt);
    WN*  Simd_Finalize_Shift_Scale(WN *loop, SIMD_INFO *simd, WN *stmt,
				   SHIFT_SCALE *ss);
    WN*  Substitute_Scalar_Expansion_Ldid(WN *wn);
    WN*  Substitute_Scalar_Expansion_Stid(WN *wn);
    
    
    // MULR reassociation
    INT  Roundoff_Reassociable(WN *wn);
    void Mulr_Reassociation(WN *kid0, WN *kid1, WN *wn);
    void Simd_Reassociate_Mulr(WN *wn);
    void Move_Shift_Scale_Out(WN *wn, SIMD_EINFO *einfo);

public:
	
    /* Constructor and destructor */
    SIMD_LOOP(MEM_POOL *pool, LOOP_MODEL *lm,
	      WN *simd_loop, INT simd_loop_level);

    typedef HASH_TABLE<WN *, VINDEX16> WN_VIDX_MAP;
    
    /* Member access functions ----------------------------------------- */
    MEM_POOL*      Pool(void)               { return _pool;               }
    LOOP_MODEL*    Loop_Model(void)         { return _lm;                 }
    INT            Num_Loops(void)          { return _num_loops;          }
    INT            Num_Good(void)           { return _num_good;           }
    INT*           Simd_Unroll(void)        { return _simd_unroll;        }
    WN*            TD_Rem_Loop(void)        { return _td_rem;             }
    void           Set_TD_Rem_Loop(WN *l)   { _td_rem = l;                }
    EINFO_Stack   &Scalar_Redu(void)        { return _scalar_redu;        }
    EINFO_Stack   &All_EInfo(void)          { return _all_einfo;          }
    STACK<SHIFT_SCALE*>
                  &Shift_Scale(void)        { return _shift_scale;        }
    STACK<MOVE_CAND*>  &Move_Cand(void)     { return _move_cand;          }
    void           Set_Loop_Model(LOOP_MODEL *lm) { _lm = lm;             }

    INT            Max_Vector_Length (void) const { return _max_vl; }
    void           Set_Max_Vector_Length (INT vl) { _max_vl = vl; }

    void           Set_Invar_Table(INVAR_TABLE *a) { _invar_table = a;    }
    INVAR_TABLE   *Invar_Table(void)        { return _invar_table;        }

    void           Add_Move_Candidate(WN *loop, WN *a, WN *e);

    MOVE_CAND*     Get_Move_To(WN *loop);

    void           Push_Unroll_Candidate(INT j, SIMD_EINFO *einfo)
    { 
	_unroll_candidate[j]->Push(einfo);   
    }

    EINFO_Stack*   Unroll_Candidate(INT j)  { return _unroll_candidate[j];}

    bool           Level_Has_Cycle(INT l) {
	return (_level_flag[l] & HAS_CYCLE);
    }
    void           Set_Level_Has_Cycle(INT l) {
	_level_flag[l] |= HAS_CYCLE;   
    }

    WN*            VSAR(void )               { return _VSAR;               }
    void           Set_VSAR(WN* amount)      { _VSAR = amount;
                                               _VSAR_cnt++;                }
    INT            VSAR_Cnt(void)            { return _VSAR_cnt;           }

    WN*            VSAR_Expr(void)           { return _VSAR_expr;          }
    void           Set_VSAR_Expr(WN *e)      { _VSAR_expr = e;             }
    
    WN*            Round_Expr(void)          { return _round_expr;         }
    void           Set_Round_Expr(WN *e)     { _round_expr = e;            }

    WN*            Simd_Loop(void)           { return _simd_loop;          }
    void           Set_Simd_Loop(WN *l)      { _simd_loop = l;             }
    INT            Simd_Loop_Level(void)     { return _simd_loop_level;    }
    void           Set_Simd_Loop_Level(INT l){ _simd_loop_level = l;      }

    WN*            Permloop(INT i)           { return _permloop[i];        }
    void           Set_Permloop(WN** p)      { _permloop = p;              }

    SIMD_CONST_Map *Const_Map(void)         { return _const_map;          }

    STACK<WN*>&    Old_Stmt(void)           { return _old_stmt;           }
    STACK<WN*>&    Inv_Ldid(void)           { return _inv_ldid;           }

    STACK<WN*>&    Post_Loop(void)          { return _post_loop;          }

    SIMD_SMap&     Scalar_Map(void)         { return _scalar_map;         }

    SINFO_Map&     S_Info_Map(void)         { return _sinfo_map;          }

    SINFO_Stack&   S_Info_Stack(void)       { return _sinfo_stack;        }

    SIMD_SMap&     Cur_Scalar(void)         { return _cur_scalar;         }

    SIMD_SMap&     Redu_Scalar(void)        { return _redu_scalar;        }

    /* Indirect memory load/store information -------------------------- */
    IMEM_Map&      IMem_Map(void)           { return *_imem_map;          }

    SIMD_PREG*     Create_Sel_Preg (INT64 val);
    
    PREG_NUM  First_Simd_Preg_Num(void)           { return _first_preg;      }
    void      Set_First_Simd_Preg_Num(PREG_NUM p) { _first_preg = p;         }

    PREG_NUM  Last_Simd_Preg_Num(void)            { return _last_preg;       }
    void      Set_Last_Simd_Preg_Num(PREG_NUM p)  { _last_preg = p;          }

    SIMD_IF_Map &If_Map(void)                     { return _if_map;          }
    SIMD_IF_CONV_Map &If_Conv_Map(void)           { return _if_conv_map;     }
    SIMD_IF_ANA_Map  &If_Ana_Map(void)            { return _if_ana_map;      }
    SIMD_IF_CONV     *If_Conv(void)               { return _if_conv;         }
    SIMD_IF_CONV     *Set_If_Conv(SIMD_IF_CONV *t){ _if_conv = t;            }

    /* set VSAR = shift_amount before 'stmt' */
    void           Generate_VSAR_Amount(WN *shift_amount, WN *stmt, 
					SIMD_INFO *simd);

    /* Original vertex stack              ------------------------------ */
    VERTEX_STACK&  Original_Vertices(void)  { return _orig_ver;           }

    /* find the corresponding symbol for a constant */
    SIMD_PREG*     Find_Constant(WN *expr, INT bitsize);

    /* Find the corresponding symbol for a constant. 
       If none exists, generate a new SIMD_PREG to hold the constant
       and an assignment in the loop header */
    SIMD_PREG*     Get_Constant(WN *expr, TYPE_ID val_type, SIMD_INFO *simd); 

    /* SIMD_EINFO map   ------------------------------------------------ */
    SIMD_EMap&     E_Info(void)             { return _eInfo;              }

    /* Get an expr's SIMD_EINFO ---------------------------------------- */
    SIMD_EINFO*    Get_E_Info(WN *expr)
    {
	return E_Info().Find(expr);
    }

    /* Flags ----------------------------------------------------------- */
    bool           Peel_Align(void)         { return (_flag & PEEL_ALIGN) != 0; }
    void           Set_Peel_Align(void)     { _flag |= PEEL_ALIGN;              }
    void           Reset_Peel_Align(void)   { _flag &= ~PEEL_ALIGN;             }

    bool           Peeled(void)             { return (_flag & PEELED)!= 0;   }
    void           Set_Peeled(void)         { _flag |= PEELED;               }
    void           Reset_Peeled(void)       { _flag &= ~PEELED;              }

    bool           Has_Scalar_Dep(void)     { return (_flag & HAS_S_DEP)!=0; }
    void           Set_Has_Scalar_Dep(void) { _flag |= HAS_S_DEP;            }
    void           Reset_Has_Scalar_Dep(void){ _flag &= ~HAS_S_DEP;          }

    bool           Has_Call(void)           { return (_flag & HAS_CALL)!= 0; }
    void           Set_Has_Call(void)       { _flag |= HAS_CALL;             }
    void           Reset_Has_Call(void)     { _flag &= ~HAS_CALL;            }

    bool           Has_Bad_Mem(void)        { return (_flag & BAD_MEM) != 0; }
    void           Set_Has_Bad_Mem(void)    { _flag |= BAD_MEM;              }
    void           Reset_Has_Bad_Mem(void)  { _flag &= ~BAD_MEM;             }

    bool           Has_Dep(void)            { return (_flag & HAS_DEP)!=0;   }
    void           Set_Has_Dep(void)        { _flag |= HAS_DEP;              }
    void           Reset_Has_Dep(void)      { _flag &= ~HAS_DEP;             }

    bool           Has_Bad_Type(void)       { return (_flag & BAD_TYPE) != 0;}
    void           Set_Has_Bad_Type(void)   { _flag |= BAD_TYPE;             }
    void           Reset_Bad_Type(void)     { _flag &= ~BAD_TYPE;            }

    bool           Too_Small(void)          { return (_flag & TOO_SMALL) != 0;}
    void           Set_Too_Small(void)      { _flag |= TOO_SMALL;             }
    void           Reset_Too_Small(void)    { _flag &= ~TOO_SMALL;            }

    bool           Bad_Shift(void)          { return (_flag & BAD_SHIFT) != 0;}
    void           Set_Bad_Shift(void)      { _flag |= BAD_SHIFT;             }
    void           Reset_Bad_Shift(void)    { _flag &= ~BAD_SHIFT;            }

    bool           Bad_Stride(void)         { return (_flag & BAD_STRIDE)!= 0;}
    void           Set_Bad_Stride(void)     { _flag |= BAD_STRIDE;            }
    void           Reset_Bad_Stride(void)   { _flag &= ~BAD_STRIDE;           }

    bool           Struct_Gap(void)         { return (_flag&STRUCT_GAP) != 0; }
    void           Set_Struct_Gap(void)     { _flag |= STRUCT_GAP;            }
    void           Reset_Struct_Gap(void)   { _flag &= ~STRUCT_GAP;           }

    bool           Trapezoidal(void)        { return (_flag&TRAPEZOIDAL) != 0;}
    void           Set_Trapezoidal(void)    { _flag |= TRAPEZOIDAL;           }
    void           Reset_Trapezoidal(void)  { _flag &= ~TRAPEZOIDAL;          }

    bool           Has_Narrow_Element_Size(void) 
                                            { return (_flag & ELM_SZ_N);     }
    bool           Has_Wide_Element_Size(void) 
                                            { return (_flag & ELM_SZ_W);     }
    void           Set_Has_Narrow_Element_Size(void)
                                            { _flag |= ELM_SZ_N;             }
    void           Set_Has_Wide_Element_Size(void)
                                            { _flag |= ELM_SZ_W;             }

    virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p);  
    virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info);  
    virtual INT Set_V_Unroll_Factor(INT factor) {}
    virtual INT V_Unroll_Factor(SIMD_INFO *simd =NULL) {
	if (Has_Narrow_Element_Size() || Has_N_To_W()) {
	    return simd->Get_Narrow_Reg_Length();
	} else if (Has_Wide_Element_Size()) {
	    return simd->Get_Wide_Reg_Length();
	} else {
	    return 0;
	}
    }

    /* Number of SIMD registers to hold the unroll amount */
    INT            Vec_Length(SIMD_INFO *simd, TYPE_ID res_type)
    {
	INT l = V_Unroll_Factor(simd) / simd->Get_SIMD_Width_Scalar(res_type);
	Is_True(l > 0, ("Vec_Length is zero"));
	return l;
    }

    bool           Bad_Operator(void)       { return (_flag & BAD_OPER);     }
    void           Set_Bad_Operator(void)   { _flag |= BAD_OPER;             }
    void           Reset_Bad_Operator(void) { _flag &= ~BAD_OPER;            }

    void           Set_Bad_Oper_Msg (const WN *wn);
    void           Set_Bad_Oper_Msg (AT_MSG_ID msg_id, const WN *wn, ...);

    bool           Used_Round_Reg(void)     { return (_flag & USED_ROUND);   }
    void           Set_Used_Round_Reg(void) { _flag |= USED_ROUND;           }
    void           Reset_Used_Round_Reg(void){ _flag &= ~USED_ROUND;         }

    bool           DGraph_Overflow(void)    { return (_flag & D_OVERFLOW);   }
    void           Set_DGraph_Overflow(void){ _flag |= D_OVERFLOW;           }

    bool           Outer_Unrolled(void)     { return (_flag & OUTER_UNR);    }
    void           Set_Outer_Unrolled(void) { _flag |= OUTER_UNR;            }

    bool           Has_N_To_W(void)         { return (_flag & N_TO_W);       }
    void           Set_Has_N_To_W(void);     

    /* SIMD compute expression bit size */
    virtual INT    Screen_Operator_Compute_Size(WN*, SIMD_IF_ANA *);

    virtual SIMD_EINFO *Simd_Preprocess_Transform(WN* wn);

    INT            Compute_Size(WN*);

    INT            Get_Max_Def_Bits(WN *);

    INT64  Max_Iteration_Count(void);

    // annotate vector and alignment information on the array reference and symbol
    // nodes
    void           Annotate_Vector_Symbol_Tree(SYMBOL_TREE_NODE *,
					       LNO_REGCLASS_INFO *);
    void           Annotate_Vector_Info(ARRAY_REF *, SYMBOL_TREE *);

    void           Record_Dependence_Subgraph(
	BOOL *can_be_unrolled,                  // unroll candidate
	STACK<WN*> *v_to_wn,                    // vertex to wn map
	WN_VIDX_MAP *wn_to_v,                   // wn to vertex map
	ARRAY_DIRECTED_GRAPH16 *dg,             // array dependence graph    
	EDGE_STACK *edge_on_level               // output: edges for model loop level
	);
    
    // quick checks
    bool          Test_Model_Loop(DOLOOP_STACK *dl_stack);
    bool          Test_Scalar_Dependence_Ok(); // scalar dependences

    virtual bool Test_If_Conversion (void);

    /* Return true if any enclosing loop has SIMD_If_Convert pragma. */
    static bool Has_If_Convert_Pragma (WN *wn);

    // test load/store support: invariant levels, groups, alignment, etc.
    virtual bool  Test_Imem_Alignment (void);
    virtual bool  Test_Imem_Field_Selection (void);
    bool  Test_Imem (void);

    // test loads_xu for variable stride support
    virtual bool  Test_Imem_Load_Variable_Stride(IMEM_GROUP *ig);

    // test loads_iu for reuse support
    virtual bool  Test_Imem_Load_Reuse_Supported(IMEM_GROUP *ig);

    // Array dependences
    bool          Test_Array_Dependences(BOOL *can_be_unrolled, 
					 INT outmost_can_be_tiled,
					 ARRAY_DIRECTED_GRAPH16 *dg);
    bool          Test_Array_Reduction (SCC_DIRECTED_GRAPH16 *level_dg,
					STACK<WN*> *v_to_wn);

    // Test_Vectorization is the driver for all the vectorization tests
    bool          Test_Vectorization(BOOL *can_be_unrolled, 
				     INT outmost_can_be_tiled,
				     ARRAY_DIRECTED_GRAPH16 *dg,
				     DOLOOP_STACK *dl_stack);
    
    IMEM_INFO*     Add_Imem(SIMD_EINFO *e_info, WN *wn);
    
    // SIMD transformation setup for a given loop model
    // This is the last chance of the vectorizer to bail out
    bool           Setup_With_Loop_Model();
    virtual void Setup_Peeling (void);
    
    // Check if it is ok for 'inner' to be the loop
    bool           Is_Inner_Ok (INT inner, bool msg=false);

    /* peeling for store alignment */
    SX_INFO*       Simd_Loop_Peeling(WN *loop, SX_INFO *pinfo, WN* permloop[],
				     SIMD_INFO *simd, MEM_POOL *pool, INT simd_u);
    /* Check for no-op CVT/CVTL */
    bool Is_Noop_Cvt (WN *wn);

    // setup type conversion -- different in the generic vectorizer
    virtual SIMD_EINFO *Setup_Type_Conversion(WN *wn);
    SIMD_EINFO *Setup_Type_Conversion_Rec(WN* wn); 

    // if conversion 
    void Simd_If_Conv(WN *wn);      

    /* rescan the loop body to build SIMD data structure */
    void           Simd_Rescan_Loop_Body(WN *outerloop);

    /* setup scalar expansion */
    void           Setup_Scalar_Expansion(SIMD_INFO *simd);

    /* SIMD code generation */
    void           SimdTransform(MEM_POOL *local_pool, SIMD_INFO *simd);

    /* Inner loop unroll and MULABC */
    virtual
    void           Further_Unroll_Loop(WN *loop, INT level, SIMD_INFO *simd,
				       EST_REGISTER_USAGE est_register_usage,
				       SX_INFO* pinfo,
				       INT pinfo_depth,
				       BOOL no_further_unroll,
				       SX_INFO** wdpinfo_ptr);

    void           Simd_Transform_Loop(WN *loop, SIMD_INFO *simd, 
				       WN *copy_loop);
    virtual SIMD_EINFO* Simd_Transform(WN *, WN*, SIMD_INFO *);

    /* update dependence graph */
    INT            Update_Dependence(void);

    /* Update DU chains */
    void Simd_Build_Du_Info(WN *loop);

    /* update dependence edge */
    INT Add_Dependence_Edges(VINDEX16_ARRAY *newvs,
			     VINDEX16_ARRAY *new_sinkvs,
			     EINDEX16 edge, INT dim);

    /* Generate last unaligned store after loops*/
    void Simd_Finalize_Unaligned_Store(WN *loop, SIMD_INFO *simd, WN *stmt);

    /* Is loop invariant to SIMD_LOOP and all the nested inner loops
       according to the INVAR_TABLE */
    bool Invariant_In_Simd_Loop(WN *wn);

    /* Not invariant to SIMD_LOOP, ignore inner loops, 
       according to the INVAR_TABLE */
    bool Varies_With_Loop (WN *wn, INT loop);

    /* Any inner loop is trapezoidal                  */
    bool Has_Inner_Trapezoidal_Loop(DOLOOP_STACK *dl_stack, INT loop);

    /* scalar expansion substitution in the 2D remainder loop */
    WN*  Substitute_Scalar_Expansion(WN *wn);
    
    /* generate select */
    virtual void Generate_Sel_Imm(WN *stmt, SIMD_PREG *out0, SIMD_PREG *out1,
				  SIMD_PREG *in0, SIMD_PREG *in1, INT64 sel0,
				  INT64 sel1, INT64 sel);

    virtual void Generate_Sel_Imm(WN *stmt, SIMD_PREG *out0, 
				  SIMD_PREG *in1, SIMD_PREG *in2, INT64 imm,
				  WN *block =NULL);

    /* flaten IF construct into SEL */
    WN*     Simd_If_Conv_Ldid(WN *wn);
    WN*     Simd_If_Conv_Iload(WN *wn);
    WN*     Simd_If_Conv_Read(WN *wn);

    WN*     Simd_If_Conv_Stid(WN *wn, SIMD_IF *simd_if, bool is_then);
    WN*     Simd_If_Conv_Istore(WN *wn, SIMD_IF *simd_if, bool is_then);
    WN*     Simd_If_Conv_Write(WN *wn, SIMD_IF *simd_if, bool is_then);

    void    Simd_If_Conv_If(WN *wn_if, SIMD_IF *p_simd_if, bool is_then);
    void    Simd_If_Conv(WN *wn, SIMD_IF *simd_if, bool is_then);
    void    Simd_If_Conv_Pop(WN *wn);

    void    Copy_EInfo_Rec(WN *orig_wn, WN *copy_wn);

    /* Printing --------------------------------------------------------- *
     * a good place to see how the fields are walked                      *
     *------------------------------------------------------------------- */
    void Print(FILE *outFile=stderr, int indent = 0);
};

/*---------------------------------------------------------------------------*
 * Initialization / Finalization                                             *
 *---------------------------------------------------------------------------*/
extern void SIMD_Initialize(void);
extern void SIMD_Finalize(void);

/*---------------------------------------------------------------------------*
 * Push/Pop memory pools                                                     *
 *---------------------------------------------------------------------------*/
extern void SIMD_Push_Pool(void);
extern void SIMD_Pop_Pool(void);

// generate a vector register symbol corresponding to elem_type
extern SIMD_PREG* Gen_Symbol(SIMD_INFO *simd, TYPE_ID elem_type, MEM_POOL *pool); 

// generate a register symbol of type
extern SIMD_PREG* Gen_Symbol(TYPE_ID type, MEM_POOL *pool, const char *name=NULL);

/*---------------------------------------------------------------------------*
 * Generate alignment addr symbol                                            *
 *---------------------------------------------------------------------------*/
extern SIMD_PREG* Generate_Align_Addr_Reg(SIMD_INFO *simd, MEM_POOL *pool);

/*---------------------------------------------------------------------------*
 * Get the symbol holding narrow 1                                           *
 *---------------------------------------------------------------------------*/
extern WN* Get_Narrow_One(SIMD_INFO *simd, SIMD_LOOP *simd_loop, TYPE_ID
    scalar_type = MTYPE_UNKNOWN);

extern void Simd_Make_Vectra_Macro(char *prefix, char *surfix, INT bits, char* res);

extern void SIMD_Msg (AT_MSG_ID msg_id, const WN *wn, ...);
extern void SIMD_Msgv (AT_MSG_ID msg_id, const WN *wn, va_list vargs);
extern char *SIMD_OPCODE_Msg (const WN *wn);
extern const char *SIMD_MTYPE_Msg (TYPE_ID mtype);
extern const char *SIMD_Base_Name_Msg (const WN *wn);

/* Return the filename associated with 'pos'. If 'fullpath' is true
 * return the full pathname of the file, otherwise return the
 * basename. */
extern const char *SIMD_Filename (SRCPOS pos, bool fullpath);


extern void fprint_indent(FILE *out_file, int indent);

extern WN *Alignment_Load_Init_Addr (WN *addr, INT size);

extern INT Add_Edge_Combine(VINDEX16 src, VINDEX16 sink, DEPV_ARRAY *depv, 
			    MEM_POOL *pool);


/* Create a new SIMD_LOOP/SIMD_LOOP_AT/SIMD_LOOP_V2 instance for the given
   loop. The structure is allocated on the SIMD_default_pool. */
extern SIMD_LOOP *
SIMD_LOOP_New (LOOP_MODEL *lm, WN *simd_loop, INT simd_loop_level);

/* Copy the top-level invariant information from wn_orig to wn_copy in inv_table. */
extern void
Simd_Copy_Invariant_Top_Level(WN *wn_orig, WN *wn_copy, INVAR_TABLE *);

/* Recursively copy the invariant information from wn_orig to wn_copy in inv_table. */
extern void
Simd_Copy_Invariant(WN *wn_orig, WN *wn_copy, INVAR_TABLE *inv_table);

/* Perform loop-invariant hoisting in the 'wn_outer' loop. */
extern void
SIMD_Move_Invariant(WN *wn_outer, INVAR_TABLE *invar_table);

/* Reverse all backward (negative step) loops if SIMD is enabled and if possible. */
extern void
SIMD_Reverse_Backward_Loops (WN *wn);

/* Given a comparison operator 'oper', return its negated version. */
extern OPERATOR
SIMD_Negate_Comparison (OPERATOR oper);

/* Given a comparison operator 'oper', return the version with swapped operands. */
extern OPERATOR
SIMD_Swap_Comparison (OPERATOR oper);


/* Find the (known) GCD of the access vector 'av' and 'max' based on 'atab'.
   If max=0, find the greatest denominator of 'av' only. */
extern INT
gcd (ACCESS_VECTOR *av, ALIGNMENT_TABLE *atab, INT max);

/* Find the alignment of the LDID symbol based on its definition.
   Return 1 if can't determine. */
extern INT Ldid_Def_Alignment (WN *ldid);

extern void Simd_Copy_Dependence(WN *wn_orig, WN *wn_copy);


TYPE_ID Get_Packed_Output_Type(INTRINSIC intrin_id);
TYPE_ID Get_Packed_Output_Type(TIE_MACRO_ID tie_macro_id);


//
// SIMD_TARGET
//

enum SIMD_TARGET
{
    SIMD_TARG_UNKNOWN = 0,
    SIMD_TARG_VECTRA_I,
    SIMD_TARG_VECTRA_II,
    SIMD_TARG_GENERIC,
    SIMD_TARG_AT_ANALYSIS
};

extern SIMD_TARGET Simd_Target;
extern const char *Simd_Target_String (SIMD_TARGET target);
extern bool Is_Shift (WN *expr, bool &immed, INT &shift_amount);

// find a tie intrinsic that is a vector version of 'wn'
// TODO: handle cases when not all arguments are vectors
extern INTRINSIC Tie_Vector_Intrinsic (WN *wn, INT vl =0);
    

#endif /* __SIMD_INFO__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
