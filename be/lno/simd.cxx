
// simd.cpp
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Automatic SIMD                                                         *
 *                                                                           *
 *---------------------------------------------------------------------------*/


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

// $Id: simd.cpp $

#include <stdio.h>
#include <ctype.h>
#include "model.h"
#include "simd.h"
#include "opt_du.h"
#include "reduc.h"
#include "snl.h"
#include "prompf.h"
#include "lwn_util.h"
#include "wind_down.h"
#include "const.h"
#include "dep.h"
#include "data_layout.h"
#include "lnoutils.h"
#include "fusion.h"
#include "reverse.h"
#include "be_symtab.h"
#include "cond.h"
#include "glob.h"
#include "simd_at.h"
#include "pu_info.h"
#include "simd_imem.h"
#include "simd_loop_at.h"
#include "simd_select.h"
#include "simd_ti.h"
#include "simd_ti_v2.h"
#include "simd_loop_v2.h"
#include "tie.h"
#include "tietypes.h"
#include "wintrinsic.h"
#include "intrn_info.h"
#include "xt_mempool.h"
#include "simd_if.h"
#include "dwarf_DST.h"

#define VECTRA_II_PACKAGE "xt_vectralx"
#define DC_545CK_PACKAGE "DC_545CK"

extern WN* Find_Stmt_Under(WN *stmt, WN *body);
extern ARRAY_DIRECTED_GRAPH16*	Array_Dependence_Graph; 
                                        // Dep graph
extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
extern DU_MANAGER *Du_Mgr;          	// PU DU manager
extern WN_MAP     LNO_Info_Map;
extern INT        name_counter;

SIMD_LOOP *Cur_Simd_Loop;               // Current SIMD loop
SIMD_INFO *Simd_Info;                   // SIMD info
SIMD_TI *Simd_Ti;                       // SIMD target-info
SIMD_TI_V2 *Simd_Ti_V2;         // SIMD Vectra2 target-info


MEM_POOL SIMD_default_pool;      // SIMD default mem pool
MEM_POOL SIMD_local_pool;        // SIMD local mem pool
MEM_POOL SIMD_auto_pool;         // SIMD autotie analysis pool

// trace SIMD
bool simd_debug = false;


// vectorization target
SIMD_TARGET Simd_Target;


SIMD_PREG::SIMD_PREG(TYPE_ID mtype, PREG_NUM preg) : _mtype(mtype), _preg(preg)
{
    /* remember the first and last 'preg' for building DU chains */
    Cur_Simd_Loop->Set_Last_Simd_Preg_Num(preg);
    if (Cur_Simd_Loop->First_Simd_Preg_Num() == 0) {
	Cur_Simd_Loop->Set_First_Simd_Preg_Num(preg);
    }
}

/*---------------------------------------------------------------------------*
 * Generate SIMD PREG ldid, add D/U chain                                    *
 *---------------------------------------------------------------------------*/
WN*
SIMD_PREG::Simd_Preg_Ldid(void) {
    WN *ldid = WN_LdidPreg(_mtype, _preg);
    return ldid;
}

WN*
SIMD_PREG::Simd_Preg_Ldid_With_EInfo(void) {
    WN *ldid = WN_LdidPreg(_mtype, _preg);
    Is_True(!Cur_Simd_Loop->E_Info().Find(ldid), ("LDID already in the hash table?"));

    SYMBOL symbol(ldid);
    
    SIMD_SCALAR *s_info = Cur_Simd_Loop->S_Info_Map().Find(symbol);
    Is_True(s_info, ("SIMD_PREG::Simd_Preg_Ldid_With_EInfo: NULL sinfo"));
    
    WN *t = s_info->Lod_Sto().Bottom_nth(0);
    SIMD_EINFO *old_info = Cur_Simd_Loop->E_Info().Find(t);
    Is_True(old_info, ("SIMD_PREG::Simd_Preg_Ldid_With_EInfo: old EINFO is NULL"));
    SIMD_EINFO *e_info = CXX_NEW(SIMD_EINFO(ldid, old_info), old_info->Pool());
    Cur_Simd_Loop->E_Info().Enter(ldid, e_info);
    s_info->Add_Lod_Sto(ldid);

    Is_True(WN_operator(t) == OPR_STID, 
	    ("SIMD_PREG::Simd_Preg_Ldid_With_EInfo: first WN is not a STID"));
    Du_Mgr->Add_Def_Use(t, ldid);

    return ldid;
}

/*---------------------------------------------------------------------------*
 * Generate SIMD PREG stid, memorize definition                              *
 *---------------------------------------------------------------------------*/
WN* 
SIMD_PREG::Simd_Preg_Stid(WN* rhs) {
    WN *stid = WN_StidPreg(_mtype, _preg, rhs);
    LWN_Set_Parent(rhs, stid);
    return stid;
}

void 
SIMD_PREG::Print(FILE *outfile, int indent)
{
    fprint_indent(outfile, indent);
    fprintf(outfile, "SIMD_PREG: %s, %d\n", MTYPE_name(_mtype), _preg);
}

// Reset the destination simd_preg array and copy all elements from the source
// array to the destination

void SIMD_PREGS_Copy(SIMD_PREGS *src, SIMD_PREGS *dst) {
    dst->Resetidx();
    for (INT i=0; i<src->Elements(); i++) {
	dst->AddElement((*src)[i]);
    }
}

SIMD_PREG *SIMD_PREGS_Get (SIMD_PREGS *pregs, UINT32 idx) {
    if (idx < pregs->Elements()) {
	return pregs->Get(idx);
    } else {
	return NULL;
    }
}

void SIMD_PREGS_Set(SIMD_PREGS *pregs, UINT32 idx, SIMD_PREG *preg) {
    if (idx < pregs->Elements()) {
	pregs->Set(idx,preg);
    } else {
	while (idx>pregs->Elements()) {
	    pregs->AddElement(NULL);
	}
	Is_True(idx==pregs->Elements(),("Unexpected last index."));
	pregs->AddElement(preg);
	Is_True(pregs->Get(idx)==preg,("Bad SIMD_PREGS_Set."));
    }
}

SIMD_PREG *SIMD_CONST_Map_Find(SIMD_CONST_Map *map,
			       TYPE_ID preg_type, INT64 val) {
    Is_True(map,("Null const map."));
    CONST_MAP_KEY key = {preg_type, val};
    return map->Find(key);
}

void SIMD_CONST_Map_Enter(SIMD_CONST_Map *map,
			  TYPE_ID preg_type, INT64 val, SIMD_PREG *preg) {
    Is_True(map,("Null const map."));
    CONST_MAP_KEY key = {preg_type, val};
    map->Enter(key,preg);
}

// SIMD_CONST_Map_Get looks up 'map' for a SIMD_PREG corresponding
// to (preg_type,val) and returns it if found. Otherwise,
// creates a new SIMD_PREG of type preg_type and
// inserts an initialization statement before 'stmt'.
// val_type must be integer (I1,I2,I4,I8,U1,U2,U4,U8).

SIMD_PREG *SIMD_CONST_Map_Get(SIMD_CONST_Map *map,
			      TYPE_ID preg_type, INT64 val,
			      TYPE_ID val_type, WN *stmt, bool is_select,
			      char *name, MEM_POOL *pool) {
    Is_True(map,("Null const map."));

    // try to find it in the map first
    SIMD_PREG *res = SIMD_CONST_Map_Find(map,preg_type,val);
    if (res!=NULL) {
	return res;
    }
    
    // not found, generate a new one
    char new_name[128];
    if (name==NULL) {
	name = "_tmp_reg";
    }
    sprintf(new_name,"%s_%d",name,name_counter++);
    if (pool==NULL) {
	pool = map->Mem_Pool();
    }
    
    PREG_NUM preg_num  = Create_Preg(preg_type, new_name);
    res = CXX_NEW(SIMD_PREG(preg_type, preg_num), pool);
    SIMD_CONST_Map_Enter(map,preg_type,val,res);
    
    /* generate a store of 'constant' before stmt */
    Is_True(stmt,("Null statement to insert before."));
    TYPE_ID const_type = Promote_Type(val_type);
    WN *rhs = NULL;
    if (!is_select || MTYPE_byte_size(const_type) < 8) {
	WN * constant = WN_CreateIntconst(OPR_INTCONST,const_type,MTYPE_V,val);
	OPCODE opc    = OPCODE_make_op(OPR_CVT, preg_type, val_type);
	rhs           = LWN_CreateExp1(opc, constant);
	
    } else {
	ST*    st       = Simd_Info->Create_64bit_Integer_Var(val);
	TY_IDX ty       = MTYPE_To_TY(preg_type);
	TY_IDX ty_ptr   = Make_Pointer_Type(ty);
	OPCODE op_lda   = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
	WN*    wn_lda   = WN_CreateLda(op_lda, 0, ty_ptr, st);
	OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, preg_type, preg_type);
	rhs             = LWN_CreateIload(op_iload, 0, ty, ty_ptr, wn_lda);
	if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(rhs);
	}
    }
    
    WN    *store = res->Simd_Preg_Stid(rhs);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, store);
    WN_linenum(store) = LWN_Get_Linenum(stmt);
    return res;
}

/*---------------------------------------------------------------------------*
 * Initialize vectra configuration from libcc                                *
 *---------------------------------------------------------------------------*/
void
SIMD_INFO::Init_Vectra_Config()
{
  FmtAssert(0, ("We should never get here -- Vectra is not supported."));

  _vectra_ok = false;
}

/*---------------------------------------------------------------------------*
 * Initialize vectra configuration from libcc                                *
 *---------------------------------------------------------------------------*/
void
SIMD_INFO::Print_Vectra_Config(FILE *file)
{
    fprintf(file,
	    "\n"
	    "---------------------\n"
	    "Vectra Configuration:\n"
	    "---------------------\n");
    fprintf(file, "Type 1 simd register: %d\n", Type1_Simd());
    fprintf(file, "Type 2 simd register: %d\n", Type2_Simd());
    fprintf(file, "Type 1 memory bits:   %d\n", Type1_Mem_Bits());
    fprintf(file, "Type 1 register bits: %d\n", Type1_Reg_Bits());
    fprintf(file, "Type 2 memory bits:   %d\n", Type2_Mem_Bits());
    fprintf(file, "Type 2 register bits: %d\n", Type2_Reg_Bits());
    fprintf(file, "SIMD ratio:           %d\n", Simd_Ratio());
    fprintf(file, "Has ALU operator:     %d\n", (Alu_Ops() ? 1 : 0));
    fprintf(file, "Has Avg operator:     %d\n", (Avg() ? 1 : 0));
    fprintf(file, "Has Mul vec/vec:      %d\n", (Mul_V_V() ? 1 : 0));
    fprintf(file, "Has Mul vec/coeff:    %d\n", (Mul_V_C() ? 1 : 0));
    fprintf(file, "Mul input 1 bits:     %d\n", Mul_In1_Bits());
    fprintf(file, "Mul input 2 bits:     %d\n", Mul_In2_Bits());
    fprintf(file, "Mul out shift bits:   %d\n", Mul_Out_Shift());
    fprintf(file, "Number of align reg:  %d\n", N_A_Regs());  
    fprintf(file, "Number of S reg:      %d\n", N_S_Regs()); 
    fprintf(file, "Number of vec reg:    %d\n", N_V_Regs());
    fprintf(file, "Has Mula by coeff:    %d\n", (Mula_B_C() ? 1 : 0));
    fprintf(file, "Has Muls by coeff:    %d\n", (Muls_B_C() ? 1 : 0));
}

void
SIMD_INFO::Init_SIMD_Type_Map(void) {
    _scalar_simd_map = CXX_NEW(SIMD_TYPE_Map(127,Mem_Pool()),Mem_Pool());
    _simd_simd_map = CXX_NEW(SIMD_TYPE_Map(127,Mem_Pool()),Mem_Pool());

    SIMD_TYPE *simd_type;
    
    INT size = 1;
    INT sel_bits = LNO_Min_Pow_Of_Two_Exp(2 * Type1_Simd());
    
    _s16type = MTYPE_UNKNOWN;
    _s32type = MTYPE_UNKNOWN;
    
    if (V8x16type()!=TIE_INVALID_ID && V8x20type()!=TIE_INVALID_ID) {
	TYPE_ID v8x16type = V8x16type();
	TYPE_ID v8x20type = V8x20type();
	_s16type = Bit_Size_To_Int(Type1_Mem_Bits());
	SIMD_SELECT *simd_select = CXX_NEW(SIMD_SELECT(Type1_Simd(), sel_bits, 1),
					   Mem_Pool());
	simd_type = CXX_NEW(SIMD_TYPE(v8x20type, v8x16type, Type1_Simd(), simd_select),
			    Mem_Pool());
	_scalar_simd_map->Enter(_s16type,simd_type);
	_simd_simd_map->Enter(v8x16type,simd_type);
	_simd_simd_map->Enter(v8x20type,simd_type);
	
	// Map the unsigned scalar integer type to the same SIMD type
	// for handling sign independent ops
	_scalar_simd_map->Enter(Mtype_TransferSign(MTYPE_U4,_s16type),simd_type);
    }
    
    if (V4x32type()!=TIE_INVALID_ID && V4x40type()!=TIE_INVALID_ID) {
	TYPE_ID v4x32type = V4x32type();
	TYPE_ID v4x40type = V4x40type();
	_s32type = Bit_Size_To_Int(Type2_Mem_Bits());
	SIMD_SELECT *simd_select = CXX_NEW(SIMD_SELECT(Type2_Simd(), sel_bits, 2),
					  Mem_Pool());
	simd_type = CXX_NEW(SIMD_TYPE(v4x40type, v4x32type, Type2_Simd(), simd_select),
			    Mem_Pool());
	_scalar_simd_map->Enter(_s32type,simd_type);
	_simd_simd_map->Enter(v4x32type,simd_type);
	_simd_simd_map->Enter(v4x40type,simd_type);
	
	// Map the unsigned scalar integer type to the same SIMD type
	// for handling sign independent ops
	_scalar_simd_map->Enter(Mtype_TransferSign(MTYPE_U4,_s32type),simd_type);
    }
}

INT
SIMD_INFO::Guard_Bits(TYPE_ID scalar_type, INT vl)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    if (type == NULL) {
	return 0;
    } else {
	return (type->Elem_Reg_Bit_Size() - type->Elem_Mem_Bit_Size());
    }
}

bool
SIMD_INFO::Has_Update_Load_Store(IMEM_GROUP *ig, bool load, bool store)
{
    TYPE_ID res_type, desc_type;
    ig->Get_Vector_Load_Types(res_type, desc_type);
    INT vs = ig->Is_Vector() ? 0 : 1;
    if (load && Search_Vector_Macro(OPR_ILOAD, desc_type, res_type, vs, true) ==
	TIE_INVALID_ID) {
	return false;
    }
    if (store && Search_Vector_Macro(OPR_ISTORE, desc_type, res_type, vs, true)
	== TIE_INVALID_ID) {
	return false;
    }
    return true;
}

bool
SIMD_INFO::Has_Update_Indexed_Load_Store(IMEM_GROUP *ig, bool load, bool store)
{
    TYPE_ID res_type, desc_type;
    ig->Get_Vector_Load_Types(res_type, desc_type);
    INT vs = ig->Is_Vector() ? 4 : 3;
    if (load && 
	(Search_Vector_Macro(OPR_ILOAD, desc_type, res_type, vs, true) ==
	 TIE_INVALID_ID)) {
	return false;
    }
    if (store &&
	(Search_Vector_Macro(OPR_ISTORE, desc_type, res_type, vs, true) ==
	 TIE_INVALID_ID)) {
	return false;
    } 
    return true;
}

const char *
Find_Vectra_Package (void)
{
    if (tie_info == NULL)
	return NULL;

    // FIXME: Currently, there is no way to get a list of configured TIE packages.
    // The hack we use to find if a Vectra package is configured is to traverse
    // all TIE types and see if their associated package matches a name that we like.
    for (TYPE_ID mtype = MTYPE_FIRST; mtype <= Mtype_Last; mtype++)
    {
	if (MTYPE_is_tie(mtype) && !MTYPE_is_tie_packed(mtype))
	{
	    TIE_TYPE_ID tie_type_id = Mtype_To_Tie_Type_Id(mtype);
	    const char *package = tie_info->ctype_package(tie_type_id);
	    if (!strcmp(package, VECTRA_II_PACKAGE) ||
                !strcmp(package, DC_545CK_PACKAGE))
              return package;
	}
    }
    
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Initialize the pools                                                      *
 *---------------------------------------------------------------------------*/
extern void SIMD_Initialize()
{
    Simd_Info = NULL;
    Simd_Ti = NULL;
    Simd_Ti_V2 = NULL;
    
    Simd_Target = SIMD_TARG_UNKNOWN;
    
    if (LNO_SIMD_Level != 0) {
	MEM_POOL_Initialize(&SIMD_default_pool, "SIMD_default_pool", FALSE);
	MEM_POOL_Initialize(&SIMD_local_pool, "SIMD_local_pool", FALSE);
	MEM_POOL_Initialize(&SIMD_auto_pool, "SIMD_auto_pool", FALSE);
	MEM_POOL_Push(&SIMD_default_pool);
	MEM_POOL_Push(&SIMD_local_pool);
	MEM_POOL_Push(&SIMD_auto_pool);

	// get tracing options
	simd_debug = Get_Trace(TP_LNOPT, TT_LNO_SIMD_DEBUG);
	
        /*** We would check for Vectra I here but Vectra I is
             not supported anymore so we don't. ****/

        if (Simd_Target == SIMD_TARG_UNKNOWN &&
	    TI_TIE_First_Simd_Type_Info() != NULL)
	{
	    // check for Vectra II
	    if (Find_Vectra_Package()) {
		Simd_Target = SIMD_TARG_VECTRA_II;
	    } else {
		Simd_Target = SIMD_TARG_GENERIC;
	    }
	}
        
        /* If no SIMD configuration is available and AutoTIE analysis
           is enabled, perform AutoTIE vectorization analysis. */
        if (Simd_Target == SIMD_TARG_UNKNOWN && 
            (PU_Info_autotie_ptr(Current_PU_Info) != NULL)) {
          Simd_Target = SIMD_TARG_AT_ANALYSIS;
	}

	switch (Simd_Target)
	{
	case SIMD_TARG_VECTRA_I:
	    Simd_Info = CXX_NEW(SIMD_INFO(&SIMD_default_pool), &SIMD_default_pool);
	    if (!Simd_Info->Vectra_Ok())
	    {
		Simd_Info = NULL;
		Simd_Target = SIMD_TARG_UNKNOWN;
	    }
	    break;
	    
	case SIMD_TARG_VECTRA_II:
	    Simd_Info = Simd_Ti = Simd_Ti_V2 =
		CXX_NEW(SIMD_TI_V2(&SIMD_default_pool), &SIMD_default_pool);
	    break;
	    
	case SIMD_TARG_GENERIC:
	case SIMD_TARG_AT_ANALYSIS:
	    Simd_Info = Simd_Ti = 
		CXX_NEW(SIMD_TI(&SIMD_default_pool), &SIMD_default_pool);
	    break;
	    
	default:
	    break;
	}
	
	if (Simd_Target == SIMD_TARG_UNKNOWN)
	{
          static bool printed_once = false;
          if (!printed_once)
          {
            ErrMsg (EC_Simd_Unsupported);
            printed_once = true;
          }
          LNO_SIMD_Verbose = 0;
	}
    } else if (LNO_SIMD_Verbose > 0) {
	SIMD_Msg(AT_MSG_SIMD_DISABLED, NULL);
	LNO_SIMD_Verbose = 0;
    }
    
    if (simd_debug) {
	fprintf(TFile, "... Simd Target: %s ...\n", Simd_Target_String(Simd_Target));
    }
	
}

/*---------------------------------------------------------------------------*
 * Release the pools                                                         *
 *---------------------------------------------------------------------------*/
extern void SIMD_Finalize()
{
    if (LNO_SIMD_Level != 0) {
	Simd_Info = NULL;
	Simd_Ti = NULL;
	Simd_Ti_V2 = NULL;

        Simd_Target = SIMD_TARG_UNKNOWN;

	MEM_POOL_Pop(&SIMD_default_pool);
	MEM_POOL_Pop(&SIMD_local_pool);
	MEM_POOL_Pop(&SIMD_auto_pool);
	MEM_POOL_Delete(&SIMD_default_pool);
	MEM_POOL_Delete(&SIMD_local_pool);
	MEM_POOL_Delete(&SIMD_auto_pool);
    }
}

/*---------------------------------------------------------------------------*
 * SIMD push/pop pools                                                       *
 *---------------------------------------------------------------------------*/
extern void SIMD_Push_Pool()
{
    // Don't push SIMD_auto_pool -- we need it throughout the whole PU
    if (LNO_SIMD_Level != 0) {
	MEM_POOL_Push(&SIMD_default_pool);
	MEM_POOL_Push(&SIMD_local_pool);
    }
}

extern void SIMD_Pop_Pool()
{
    // Don't pop SIMD_auto_pool -- we need it throughout the whole PU
    if (LNO_SIMD_Level != 0) {
	MEM_POOL_Pop(&SIMD_default_pool);
	MEM_POOL_Pop(&SIMD_local_pool);
    }
}

static
void Simd_Make_Vectra_Type(INT elements, INT bits, char* res)
{
    sprintf(res, "_TIE_Vectra_vec%dx%d", elements, bits);
    return;
}

/*---------------------------------------------------------------------------*
 * Find SIMD function name for 'op' in SIMD type 'type'                      *
 *---------------------------------------------------------------------------*/
SIMD_INFO::SIMD_INFO(MEM_POOL *pool) :
    _pool(pool), _type1simd(0), _type2simd(0),
    _type1membits(0), _type1regbits(0), _type2membits(0), _type2regbits(0),
    _simdratio(0), _aluops(false), _avg(false), _mulvv(false), _mulvc(false),
    _mulin1bits(0), _mulin2bits(0), _muloutshift(0), _naregs(0), _nsregs(0), 
    _nvregs(0), _mul16(false), _mulabc(false), _mulsbc(false), 
    _store_align(NULL), _scalar_simd_map(NULL), _simd_simd_map(NULL), _simd_at(NULL), _vse_ty(pool)
{
  /* Initialize and maintain a set of emitted messages to avoid duplicates. */
    _message_set = CXX_NEW(STR_SET(63, &SIMD_auto_pool), &SIMD_auto_pool);

    /* read vectra configuration parameters */
    /* Init_Vectra_Config(); */
    Is_True(tie_info, ("Tie info is NULL"));

    /* set up the Type_IDs */
    char type_name[64];
    Simd_Make_Vectra_Type(_type1simd, _type1membits, type_name);
    _v8x16type  = tie_info->mtype_id(type_name);
    
    Simd_Make_Vectra_Type(_type2simd, _type2membits, type_name);
    _v4x32type  = tie_info->mtype_id(type_name);

    Simd_Make_Vectra_Type(_type1simd, _type1regbits, type_name);
    _v8x20type  = tie_info->mtype_id(type_name);

    Simd_Make_Vectra_Type(_type2simd, _type2regbits, type_name);
    _v4x40type  = tie_info->mtype_id(type_name);

    Simd_Make_Vectra_Type(_type1simd*_type1membits, 1, type_name);
    _v128x1type  = tie_info->mtype_id(type_name);

    Simd_Make_Vectra_Type(_type1simd*_type1regbits, 1, type_name);
    _v160x1type  = tie_info->mtype_id(type_name);

    _vsel_type  = tie_info->mtype_id("_TIE_Vectra_vsel");
    _align_type = tie_info->mtype_id("_TIE_Vectra_valign");
    _coeff_type = tie_info->mtype_id("_TIE_Vectra_vcoeff");

    Init_SIMD_Type_Map();
    
    if (PU_Info_autotie_ptr(Current_PU_Info) != NULL) {
	_simd_at = CXX_NEW(SIMD_AT(&SIMD_auto_pool), &SIMD_auto_pool);
    }
    
    _alignment_table = CXX_NEW(ALIGNMENT_TABLE(31,pool),pool);  
}

TYPE_ID
SIMD_INFO::Get_SIMD_Xtbool(INT vl)
{
    switch (vl) {
	case 2: return tie_info->mtype_id("_TIE_xtbool2");
	case 4: return tie_info->mtype_id("_TIE_xtbool4");
	case 8: return tie_info->mtype_id("_TIE_xtbool8");
	case 16:return tie_info->mtype_id("_TIE_xtbool16");
	default:
	    Is_True(0, ("SIMD_INFO::Get_SIMD_Xtbool: VL is %d", vl));
    }
    return MTYPE_UNKNOWN;
}

bool
SIMD_INFO::AT_Analysis_Phase (void) {
    return Simd_Target == SIMD_TARG_AT_ANALYSIS;
}


void Simd_Make_Vectra_Macro(char *prefix, char *surfix, INT bits, char* res)
{
    sprintf(res, "_TIE_Vectra_%s%d%s", prefix, bits, surfix);
    return;
}

TIE_MACRO_ID
SIMD_INFO::Search_Vector_Macro(OPERATOR op, TYPE_ID dtype, TYPE_ID rtype,
			       INT lohi, bool update)
{
    Is_True(tie_info, ("Tie Info is NULL"));
    TIE_MACRO_ID macro_id = TIE_INVALID_ID;
    char macro_name[64];

    SIMD_TYPE *type=SIMD_SIMD_Map()->Find((lohi == 1 || lohi==3) ? 
					  rtype: dtype);
    if (type==NULL) {
	DevWarn("Unhandled Vectra type %s.",MTYPE_name(dtype));
	return TIE_INVALID_ID;
    }

    INT reg_bits = type->Elem_Reg_Bit_Size();
    INT mem_bits = type->Elem_Mem_Bit_Size();
    INT mul_bits = Get_Mul_In1_Bits();
    if (mul_bits<mem_bits || mul_bits>reg_bits) {
	// for now, multiplication of all memory bits allowed only
	mul_bits=0;
    }

    switch (op) {
	case OPR_ABS:
	    Simd_Make_Vectra_Macro("ABS", "", reg_bits, macro_name); 
	    break;

	case OPR_NEG:
	    Simd_Make_Vectra_Macro("NEG", "", reg_bits, macro_name); 
	    break;

	case OPR_ADD:
	    Simd_Make_Vectra_Macro("ADD", "", reg_bits, macro_name); 
	    break;
	    
	case OPR_SUB:
		Simd_Make_Vectra_Macro("SUB", "", reg_bits, macro_name); 
		break;
		
	case OPR_MAX:
	    Simd_Make_Vectra_Macro("MAX", "", reg_bits, macro_name); 
	    break;

	case OPR_MIN:
	    Simd_Make_Vectra_Macro("MIN", "", reg_bits, macro_name); 
	    break;

	case OPR_ASHR:
	case OPR_LSHR:
	case OPR_DIV:
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("SRAI", "", reg_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("SRA", "", reg_bits, macro_name); 
	    }
	    break;

	case OPR_SHL:
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("SLLI", "", reg_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("SLL", "", reg_bits, macro_name); 
	    }
	    break;

	case OPR_HIGHMPY: /* MULR16 */
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("MULR", "_0", mul_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("MULR", "_1", mul_bits, macro_name); 
	    }
	    break;

	case OPR_XMPY: /* MULABC16 */
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("MULABC", "_0", mul_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("MULABC", "_1", mul_bits, macro_name); 
	    }
	    break;

	case OPR_MPY:
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("MUL", "_0", mul_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("MUL", "_1", mul_bits, macro_name); 
	    }
	    break;

	case OPR_MADD:
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("MULA", "_0", mul_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("MULA", "_1", mul_bits, macro_name); 
	    }
	    break;

	case OPR_MSUB:
	    if (lohi == 0) {
		Simd_Make_Vectra_Macro("MULS", "_0", mul_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("MULS", "_1", mul_bits, macro_name); 
	    }
	    break;
		    
	case OPR_LDID:
	    Is_True(lohi == 1, ("Load scalar"));
	    if (update) {
		Simd_Make_Vectra_Macro("LS", "_IU", mem_bits, macro_name); 
	    } else {
		Simd_Make_Vectra_Macro("LS", "_I", mem_bits, macro_name); 
	    }
	    break;
		
	case OPR_ILOAD:
	    if (lohi == 0) {
		if (update) {
		    Simd_Make_Vectra_Macro("LV", "_IU", mem_bits, macro_name); 
		} else {
		    Simd_Make_Vectra_Macro("LV", "_I", mem_bits, macro_name); 
		}
	    } else if (lohi == 1){
		if (update) {
		    Simd_Make_Vectra_Macro("LS", "_IU", mem_bits, macro_name); 
		} else {
		    Simd_Make_Vectra_Macro("LS", "_I", mem_bits, macro_name); 
		}
	    } else if (lohi == 2) {
		if (update) {
		    Simd_Make_Vectra_Macro("LV", "A_IU", mem_bits, macro_name);
		} else {
		    Simd_Make_Vectra_Macro("LV", "A_I", mem_bits, macro_name);
		}
	    } else if (lohi == 3) {
		if (update) {
		    Simd_Make_Vectra_Macro("LS", "_XU", mem_bits, macro_name);
		} else {
		    Simd_Make_Vectra_Macro("LS", "_X", mem_bits, macro_name);
		}
	    } else {
		Is_True(lohi == 4, ("Unknown addressing mode "));
		if (update) {
		    Simd_Make_Vectra_Macro("LV", "_XU", mem_bits, macro_name);
		} else {
		    Simd_Make_Vectra_Macro("LV", "_X", mem_bits, macro_name);
		}
	    }

	    break;

	case OPR_STID:
	case OPR_ISTORE:
	    if (lohi == 0) {
		if (update) {
		    Simd_Make_Vectra_Macro("SV", "_IU", mem_bits, macro_name); 
		} else {
		    Simd_Make_Vectra_Macro("SV", "_I", mem_bits, macro_name); 
		}
	    } else {
		Is_True(lohi == 4, ("Unknown addressing mode "));
		if (update) {
		    Simd_Make_Vectra_Macro("SV", "_XU", mem_bits, macro_name);
		} else {
		    Simd_Make_Vectra_Macro("SV", "_X", mem_bits, macro_name);
		}
	    }
		
	    break;

	case OPR_SELECT:
	    if (lohi == 0) {
		if (reg_bits == Get_Narrow_Element_Reg_Bits()) {
		    sprintf(macro_name, "%s", "_TIE_Vectra_SEL");
		} else {
		    Simd_Make_Vectra_Macro("SEL", "", reg_bits, macro_name);
		}
	    } else {
		sprintf(macro_name, "%s", "_TIE_Vectra_SELI");
	    }
	    break;

	case OPR_MOD: // this is a to represent PACK
	    sprintf(macro_name, "%s", "_TIE_Vectra_PACK");
	    break;
	    
	case OPR_BNOT:
	    Simd_Make_Vectra_Macro("COM", "", reg_bits, macro_name);
	    break;

	case OPR_BXOR:
	    Simd_Make_Vectra_Macro("XOR", "", reg_bits, macro_name);
	    break;

	case OPR_BAND:
	    Simd_Make_Vectra_Macro("AND", "", reg_bits, macro_name);
	    break;

	case OPR_BIOR:
	    Simd_Make_Vectra_Macro("OR", "", reg_bits, macro_name);
	    break;

	default:
	    DevWarn("Unhandled operator");
	    return TIE_INVALID_ID;
    }

    macro_id = tie_info->tie_macro_id(macro_name);
    return macro_id;
}	

/*---------------------------------------------------------------------------*
 * Find SIMD function name for 'op' in SIMD type 'type'                      *
 *---------------------------------------------------------------------------*/
INTRINSIC
SIMD_INFO::Find_Function(OPERATOR op, TYPE_ID dtype, TYPE_ID rtype, INT lohi, bool update)
{
    TIE_MACRO_ID macro_id = Search_Vector_Macro(op, dtype, rtype, lohi, update);
    FmtAssert(macro_id != TIE_INVALID_ID,
	      ("Cannot find SIMD macro name (%s:%s:%s)",
	       OPERATOR_name(op), MTYPE_name(dtype), MTYPE_name(rtype)));
    return Tie_Macro_Id_To_Intrinsic(macro_id);
}

/*---------------------------------------------------------------------------*
 * Find SIMD function name for MOVT                                          *
 *---------------------------------------------------------------------------*/
INTRINSIC
SIMD_INFO::CMov(TYPE_ID rtype, TYPE_ID bool_type)
{
    Is_True(tie_info, ("Tie Info is NULL"));
    TIE_MACRO_ID macro_id = TIE_INVALID_ID;
    char macro_name[64];

    SIMD_TYPE *type=SIMD_SIMD_Map()->Find(rtype);
    INT reg_bits = type->Elem_Reg_Bit_Size();
    Simd_Make_Vectra_Macro("MOVT", "", reg_bits, macro_name); 
    macro_id = tie_info->tie_macro_id(macro_name);
    FmtAssert(macro_id != TIE_INVALID_ID,
	      ("Cannot find SIMD macro name (%s)", macro_name));
    return Tie_Macro_Id_To_Intrinsic(macro_id);
}

/*---------------------------------------------------------------------------*
 * Find SIMD function name for 'op' in SIMD type 'type'                      *
 *---------------------------------------------------------------------------*/
INTRINSIC
SIMD_INFO::Radd (TYPE_ID res_type, TYPE_ID desc_type)
{
    char macro_name[64];
    INT reg_bits = Get_Elem_Reg_Bits_SIMD(desc_type);
    Simd_Make_Vectra_Macro("RADD","", reg_bits, macro_name);
    TIE_MACRO_ID macro_id = tie_info->tie_macro_id(macro_name);
    FmtAssert(macro_id != TIE_INVALID_ID, ("Cannot find SIMD macro name"));
    return Tie_Macro_Id_To_Intrinsic(macro_id);
}


INTRINSIC
SIMD_INFO::Rmin (TYPE_ID res_type, TYPE_ID desc_type)
{
  return INTRINSIC_INVALID;
}


INTRINSIC
SIMD_INFO::Rmax (TYPE_ID res_type, TYPE_ID desc_type)
{
  return INTRINSIC_INVALID;
}


/*---------------------------------------------------------------------------*
 * Find SIMD function name for 'MOVAR' for SIMD type 'type'                  *
 *---------------------------------------------------------------------------*/
INTRINSIC
SIMD_INFO::Find_Movar(TYPE_ID mtype)
{
    char macro_name[64];
    INT mem_bits = Get_Elem_Mem_Bits_SIMD(mtype);
    Simd_Make_Vectra_Macro("MOVAR","", mem_bits, macro_name);
    TIE_MACRO_ID macro_id = tie_info->tie_macro_id(macro_name);
    FmtAssert(macro_id != TIE_INVALID_ID, ("Cannot find SIMD macro name"));
    return Tie_Macro_Id_To_Intrinsic(macro_id);
}


bool
SIMD_INFO::Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type) {
    return true;
}


/*---------------------------------------------------------------------------*
 * Return the proper SIMD register according to element 'type'               *
 *---------------------------------------------------------------------------*/
SIMD_PREG * 
SIMD_EINFO::Pick_Simd_Reg(EINFO_PROP prop, INT hilo)
{
    if (prop == Eprop_Normal) {
	Is_True(!Interleaved() || Need_Deinterleave(),
		("Normal data unavailable"));
	return Simd_Reg(hilo);
    } else if (prop == Eprop_Interleaved) {
	Is_True(Interleaved() || Need_Interleave(), 
		("Interleaved data unavailable"));
	return Simd_IReg(hilo);
    }

    Is_True(prop == Eprop_Donot_Care, ("Unhandled EINFO_PROP"));
    if (Interleaved()) {
	return Simd_IReg(hilo);
    } else {
	return Simd_Reg(hilo);
    }
}

/*---------------------------------------------------------------------------*
 * Get the register containing the final result of an EINFO                  *
 *---------------------------------------------------------------------------*/
SIMD_PREG *
SIMD_EINFO::Get_Final_Reg(INT hilo)
{
    SIMD_PREG *reg = NULL;
    if (Need_Interleave()) {
	reg = Simd_IReg(hilo);
    } else if (Need_Deinterleave()) {
	reg = Simd_Reg(hilo);
    } else if (Interleaved()) {
	reg = Simd_IReg(hilo);
    } else {
	reg = Simd_Reg(hilo);
    }
    return reg;
}

/*---------------------------------------------------------------------------*
 * Get the result register to save an expression result                      *
 *---------------------------------------------------------------------------*/
SIMD_PREG *
SIMD_EINFO::Get_Res_Reg(INT hilo)
{
    SIMD_PREG *res = NULL;
    if (Interleaved()) {
	res = Simd_IReg(hilo);
    } else {
	res = Simd_Reg(hilo);
    }
    return res;
}

/*---------------------------------------------------------------------------*
 * Requiring property of operand                                             *
 *---------------------------------------------------------------------------*/
EINFO_PROP
SIMD_EINFO::Get_Required_Operand_Prop(bool is_madd)
{
    OPERATOR op = WN_operator(Expr());
    if (Interleaved()) {
	if (is_madd  || op == OPR_MPY || op == OPR_CVT || op == OPR_CVTL) {
	    return Eprop_Normal;
	} else {
	    return Eprop_Interleaved;
	}
    } else {
	if (op == OPR_CVT || op == OPR_CVTL) {
	    return Eprop_Interleaved;
	} else {
	    return Eprop_Normal;
	}
    }
}

/*---------------------------------------------------------------------------*
 * Return a LOD of SIMD register according to element type and idx           *
 *---------------------------------------------------------------------------*/
WN *
SIMD_EINFO::Gen_Lod_Reg(EINFO_PROP prop, INT idx)
{
    SIMD_PREG *reg = Pick_Simd_Reg(prop, idx);
    return reg->Simd_Preg_Ldid();
}

/*---------------------------------------------------------------------------*
 * Generate a SIMD instrinsic call with two parameters                       *
 *---------------------------------------------------------------------------*/
WN *
SIMD_INFO::Generate_Binary_Call (WN *stmt, OPERATOR op, SIMD_PREG *res_preg,
				 WN *operand1, WN *operand2)
{
  TYPE_ID out_type = res_preg->Type();
  TYPE_ID in_type1  = WN_rtype(operand1);
  TYPE_ID in_type2  = WN_rtype(operand2);

  bool is_mula = ((op == OPR_MADD) || (op == OPR_MSUB));
  
  /* Use operand1's rtype as the descriptor type, and out_type as the
     return type. Note that in most cases in_type1 is the same as in_type2
     except shifts. */
  INTRINSIC intrin_id = Find_Function(op, in_type1, out_type);

  TIE_MACRO_ID macro_id = Intrinsic_To_Tie_Macro_Id(intrin_id);
  TIE_MACRO *macro = tie_info->tie_macro(macro_id);
  bool is_op = macro->is_whirl_intrinsic_op();
  
  WN *apr[3];
  OPCODE intrin_op = OPCODE_UNKNOWN;

  INT in_idx = 0;
  if (is_op)
  {
    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, out_type, MTYPE_V);
  }
  else
  {
    Is_True(stmt, ("Null stmt for intrinsic call."));
    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
  }

  if (is_mula || !is_op)
  {
    apr[0] = LWN_CreateParm(out_type, res_preg->Simd_Preg_Ldid(), MTYPE_To_TY(out_type),
                            WN_PARM_BY_VALUE);
    in_idx = 1;
  }
  
  apr[in_idx + 0] = LWN_CreateParm(in_type1, operand1, MTYPE_To_TY(in_type1), 
                                   WN_PARM_BY_VALUE);
  apr[in_idx + 1] = LWN_CreateParm(in_type2, operand2, MTYPE_To_TY(in_type2), 
                                   WN_PARM_BY_VALUE);
  
  WN *res = LWN_Create_Intrinsic(intrin_op, intrin_id, in_idx + 2, apr);
  
  if (!is_op) {
    if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
      Array_Dependence_Graph->Add_Vertex(res);
    }
    
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, res);
    WN_linenum(res) = LWN_Get_Linenum(stmt);
    WN *ldid = WN_Ldid(out_type, -1,
                       Tie_Output_Volatile_Preg,
                       Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(res, ldid);
    res = ldid;
  }

  return res;
}


/*---------------------------------------------------------------------------*
 * Generate an SIMD compare instrinsic call  with two parameter              *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Generate_Compare_Expr(OPERATOR op, WN *expr, WN *stmt,
				  SIMD_EINFO *c1_info, SIMD_INFO *simd,
				  SIMD_EINFO *c2_info, INT idx, bool &negated)
{
  negated = false;

  TYPE_ID     out_type  = Get_Res_Reg()->Type();
  EINFO_PROP  prop      = Get_Required_Operand_Prop();
  SIMD_PREG  *c1_preg   = c1_info->Pick_Simd_Reg(prop, idx);
  SIMD_PREG  *c2_preg   = c2_info->Pick_Simd_Reg(prop, idx);
  TYPE_ID     in_type   = c1_preg->Type();
  WN         *c1_lod    = c1_preg->Simd_Preg_Ldid();
  WN         *c2_lod    = c2_preg->Simd_Preg_Ldid();
  
  TIE_MACRO_ID macro_id = simd->Search_Vector_Macro(op, in_type, out_type);
  if (macro_id == TIE_INVALID_ID) {
    OPERATOR new_oper = OPERATOR_UNKNOWN;
    if (op == OPR_NE || op == OPR_EQ) {
      new_oper = SIMD_Negate_Comparison(op);
      negated = true;
    } else {
      new_oper = SIMD_Swap_Comparison(op);
      WN *t_lod = c1_lod;
      c1_lod = c2_lod;
      c2_lod = t_lod;
    }
    
    macro_id = simd->Search_Vector_Macro(new_oper, in_type, out_type);
    if (macro_id != TIE_INVALID_ID) {
      op = new_oper;
    }
  }
  
  INTRINSIC intrin_id = simd->Find_Function(op, in_type, out_type);
  OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, out_type, MTYPE_V);
  
  WN* apr[2];
  apr[0] = LWN_CreateParm(in_type, c1_lod, MTYPE_To_TY(in_type), 
                          WN_PARM_BY_VALUE);
  apr[1] = LWN_CreateParm(in_type, c2_lod, MTYPE_To_TY(in_type), 
                          WN_PARM_BY_VALUE);
  
  WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
  WN  *sto      = Get_Res_Reg(idx)->Simd_Preg_Stid(callNode);
  LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
  WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call with for pack                            *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Pack_Call(TYPE_ID out_type,
			      WN *operand1, WN *operand2, WN* result)
{
    TYPE_ID   in_type1  = WN_rtype(operand1);
    TYPE_ID   in_type2  = WN_rtype(operand2);

    /* the OPR_MOD is used to get PACK */
    OPERATOR  op        = OPR_MOD;
    INTRINSIC intrin_id = Find_Function(op, out_type);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    WN* apr[3];
    apr[0] = LWN_CreateParm(out_type, result, MTYPE_To_TY(out_type),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(in_type1, operand1, MTYPE_To_TY(in_type1), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type2, operand2, MTYPE_To_TY(in_type2), 
			    WN_PARM_BY_VALUE);

    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call with for SRA/SLL                         *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_SRL_Call(OPERATOR op, WN *operand1, WN* result)
{
    TYPE_ID   in_type1  = WN_rtype(operand1);
    TYPE_ID   out_type  = WN_rtype(result);

    /* passing hilo (1) to get SRA and SLL */
    INTRINSIC intrin_id = Find_Function(op, in_type1, MTYPE_UNKNOWN, 1);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    WN* apr[2];
    apr[0] = LWN_CreateParm(out_type, result, MTYPE_To_TY(out_type),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(in_type1, operand1, MTYPE_To_TY(in_type1), 
			    WN_PARM_BY_VALUE);

    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate a SIMD instrinsic call with for CMov                             *
 *---------------------------------------------------------------------------*/
WN *
SIMD_INFO::Generate_CMov_Call(WN *operand1, WN *operand2, WN* result)
{
    TYPE_ID   in_type1  = WN_rtype(operand1);
    TYPE_ID   in_type2  = WN_rtype(operand2);
    TYPE_ID   out_type  = WN_rtype(result);

    INTRINSIC intrin_id = CMov(out_type, in_type2);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    WN* apr[3];
    apr[0] = LWN_CreateParm(out_type, result, MTYPE_To_TY(out_type),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(in_type1, operand1, MTYPE_To_TY(in_type1), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type2, operand2, MTYPE_To_TY(in_type2), 
			    WN_PARM_BY_VALUE);

    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    return callNode;
}


bool
SIMD_INFO::Add_Message_Str (const char *msg)
{
  if (_message_set->Find(msg))
    return false;
  
  _message_set->Enter(msg, true);
  return true;
}


void
SIMD_SCALAR::Add_Lod_Sto (WN *expr)
{
  _lodsto.Push(expr);
  if (WN_operator(expr) == OPR_LDID) {
    Set_Has_Use();
  } else {
    Is_True(WN_operator(expr) == OPR_STID,("Expect a STID"));
    Set_Has_Def();
  }
}


/*-------------------------------------------------------------------------*
 * Checking loop index use                                                 *
 *-------------------------------------------------------------------------*/
void 
SIMD_SCALAR::Check_Loop_Index(WN *loop)
{
    for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	WN *ldid = Lod_Sto().Bottom_nth(i);
	Is_True(WN_operator(ldid) == OPR_LDID, ("Expect LDID"));
	DEF_LIST      *def_list = Du_Mgr->Ud_Get_Def(ldid);
	DEF_LIST_ITER  iter_def(def_list);
	for (DU_NODE *def_node = iter_def.First(); !iter_def.Is_Empty();
	     def_node = (DU_NODE *) iter_def.Next()){
	    WN *def = def_node->Wn();
	    if (LWN_Get_Parent(def) == loop) {
		Set_Loop_Index();

		/* will relax the following later */
		Set_Bad_Dep();
	    }
	    Is_True(def, ("Null pointer in USE-DEF chain"));
	    WN *stmt_block = Find_Stmt_Under(def, WN_do_body(loop));
	    if (stmt_block != NULL) {
		/* using inner loop index */
		Set_Bad_Dep();
		return;
	    }
	}
    }
}

/*-------------------------------------------------------------------------*
 * Checking definition is outside of loop                                  *
 *-------------------------------------------------------------------------*/
void 
SIMD_SCALAR::Check_Def_Outside(WN* loop)
{
    for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	WN *def = Lod_Sto().Bottom_nth(i);
	Is_True(WN_operator(def) == OPR_STID, ("Expect STID"));
	WN *stmt_block = Find_Stmt_Under(def, WN_do_body(loop));
	if (stmt_block != NULL) {
	    Set_Bad_Dep();
	    return;
	}
    }
}


/*-------------------------------------------------------------------------*
 * Checking scalar expansion                                               *
 *-------------------------------------------------------------------------*/
void 
SIMD_SCALAR::Check_Expansion(WN *loop)
{
  Is_True(Has_Use() && Has_Def(),
	  ("Can't check expansion for symbol %s", Symbol().Name()));

  /* Check privatization:
     (1) no live in
     (2) no live out */
  for (INT i = 0; i < Lod_Sto().Elements(); i++) {
    WN *ls = Lod_Sto().Bottom_nth(i);
    if (WN_operator(ls) == OPR_LDID) {
      /* Check live-in. */
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ls);
      DEF_LIST_ITER iter_def(def_list);
      for (DU_NODE *def_node = iter_def.First(); !iter_def.Is_Empty();
	   def_node = (DU_NODE *) iter_def.Next()){
	WN *def = def_node->Wn();
	Is_True(def, ("Null pointer in USE-DEF chain"));
	if (!Is_Descendent(def, WN_do_body(loop))) {
	  Set_Has_Exp_Use();
	  break;
	}
      }
    } else { /* Check live-out */
      Is_True(WN_operator(ls) == OPR_STID, ("Expect STID"));
      USE_LIST *use_list = Du_Mgr->Du_Get_Use(ls);
      USE_LIST_ITER iter_use(use_list);
      for (DU_NODE *use_node = iter_use.First(); !iter_use.Is_Empty();
	   use_node = (DU_NODE *) iter_use.Next()) {
	WN *use = use_node->Wn();
	Is_True(use, ("Null pointer in USE-DEF chain"));
	if (!Is_Descendent(use, WN_do_body(loop))) {
	  Set_Live_Out();
	  break;
	}
      }
    }
  }
  
  /* Set scalar expansion. */
  if (!Has_Exp_Use() && !Live_Out()) {
    Set_Private();
  }
}


WN *
SIMD_SCALAR::Get_Load_Copy()
{
    WN *copy = NULL;
    if (Has_Use()) {
	for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	    WN *cur = Lod_Sto().Bottom_nth(i);
	    if (WN_operator(cur) == OPR_LDID) {
		copy = LWN_Copy_Tree(cur);
		LWN_Copy_Def_Use(cur, copy, Du_Mgr);
		break;
	    }
	}
	Is_True(copy, ("Cannot find the load"));
    } else {
	WN *cur = Lod_Sto().Bottom_nth(0);
	OPCODE opc = OPCODE_make_op(OPR_LDID, Promote_Type(WN_desc(cur)), WN_desc(cur));
	copy = LWN_CreateLdid(opc, cur);
    }
    WN *t = Lod_Sto().Bottom_nth(0);
    SIMD_EINFO *old_info = Cur_Simd_Loop->E_Info().Find(t);
    Is_True(old_info, ("SIMD_SCALAR::Get_Load_Copy: old EINFO is NULL"));
    SIMD_EINFO *e_info = Cur_Simd_Loop->Create_Simd_EInfo(copy, old_info);
    Cur_Simd_Loop->E_Info().Enter(copy, e_info);
    SYMBOL symbol(t);
    SIMD_SCALAR *s_info = Cur_Simd_Loop->S_Info_Map().Find(symbol);
    Is_True(s_info, ("SIMD_SCALAR::Get_Load_Copy: old SINFO is NULL"));
    s_info->Add_Lod_Sto(copy);
    return copy;
}

/*--------------------------------------------------------------------------*
 * Check if an expression is a vector                                       *
 *--------------------------------------------------------------------------*/ 
static bool
Check_Reduction_Expression (WN *wn, REDUCTION_TYPE red, SIMD_LOOP *simd_loop)
{
  if (simd_loop->Invariant_In_Simd_Loop(wn)) {
    return true;
  }

  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_ILOAD) {
    return true;
  }
  
  if (oper == OPR_LDID) {
    /* check scalar */
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = simd_loop->S_Info_Map().Find(symbol);
    if (!s_info || s_info->Bad_Dep())
      return false;
    if (!s_info->Private() && s_info->Red_Type() != red) {
      return false;
    }
    
    return true;
  }
  
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    if (!Check_Reduction_Expression(WN_kid(wn, i), red, simd_loop))
      return false;
  }

  return true;
}


/*--------------------------------------------------------------------------*
 * Check if it is a reduction                                               *
 *--------------------------------------------------------------------------*/ 
void 
SIMD_SCALAR::Check_Reduction (WN *loop, INT depth, SIMD_LOOP *simd_loop)
{
  if (Private() || !Has_Def())
    return;
  
  /* Defined inside the loop, but not privatizable -- 
     check for reduction. */
  for (INT i = 0; i < Lod_Sto().Elements(); i++) {
    WN *ls = Lod_Sto().Bottom_nth(i);
    
    Is_True(Is_Descendent(ls, WN_do_body(loop)),
	    ("Scalar access to %s outside simd loop.", Symbol().Name()));
    
    REDUCTION_TYPE red = red_manager->Which_Reduction(ls);
    if (red == RED_NONE ||
	/* Different reduction */
	(Red_Type() != RED_NONE && Red_Type() != red)) {
      Set_Bad_Dep();
      return;
    }
    
    Set_Red_Type(red);
    
    if (WN_operator(ls) == OPR_LDID) {
      /* The loop that carries the reduction must be either the
	 SIMD loop, or a loop enclosing it. */
      DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ls);
      WN *def_loop = def_list->Loop_stmt();
      if (!def_loop || !Is_Descendent(loop, def_loop)) {
	Set_Bad_Dep();
	return;
      } 

      /* Check reduction is vector. */
      WN *red_wn = LWN_Get_Parent(ls);
      Is_True(WN_kid_count(red_wn) == 2,
	      ("Unexpected reduction wn kid count (%d)", WN_kid_count(red_wn)));
      WN *kid = WN_kid1(red_wn);
      if (kid == ls) {
	kid = WN_kid0(red_wn);
      }
      
      if (!Check_Reduction_Expression(kid, red, simd_loop)) {
	Set_Bad_Dep();
	return;
      }
      
      SIMD_EINFO *e_info = simd_loop->E_Info().Find(ls);
      Is_True(e_info != NULL, ("Null einfo!"));
      if (!Simd_Info->Reduction_Possible(red, e_info->Res_Type())) {
	SIMD_Msg(AT_MSG_SIMD_NO_REDUCTION, red_wn, SIMD_OPCODE_Msg(red_wn));
	Set_Bad_Dep();
	return;
      }
    }
  }
}


/*-------------------------------------------------------------------------*
 * Check missing DU/UD chains                                              *
 *-------------------------------------------------------------------------*/
bool 
SIMD_SCALAR::Check_Missing_Du()
{
    for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	WN *ls = Lod_Sto().Bottom_nth(i);
	if (WN_operator(ls) == OPR_LDID) {
	    /* check missing def */
	    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ls);
	    if (!def_list || (!def_list->Loop_stmt() && 
			      def_list->Incomplete())) {
              if (!Cur_Simd_Loop->Invariant_In_Simd_Loop(ls)) {
		Set_Bad_Dep();
		return true;
              }
	    }
	} else { /* check missing use */
	    Is_True(WN_operator(ls) == OPR_STID, ("Expect STID"));
	    USE_LIST *use_list = Du_Mgr->Du_Get_Use(ls);
	    if (!use_list) {
		Set_Bad_Dep();
		return true;
	    }
	}
    }
    return false;
}

/*-------------------------------------------------------------------------*
 * Test scalar dependence with repect to the cur loop                      *
 *-------------------------------------------------------------------------*/
void
SIMD_SCALAR::Setup_Scalar_Dependence_Info(WN *loop)
{
    if (Check_Missing_Du()) {
	return;
    }

    if (!Has_Def()) {
	/* check loop induction variable */
	Check_Loop_Index(loop);
    } else if (!Has_Use()) {
	Check_Def_Outside(loop);
    } else {
	/* both def and use */
	Check_Expansion(loop);
    }
}

// recursively copy the array dependences for all ILOADs from wn_orig to wn_copy
void
Simd_Copy_Dependence(WN *wn_orig, WN *wn_copy)
{
    OPERATOR op_orig = WN_operator(wn_orig);
    OPERATOR op_copy = WN_operator(wn_copy);
    Is_True(op_orig==op_copy,("Whirl trees don't match; can't copy dependences"));
    
    if (OPERATOR_is_leaf(op_orig)) {
	return;
    }
    
    if (op_orig==OPR_ILOAD) {
	ARRAY_DIRECTED_GRAPH16* adg = Array_Dependence_Graph;
	VINDEX16 v    = adg->Get_Vertex(wn_orig);
	if (v) {
	    VINDEX16 to_v = adg->Add_Vertex(wn_copy);
	    EINDEX16 edge = adg->Get_Out_Edge(v);
	    while (edge) {
		VINDEX16 sinkv  = adg->Get_Sink(edge);
		
		DEPV_ARRAY *orig_depv = adg->Depv_Array(edge);
		DEPV_ARRAY *depv = Create_DEPV_ARRAY(orig_depv, adg->Pool());
		adg->Add_Edge(to_v, sinkv, depv);
		edge = adg->Get_Next_Out_Edge(edge);
	    }
	    
	    edge = adg->Get_In_Edge(v);
	    while (edge) {
		VINDEX16 srcv  = adg->Get_Source(edge);
		
		DEPV_ARRAY *orig_depv = adg->Depv_Array(edge);
		DEPV_ARRAY *depv = Create_DEPV_ARRAY(orig_depv, adg->Pool());
		adg->Add_Edge(srcv, to_v, depv);
		edge = adg->Get_Next_In_Edge(edge);
	    }
	}
	return;
    }
    
    for (INT i=0; i<WN_kid_count(wn_orig); i++) {
	Simd_Copy_Dependence(WN_kid(wn_orig,i),WN_kid(wn_copy,i));
    }
}

// copy the top-level invariant information from wn_orig to wn_copy in inv_table
void
Simd_Copy_Invariant_Top_Level(WN *wn_orig, WN *wn_copy, INVAR_TABLE *inv_table) {
    Is_True(inv_table!=NULL,("Null invariant table."));
    
    BIT_VECTOR *bv_orig = inv_table->Find(wn_orig);
    if (bv_orig) {
	MEM_POOL *pool = inv_table->Mem_Pool();
	BIT_VECTOR *bv_copy = CXX_NEW(BIT_VECTOR(bv_orig->Size(),pool),pool);
	*bv_copy = *bv_orig;
	
	inv_table->Enter(wn_copy,bv_copy);
    }
}

// recursively copy the invariant information from wn_orig to wn_copy in inv_table
void
Simd_Copy_Invariant(WN *wn_orig, WN *wn_copy, INVAR_TABLE *inv_table) {
    Is_True(inv_table!=NULL,("Null invariant table."));
    
    OPERATOR op_orig = WN_operator(wn_orig);
    OPERATOR op_copy = WN_operator(wn_copy);
    Is_True(op_orig==op_copy,("Operators don't match; can't copy invariant info"));
    
    Simd_Copy_Invariant_Top_Level(wn_orig, wn_copy, inv_table);
    
    if (OPERATOR_is_leaf(op_orig)) {
	return;
    }

    if (op_orig == OPR_BLOCK) {
      WN *kid_orig = WN_first(wn_orig);
      WN *kid_copy = WN_first(wn_copy);
      while (kid_orig) {
        Is_True(kid_copy, ("Blocks don't match; can't copy invariant info"));
        Simd_Copy_Invariant(kid_orig, kid_copy, inv_table);
        kid_orig = WN_next(kid_orig);
        kid_copy = WN_next(kid_copy);
      }
      Is_True(!kid_copy, ("Blocks don't match; can't copy invariant info"));
      return;
    }
    
    for (INT i = 0; i < WN_kid_count(wn_orig); i++) {
      Simd_Copy_Invariant(WN_kid(wn_orig, i), WN_kid(wn_copy, i), inv_table);
    }
}

// recursively remove the invariant information for wn in inv_table
static void
Simd_Remove_Invariant(WN *wn, INVAR_TABLE *inv_table) {
    Is_True(inv_table!=NULL,("Null invariant table."));
    
    if (inv_table->Find(wn)) {
	inv_table->Remove(wn);
    }
    
    if (OPERATOR_is_leaf(WN_operator(wn))) {
	return;
    }
    
    for (INT i=0; i<WN_kid_count(wn); i++) {
	Simd_Remove_Invariant(WN_kid(wn,i),inv_table);
    }
}


class SIMD_INV {
    WN        *_wn;
    WN        *_loop;
    SIMD_PREG *_preg;

public:
    SIMD_INV(WN *wn, WN *loop, SIMD_PREG *preg) :
	_wn(wn), _loop(loop), _preg(preg) 
    {}
    ~SIMD_INV()                {}

    WN*        Wn(void)        { return _wn;  }
    WN*        Loop(void)      { return _loop;}
    SIMD_PREG* Simd_Preg(void) { return _preg;}
};

typedef DYN_ARRAY <SIMD_INV *> SIMD_INV_ARRAY;

/*-----------------------------------------------------------------------*
 * Find existing invariant                                               *
 *-----------------------------------------------------------------------*/
SIMD_PREG*
SIMD_Find_Existing(WN *wn, WN *inv_loop, SIMD_INV_ARRAY *inv_array,
		   WN *& stid)
{
    for (INT i = 0; i < inv_array->Elements(); i++) {
	SIMD_INV *cur = inv_array->Get(i);
	if (cur->Loop() == inv_loop && Tree_Equiv(cur->Wn(), wn)) {
	    stid = LWN_Get_Parent(cur->Wn());
	    Is_True(stid && WN_operator(stid) == OPR_STID, ("Expecting STID"));
	    return cur->Simd_Preg();
	}
    }
    return NULL;
}

/*-----------------------------------------------------------------------*
 * Move loop invariant expression out                                    *
 *-----------------------------------------------------------------------*/
WN*
SIMD_Move_Invariant(WN *wn, WN *inv_loop, SIMD_INV_ARRAY *inv_array, 
		    bool &exist)
{
    WN *parent = LWN_Get_Parent(wn);
    Is_True(parent != NULL, ("SIMD_Move_Invariant: parent is NULL"));
    
    exist = true;
    WN *stid = NULL;
    SIMD_PREG *preg = SIMD_Find_Existing(wn, inv_loop, inv_array, stid);
    SIMD_EINFO *einfo = Cur_Simd_Loop->Get_E_Info(wn);
    if (preg == NULL) {
	exist = false;
	preg = Gen_Symbol(WN_rtype(wn), &SIMD_local_pool, "inv");
	SIMD_INV *new_inv = 
	    CXX_NEW(SIMD_INV(wn, inv_loop, preg), &SIMD_local_pool);
	inv_array->AddElement(new_inv);
	
	stid = preg->Simd_Preg_Stid(wn);
	LWN_Insert_Block_Before(LWN_Get_Parent(inv_loop), inv_loop, stid);
	WN_linenum(stid) = LWN_Get_Linenum(wn);
        
        /* If the invariant is still in the simd loop, attach the appropriate
           expression and scalar info structures. */
        if (Is_Descendent(stid, Cur_Simd_Loop->Simd_Loop())) {
          if (einfo) {
            SIMD_EINFO *copy_einfo = Cur_Simd_Loop->Create_Simd_EInfo(stid, einfo);
            Cur_Simd_Loop->E_Info().Enter(stid, copy_einfo);
          }
          
          SYMBOL symbol(stid);
          Is_True(!Cur_Simd_Loop->S_Info_Map().Find(symbol),
                  ("Symbol already exists"));
          SIMD_SCALAR *s_info = CXX_NEW(SIMD_SCALAR(symbol, stid, Cur_Simd_Loop->Pool()),
                                        Cur_Simd_Loop->Pool());
          Cur_Simd_Loop->S_Info_Map().Enter(symbol, s_info);
          Cur_Simd_Loop->S_Info_Stack().Push(s_info);
        }
        
	if (simd_debug) {
	  fprintf(TFile,"--- SIMD invariant expression:\n");
	  fdump_tree(TFile,wn);
	  fprintf(TFile,"--- SIMD invariant STID:\n");
	  fdump_tree(TFile,stid);
	}
    } 
    
    WN *ldid = preg->Simd_Preg_Ldid();
    if (einfo) {
      SIMD_EINFO *copy_einfo = Cur_Simd_Loop->Create_Simd_EInfo(ldid, einfo);
      Cur_Simd_Loop->E_Info().Enter(ldid, copy_einfo);
    }
    
    SYMBOL symbol(ldid);
    SIMD_SCALAR *s_info = Cur_Simd_Loop->S_Info_Map().Find(symbol);
    if (s_info == NULL) {
      s_info = CXX_NEW(SIMD_SCALAR(symbol, ldid, Cur_Simd_Loop->Pool()),
		       Cur_Simd_Loop->Pool());
      Cur_Simd_Loop->S_Info_Map().Enter(symbol, s_info);
      Cur_Simd_Loop->S_Info_Stack().Push(s_info);
    } else {
      s_info->Add_Lod_Sto(ldid);
    }

    Du_Mgr->Add_Def_Use(stid, ldid);
    for (INT i = 0; i < WN_kid_count(parent); i++) {
	if (WN_kid(parent, i) == wn) {
	    WN_kid(parent, i) = ldid;
	    LWN_Set_Parent(ldid, parent);
	    return ldid;
	}
    }
}


INT
SIMD_Move_Invariant_Update_Dep (WN *wn, bool delete_only)
{
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    if (!SIMD_Move_Invariant_Update_Dep(WN_kid(wn, i), delete_only))
      return 0;
  }
  
  ARRAY_DIRECTED_GRAPH16* adg = Array_Dependence_Graph;
  VINDEX16 v = adg->Get_Vertex(wn);
  if (!v)
    return 1;
  
  for (INT dir = 0; dir < 2; dir++) {
    /* dir == 0 -- output edges
       dir == 1 -- input edges */
    bool is_out = (dir == 0);
    EINDEX16 edge = is_out ? adg->Get_Out_Edge(v) : adg->Get_In_Edge(v);
    while (edge) {
      EINDEX16 cur_edge = edge;
      edge = is_out ? adg->Get_Next_Out_Edge(edge) : adg->Get_Next_In_Edge(edge);
      VINDEX16 tv = is_out ? adg->Get_Sink(cur_edge) : adg->Get_Source(cur_edge);
      
      INT new_dims = 0;

      if (!delete_only) {
        /* If 'delete_only' is true, new_dims will remain 0, and we'll
           delete the edge. Otherwise, find the target vertex and based
           on the common ancestor loop, determine the correct number of
           dependence vector dimensions. */
        WN *twn = adg->Get_Wn(tv);
        WN *common = LNO_Common_Loop(wn, twn);
        new_dims = Good_Do_Depth(common) + 1;
      }
      
      DEPV_ARRAY *orig_depv = adg->Depv_Array(cur_edge);
      Is_True(new_dims <= orig_depv->Num_Dim(),
              ("New edge needs more dimensions than it used to have..."));
      
      if (new_dims == 0) {
        /* Delete the edge */
        adg->Delete_Edge(cur_edge);
      } else if (new_dims < orig_depv->Num_Dim()) {
        DEPV_ARRAY *depv = orig_depv->Shorten(new_dims, adg->Pool());

        /* delete the edge */
        adg->Delete_Edge(cur_edge);
        
        VINDEX16 srcv = is_out ? v : tv;
        VINDEX16 snkv = is_out ? tv : v;
        if (adg->Add_Edge(srcv, snkv, depv) == 0) {
          return 0;
        }
      } /* else the number of dimensions remains unchanged. */
    }
  }
  
  if (delete_only || (Good_Do_Depth(wn) < 0)) {
    adg->Delete_Vertex(v);
  }
  
  return 1;
}


void
SIMD_Move_Invariant(WN *wn, DOLOOP_STACK *do_stack, INVAR_TABLE *invar_table,
		    SIMD_INV_ARRAY *inv_array)
{
    OPCODE opcode = WN_opcode(wn);

    if (opcode == OPC_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    SIMD_Move_Invariant(kid, do_stack, invar_table, inv_array);
	}
	return;
    }

    if (opcode == OPC_DO_LOOP) {
      SIMD_Move_Invariant(WN_do_body(wn),do_stack,invar_table,inv_array);
      return;
    }

    if (WN_operator(wn) == OPR_PARM) {
      // skip through PARM nodes
      Is_True(WN_kid_count(wn)==1,("Expected OPR_PARM with 1 kid."));
      SIMD_Move_Invariant(WN_kid0(wn),do_stack,invar_table,inv_array);
      return;
    }
    
    if (OPCODE_is_leaf(opcode) || WN_operator(wn) == OPR_ARRAY) {
	/* don't move leaf or array nodes */
	return;
    }
    
    INT inv_level = -1;
    BIT_VECTOR *bv = invar_table->Find(wn);
    if (bv && bv->Pop_Count()) {
	for (INT i = do_stack->Elements()-1; i >= 0; i--) {
	    if (bv->Test(i)) {
		inv_level = i;
	    } else {
		break;
	    }
	}
    }
    
    if (inv_level>=0) {
	WN *enclosing_loop = Enclosing_Do_Loop(wn);
	if (inv_level <= Do_Depth(enclosing_loop)) {
	    /* move the invariant out */
	    WN *invar_loop = do_stack->Bottom_nth(inv_level);
	    bool duplicate;
	    WN *ldid = SIMD_Move_Invariant(wn, invar_loop,
					   inv_array, duplicate);
	    
	    /* update the dependence edge */
	    if (!SIMD_Move_Invariant_Update_Dep(wn, duplicate)) {
		LNO_Erase_Dg_From_Here_In(invar_loop,
                                          Array_Dependence_Graph);
		return;
	    }
	    
	    /* add the new LDID to the invariant table */
	    Simd_Copy_Invariant_Top_Level(wn,ldid,invar_table);
	    
	    if (duplicate) {
		Simd_Remove_Invariant(wn,invar_table);
		LWN_Delete_Tree(wn);
	    }
	    return;
	}
    }

    // the expression wasn't moved out, try the kids
    for (INT i = 0; i < WN_kid_count(wn); i++) {
	SIMD_Move_Invariant(WN_kid(wn, i), do_stack, 
			    invar_table, inv_array);
    }
}

static
DOLOOP_STACK *SIMD_Get_Doloop_Stack(WN *wn_outer)
{
    DOLOOP_STACK *do_stack = CXX_NEW(DOLOOP_STACK(&SIMD_local_pool),
				     &SIMD_local_pool);
    WN *cur = wn_outer;
    WN *wn_inner;

    do {
	wn_inner = cur;
	cur = Find_Next_Innermost_Do(cur);
    } while (cur != NULL);

    Build_Doloop_Stack(wn_inner, do_stack);
    return do_stack;
}

void
SIMD_Move_Invariant(WN *wn_outer, INVAR_TABLE *invar_table)
{
    MEM_POOL_Popper popper(&SIMD_local_pool);
    DOLOOP_STACK *do_stack    = SIMD_Get_Doloop_Stack(wn_outer);
    SIMD_INV_ARRAY *inv_array = CXX_NEW(SIMD_INV_ARRAY(&SIMD_local_pool),
					&SIMD_local_pool);

    Mark_Invar(WN_do_body(wn_outer), do_stack->Elements(), do_stack, 
	       invar_table, &LNO_local_pool, FALSE, TRUE);
    
    SIMD_Move_Invariant(wn_outer, do_stack, invar_table, inv_array);
}


#define ABS(x) ( ((x) < 0) ? -(x) : (x))

/*
   GCD
   
   Find the greatest common divisor of a and b. If a = b = 0,
   return 0. The returned value is always a non-negative number.
*/

static INT64
gcd (INT64 a, INT64 b)
{
  a = ABS(a);
  b = ABS(b);
  if (b == 0) {
    return a;
  }

  INT r;
  while ((r = a % b) != 0) {
    a = b;
    b = r;
  }

  return b;
}


/* Find the (known) GCD of the access vector 'av' and 'max' based on 'atab'
   If max=0, finds the greatest denominator of 'av' only. */
INT
gcd (ACCESS_VECTOR *av, ALIGNMENT_TABLE *atab, INT max)
{
  INT res = gcd(av->Const_Offset, max);
  
  for (INT i=0; i<av->Nest_Depth(); i++) {
    res = gcd(av->Loop_Coeff(i),res);
  }
  if (av->Lin_Symb) {
    INTSYMB_ITER iter(av->Lin_Symb);
    for (INTSYMB_NODE *n = iter.First(); !iter.Is_Empty(); n = iter.Next()) {
      INT c = gcd(n->Coeff, res);
      INT align = atab->Find(n->Symbol);
      if (align > 1) {
	c = gcd(c * align, res);
      }
      res = gcd(c, res);
    }
  }
  if (av->Non_Lin_Symb) {
    SUMPROD_ITER iter(av->Non_Lin_Symb);
    for (SUMPROD_NODE *n = iter.First(); !iter.Is_Empty(); n=iter.Next()) {
      INT64 c = gcd(n->Coeff,res);
      if (n->Prod_List) {
	SYMBOL_ITER iters(n->Prod_List);
	for (SYMBOL_NODE* sn = iters.First(); !iters.Is_Empty(); sn = iters.Next()) {
	  INT align = atab->Find(sn->Symbol);
	  if (align > 1) {
	    c = gcd(c * align, res);
	  }
	}
      }
      res = gcd(c,res);
    }
  }
  return res;
}

// Needs_Wind_Down tries to verify whether loop unrolling of 'loop' by 'unroll_amount'
// needs a wind-down loop based on the information in 'atab', the difference between
// the upper and the lower bound, and the step of the loop
static bool
Needs_Wind_Down(WN *loop, INT unroll_amount,
		ALIGNMENT_TABLE *atab, MEM_POOL *pool) {
  Is_True(unroll_amount>0,("Bad unroll amount %d",unroll_amount));
  
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
  Is_True(dli,("Can't find DO_LOOP_INFO."));
  
  ACCESS_ARRAY *lb_array = dli->LB;
  if (lb_array->Num_Vec() != 1) // should not be a MIN/MAX of multiple expressions
    return true;
  
  ACCESS_VECTOR *lb_vector = lb_array->Dim(0);
  if (lb_vector->Too_Messy)
    return true;
  
  ACCESS_ARRAY *ub_array = dli->UB;
  if (ub_array->Num_Vec() != 1) // should not be a MIN/MAX of multiple expressions
    return true;
  
  ACCESS_VECTOR *ub_vector = ub_array->Dim(0);
  if (ub_vector->Too_Messy)
    return true;
  
  INT64 step = Step_Size(loop);
  if (step<=0) {
    // TODO: Figure out how everything works for negative steps
    return true;
  }
  
  ACCESS_VECTOR *diff = Add(lb_vector, ub_vector, pool);
  if (diff->Too_Messy)
    return true;
  
  diff->Const_Offset += step;
  
  if (simd_debug) {
    fprintf(TFile,("LB:\n"));
    lb_vector->Print(TFile,true);
    fprintf(TFile,("\n"));
    fprintf(TFile,("UB:\n"));
    ub_vector->Print(TFile,true);
    fprintf(TFile,("\n"));
    fprintf(TFile,("Difference:\n"));
    diff->Print(TFile);
    fprintf(TFile,("\n"));
  }
  
  INT req_factor = step*unroll_amount;
  
  INT factor = gcd(diff,atab,req_factor);
  if (simd_debug) {
    fprintf(TFile,"Loop bound difference is multiple of %d, step %d, unroll %d\n",
	    factor,(int)step,unroll_amount);
  }
  
  // the only way this can be true is if ABS(factor)==ABS(req_factor)
  return factor%req_factor!=0;
} // Needs_Wind_Down


static void
Update_Alignment_Table (ALIGNMENT_TABLE *update_table, WN *wn)
{
  if (WN_operator(wn) == OPR_LDID) {
    SYMBOL sym(wn);
    ALIGNMENT_TABLE *global_table = Simd_Info->Alignment_Table();
    if (global_table->Find(sym))
      return;

    /* Folllow the DU-chain to find the alignment. For LDID'ed symbols,
       'alignment' in our context is the greatest power-of-two factor of
       their value. */
    INT align = Ldid_Def_Alignment(wn);
    if (align > 1) {
      global_table->Enter(sym, align);
      update_table->Enter(sym, align);
      
      if (simd_debug) {
	fprintf(TFile, "Update symbol '%s' local alignment %d.\n", sym.Name(), align);
      }
    }
    
    return;
  }

  for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++) {
    Update_Alignment_Table(update_table, WN_kid(wn, kid_idx));
  }
}


static ALIGNMENT_TABLE *
Simd_Update_Alignment_Table (MEM_POOL *temp_pool, WN *loop)
{
  ALIGNMENT_TABLE *update_table = CXX_NEW(ALIGNMENT_TABLE(31, temp_pool), temp_pool);
  Update_Alignment_Table(update_table, WN_start(loop));
  return update_table;
}


static void
Simd_Revert_Alignment_Table (ALIGNMENT_TABLE *update_table)
{
  ALIGNMENT_TABLE *global_table = Simd_Info->Alignment_Table();
  SYMBOL sym;
  INT align;
  ALIGNMENT_TABLE_ITER iter(update_table);
  while (iter.Step(&sym, &align)) {
    global_table->Remove(sym);
  }
}

//-----------------------------------------------------------------------
// ROUTINES FOR SIMD CODE GENERATION
//-----------------------------------------------------------------------

//-----------------------------------------------------------------------
// NAME: SNL_Simd_Loop
// FUNCTION: SIMD tile the 'outerloop' with an unrolling factor of 
//   'u'. 
// WARNING: I'm not sure of the exact meaning of all of these parameters,
//   so I'll attempt to fill them in later. 
//-----------------------------------------------------------------------

extern SNL_REGION SNL_Simd_Loop(WN* outerloop,
				INT simd_u,
				INT nloops,
				BOOL unroll_just_inner,
				EST_REGISTER_USAGE est_register_usage,
				SX_INFO* pinfo,
				INT pinfo_depth,
				BOOL no_further_unroll,
				WN* permloop[],
				HASH_TABLE<WN*,WN*>** loop_map_ptr,
				SX_INFO** wdpinfo_ptr)
{
  // SNL_Simd_Loop() is passed a loop.  That loop is the outer loop being
  // unrolled.  u copies are made and passed to Dror's routine to update the
  // dependences.  Then they are grafted into one big loop.  E.g.
  // 	do i
  //	  S1
  //	  do j
  //	    S2
  // with u=2 means that we duplicate i's body and pass the original and body
  // to dror's routines.  Then we increment i in the copy and merge the bodies
  // back into the original i and j loops, adjusting the step of i (or is that
  // done elsewhere ... and I assume that the wind-down stuff is already taken
  // care of).  So we get
  // 	do i
  //	  S1; S1'
  //	  do j
  //	    S2; S2'
  // and we have only leftover copies of do j and empty bodies to delete.
  //
  // NOTE: SNL_Simd_Loop() does not update the DO annotations properly.
  // That's ok, just call Renumber_Loops() after using it.
  // TODO OK: The privatizable info must be up-to-date.  Right now, if there is
  // no permutation or cache tiling occurs, then the pinfo isn't altered and
  // there is no problem; and if one of those occurs, then everything is
  // scalar expanded except the inner loop, so no problem.  But if some day
  // we scalar expand some variables (or none) and do a permutation, then the
  // pinfo must be updated.
  //
  // If wdpinfo_ptr is set, then wdpinfo is returned.  It's allocated from
  // LNO_local_pool and, if deleted, must be handles appropriately.  Likewise
  // for loop_map_ptr.
  // 
  // The "further_unrolling" only works for invariant nests, because with
  // general nests the body is mucked around with in a nasty way.  So pass
  // an indicator to that effect.

  ARRAY_DIRECTED_GRAPH16*	dg = Array_Dependence_Graph;
  DU_MANAGER*			du = Du_Mgr;
  
  if (simd_debug) {
      fprintf(TFile,
	      "\n"
	      "WHIRL before peel, wind-down and prescan for SIMD transformation:\n"
	      "-----------------------------------------------------------------\n");
      fdump_tree(TFile,outerloop);
  }

  // Save the previous and the next statements to form the SNL region at the end
  WN *region_prev = WN_prev(outerloop);
  WN *region_next = WN_next(outerloop);
  WN *region_block = LWN_Get_Parent(outerloop);
  Is_True(WN_operator(region_block)==OPR_BLOCK,("No enclosing block around a loop!"));

  /* screen out too small trip count */
  INT64 iteration_count = Iterations(outerloop, &LNO_local_pool);
  if (iteration_count >= 0 && iteration_count < simd_u) {
      if (loop_map_ptr) {
	  *loop_map_ptr = NULL;
      }
      if (wdpinfo_ptr) {
	  *wdpinfo_ptr = NULL;
      }
      SNL_REGION region(outerloop, outerloop);
      return region;
  }
  
  Cur_Simd_Loop->Set_Simd_Loop(outerloop);
  bool is_trapezoidal = Cur_Simd_Loop->Trapezoidal();
  
  // loop peeling if necessary
  Cur_Simd_Loop->Reset_Peeled();
  if (Cur_Simd_Loop->Peel_Align()) {
      pinfo = Cur_Simd_Loop->Simd_Loop_Peeling(outerloop, pinfo, permloop,
					       Simd_Info, &SNL_local_pool,
					       simd_u);
      outerloop=Cur_Simd_Loop->Simd_Loop();
      Is_True((outerloop && WN_operator(outerloop)==OPR_DO_LOOP),
	      ("Wrong peeling in SIMD."));
  }
  
  // create a brand new SIMD_LOOP for the transformation
  // this is necessary because outer loop unroll may create multiple loops
  // to SIMD (in the main nest and in the wind-down nest), so that all
  // data structures need to be reset
  SIMD_LOOP *new_simd_loop =
    SIMD_LOOP_New(Cur_Simd_Loop->Loop_Model(), outerloop, Do_Loop_Depth(outerloop));
  
  if (Cur_Simd_Loop->Peeled()) {
    // necessary for setting the aligned attribute on the previously
    // unaligned accesses
    new_simd_loop->Set_Peeled();
  }
  new_simd_loop->Set_Max_Vector_Length(Cur_Simd_Loop->Max_Vector_Length());
  
  // forget about the last SIMD_LOOP
  // note that the original model SIMD_LOOP is still in the LOOP_MODEL
  Cur_Simd_Loop = new_simd_loop;
  
  /* build an invariant table */
  MEM_POOL_Popper popper(&LNO_local_pool);
  
  INVAR_TABLE *inv_table = CXX_NEW(INVAR_TABLE(511, &LNO_local_pool), &LNO_local_pool);
  
  // move invariants out 
  SIMD_Move_Invariant(outerloop, inv_table);
  Cur_Simd_Loop->Set_Invar_Table(inv_table);

  /* Update the alignment table with local alignment info. */
  ALIGNMENT_TABLE *update_align_table =
    Simd_Update_Alignment_Table(&LNO_local_pool, outerloop);
  
  /* Rescan the outer loop to build the SIMD data structures. */
  Cur_Simd_Loop->Simd_Rescan_Loop_Body(outerloop);

  if (Cur_Simd_Loop->V_Unroll_Factor() <= 0) {
      Is_True(Cur_Simd_Loop->Too_Small(), ("Expected too small trip count."));
      
      /* too small trip count, such as the 2D register tiled inner loops */
      // Form the SNL region and verify it
      WN *region_first = region_prev?WN_next(region_prev):WN_first(region_block);
      WN *region_last = region_next?WN_prev(region_next):WN_last(region_block);
      Is_True(region_first && region_last,("Bad region formed."));
      
      SNL_REGION region(region_first,region_last);
      if (!Valid_SNL_Region(region)) {
	  DevWarn("SNL_Simd_Loop: Invalid SNL_REGION [0x%x,0x%x]",
		  region.First, region.Last);
      }
      
      if (simd_debug) {
	  fprintf(TFile,
		  "\n"
		  "Not vectorizing low iteration count loop\n");
	  fprintf(TFile,
		  "\n"
		  "WHIRL after SIMD transformation:\n"
		  "--------------------------------\n");
	  for (WN *wn=region.First; wn!=NULL; wn=WN_next(wn)) {
	      fdump_tree(TFile,wn);
	      if (wn==region.Last)
		  break;
	  }
      }

      
      /* Revert any local changes to the global alignment table. */
      Simd_Revert_Alignment_Table(update_align_table);

      return region;
  }

  /* setup scalar expansion */
  if (is_trapezoidal) {
      Cur_Simd_Loop->Set_Trapezoidal();
      Cur_Simd_Loop->Setup_Scalar_Expansion(Simd_Info);
  }

  if (Prompf_Info != NULL && Prompf_Info->Is_Enabled()) {
    INT loop_id = WN_MAP32_Get(Prompf_Id_Map, outerloop); 
    Prompf_Info->Register_Tile(loop_id);
  } 

  WN*                       wdloop = NULL;
  SX_INFO*                  wdpinfo = NULL;
  HASH_TABLE<WN*,WN*>*      loop_map = NULL;

  FmtAssert(simd_u > 1, ("SIMD unrolling too little: %d\n", simd_u));
  INT64 iters = Iterations(outerloop, &SNL_local_pool);
  bool do_winddown = ((iters < 0) ?
		      Needs_Wind_Down(outerloop, simd_u,
				      Simd_Info->Alignment_Table(), &SNL_local_pool) :
		      ((iters % simd_u) != 0));
  
  if (do_winddown) {
    // if the unrolled fit, so does this.  Otherwise, who knows.
    EST_REGISTER_USAGE ru;
    ru.Set_Fits(est_register_usage.Fits());
    wdloop = Wind_Down(outerloop, simd_u, FALSE, ru);

    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wdloop);
    dli->Est_Max_Iterations_Index = simd_u;
    dli->Est_Num_Iterations = simd_u /2;

    loop_map = Make_Loop_Mapping(outerloop, wdloop, &SNL_local_pool);

    // If the lower bound and upper bound are constant, replace the lower
    // bound of the wind down loop. 
    WN* lower_bound = WN_kid0(WN_start(outerloop)); 
    WN* upper_bound = SNL_UBexp(WN_end(outerloop)); 
    if (WN_operator(lower_bound) == OPR_INTCONST 
	&& WN_operator(upper_bound) == OPR_INTCONST) { 

      // Replace the lower bound of the wind down loop.
      INT64 lb = WN_const_val(lower_bound); 
      INT64 ub = WN_const_val(upper_bound); 
      INT64 wdlb = lb + iters/simd_u * simd_u; 
      
      LWN_Delete_Tree(WN_kid0(WN_start(wdloop)));
      WN* wd_lower_bound = LWN_Copy_Tree(lower_bound, TRUE, LNO_Info_Map); 
      WN_kid0(WN_start(wdloop)) = wd_lower_bound; 
      LWN_Copy_Frequency(WN_start(wdloop),wd_lower_bound);
      LWN_Set_Parent(wd_lower_bound, WN_start(wdloop));
      WN_const_val(wd_lower_bound) = wdlb; 
    }
    if ((wdloop && !no_further_unroll &&
         simd_u >= LNO_Outer_Unroll_Min_For_Further_Unroll) ||
        wdpinfo_ptr) {
	wdpinfo = CXX_NEW(SX_INFO(*pinfo, outerloop,
				  loop_map, &SNL_local_pool), &SNL_local_pool);
    }

    // make blocked loop have upper bound of simd_u-1 less, step of simd_u;
    // unnecessary in !do_winddown case

    // When the loop counts are small, subtracting simd_u-1 from the upper
    // bound may actually lead to an infinite loop when the comparison
    // is unsigned.
    if (MTYPE_is_unsigned(WN_desc(WN_end(outerloop)))) {
	Increase_By(SNL_UBvar(WN_end(outerloop)), (simd_u-1), WN_end(outerloop));
    } else {
	Increase_By(SNL_UBexp(WN_end(outerloop)), -(simd_u-1), WN_end(outerloop));
    }
  }

  // adjust step

  INT64  ostep = Step_Size(outerloop, simd_u);
  FmtAssert(ostep == 1, ("Non-unit step %" LLD_FMT " for loop %s",
                         ostep, SYMBOL(WN_index(outerloop)).Name()));
  
  WN *guard = Guard_A_Do(outerloop, false);
  
  if (simd_debug)
  {
    // Form the SNL region and verify it
    WN *region_first = region_prev?WN_next(region_prev):WN_first(region_block);
    WN *region_last = region_next?WN_prev(region_next):WN_last(region_block);
    Is_True(region_first && region_last,("Bad region formed."));
    
    SNL_REGION region(region_first,region_last);
    if (!Valid_SNL_Region(region)) {
      DevWarn("SNL_Simd_Loop: Invalid SNL_REGION [0x%x,0x%x]",
	      region.First, region.Last);
    }
    
    fprintf(TFile,
	    "\n"
	    "WHIRL right before SIMD transformation:\n"
	    "---------------------------------------\n");
    for (WN *wn = region.First; wn != NULL; wn = WN_next(wn)) {
      fdump_tree(TFile, wn);
      if (wn == region.Last)
	break;
    }
  }


  // Now unroll body (simd_u) with index increased 1 further each time.

  WN** unroll_body = CXX_NEW_ARRAY(WN*, 2, &SNL_local_pool);
  unroll_body[0] = outerloop;
 
  unroll_body[1] = LWN_Copy_Tree(outerloop);
  Cur_Simd_Loop->Set_Permloop(permloop);
  Cur_Simd_Loop->Simd_Transform_Loop(outerloop, Simd_Info, unroll_body[1]);
  if (Cur_PU_Feedback)
      Cur_PU_Feedback->FB_unroll_loop(outerloop, simd_u, true /* jamming */);

  INT i;
  if (Cur_Simd_Loop->Update_Dependence() == 0) {
      /* failed dependence update */
      for (i = 0; i < 1; i++) {
	  LNO_Erase_Dg_From_Here_In(unroll_body[i], dg);
      }
      Unmapped_Vertices_Here_Out(LWN_Get_Parent(outerloop)); 
  }
  
  /* Revert any local changes to the global alignment table. */
  Simd_Revert_Alignment_Table(update_align_table);
  
#if 0      
  /* There seems to be no new reduction (in the traditional sense),
     after SIMDizing a loop. I could be wrong though.  I need to think 
     through this later. */
  if (red_manager)
    red_manager->Unroll_Update(unroll_body, u);
#endif 

  // If there are privatizable scalars in this loop, or in loops farther
  // inside (including the innermost loop), make copies.  That is,
  // rename every privatizable scalar on in when duplicating the bodies.  

  INT privcnt = 0;
  SX_PITER ii(&pinfo->Plist);
  INT outer = Do_Depth(outerloop);
  for (SX_PNODE* n = ii.First(); !ii.Is_Empty(); n = ii.Next()) {
    switch (n->Transformable(outer)) {
     case SX_PNODE::SE_NOT_REQD:
      break;
     case SX_PNODE::SE_REQD:
      if (n->Expansion_Depth() >= pinfo_depth)
        privcnt++;
      break;
     case SX_PNODE::ILLEGAL:
	 /*-----------------------------------------------------------------
	   FmtAssert(0, ("Bug: can't expand scalar %s", n->Symbol().Name()));
	   Allow this to pass for inner loop vectorization. p.t. 
	   -----------------------------------------------------------------*/
      break;
     default:
      FmtAssert(0, ("Illegal value for SX_PNODE::STATUS"));
      break;
    }
  }

  if (privcnt > 0) {
    SYMBOL*     oldsyms = CXX_NEW_ARRAY(SYMBOL, privcnt, &SNL_local_pool);
    WN**        rloop   = CXX_NEW_ARRAY(WN*, privcnt, &SNL_local_pool);
    INT*        srqd    = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);
    INT*        nsrqd   = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);
    INT*        ed      = CXX_NEW_ARRAY(INT, privcnt, &SNL_local_pool);

    INT privcnt2 = 0;
    SX_PITER ii(&pinfo->Plist);
    SX_PNODE* nnext = NULL;
    for (SX_PNODE* n = ii.First(); n; n = nnext) {
      nnext = ii.Next();
      if (n->Transformable(outer) != SX_PNODE::SE_REQD)
        continue;

      if (n->Expansion_Depth() >= pinfo_depth) {
        oldsyms[privcnt2] = n->Symbol();
        rloop[privcnt2] = n->Reduction_Carried_By();
        srqd[privcnt2] = n->Outer_Se_Reqd();
        nsrqd[privcnt2] = n->Outer_Se_Not_Reqd();
        ed[privcnt2++] = n->Expansion_Depth();
	// don't remove pinfo!  AFter all, it's still there.
      }
    }
    FmtAssert(privcnt == privcnt2, ("Just checking .. easy to mess up"));

    SYMBOL*  newsyms = CXX_NEW_ARRAY(SYMBOL, (2-1)*privcnt, &SNL_local_pool);
    WN**     ancestors = CXX_NEW_ARRAY(WN*, privcnt, &SNL_local_pool);

    INT      newsymscnt = 0;
    for (i = 1; i < 2; i++) {
      for (INT j = 0; j < privcnt; j++) {
        const INT  bufsz = 64;
        char       buf[bufsz];
        INT        bufcnt;

        ancestors[j] = unroll_body[i];  // same for all of them
        bufcnt = sprintf(buf, "$rse_");
	oldsyms[j].Name(buf+bufcnt, bufsz-bufcnt);
	SYMBOL newsym = Create_Preg_Symbol(buf, oldsyms[j].Type);
	newsyms[newsymscnt++] = newsym;
        pinfo->Enter(NULL, newsym, rloop[j], srqd[j], nsrqd[j], ed[j], 
	  FALSE, FALSE);
      }
      Replace_Symbols(unroll_body[i], oldsyms,
		      &newsyms[privcnt*(i-1)], privcnt, NULL, ancestors);
    }

    CXX_DELETE_ARRAY(oldsyms, &SNL_local_pool);
    CXX_DELETE_ARRAY(srqd, &SNL_local_pool);
    CXX_DELETE_ARRAY(nsrqd, &SNL_local_pool);
    CXX_DELETE_ARRAY(rloop, &SNL_local_pool);
    CXX_DELETE_ARRAY(ed, &SNL_local_pool);

    CXX_DELETE_ARRAY(newsyms, &SNL_local_pool);
    CXX_DELETE_ARRAY(ancestors, &SNL_local_pool);
  }

  // Grafting back in.  Find the next innermost body in each case
  // and throw them all back into the original nest.

  WN** loop = CXX_NEW_ARRAY(WN*, 2, &SNL_local_pool);
  WN** nloop = CXX_NEW_ARRAY(WN*, 2, &SNL_local_pool);

  for (i = 0; i < 2; i++) {
    loop[i] = unroll_body[i];
    nloop[i] = Find_Next_Innermost_Do(loop[i]);
  }

  for (INT d = 0; d < nloops - 1; d++) {

    // Find insertion point for statements below main loop body
    WN* wn_last = NULL;  
    for (WN* wn = WN_first(WN_do_body(loop[0])); wn != NULL; wn = WN_next(wn)) 
      wn_last = wn; 

    Is_True(nloop[0], ("no way"));
    for (i = 1; i < 2; i++) {
	if (WN_prev(nloop[i])) {
	    WN* above = LWN_Create_Block_From_Stmts_Above(nloop[i]);
	    LWN_Insert_Block_Before(LWN_Get_Parent(nloop[0]), nloop[0], above);
	}
    }
    for (i = 2-1; i >= 1; i--) {
	if (WN_next(nloop[i])) {
	    WN* below = LWN_Create_Block_From_Stmts_Below(nloop[i]);
	    LWN_Insert_Block_After(LWN_Get_Parent(wn_last), wn_last, below);
	}
    }

    for (i = 0; i < 2; i++) {
      // the loops have DU information pointing uselessly to loops that are
      // about to go away.  Change that
      if (i != 0)
	SNL_Add_Du_To_Index_Ldid(loop[0], WN_do_body(loop[i]), du, TRUE);

      loop[i] = nloop[i];
      nloop[i] = Find_Next_Innermost_Do(loop[i]);
    }
  }

  for (i = 1; i < 2; i++) {
      WN* bdy = WN_do_body(loop[i]);
      WN_do_body(loop[i]) = WN_CreateBlock();
      LWN_Insert_Block_Before(WN_do_body(loop[0]), NULL, bdy);
      
      /* insert post loop statements */
      WN *post = WN_next(unroll_body[i]);
      WN *insert = unroll_body[0];
      while (post) {
	  WN *wn_next = WN_next(post);
	  LWN_Insert_Block_After(LWN_Get_Parent(insert), insert, post);
	  Cur_Simd_Loop->Post_Loop().Push(post);
	  insert = post;
	  post = wn_next;
      }

      LWN_Delete_Tree(unroll_body[i]);
  }

  /* delete the old statements in the loop */
  for (i = 0; i < Cur_Simd_Loop->Old_Stmt().Elements(); i++) {
      WN *stmt = Cur_Simd_Loop->Old_Stmt().Bottom_nth(i);
      LWN_Delete_Tree(stmt);
  }
  
  WN *common_ancestor = LWN_Get_Parent(outerloop);

  /* delete unused invariant STID */
  for (i = 0; i < Cur_Simd_Loop->Inv_Ldid().Elements(); i++) {
      WN *wn = Cur_Simd_Loop->Inv_Ldid().Bottom_nth(i);
      Is_True(WN_operator(wn) == OPR_STID, ("Expecting STID"));
      common_ancestor = Common_Ancestor(wn, common_ancestor);

      USE_LIST *ul = Du_Mgr->Du_Get_Use(wn);
      if ((!ul || !ul->Incomplete() && ul->Len() == 0) &&
	  !((ST_class(WN_st(wn)) == CLASS_PREG) && 
	    Preg_Is_Dedicated(WN_offset(wn))) && 
	  !ST_has_nested_ref(WN_st(wn))) {
	  LWN_Delete_Tree(wn);
      }
  }

  // build DU chains for SIMD pregs 
  WN *top_loop_block=common_ancestor;
  WN *tmp = top_loop_block;
  while (LWN_Get_Parent(tmp)) {
    if (WN_opcode(LWN_Get_Parent(tmp)) == OPC_DO_LOOP) {
      top_loop_block = tmp;
    }
    tmp = LWN_Get_Parent(tmp);
  } 
  Cur_Simd_Loop->Simd_Build_Du_Info(top_loop_block);

  /* Update pending store merge dependences */
  Cur_Simd_Loop->IMem_Map().Add_Store_Dependence(Cur_Simd_Loop->Pool());
  
  bool changed = false;

  /* perform inner loop unroll if it is necessary */
  for (i = 1; i < nloops; i++) {
      if (Cur_Simd_Loop->Unroll_Candidate(i)->Elements()) {
	  WN *inner_loop = NULL;
	  if (permloop) {
	      inner_loop = permloop[i];
	  } else {
	      Is_True(i==1, ("Unexpected 2d SIMD inner unroll"));
	      inner_loop = Find_Next_Innermost_Do(outerloop);
	      Is_True(inner_loop != NULL, 
		      ("No inner loop for 2d SIMD unroll"));
	  }
	  Cur_Simd_Loop->Further_Unroll_Loop(inner_loop, i, Simd_Info,
					     est_register_usage,
					     pinfo,
					     pinfo_depth,
					     no_further_unroll,
					     wdpinfo_ptr);
	  changed = true;
      }
  }

  if (changed) {
      // rebuild DU chains for SIMD pregs 
      Cur_Simd_Loop->Simd_Build_Du_Info(LWN_Get_Parent(outerloop));
  }

  if (loop_map_ptr)
    *loop_map_ptr = loop_map;
  else if (loop_map)
    CXX_DELETE(loop_map, &SNL_local_pool);

  if (wdpinfo_ptr)
      *wdpinfo_ptr = wdpinfo;
  else if (wdpinfo)
      CXX_DELETE(wdpinfo, &SNL_local_pool);

  // Form the SNL region and verify it
  WN *region_first = region_prev?WN_next(region_prev):WN_first(region_block);
  WN *region_last = region_next?WN_prev(region_next):WN_last(region_block);
  Is_True(region_first && region_last,("Bad region formed."));
  
  SNL_REGION region(region_first,region_last);
  if (!Valid_SNL_Region(region)) {
      DevWarn("SNL_Simd_Loop: Invalid SNL_REGION [0x%x,0x%x]",
	      region.First, region.Last);
  }
  
  if (simd_debug) {
      fprintf(TFile,
	      "\n"
	      "WHIRL after SIMD transformation:\n"
	      "--------------------------------\n");
      for (WN *wn=region.First; wn!=NULL; wn=WN_next(wn)) {
	  fdump_tree(TFile,wn);
	  if (wn==region.Last)
	      break;
      }
  }
  return region;
}

/*---------------------------------------------------------------------------*
 * Generate WC from Simd_Reg to Coef                                          *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Generate_Write_Coef(WN *stmt, SIMD_INFO *simd)
{
    TYPE_ID type_id = Coef_Reg()->Type();
    TIE_MACRO_ID macro_id = tie_info->tie_macro_id("_TIE_Vectra_WC");
    Is_True(macro_id != TIE_INVALID_ID, ("Cannot WC instruction"));

    INTRINSIC intrin_id = Tie_Macro_Id_To_Intrinsic(macro_id);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, type_id, MTYPE_V);

    WN *apr[1];
    apr[0] = LWN_CreateParm(Get_Res_Reg()->Type(), 
			    Get_Res_Reg()->Simd_Preg_Ldid(),
			    MTYPE_To_TY(Get_Res_Reg()->Type()), 
			    WN_PARM_BY_VALUE);

    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);

    /* coef = WC(simd_preg) */
    WN  *sto = Coef_Reg()->Simd_Preg_Stid(callNode);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Generate a SIMD symbol according to 'elem_type'                           *
 *---------------------------------------------------------------------------*/
SIMD_PREG*
Gen_Symbol(SIMD_INFO *simd, MTYPE elem_type, MEM_POOL *pool) 
{
    char new_name[64];
    sprintf(new_name, "%s_%d", "V", name_counter++);
    TYPE_ID    vec_type = simd->Get_SIMD_Reg_Type_Scalar(elem_type);
    PREG_NUM   preg_num = Create_Preg(vec_type, new_name);
    SIMD_PREG *res      = CXX_NEW(SIMD_PREG(vec_type, preg_num), pool);
    return res;
}

SIMD_PREG*
Gen_Symbol(TYPE_ID type, MEM_POOL *pool, const char *name) {
    char new_name[64];
    if (name==NULL) {
	name="V";
    }
    sprintf(new_name, "%s_%d", name, name_counter++);
    PREG_NUM   preg_num = Create_Preg(type, new_name);
    SIMD_PREG *res      = CXX_NEW(SIMD_PREG(type, preg_num), pool);
    return res;
}

/*---------------------------------------------------------------------------*
 * Generate an alignment symbol                                              *
 *---------------------------------------------------------------------------*/
SIMD_PREG*
SIMD_INFO::Gen_Align_Symbol(TYPE_ID scalar_type, MEM_POOL *pool) 
{
    char new_name[64];
    sprintf(new_name, "%s_%d", "A", name_counter++);
    TYPE_ID    align_type = Get_Alignment_Type_Scalar(scalar_type);
    Is_True(align_type != MTYPE_UNKNOWN,
	    ("Can't find alignment type for %s", MTYPE_name(scalar_type)));
    PREG_NUM   preg_num   = Create_Preg(align_type, new_name);
    SIMD_PREG *res        = CXX_NEW(SIMD_PREG(align_type, preg_num), pool);
    return res;
}

/*---------------------------------------------------------------------------*
 * Generate a select symbol                                                  *
 *---------------------------------------------------------------------------*/
SIMD_PREG*
SIMD_INFO::Gen_Sel_Symbol(MEM_POOL *pool) 
{
    char new_name[64];
    sprintf(new_name, "%s_%d", "sel", name_counter++);
    TYPE_ID    type      = Get_Sel_Type();
    PREG_NUM   preg_num  = Create_Preg(type, new_name);
    SIMD_PREG *res       = CXX_NEW(SIMD_PREG(type, preg_num), pool);
    return res;
}

/*---------------------------------------------------------------------------*
 * Copy an alignment load, increment the address to the next vector          *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO::Generate_Load_Ahead(WN *simd_expr, SIMD_INFO *simd, MEM_POOL *pool,
				bool insert_after)
{
    TYPE_ID res_type = Res_Type();

    /* alignment register */
    SIMD_PREG *align = IMem()->Imem_Offset()->Parent_Group()->Load_Align_Reg();
    
    /* address register */
    SIMD_PREG *addr_preg = IMem()->Imem_Offset()->Parent_Group()->Load_Addr_Reg();

    /* allocate a SIMD_PREG to hold the load ahead result */
    SIMD_PREG *target = Load_Ahead();
    if (target == NULL) {
	target = Gen_Symbol(simd, res_type, pool);
	Set_Load_Ahead(target);
    }
    
    /* copy the load */
    INTRINSIC  intrin_id   = WN_intrinsic(simd_expr);
    OPCODE     intrin_call = WN_opcode(simd_expr);
    char       macro_name[64];
    INT        bits = simd->Get_Narrow_Element_Mem_Bits();
    if (insert_after) {
	Simd_Make_Vectra_Macro("LV", "A_I", bits, macro_name);
	Is_True(intrin_id == 
		Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name)),
	    ("Expecting narrow element alignment register load"));
	Simd_Make_Vectra_Macro("LV", "A_IU", bits, macro_name);
	intrin_id = 
	    Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name));
	intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    } else {
	Simd_Make_Vectra_Macro("LV", "A_IU", bits, macro_name);
	Is_True(intrin_id == 
		Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name)),
	    ("Expecting narrow element alignment register IU load"));
    }

    WN *apr[4];
    WN *targ_ldid  = target->Simd_Preg_Ldid();
    WN *align_ldid = align->Simd_Preg_Ldid();
    
    /* copy the address */
    WN *addr = LWN_Copy_Tree(WN_kid0(WN_kid(simd_expr, 2)));
    LWN_Copy_Def_Use(WN_kid0(WN_kid(simd_expr, 2)), addr, Du_Mgr);
    
    /* generate the increment for address */
    WN *inc_cst = WN_CreateIntconst(OPC_I4INTCONST,
				    simd->Get_SIMD_Mem_Bytes_Scalar(res_type));
    
    apr[0] = LWN_CreateParm(target->Type(), targ_ldid,
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(align->Type(), align_ldid,
			    MTYPE_To_TY(align->Type()),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(WN_rtype(addr), addr, 
			    MTYPE_To_TY(WN_rtype(addr)), 
			    WN_PARM_BY_VALUE);
    apr[3] = LWN_CreateParm(MTYPE_I4, inc_cst, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);
    
    WN *load_ahead = LWN_Create_Intrinsic(intrin_call, intrin_id, 4, apr);
    
    WN *stmt = simd_expr;

    if (insert_after) {
	stmt = WN_next(WN_next(WN_next(stmt)));
    }

    /* Insert before stmt */
    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), stmt, load_ahead);
    WN_linenum(load_ahead) = LWN_Get_Linenum(simd_expr);
    
    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(load_ahead, parm_ldid);
    WN *targ_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), stmt, targ_stid);
    WN_linenum(targ_stid) = LWN_Get_Linenum(simd_expr);

    parm_ldid = WN_Ldid(align->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(load_ahead, parm_ldid);
    WN *parm_stid = align->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(simd_expr);

    parm_ldid = WN_Ldid(addr_preg->Type(), -3, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(load_ahead, parm_ldid);
    WN *addr_stid = addr_preg->Simd_Preg_Stid(parm_ldid);
    Du_Mgr->Add_Def_Use(addr_stid, addr);
    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), stmt, addr_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(simd_expr);

    return load_ahead;
}


/*---------------------------------------------------------------------------*
 * Create a local symbol to hold a 64 bit value                              *
 *---------------------------------------------------------------------------*/
ST*
SIMD_INFO::Create_64bit_Integer_Var(INT64 val)
{
    char name[64];
    sprintf(name, "__vec_I8_%d", name_counter++);
    
    ST *st;

    /* create a new variable */
    TYPE_ID mtype = MTYPE_I8;
    TY_IDX  ty_idx = MTYPE_To_TY(mtype);

    st = New_ST(CURRENT_SYMTAB);
    ST_Init(st,
	    Save_Str("__vec_I8"),
	    CLASS_VAR,
	    SCLASS_PSTATIC,
	    EXPORT_LOCAL,
	    ty_idx);
    Set_ST_is_const_var(st);
    
    /* Initialize the select constants */
    Set_ST_is_initialized(st);
    INITO_IDX  inito = New_INITO(st);
    INITV_IDX  inv = New_INITV();
    INITV_Init_Integer(inv, mtype, val);
    Set_INITO_val(inito, inv);

    return st;
}

/*---------------------------------------------------------------------------*
 * Create a global array with the select constants for different alignments  *
 *---------------------------------------------------------------------------*/
ST*
SIMD_INFO::Create_Global_Alignment_Select()
{
    char name[64];
    sprintf(name, "__vectra_str_align");
    
    FmtAssert(Simd_Target == SIMD_TARG_VECTRA_I,
	      ("Store select supported only on Vectra I"));
    
    /* see if the variable already exists */
    ST *st;
    INT i;
    bool found = false;
    FOREACH_SYMBOL(GLOBAL_SYMTAB,st,i) {
	if ((ST_class(st) == CLASS_VAR) && (strcmp(ST_name(st), name) == 0)) {
	    found = true;
	    break;
	}
    }

    if (found) {
	_store_align = st;
	return _store_align;
    }

    /* create a new variable */
    TYPE_ID mtype = MTYPE_U4;
    TY_IDX  array_ty_idx;
    TY& array_ty = New_TY(array_ty_idx);

    TYPE_ID vec_type = V8x16type();
    SIMD_SELECT *simd_sel = Get_SIMD_Select_SIMD(vec_type);
    Is_True(simd_sel != NULL, ("NULL SIMD select object (%s).", MTYPE_name(vec_type)));

    INT vl = simd_sel->Vector_Length();
    INT size = vl;
    
    /* one arrays, merge */
    TY_Init(array_ty,
	    1 * size * TY_size(Be_Type_Tbl(mtype)),
	    KIND_ARRAY,
	    MTYPE_UNKNOWN,
	    Save_Str("array_U4"));
    ARB_HANDLE arb = New_ARB();
    ARB_Init(arb, 0, size - 1, 1);
    Set_ARB_first_dimen(arb);
    Set_ARB_last_dimen(arb);
    Set_TY_arb(array_ty, arb);
    Set_TY_etype(array_ty, Be_Type_Tbl(mtype));
    Set_TY_align(array_ty_idx, TY_align(Be_Type_Tbl(mtype)));

    st = New_ST(GLOBAL_SYMTAB);
    ST_Init(st,
	    Save_Str("__vectra_str_align"),
	    CLASS_VAR,
	    SCLASS_DGLOBAL,
	    EXPORT_PREEMPTIBLE,
	    array_ty_idx);
    Set_ST_is_const_var(st);
    
    // We need a single copy of the table
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR& st_attr = New_ST_ATTR(GLOBAL_SYMTAB, st_attr_idx);
    ST_ATTR_Init(st_attr, ST_st_idx(st), ST_ATTR_SECTION_NAME,
		 Save_Str(".gnu.linkonce.r.__vectra_str_align"));
    Set_ST_has_named_section (st);
    
    /* Initialize the select constants */
    Set_ST_is_initialized(st);
    INITO_IDX  aggregate_inito = New_INITO (st);
    INITV_IDX  last_aggregate_initv = 0;
    bool little_endian = (Target_Byte_Sex == LITTLE_ENDIAN);

    /* merge */
    for (INT offset = 0; offset < size; offset++) {
	INITV_IDX inv = New_INITV();
	UINT val = (UINT)simd_sel->Sel_4321(vl - offset);
	INITV_Init_Integer(inv, mtype, val);
	if (offset == 0) {
	    Set_INITO_val(aggregate_inito, inv);
	} else {
	    Is_True(last_aggregate_initv != 0, ("Unexpected last initv 0"));
	    Set_INITV_next(last_aggregate_initv, inv);
	}
	last_aggregate_initv = inv;
    }

#if 0
    /* this is not necessary because hardware already performed a rotation from
       unaligned memory address */
    /* post-shift */
    for (INT offset = 0; offset < size; offset++) {
	INITV_IDX inv = New_INITV();
	UINT val = Gen_Store_Select_Desc_Post(this, offset);
	INITV_Init_Integer(inv, mtype, val);
	Is_True(last_aggregate_initv != 0, ("Unexpected last initv 0"));
	Set_INITV_next(last_aggregate_initv, inv);
	last_aggregate_initv = inv;
    }
#endif

    _store_align = st;
    return _store_align;
}

/*---------------------------------------------------------------------------*
 * Create a local array for vector scalar                                    *
 *---------------------------------------------------------------------------*/
ST*
SIMD_INFO::Create_SE_Array(TYPE_ID scalar_type, INT length)
{
    /* find the TY_IDX */
    TYPE_ID mtype        = scalar_type;
    INT     size         = length;
    TY_IDX  array_ty_idx = Vse_Ty_Find(mtype, size);

    if (array_ty_idx == TY_IDX_ZERO) {
	/* create a new TY entry */
	TY&     array_ty = New_TY(array_ty_idx);

	char ty_name[64];
	sprintf(ty_name, "__vse_ty_%d_of_%s", size, MTYPE_name(mtype));

	/* one array */
	TY_Init(array_ty,
		size * TY_size(Be_Type_Tbl(mtype)),
		KIND_ARRAY,
		MTYPE_UNKNOWN,
		Save_Str(ty_name));
	ARB_HANDLE arb = New_ARB();
	ARB_Init(arb, 0, size - 1, 1);
	Set_ARB_first_dimen(arb);
	Set_ARB_last_dimen(arb);
	Set_TY_arb(array_ty, arb);
	Set_TY_etype(array_ty, Be_Type_Tbl(mtype));
	Set_TY_align(array_ty_idx, TY_align(Be_Type_Tbl(mtype)));
	
	VSE_TY *vse_ty = CXX_NEW(VSE_TY(mtype, size, array_ty_idx), 
				 Mem_Pool());
	Add_Vse_Ty(vse_ty);
    }

    char name[64];
    sprintf(name, "__vse%d", name_counter++);
    
    ST *st = New_ST(CURRENT_SYMTAB);
    ST_Init(st,
	    Save_Str(name),
	    CLASS_VAR,
	    SCLASS_AUTO,
	    EXPORT_LOCAL,
	    array_ty_idx);
#ifdef _NEW_SYMTAB
    Clear_ST_addr_not_passed(st);
    Clear_ST_addr_not_saved(st);
#else
    Set_ST_addr_taken_passed(st);
#endif

    return st;
}

/*---------------------------------------------------------------------------*
 * Generate updating store                                                   *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Update_Store(TYPE_ID reg_type, TYPE_ID mem_type,
				 WN *stmt, WN *lod_val, SIMD_PREG *addr, 
				 INT increment, WN *block)
{
    OPERATOR  op          = OPR_ISTORE;
    INTRINSIC intrin_id   = Find_Function(op, mem_type, reg_type, 0, true);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);  

    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *inc       = WN_CreateIntconst(const_opc, increment);

    WN *addr_ldid = addr->Simd_Preg_Ldid();
    WN *apr[3];
    apr[0] = LWN_CreateParm(WN_rtype(lod_val), lod_val,
			    MTYPE_To_TY(WN_rtype(lod_val)),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr->Type(), addr_ldid,
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(MTYPE_I4, inc, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);

    INT64 line_num = 0;
    if (block == NULL) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 3, apr);
    LWN_Insert_Block_Before(block, stmt, callNode);
    WN_linenum(callNode) = line_num;

    WN *parm_ldid = WN_Ldid(addr->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = addr->Simd_Preg_Stid(parm_ldid);
    Du_Mgr->Add_Def_Use(parm_stid, addr_ldid);
    LWN_Insert_Block_Before(block, stmt, parm_stid);
    WN_linenum(parm_stid) = line_num;
    
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate updating indexed store                                           *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Update_Indexed_Store(TYPE_ID reg_type, TYPE_ID mem_type,
				 WN *stmt, WN *lod_val, SIMD_PREG *addr, 
				 SIMD_PREG *index)
{
    OPERATOR  op          = OPR_ISTORE;
    INTRINSIC intrin_id   = Find_Function(op, mem_type, reg_type, 4, true);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);  

    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *inc       = index->Simd_Preg_Ldid();

    WN *addr_ldid = addr->Simd_Preg_Ldid();
    WN *apr[3];
    apr[0] = LWN_CreateParm(WN_rtype(lod_val), lod_val,
			    MTYPE_To_TY(WN_rtype(lod_val)),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr->Type(), addr_ldid,
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(MTYPE_I4, inc, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);

    INT64 line_num = LWN_Get_Linenum(stmt);
    WN   *block    = LWN_Get_Parent(stmt);

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 3, apr);
    LWN_Insert_Block_Before(block, stmt, callNode);
    WN_linenum(callNode) = line_num;

    WN *parm_ldid = WN_Ldid(addr->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = addr->Simd_Preg_Stid(parm_ldid);
    Du_Mgr->Add_Def_Use(parm_stid, addr_ldid);
    LWN_Insert_Block_Before(block, stmt, parm_stid);
    WN_linenum(parm_stid) = line_num;
    
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call with one parameter                       *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Unary_Call(OPERATOR op, TYPE_ID type, WN *operand)
{
    INTRINSIC intrin_id = Find_Function(op, type);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);
    WN* apr[1];
    apr[0] = LWN_CreateParm(type, operand, MTYPE_To_TY(type), WN_PARM_BY_VALUE);
    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call to MUL16_0/MUL16_1 according to 'lohi'   *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Mulr_Call(OPERATOR op, TYPE_ID type, WN *operand1,
			      WN *operand2, WN *operand3, INT lohi)
{
    TYPE_ID   in_type   = WN_rtype(operand2);
    INTRINSIC intrin_id = Find_Function(op, in_type, MTYPE_UNKNOWN, lohi);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    WN *apr[3];
    
    apr[0] = LWN_CreateParm(WN_rtype(operand1), operand1, 
			    MTYPE_To_TY(WN_rtype(operand1)), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(WN_rtype(operand2), operand2,
			    MTYPE_To_TY(WN_rtype(operand2)), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(WN_rtype(operand3), operand3,
			    MTYPE_To_TY(WN_rtype(operand3)), WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call to MUL16_0/MUL16_1 according to 'lohi'   *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Mac_Call(OPERATOR op, TYPE_ID type, WN *operand1, 
			     WN *operand2, INT lohi, WN *operand3, 
			     WN *operand4, WN *imm)
{
    TYPE_ID   in_type   = (imm) ? WN_rtype(operand3) : WN_rtype(operand2);
    INTRINSIC intrin_id = Find_Function(op, in_type, type, lohi);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);

    WN *apr[5];
    
    apr[0] = LWN_CreateParm(WN_rtype(operand1), operand1, 
			    MTYPE_To_TY(WN_rtype(operand1)), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(WN_rtype(operand2), operand2,
			    MTYPE_To_TY(WN_rtype(operand2)), WN_PARM_BY_VALUE);
    WN *callNode = NULL;
    if (operand3) {
	apr[2] = 
	    LWN_CreateParm(WN_rtype(operand3), operand3, 
			   MTYPE_To_TY(WN_rtype(operand3)), WN_PARM_BY_VALUE);
	if (operand4) {
	    /* MULABC16 */
	    apr[3] = LWN_CreateParm(WN_rtype(operand4), operand4, 
				    MTYPE_To_TY(WN_rtype(operand4)),
				    WN_PARM_BY_VALUE);
	    Is_True(imm && WN_operator(imm) == OPR_INTCONST, 
		    ("Expecting an integer constant"));
	    apr[4] = LWN_CreateParm(MTYPE_I4, imm, MTYPE_To_TY(MTYPE_I4),
				    WN_PARM_BY_VALUE);
	    callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 5, apr);
	} else {
	    /* MULA16 */
	    callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
	}
    } else {
	callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
    }
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate a LDID/ILOAD with buildin operator                              *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Load(OPERATOR op, TYPE_ID type, WN *stmt, WN *addr, 
			 WN_OFFSET wn_offset, SIMD_PREG *target, 
			 TYPE_ID desc)
{
    WN *res = NULL;
    WN *cvt = NULL;
    OPCODE opc;

    if (target->Type() != type) {
	type = target->Type();
    }
    if (op == OPR_LDID) {
	/* scalar load */
	opc = OPCODE_make_op(op, type, desc);
	res = WN_CreateLdid(opc, wn_offset, WN_st(addr), MTYPE_To_TY(type));
	LWN_Copy_Def_Use_Node(addr, res, Du_Mgr);
    } else {
	Is_True(op == OPR_ILOAD, ("Not a load"));
	opc = OPCODE_make_op(op, type, desc);
	res = LWN_CreateIload(opc, wn_offset, 
			      MTYPE_To_TY(type),
			      Make_Pointer_Type(MTYPE_To_TY(type)), 
			      addr); 
    }

    cvt = res;

#if 0
    if (target->Type() != type) {
	opc = OPCODE_make_op(OPR_CVT, target->Type(), type);
	cvt = LWN_CreateExp1(opc, res);
    } else {
	cvt = res;
    }
#endif

    WN *sto      = target->Simd_Preg_Stid(cvt);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
    return res;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD LV_U/LS_U intrinsic with two parameter vs (0 v, 1 s)     *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_Update_Load(OPERATOR op, TYPE_ID rtype, TYPE_ID desc,
				WN *stmt, SIMD_PREG *addr_preg, 
				SIMD_PREG *target, INT increment, INT vs)
{
    INTRINSIC intrin_id   = Find_Function(op, desc, rtype, vs, true);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *inc = WN_CreateIntconst(const_opc, increment);

    WN *apr[3];
    apr[0] = LWN_CreateParm(target->Type(), target->Simd_Preg_Ldid(),
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr_preg->Type(), addr_preg->Simd_Preg_Ldid(),
			    Make_Pointer_Type(MTYPE_To_TY(desc)), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(MTYPE_I4, inc, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 3, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);

    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    parm_ldid = WN_Ldid(addr_preg->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = addr_preg->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    return callNode;
}

WN *
SIMD_INFO::Generate_LoadA_Prime (TYPE_ID scalar_type, WN *stmt, WN *addr,
				 SIMD_PREG *addr_reg, SIMD_PREG *target,
				 SIMD_PREG *align) {
    WN *prime_pt = stmt;
    WN *prime_blk = LWN_Get_Parent(stmt);
    
    INT inc = Get_SIMD_Mem_Bytes_Scalar(scalar_type);
    addr = Alignment_Load_Init_Addr(addr, inc);
    
    WN *stid = addr_reg->Simd_Preg_Stid(addr);
    LWN_Insert_Block_Before(prime_blk, prime_pt, stid);
    WN_linenum(stid) = LWN_Get_Linenum(stmt);
    
    WN *callNode = Generate_LoadA(scalar_type, 
				  prime_pt,
				  addr_reg->Simd_Preg_Ldid(),
				  target, 
				  align);
    return callNode;
}

WN *
SIMD_INFO::Generate_LoadA_Update (TYPE_ID scalar_type, WN *stmt, SIMD_PREG *addr,
				  SIMD_PREG *target, SIMD_PREG *align,
				  INT inc) {
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    Is_True(target->Type() == reg_type,
	    ("Different type of LOADA_IMM_UPDATE (%s and %s)",
	     MTYPE_name(target->Type()), MTYPE_name(reg_type)));
    
    INTRINSIC intrin_id   = Find_Function(OPR_ILOAD, mem_type, reg_type, 2 /* LVA */, true /*IU*/);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *wn_inc       = WN_CreateIntconst(const_opc, inc);
    
    WN *addr_ldid = addr->Simd_Preg_Ldid();
    WN *apr[4];
    apr[0] = LWN_CreateParm(target->Type(), target->Simd_Preg_Ldid(),
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(align->Type(), align->Simd_Preg_Ldid(),
			    MTYPE_To_TY(align->Type()),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(addr->Type(), addr_ldid,
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    apr[3] = LWN_CreateParm(MTYPE_I4, wn_inc, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);
    
    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 4, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);
    
    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    parm_ldid = WN_Ldid(align->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = align->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    parm_ldid = WN_Ldid(addr->Type(), -3, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = addr->Simd_Preg_Stid(parm_ldid);
    Du_Mgr->Add_Def_Use(parm_stid, addr_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    return callNode;
}


WN *
SIMD_INFO::Generate_LoadA (TYPE_ID scalar_type, WN *stmt, WN *addr,
			   SIMD_PREG *target, SIMD_PREG *align) {
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    Is_True(target->Type() == reg_type,
	    ("Different type of LOADA_IMM (%s and %s)",
	     MTYPE_name(target->Type()), MTYPE_name(reg_type)));
    
    INTRINSIC intrin_id   = Find_Function(OPR_ILOAD, mem_type, reg_type, 2/* for LVA_I */);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *zero = WN_CreateIntconst(const_opc, 0);

    WN *apr[4];
    WN *targ_ldid  = target->Simd_Preg_Ldid();
    WN *align_ldid = align->Simd_Preg_Ldid();

    apr[0] = LWN_CreateParm(target->Type(), targ_ldid,
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(align->Type(), align_ldid,
			    MTYPE_To_TY(align->Type()),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(WN_rtype(addr), addr, 
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    apr[3] = LWN_CreateParm(MTYPE_I4, zero, MTYPE_To_TY(MTYPE_I4),
			    WN_PARM_BY_VALUE);

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 4, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);

    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    parm_ldid = WN_Ldid(align->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = align->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    return callNode;
}


void
SIMD_INFO::Generate_StoreA_Prime (IMEM_GROUP *ig, WN *stmt) {
    // NOTE: the store address register in 'ig' needs to be initialized
    // to the correct value

    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    
    INT inc = MTYPE_byte_size(mem_type);
    
    SIMD_PREG *store_addr_reg = ig->Store_Addr_Reg();
    WN *prime_pt = stmt;
    WN *prime_blk = LWN_Get_Parent(prime_pt);
    
    SIMD_PREG* store_copy = ig->Store_Copy();
    Is_True(store_copy->Type() == reg_type,
	    ("Types don't match (%s and %s)",
	     MTYPE_name(store_copy->Type()), MTYPE_name(reg_type)));
    
    OPCODE     iload_opc  = OPCODE_make_op(OPR_ILOAD, mem_type, mem_type);
    OPCODE     add_opc    = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
    WN        *inc_wn     = WN_CreateIntconst(OPC_I4INTCONST, inc);
    WN        *ld_addr    = LWN_CreateExp2(add_opc, 
					   inc_wn,
					   store_addr_reg->Simd_Preg_Ldid());
    
    WN*        iload      = LWN_CreateIload(iload_opc, 0, MTYPE_To_TY(mem_type),
					    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
					    ld_addr);
    
    OPCODE     cvt_opc    = OPCODE_make_op(OPR_CVT, reg_type, mem_type);
    WN*        cvt        = LWN_CreateExp1(cvt_opc, iload);
    
    WN *stid              = store_copy->Simd_Preg_Stid(cvt);
    LWN_Insert_Block_Before(prime_blk, prime_pt, stid);
    
    if (Enclosing_Do_Loop(prime_blk) != NULL) {
	Array_Dependence_Graph->Add_Vertex(iload);
	ig->Pending_Wn().Push(iload);
    }
    
    /* generate store index address */
    ig->Generate_Store_Offset_Index(store_addr_reg, prime_pt, Cur_Simd_Loop);
    
    /* generate merge shift reg */
    stid  = ig->Generate_Store_Select(prime_pt, 0, ig->Store_Index_Reg(),
				      ig->Store_Sel_Reg());
    LWN_Insert_Block_Before(prime_blk, prime_pt, stid);
}


void
SIMD_INFO::Generate_StoreA_Update (IMEM_GROUP *ig, WN *stmt, SIMD_PREG *val, INT inc) {
    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    
    WN *lod_val = val->Simd_Preg_Ldid();
    
    SIMD_PREG *store_addr_reg = ig->Store_Addr_Reg();
    SIMD_PREG* store_copy = ig->Store_Copy();
    Is_True(store_copy->Type() == reg_type,
	    ("Types don't match (%s and %s)",
	     MTYPE_name(store_copy->Type()), MTYPE_name(reg_type)));
    
    /* generate a merge with the preloaded value */
    lod_val = ig->Generate_Unaligned_Store_Merge(lod_val, store_copy);
    
    /* Generating updating store */
    WN *res = Simd_Info->Generate_Update_Store(reg_type, mem_type, stmt, 
					       lod_val, store_addr_reg, inc);
    
    /* add 'res' to the V_Stores() for updating dependence later */
    ig->V_Stores()->AddElement(res);
    
    /* advance store copy */
    ig->Generate_Unaligned_Store_Copy(val->Simd_Preg_Ldid(), stmt);
}


void
SIMD_INFO::Generate_StoreA_Flush (IMEM_GROUP *ig, WN *stmt, bool ins_after) {
    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    
    SIMD_PREG  *addr_preg  = ig->Store_Addr_Reg();
    Is_True(addr_preg, ("Expect store address for unaligned store"));
    
    SIMD_PREG *res      = Gen_Symbol(reg_type, Pool());
    
    /* inc + addr */
    INT     inc    = MTYPE_bit_size(mem_type) >> 3;
    WN     *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, inc);
    OPCODE  opc    = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
    WN     *a_addr = LWN_CreateExp2(opc, inc_wn, addr_preg->Simd_Preg_Ldid());
    
    /* res = iload(addr + inc) */
    OPCODE  iload_opc = OPCODE_make_op(OPR_ILOAD, mem_type, mem_type);
    WN     *iload     = 
	LWN_CreateIload(iload_opc, 0, MTYPE_To_TY(mem_type),
			Make_Pointer_Type(MTYPE_To_TY(mem_type)), a_addr);

    WN *load = iload;
    if (reg_type != mem_type) {
	OPCODE  opc_cvt = OPCODE_make_op(OPR_CVT, reg_type, mem_type);
	load     = LWN_CreateExp1(opc_cvt, iload);
    }
    WN     *stid    = res->Simd_Preg_Stid(load);
    
    if (ins_after) {
	if (LWN_Get_Parent(stmt)) {
	    LWN_Insert_Block_After(LWN_Get_Parent(stmt), stmt, stid);
	} else {
	    WN_next(stid) = WN_next(stmt);
	    if (WN_next(stmt)) {
		WN_prev(WN_next(stmt)) = stid;
	    }
	    WN_next(stmt) = stid;
	    WN_prev(stid) = stmt;
	}
    } else {
	Is_True(LWN_Get_Parent(stmt) != NULL, ("Expected non-null parent block"));
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
    }
    
    /* generate a merge */
    WN *merged_val = 
	ig->Generate_Unaligned_Store_Merge(res->Simd_Preg_Ldid(), ig->Store_Copy());
    
    /* generate a store back to meory */
    OPCODE  istore_opc = OPCODE_make_op(OPR_ISTORE, MTYPE_V, mem_type);
    inc_wn = WN_CreateIntconst(OPC_I4INTCONST, inc);
    a_addr = LWN_CreateExp2(opc, addr_preg->Simd_Preg_Ldid(), inc_wn);
    
    WN     *istore    = 
	LWN_CreateIstore(istore_opc, 0, Make_Pointer_Type(MTYPE_To_TY(mem_type)), 
			 merged_val, a_addr);
    
    if (ins_after) {
	if (LWN_Get_Parent(stmt)) {
	    LWN_Insert_Block_After(LWN_Get_Parent(stmt), stid, istore);
	} else {
	    WN_next(istore) = WN_next(stid);
	    if (WN_next(stid)) {
		WN_prev(WN_next(stid)) = istore;
	    }
	    WN_next(stid) = istore;
	    WN_prev(istore) = stid;
	}
    } else {
	Is_True(LWN_Get_Parent(stmt) != NULL, ("Expected non-null parent block"));
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, istore);
    }
    
    /* add dependence vertices */
    if (ig->Pending_Wn().Elements()) {
	Array_Dependence_Graph->Add_Vertex(iload);
	Array_Dependence_Graph->Add_Vertex(istore);
	
	/* push the iload/istore to pending dep update list */
	ig->Pending_Wn().Push(iload);
	ig->Pending_Wn().Push(istore);
    }
}


/*---------------------------------------------------------------------------*
 * Generate an SIMD LV_X instrinsic call with two parameter                  *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_LoadS_X(OPERATOR op, TYPE_ID type, TYPE_ID desc,
			    WN *stmt, SIMD_PREG *addr, SIMD_PREG *target, 
			    SIMD_PREG *idx, bool update)
{
    INTRINSIC intrin_id   = Find_Function(op, desc, type, 3/* for LS_X */, update);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);

    WN *apr[3];
    WN *targ_ldid = target->Simd_Preg_Ldid();
    WN *idx_ldid  = idx->Simd_Preg_Ldid();
    WN *addr_ldid = addr->Simd_Preg_Ldid();

    apr[0] = LWN_CreateParm(target->Type(), targ_ldid,
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr->Type(), addr_ldid, 
			    Make_Pointer_Type(MTYPE_To_TY(type)),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(idx->Type(), idx_ldid,
			    MTYPE_To_TY(idx->Type()),
			    WN_PARM_BY_VALUE);

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 3, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);

    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    if (update) {
	parm_ldid = WN_Ldid(addr->Type(), -2, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
	Du_Mgr->Add_Def_Use(callNode, parm_ldid);
	parm_stid = addr->Simd_Preg_Stid(parm_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
	WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    }
    
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD LV_X instrinsic call with two parameter                  *
 *---------------------------------------------------------------------------*/
WN*
SIMD_INFO::Generate_LoadV_X(OPERATOR op, TYPE_ID type, TYPE_ID desc,
			    WN *stmt, SIMD_PREG *addr, SIMD_PREG *target, 
			    SIMD_PREG *idx, bool update)
{
    INTRINSIC intrin_id   = Find_Function(op, desc, type, 4/* for LV_X */, update);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);

    WN *apr[3];
    WN *targ_ldid = target->Simd_Preg_Ldid();
    WN *idx_ldid  = idx->Simd_Preg_Ldid();
    WN *addr_ldid = addr->Simd_Preg_Ldid();

    apr[0] = LWN_CreateParm(target->Type(), targ_ldid,
			    MTYPE_To_TY(target->Type()), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr->Type(), addr_ldid, 
			    Make_Pointer_Type(MTYPE_To_TY(type)),
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(idx->Type(), idx_ldid,
			    MTYPE_To_TY(idx->Type()),
			    WN_PARM_BY_VALUE);

    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 3, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);

    WN *parm_ldid = WN_Ldid(target->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = target->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    if (update) {
	parm_ldid = WN_Ldid(addr->Type(), -2, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
	Du_Mgr->Add_Def_Use(callNode, parm_ldid);
	parm_stid = addr->Simd_Preg_Stid(parm_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
	WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    }
    
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate MUL16.lohi (dest, src1, src2), inserts it before stmt and        *
 * sets the line number according to expr if non-null                        *
 *---------------------------------------------------------------------------*/
void
SIMD_INFO::Insert_Mul_Call (WN *stmt, WN *expr, SIMD_PREG *dest,
			    WN *src1, WN *src2, INT lohi) {
    OPERATOR op = OPR_MPY;
    WN *mul = Generate_Mac_Call(op, V4x40type(),
				src1, src2, lohi);
    WN *sto = dest->Simd_Preg_Stid(mul);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = expr?WN_Whirl_Linenum(expr):WN_Whirl_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Generate MULA/MULS using intrinsic calls                                  *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO::Generate_Mula_Expr(WN *expr, WN *stmt,
			       SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
			       SIMD_EINFO *s2_info, WN *mul,
			       SIMD_INFO *simd, INT idx)
{
    Is_True(WN_operator(mul) == OPR_MPY, ("Not a multiply"));
    Is_True(s1_info && s2_info, ("children info missing"));
    Is_True(s1_info->Res_Type() == simd->S16type() &&
	    s2_info->Res_Type() == simd->S16type(),
	    ("Multiplication is not narrow element type"));
    Is_True(idx == 0 || idx == 1, ("Illegal MUL part specification"));

    TYPE_ID     res_type  = Res_Type();
    TYPE_ID     type      = Get_Res_Reg(idx)->Type();
    OPERATOR    op        = WN_operator(expr);

    /* generate a MULA call to store 'expr' result into vector register */
    Is_True(op == OPR_ADD || op == OPR_SUB, ("Unexpected operator"));

    EINFO_PROP prop = Get_Required_Operand_Prop(true);

    WN *newExpr = NULL;
    OPERATOR op1 = (op == OPR_ADD) ? OPR_MADD : OPR_MSUB;
    if (c1_info->Is_Round_Reg()) {
	op1 = OPR_HIGHMPY; /* to get MULR16 */
	WN* dummy_ldid = Get_Res_Reg(idx)->Simd_Preg_Ldid();
	newExpr = 
	    simd->Generate_Mulr_Call(op1, type, 
				     dummy_ldid,
				     s1_info->Gen_Lod_Reg(prop),
				     s2_info->Gen_Lod_Reg(prop),
				     idx);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, newExpr);
	WN_linenum(newExpr) = LWN_Get_Linenum(stmt);
	WN *parm_ldid = WN_Ldid(type, -1, Tie_Output_Volatile_Preg,
				Tie_Output_Volatile_Type);
	Du_Mgr->Add_Def_Use(newExpr, parm_ldid);
	WN *parm_stid = Get_Res_Reg(idx)->Simd_Preg_Stid(parm_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
	WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

	/* add a dependence vertex */
	Is_True(Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL,
		("Expecting MULR enclosed in a loop"));
	Array_Dependence_Graph->Add_Vertex(newExpr);
	return newExpr;
    } else {
	newExpr = 
	    simd->Generate_Mac_Call(op1, type, 
				    Get_Res_Reg(idx)->Simd_Preg_Ldid(),
				    s1_info->Gen_Lod_Reg(prop),
				    idx,
				    s2_info->Gen_Lod_Reg(prop));
	WN *sto     = Get_Res_Reg(idx)->Simd_Preg_Stid(newExpr);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
	WN_linenum(sto) = LWN_Get_Linenum(stmt);
	return newExpr;
    }
}

/*---------------------------------------------------------------------------*
 * Generate MULA/MULS                                                        *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Generate_Mula(WN *expr, WN *child2, WN *stmt,
			  SIMD_EINFO *c1_info, SIMD_LOOP *doinfo,
			  SIMD_INFO *simd)
{
    WN *kid0 = WN_kid0(child2);
    WN *kid1 = WN_kid1(child2);

    SIMD_EINFO *s1_info = doinfo->Simd_Transform(kid0, stmt, simd);
    SIMD_EINFO *s2_info = doinfo->Simd_Transform(kid1, stmt, simd);

    WN* res0 = Generate_Mula_Expr(expr, stmt, c1_info, s1_info, s2_info, 
				  child2, simd);
    WN* res1 = Generate_Mula_Expr(expr, stmt, c1_info, s1_info, s2_info, 
				  child2, simd, 1);

    if (Mulabc()) {
	/* check for MULABC candidate */
	Is_True(WN_operator(expr) == OPR_ADD && 
		WN_operator(kid0) == OPR_ILOAD && 
		WN_operator(kid1) == OPR_ILOAD, ("Invalid MULABC form"));
	
	Is_True(Simd_Exprs()->Elements()==0,("Expressions already set."));
	/* indicate MULABC for MULA_0 */
	Add_Simd_Expr(res0);
	
	/* indicate MULABC for MULA_1 */
	Add_Simd_Expr(res1);
    }
}

void
SIMD_EINFO::Generate_Mul_Expr(WN *expr, WN *stmt,
			      SIMD_EINFO *c1_info, SIMD_EINFO *c2_info,
			      SIMD_INFO *simd)
{
    for (INT i = 0; i < Reg_Count(); i++) {
       Generate_Simd_Expr(false, expr, stmt, c1_info, simd, c2_info, i);
    }
}

#define SHIFT_TOP 31

/*---------------------------------------------------------------------------*
 * Generate Expr result using intrinsic calls                                *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Generate_Simd_Expr(bool unary, WN *expr, WN *stmt,
			       SIMD_EINFO *c1_info, SIMD_INFO *simd,
			       SIMD_EINFO *c2_info, INT idx)
{
    TYPE_ID     res_type  = Res_Type();
    TYPE_ID     type      = Get_Res_Reg()->Type();
    OPERATOR    op        = WN_operator(expr);
    EINFO_PROP  prop      = Get_Required_Operand_Prop();
    
    /* generate a STO of 'expr' result into vector register */
    WN *newExpr = NULL;
    if (unary) {
	newExpr = 
	    simd->Generate_Unary_Call(op, type,
				      c1_info->Gen_Lod_Reg(prop, idx));
    } else {
	if (c2_info == NULL) {
	    Is_True(op == OPR_ASHR || op == OPR_LSHR || op == OPR_SHL ||
		    op == OPR_DIV, ("Not a shift"));
	    WN *kid1 = WN_kid1(expr);
	    if (op == OPR_DIV) {
		Is_True(WN_operator(kid1) == OPR_INTCONST, 
			("DIV by non-const"));
		INT64 val = WN_const_val(kid1);
		Is_True(val > 0 && 
			LNO_IS_POWER_OF_TWO(val), ("DIV is not a shift right"));
		INT shift_amount = 0;
		while ( (val & 0x1) != 0x1) {
		    val >>= 1;
		    shift_amount++;
		}
		newExpr = simd->Generate_Binary_Call(stmt,
                  OPR_ASHR, Get_Res_Reg(idx), c1_info->Gen_Lod_Reg(prop, idx),
                  WN_CreateIntconst(OPC_I4INTCONST, shift_amount));
	    } else if (WN_operator(kid1) == OPR_INTCONST) {
		newExpr = 
                  simd->Generate_Binary_Call(stmt,
                                             op, Get_Res_Reg(idx),
                                             c1_info->Gen_Lod_Reg(prop, idx),
                                             LWN_Copy_Tree(kid1));
	    } else { /* variable shift */

		/* extract the shift amount out and preserve the dependence
		   and invariant information */
		WN *shift_amount = LWN_Copy_Tree(kid1, TRUE, LNO_Info_Map);
		LWN_Copy_Def_Use(kid1, shift_amount, Du_Mgr);
		Simd_Copy_Dependence(kid1, shift_amount);
		INVAR_TABLE *invar_table = Cur_Simd_Loop->Invar_Table();
		Simd_Copy_Invariant(kid1,shift_amount,invar_table);
		
		if (op == OPR_SHL) {
		    /* update the invariant table */
		    OPCODE opc = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
		    WN *top    = WN_CreateIntconst(OPC_I4INTCONST, SHIFT_TOP);
		    shift_amount = LWN_CreateExp2(opc, top, shift_amount);
		    
		    // copy invariant info to the new expression to allow
		    // moving the WSAR instruction to its invariant level
		    Simd_Copy_Invariant_Top_Level(kid1,shift_amount,invar_table);
		}

		Cur_Simd_Loop->Generate_VSAR_Amount(shift_amount, stmt, simd);
		
		WN *shift = simd->Generate_SRL_Call(
		    op, c1_info->Gen_Lod_Reg(prop, idx),
		    Get_Res_Reg(idx)->Simd_Preg_Ldid());
				
		if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
		    Array_Dependence_Graph->Add_Vertex(shift);
		}
		LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, shift);
		WN_linenum(shift) = LWN_Get_Linenum(stmt);
		newExpr = WN_Ldid(Get_Res_Reg(idx)->Type(), -1,
				  Tie_Output_Volatile_Preg,
				  Tie_Output_Volatile_Type);
		Du_Mgr->Add_Def_Use(shift, newExpr);
	    }
	} else if (op == OPR_MPY) {
#if 0
	    Is_True(c1_info->Res_Type() == simd->S16type() &&
		    c2_info->Res_Type() == simd->S16type(),
		    ("Expected narrow element type multiply"));
	    Is_True(idx == 0 || idx == 1, ("Confused"));
#endif
	    newExpr = 
		simd->Generate_Mac_Call(
		    op, type, c1_info->Gen_Lod_Reg(prop),
		    c2_info->Gen_Lod_Reg(prop), idx);
	} else {
	    SIMD_PREG *c1_preg = c1_info->Pick_Simd_Reg(prop, idx);
	    SIMD_PREG *c2_preg = c2_info->Pick_Simd_Reg(prop, idx);
	    WN *c1_lod = c1_preg->Simd_Preg_Ldid();
	    WN *c2_lod = c2_preg->Simd_Preg_Ldid();
	    newExpr = simd->Generate_Binary_Call(stmt, op, Get_Res_Reg(idx), c1_lod, c2_lod);
	}
    }
    WN *sto     = Get_Res_Reg(idx)->Simd_Preg_Stid(newExpr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Generate CMov result using intrinsic calls                                *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Generate_CMov_Expr(WN *expr, WN *stmt, SIMD_EINFO *c_info,
			       SIMD_EINFO *t_info, SIMD_EINFO *f_info,
			       SIMD_INFO *simd, INT idx)
{
    TYPE_ID     res_type  = Res_Type();
    TYPE_ID     type      = Get_Res_Reg()->Type();
    OPERATOR    op        = WN_operator(expr);
    EINFO_PROP  prop      = Get_Required_Operand_Prop();

    /* reg = f_reg */
    WN *newExpr = f_info->Gen_Lod_Reg(prop, idx);
    WN *sto     = Get_Res_Reg(idx)->Simd_Preg_Stid(newExpr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
    
    /* reg = MOVT(t_reg, c_info) */
    WN *cmov = simd->Generate_CMov_Call(t_info->Gen_Lod_Reg(prop, idx),
					c_info->Gen_Lod_Reg(prop, idx),
					Get_Res_Reg(idx)->Simd_Preg_Ldid());
				
    if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	Array_Dependence_Graph->Add_Vertex(cmov);
    }
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, cmov);
    WN_linenum(cmov) = LWN_Get_Linenum(stmt);
    newExpr = WN_Ldid(Get_Res_Reg(idx)->Type(), -1,
		      Tie_Output_Volatile_Preg,
		      Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(cmov, newExpr);
    sto     = Get_Res_Reg(idx)->Simd_Preg_Stid(newExpr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Single use, determine if an expression is single use                      *
 *---------------------------------------------------------------------------*/
bool
SIMD_EINFO::Single_Use(void)
{
    WN *wn = _expr;
    if (WN_operator(wn) == OPR_LDID) {
	DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
	if (!def_list || def_list->Incomplete()) {
	    return false;
	}

	DEF_LIST_ITER iter_def(def_list);
	for (DU_NODE *def_node = iter_def.First();
	     !iter_def.Is_Empty(); def_node = (DU_NODE *) iter_def.Next() ){
	    WN *def = def_node->Wn();
            
            /* If there's a reaching definition outside the vector loop,
               then the value is most likely reused in every iteration. */
            if (!Is_Descendent(def, Cur_Simd_Loop->Simd_Loop()))
              return false;

	    USE_LIST *use_list = Du_Mgr->Du_Get_Use(def);
	    USE_LIST_ITER iter_use(use_list);
	    if (!use_list || use_list->Incomplete()) {
		return false;
	    }

	    for (DU_NODE *use_node = iter_use.First();
		 !iter_use.Is_Empty(); use_node = (DU_NODE *) iter_use.Next()){
		WN *use = use_node->Wn();
		if (use != wn) {
		    return false;
		}
	    }
	}

	return true;

    } else {
	return (WN_operator(wn) != OPR_INTCONST);
    }
}

/*---------------------------------------------------------------------------*
 * Determine if a LDID is overwritten in the same stmt as in                 *
 * s = s +/- child2                                                          *
 *---------------------------------------------------------------------------*/
bool
SIMD_EINFO::Is_Over_Written(void)
{
    WN *ldid = _expr;
    if (WN_operator(ldid) == OPR_LDID) {
	WN *stid = LWN_Get_Statement(ldid);
	if (WN_operator(stid) == OPR_STID) {
	    WN *rhs = WN_kid0(stid);
	    if ((WN_operator(rhs) == OPR_ADD &&
		 (WN_kid0(rhs) == ldid || WN_kid1(rhs) == ldid)) || 
		(WN_operator(rhs) == OPR_SUB) && (WN_kid0(rhs) == ldid)) {
		SYMBOL ldsym(ldid);
		SYMBOL stsym(stid);
		return (ldsym == stsym);
	    }
	}
    }
    return false;
}

TIE_MACRO_ID 
SIMD_INFO::Get_Tie_Set_Vsar()
{
    return tie_info->tie_macro_id("_TIE_Vectra_WUR240");
}

TIE_MACRO_ID 
SIMD_INFO::Get_Tie_Set_Round()
{
    return tie_info->tie_macro_id("_TIE_Vectra_WUR242");
}

/*---------------------------------------------------------------------------*
 * Initialize Vector Integer type, need to hook up with TIE                  *
 *---------------------------------------------------------------------------*/
WN * 
SIMD_INFO::Initialize_VSAR(WN *stmt, WN *amount)
{
    /* WUR240(amount) */
    TIE_MACRO_ID macro_id  = Get_Tie_Set_Vsar();

    Is_True(macro_id != TIE_INVALID_ID, ("Cannot find SetVsar inst"));
    INTRINSIC intrin_id = Tie_Macro_Id_To_Intrinsic(macro_id);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    WN       *apr       = LWN_CreateParm(WN_rtype(amount), amount, 
					 MTYPE_To_TY(WN_rtype(amount)), 
					 WN_PARM_BY_VALUE);
    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, &apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);

    /* add a dependence graph vertex */
    if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	Array_Dependence_Graph->Add_Vertex(callNode);
    }
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Initialize Vector Integer type, need to hook up with TIE                  *
 *---------------------------------------------------------------------------*/
void 
SIMD_INFO::Initialize_Rounding(WN *expr, WN *stmt)
{
    /* WUR242(expr) */
    TIE_MACRO_ID macro_id = Get_Tie_Set_Round();

    Is_True(macro_id != TIE_INVALID_ID, ("Cannot find Set Rreg inst"));
    WN       *copy      = LWN_Copy_Tree(expr, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(expr, copy, Du_Mgr);
    INTRINSIC intrin_id = Tie_Macro_Id_To_Intrinsic(macro_id);
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    WN       *apr       = LWN_CreateParm(MTYPE_I4, copy, 
					 MTYPE_To_TY(MTYPE_I4), 
					 WN_PARM_BY_VALUE);
    WN  *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, &apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);

    /* add a dependence vertex */
    if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	Array_Dependence_Graph->Add_Vertex(callNode);
    }
}

/*---------------------------------------------------------------------------*
 * Return vector length of vector register for element of 'scalar_type'      *
 *---------------------------------------------------------------------------*/
INT SIMD_INFO::Get_SIMD_Width_Scalar(TYPE_ID scalar_type)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    if (type) {
	return type->SIMD_Width();
    } else {
	return 0;
    }
}

/*---------------------------------------------------------------------------*
 * Return vector length of vector register for element of 'simd_type'        *
 *---------------------------------------------------------------------------*/
INT SIMD_INFO::Get_SIMD_Width_SIMD(TYPE_ID simd_type)
{
    SIMD_TYPE *type = SIMD_SIMD_Map()->Find(simd_type);
    Is_True(type,("Type not found in the SIMD to SIMD type map (%s).",
		  MTYPE_name(simd_type)));
    return type->SIMD_Width();
}

/*---------------------------------------------------------------------------*
 * Return vector register type for a scalar type                             *
 *---------------------------------------------------------------------------*/
TYPE_ID SIMD_INFO::Get_SIMD_Reg_Type_Scalar(TYPE_ID scalar_type)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    if (type)
	return type->SIMD_Reg_Type();
    return MTYPE_UNKNOWN;
}

/*---------------------------------------------------------------------------*
 * Return vector memory type for a scalar type                               *
 *---------------------------------------------------------------------------*/
TYPE_ID SIMD_INFO::Get_SIMD_Mem_Type_Scalar(TYPE_ID scalar_type)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    if (type)
	return type->SIMD_Mem_Type();
    return MTYPE_UNKNOWN;
}

// Return memory bytes of a SIMD type corresponding to a scalar type
    
INT SIMD_INFO::Get_SIMD_Mem_Bytes_Scalar(TYPE_ID scalar_type)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    Is_True(type,("Type not found in the scalar to SIMD type map (%s).",
		  MTYPE_name(scalar_type)));
    return type->SIMD_Mem_Byte_Size();
}

// Return element register bits of a SIMD type
INT SIMD_INFO::Get_Elem_Reg_Bits_SIMD(TYPE_ID simd_type)
{
    SIMD_TYPE *type = SIMD_SIMD_Map()->Find(simd_type);
    Is_True(type,("Type not found in the SIMD to SIMD type map (%s).",
		  MTYPE_name(simd_type)));
    return type->Elem_Reg_Bit_Size();
}

// Return element memory bits of a SIMD type
INT SIMD_INFO::Get_Elem_Mem_Bits_SIMD(TYPE_ID simd_type)
{
    SIMD_TYPE *type = SIMD_SIMD_Map()->Find(simd_type);
    Is_True(type,("Type not found in the SIMD to SIMD type map (%s).",
		  MTYPE_name(simd_type)));
    return type->Elem_Mem_Bit_Size();
}


SIMD_SELECT *
SIMD_INFO::Get_SIMD_Select_Scalar (TYPE_ID scalar_type)
{
    SIMD_TYPE *type = Scalar_SIMD_Map()->Find(scalar_type);
    if (type) {
	return type->SIMD_Select();
    } else {
	return NULL;
    }
}


SIMD_SELECT *
SIMD_INFO::Get_SIMD_Select_SIMD (TYPE_ID simd_type)
{
    SIMD_TYPE *type = SIMD_SIMD_Map()->Find(simd_type);
    if (type) {
	return type->SIMD_Select();
    } else {
	return NULL;
    }
}


/*---------------------------------------------------------------------------*
 * Copy SIMD register info from e_old to 'this'                              *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO::Copy_Simd_Reg_Info(SIMD_EINFO *e_old)
{
    SIMD_PREGS_Copy(e_old->Simd_Regs(),Simd_Regs());
    SIMD_PREGS_Copy(e_old->Simd_IRegs(),Simd_IRegs());
    Set_Pre_Load(e_old->Pre_Load());
}

/*---------------------------------------------------------------------------*
 * Generate simd_reg = XOR(simd_reg, simd_reg)                               *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Init_Sum_Reduction (WN *stmt)
{
  SIMD_PREG *res_reg = Get_Res_Reg();
  Is_True(res_reg != NULL, ("Init_Sum_Reduction: res_reg is NULL"));

  OPERATOR op = OPR_BXOR;
  WN *ldid0 = res_reg->Simd_Preg_Ldid();
  WN *ldid1 = res_reg->Simd_Preg_Ldid();
  WN *xor_wn = Simd_Info->Generate_Binary_Call(stmt, op, res_reg, ldid0, ldid1);
  WN *sto = res_reg->Simd_Preg_Stid(xor_wn);
  LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
  // just to shut off the DU chain warnings
  Du_Mgr->Add_Def_Use(sto, ldid0);
  Du_Mgr->Add_Def_Use(sto, ldid1);
  WN_linenum(sto) = LWN_Get_Linenum(stmt);
  
  SIMD_PREG *res_reg1 = Get_Res_Reg(1);
  if (res_reg1 && res_reg1 != res_reg) {
    WN *ldid0 = res_reg1->Simd_Preg_Ldid();
    WN *ldid1 = res_reg1->Simd_Preg_Ldid();
    WN *xor_wn = Simd_Info->Generate_Binary_Call(stmt, op, res_reg1,
						 ldid0, ldid1);
    WN *sto = res_reg1->Simd_Preg_Stid(xor_wn);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    // just to shut off the DU chain warnings
    Du_Mgr->Add_Def_Use(sto, ldid0);
    Du_Mgr->Add_Def_Use(sto, ldid1);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
  }
}

/*---------------------------------------------------------------------------*
 * Initialization into SIMD register of same size from LDID                  *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Init_Simd_Reg_Ldid(WN *stmt, SIMD_LOOP *simd_loop, 
			       SIMD_INFO *simd, MEM_POOL *pool)
{
    WN *res = NULL;
    TYPE_ID res_type = Res_Type();
    Is_True(res_type!=MTYPE_UNKNOWN,("Unknown result type."));
    SIMD_PREG *res_reg = Get_Res_Reg();
    Is_True(res_reg != NULL, ("Init_Simd_Reg_Ldid: Null simd_reg"));
    
    // special case single def 
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(_expr);
    DEF_LIST_ITER iter_def(def_list);
    WN *single_def = NULL;
    if (def_list && !def_list->Incomplete()) {
	for (DU_NODE *def_node = iter_def.First(); !iter_def.Is_Empty();
	     def_node = (DU_NODE *) iter_def.Next()) {
	    WN *def = def_node->Wn();
	    if (single_def) {
		single_def = NULL;
		break;
	    } else {
		single_def = def;
	    }
	}
    }

    if (single_def && WN_operator(single_def) == OPR_STID) {
	WN *rhs = WN_kid0(single_def);
	if (WN_operator(rhs) == OPR_ILOAD ||
	    (WN_operator(rhs) == OPR_LDID && 
	     ST_class(WN_st(rhs)) != CLASS_PREG)) {
	    /* generate LS */
	    res = LWN_Copy_Tree(rhs, TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(rhs, res, Du_Mgr);

	    WN_set_rtype(res, res_reg->Type());
	    WN_set_desc(res, res_type);

	    Simd_Copy_Dependence(rhs, res);
	    WN *sto = res_reg->Simd_Preg_Stid(res);
	    LWN_Insert_Block_After(
		LWN_Get_Parent(single_def), single_def, sto);	    
	    WN_linenum(sto) = LWN_Get_Linenum(stmt);
	    
	    /* for later removal of unused STID */
	    simd_loop->Inv_Ldid().Push(single_def);
	    return;
	}
    }
    
    /* fix up the loop statement */
    WN *loop = def_list->Loop_stmt();
    if (loop == stmt) {
	def_list->Set_loop_stmt(Enclosing_Do_Loop(LWN_Get_Parent(stmt)));
    }

    res = LWN_Copy_Tree(_expr);
    LWN_Copy_Def_Use(_expr, res, Du_Mgr);
    
    if (ST_class(WN_st(res)) != CLASS_PREG) {
	/* generate LS */
	WN_set_rtype(res, res_reg->Type());
	WN_set_desc(res, res_type);
    } else {
	/* generate a CVT */
	OPCODE opc = OPCODE_make_op(OPR_CVT, res_reg->Type(), res_type);
	res = LWN_CreateExp1(opc, res);
    }
    WN *sto = res_reg->Simd_Preg_Stid(res);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * copy simd_reg to all required simd registers so they are the same         *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Duplicate_To_All(INT vec_length, SIMD_PREG *simd_preg)
{
    /* copy simd_reg to all required simd registers since they are the same */
    if (Interleaved() || Need_Interleave()) {
	for (INT i = 0; i < vec_length; i++) {
	    Add_Simd_IReg(simd_preg);
	}
    }
    if (Need_Deinterleave() || !Interleaved()) {
	for (INT i = 0; i < vec_length; i++) {
	    Add_Simd_Reg(simd_preg);
	}
    }
}

/*---------------------------------------------------------------------------*
 * Loop invariant initialization of SIMD register of same size               *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO::Init_Simd_Reg_Invariant(WN *stmt, SIMD_LOOP *simd_loop, 
				    SIMD_INFO *simd, MEM_POOL *pool)
{
    WN *res = NULL;
    TYPE_ID res_type = Res_Type();
    Is_True(res_type != MTYPE_UNKNOWN,("Unknown result type."));
    SIMD_PREG *res_preg = Gen_Symbol(simd, res_type, pool);

    if (WN_operator(_expr) == OPR_ILOAD) {
	/* generate LS */
	WN_set_rtype(_expr, res_preg->Type());
	WN_set_desc(_expr, res_type);
	res = _expr;
    } else {
	/* generate a CVT */
	OPCODE opc = OPCODE_make_op(OPR_CVT, res_preg->Type(), res_type);
	res = LWN_CreateExp1(opc, _expr);
    }

    WN *sto = res_preg->Simd_Preg_Stid(res);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
    Add_Simd_Expr(res);    

    INT vec_length = simd_loop->Vec_Length(simd, res_type);

    /* copy simd_reg to all required simd registers since they are the same */
    Duplicate_To_All(vec_length, res_preg);
}

/*---------------------------------------------------------------------------*
 * Get the symbol holding narrow 1                                           *
 *---------------------------------------------------------------------------*/
WN *Get_Narrow_One(SIMD_INFO *simd, SIMD_LOOP *simd_loop, TYPE_ID scalar_type)
{
    OPCODE      opc  = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN         *one  = WN_CreateIntconst(opc, 1);
    SIMD_PREG  *addr = simd_loop->Get_Constant(one,
					       (scalar_type == MTYPE_UNKNOWN)?
					       simd->S16type() : scalar_type,
					       simd); 
    Is_True(addr, ("Cannot find constant 1 preg"));

    return addr->Simd_Preg_Ldid();
}

/*---------------------------------------------------------------------------*
 * Deinterleave the Simd_IReg(0) to SIMD_Reg(0)                              *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Simd_Deinterleave_One(WN *stmt, 
				  SIMD_INFO *simd, SIMD_LOOP *doinfo,
				  WN *block)
{
    Is_True(Simd_Reg(0) && !Simd_Reg(1), ("Null Simd_Reg"));
    
    /* Generate select deinterleave */
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(Simd_Reg(0)->Type());
    SIMD_PREG *sel = doinfo->Create_Sel_Preg(simd_sel->Sel_73625140());
    Is_True(sel != NULL, ("Unexpected NULL SIMD select register"));
    
    TYPE_ID    mtype     = Simd_Reg(0)->Type();
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = simd->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    
    INT64 line_num = 0;
    if (stmt) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }

    /* select deinterleave */
    WN* apr[3];
    apr[0] = LWN_CreateParm(mtype, Simd_IReg(0)->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, Simd_IReg(0)->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN *sto = Simd_Reg(0)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = line_num;
}

/*---------------------------------------------------------------------------*
 * Deinterleave the Simd_IReg(1), Simd_IReg(0) into Simd_Reg(1..0)           *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Simd_Deinterleave(WN *stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo,
			      WN *block)
{
    if (Simd_IReg(1) == NULL) {
      /* only one register */
      Simd_Deinterleave_One(stmt, simd, doinfo, block);
      return;
    }

    INT64 line_num = 0;
    if (stmt) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }

    SIMD_PREG *Spreg   = Simd_IReg(0);
    SIMD_PREG *Spreg_1 = Simd_IReg(1);

    Is_True(Spreg && Spreg_1, ("No interleave register in Simd_Deinterleave"));
    Is_True(Need_Deinterleave(), ("Expect Need_Deinterleave"));

    Is_True(Simd_Reg(0) && Simd_Reg(1), ("Null Simd_Reg"));
    
    /* Generate select interleave */
    TYPE_ID    mtype     = Simd_Reg(0)->Type();
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(mtype);
    Is_True(simd_sel != NULL, ("NULL SIMD select object."));
    
    SIMD_PREG *sel = doinfo->Create_Sel_Preg(simd_sel->Sel_5140());
    Is_True(sel != NULL, ("Unexpected NULL SIMD select register"));
    
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = simd->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    
    /* select low */
    WN* apr[3];
    apr[0] = LWN_CreateParm(mtype, Spreg_1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, Spreg->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN *sto = Simd_Reg(0)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(block, stmt, sto);
    WN_linenum(sto) = line_num;

    /* select high */
    sel = doinfo->Create_Sel_Preg(simd_sel->Sel_7362());
    apr[0] = LWN_CreateParm(mtype, Spreg_1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, Spreg->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    sto = Simd_Reg(1)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(block, stmt, sto);
    WN_linenum(sto) = line_num;
}

/*---------------------------------------------------------------------------*
 * Interleave the register of size 1                                         *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO::Simd_Interleave_One(WN* stmt, SIMD_INFO *simd, 
				     SIMD_LOOP *doinfo)
{
    Is_True(Need_Interleave(), ("Unexpected interleaving property"));
    Is_True(Simd_IRegs()->Elements() == 1, ("Unexpected IReg length"));
    
    /* Generate select interleave */
    TYPE_ID    mtype     = Simd_IReg(0)->Type();
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(mtype);
    Is_True(simd_sel != NULL, ("NULL SIMD select object."));
    
    SIMD_PREG *sel = doinfo->Create_Sel_Preg(simd_sel->Sel_75316420());
    Is_True(sel != NULL, ("Unexpected NULL SIMD select register"));
    
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = simd->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);

    /* select interleave */
    WN* apr[3];
    apr[0] = LWN_CreateParm(mtype, Simd_Reg(0)->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, Simd_Reg(0)->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN *sto = Simd_IReg(0)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Interleave the registers                                                  *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO::Simd_Interleave(WN* stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo)
{
    Is_True(Need_Interleave(), ("Unexpected interleaving property"));

    if (Simd_IRegs()->Elements() == 1) {
      Simd_Interleave_One(stmt, simd, doinfo);
      return;
    }

    Is_True(Simd_IRegs()->Elements() == 2, ("Unexpected IReg length"));

    TYPE_ID    mtype     = Simd_IReg(0)->Type();
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(mtype);
    Is_True(simd_sel != NULL, ("NULL SIMD select object."));
    
    SIMD_PREG *sel = doinfo->Create_Sel_Preg(simd_sel->Sel_6420());
    Is_True(sel != NULL, ("Unexpected NULL SIMD select register"));
    
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = simd->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    
    SIMD_PREGS&   source = *Simd_Regs();

    WN* apr[3];
    apr[0] = LWN_CreateParm(mtype, source[1]->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, source[0]->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN *sto = Simd_IReg(0)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
    
    sel = doinfo->Create_Sel_Preg(simd_sel->Sel_7531());
    Is_True(sel != NULL, ("Unexpected NULL SIMD select register"));
    
    apr[0] = LWN_CreateParm(mtype, source[1]->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, source[0]->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
			    MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);

    perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    sto = Simd_IReg(1)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Interleave/Deinterleave if required                                       *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO::Simd_Post_Processing(
    WN* stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo)
{
    Is_True(!(Need_Interleave() && Need_Deinterleave()),
	    ("Interleave, Deinterleave, which comes first?"));

    if (Need_Interleave()) {
	Simd_Interleave(stmt, simd, doinfo);
    }

    if (Need_Deinterleave()) {
	Simd_Deinterleave(stmt, simd, doinfo);
    }
}

extern const char *
SIMD_MTYPE_Msg(TYPE_ID mtype)
{
  const char *buf = "unknown type";
  switch (mtype) {
  case MTYPE_UNKNOWN: 
    buf = "unknown type";
    break;
  case MTYPE_B:
    buf = "predicate type";
    break;
  case MTYPE_I1:
    buf = "char";
    break;
  case MTYPE_I2:
    buf = "short";
    break;
  case MTYPE_I4:
    buf = "int";
    break;
  case MTYPE_I8:
    buf = "long long";
    break;
  case MTYPE_U1:
    buf = "unsigned char";
    break;
  case MTYPE_U2:
    buf = "unsigned short";
    break;
  case MTYPE_U4:
    buf = "unsigned int";
    break;
  case MTYPE_U8:
    buf = "unsigned long long";
    break;
  case MTYPE_F4:
    buf = "float";
    break;
  case MTYPE_F8:
    buf = "double";
    break;
  case MTYPE_F10:
    buf = "IEEE 60-bit float";
    break;
  case MTYPE_F16:
    buf = "IEEE 128-bit float";
    break;
  case MTYPE_STR:
    buf = "string";
    break;
  case MTYPE_FQ:
    buf = "128-bit float";
    break;
  case MTYPE_M:
    buf = "struct";
    break;
  case MTYPE_C4:
    buf = "complex float";
    break;
  case MTYPE_C8:
    buf = "64-bit complex float";
    break;
  case MTYPE_CQ:
    buf = "128-bit complex float";
    break;
  case MTYPE_V:
    buf = "void type";
    break;
  case MTYPE_BS:
    buf = "bits";
    break;
  case MTYPE_A4:
    buf = "32-bit address";
    break;
  case MTYPE_A8:
    buf = "64-bit address";
    break;
  case MTYPE_C10:
    buf = "80-bit complex float";
    break;
  case MTYPE_C16:
    buf = "128-bit complex float";
    break;
  case MTYPE_I16:
    buf = "128-bit int";
    break;
  case MTYPE_U16:
    buf = "128-bit unsigned int";
    break;
  case MTYPE_XTBOOL:
    buf = "xtbool";
    break;
  case MTYPE_XTBOOL2:
    buf = "xtbool2";
    break;
  case MTYPE_XTBOOL4:
    buf = "xtbool4";
    break;
  case MTYPE_XTBOOL8:
    buf = "xtbool8";
    break;
  case MTYPE_XTBOOL16:
    buf = "xtbool16";
    break;
  default:
    buf = MTYPE_name(mtype);
    break;
  }
  
  return buf;
}

static char *
SIMD_OPCODE_Msg(OPERATOR opr, TYPE_ID rtype, TYPE_ID desc)
{
  static char buffer [1024];

  switch (opr) {

    case OPR_ABS:
      // [RTYPE] : f,I4,I8,I16 [DESC] : V
      sprintf (buffer, "%s abs", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_CONST:
    case OPR_INTCONST:
      sprintf (buffer, "%s constant", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_ASM_EXPR:
      sprintf (buffer, "assembly instructions");
      break;

    case OPR_ADD:
      // [RTYPE] : f,i,p,z [DESC] : V
      sprintf (buffer, "%s +", SIMD_MTYPE_Msg(rtype));
      break;
    case OPR_SUB:
      // [RTYPE] : f,i,p,z [DESC] : V
      sprintf (buffer, "%s -", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_AGOTO:
    case OPR_ALTENTRY:
    case OPR_ASSERT:
    case OPR_BACKWARD_BARRIER:
    case OPR_BLOCK:
    case OPR_CASEGOTO:
    case OPR_COMMENT:
    case OPR_COMPGOTO:
    case OPR_DO_LOOP:
    case OPR_DO_WHILE:
    case OPR_EVAL:
    case OPR_EXC_SCOPE_BEGIN:
    case OPR_EXC_SCOPE_END:
    case OPR_FALSEBR:
    case OPR_FORWARD_BARRIER:
    case OPR_FUNC_ENTRY:
    case OPR_GOTO:
    case OPR_IDNAME:
    case OPR_IF:
    case OPR_IO:
    case OPR_IO_ITEM:
    case OPR_LABEL:
    case OPR_LOOP_INFO:
    case OPR_OPT_CHI:
    case OPR_OPT_RESERVE2:
    case OPR_PRAGMA:
    case OPR_PREFETCH:
    case OPR_PREFETCHX:
    case OPR_REGION:
    case OPR_REGION_EXIT:
    case OPR_RETURN:
    case OPR_SWITCH:
    case OPR_TRAP:
    case OPR_TRUEBR:
    case OPR_WHERE:
    case OPR_WHILE_DO:
    case OPR_XGOTO:
    case OPR_XPRAGMA:
    case OPR_AFFIRM:
    case OPR_DEALLOCA:
    case OPR_ASM_STMT:
    case OPR_ASM_INPUT:
    case OPR_GOTO_OUTER_BLOCK:
      // [RTYPE] : V [DESC] : V
      sprintf (buffer, "(control) statement (%s)", &OPERATOR_info [opr]._name [4]);
      break;

    case OPR_ARRAY:
      // [RTYPE] : p [DESC] : V
      sprintf (buffer, "%s array", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_ARRSECTION: 
    case OPR_TRIPLET:
      // [RTYPE] : p [DESC] : V
      sprintf (buffer, "%s array section", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_ARRAYEXP:
      // [RTYPE] : f,i,M,z [DESC] : V
      sprintf (buffer, "%s array expression", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_LDA:
    case OPR_LDMA:
    case OPR_ILDA:
    case OPR_LDA_LABEL:
      // [RTYPE] : p [DESC] : V
      sprintf (buffer, "taking address (&)");
      break;

    case OPR_ASHR:
    case OPR_LSHR:
      // [RTYPE] : i [DESC] : V
      sprintf (buffer, "%s >>", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_BAND:
      sprintf (buffer, "%s &", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_BIOR:
      sprintf (buffer, "%s |", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_BNOR:
      sprintf (buffer, "%s ~|", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_BNOT:
      sprintf (buffer, "%s ~", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_BXOR:
      sprintf (buffer, "%s ^", SIMD_MTYPE_Msg(rtype));
      break;
	
    case OPR_COMPOSE_BITS:
      sprintf (buffer, "%s compose bits", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_EXTRACT_BITS:
      sprintf (buffer, "%s extract bits", SIMD_MTYPE_Msg(rtype));
      break;
      
    case OPR_CVTL:
      sprintf (buffer, "%s truncation", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_DIVREM:
      sprintf (buffer, "%s composite quotient and remainder (/, %%)", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_HIGHMPY:
      sprintf (buffer, "%s extract high part", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_LOWPART:
      sprintf (buffer, "%s extract lower part", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MOD:
    case OPR_REM:
      sprintf (buffer, "%s %%", SIMD_MTYPE_Msg(rtype));
      break;
	
    case OPR_SHL:
      // [RTYPE] : i [DESC] : V
      sprintf (buffer, "%s <<", SIMD_MTYPE_Msg(rtype));
      break;
	
    case OPR_XMPY:
      // [RTYPE] : i [DESC] : V
      sprintf (buffer, "composite (high, lower) multiplication %s", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_INTRINSIC_CALL:
      sprintf(buffer, "intrinsic call");
      break;

    case OPR_CALL:
    case OPR_ICALL:
    case OPR_PICCALL:
    case OPR_VFCALL:
      sprintf (buffer, "function call");
      break;

    case OPR_CAND:
    case OPR_LAND:
      // [RTYPE] : b : [DESC] : V
      sprintf (buffer, "%s &&", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_CIOR:
    case OPR_LIOR:
      sprintf (buffer, "%s ||", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_LNOT:
      sprintf (buffer, "%s !", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_CEIL:
    case OPR_FLOOR:
    case OPR_RND:
    case OPR_TRUNC:
      // [RTYPE] : i [DESC] : f
      sprintf (buffer, "float to int conversion (%s)", &OPERATOR_info [opr]._name [4]);
      break;

    case OPR_COMMA:
    case OPR_RCOMMA:
      // [RTYPE] : f,i,M,p,z [DESC] : V
      sprintf (buffer, "%s compiler internal (COMMA)",  SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_PAIR:
      // [RTYPE] : z [DESC] : V
      sprintf (buffer, "%s compiler internal (PAIR)",  SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_CSELECT:
    case OPR_SELECT:
      // [RTYPE] : b,f,i,M,p,V,z [DESC] : V
      sprintf (buffer, "%s select (p ? a : b)", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_CVT:
      // [RTYPE] : f,i,p [DESC] : f,i,p extra
      sprintf (buffer, "type conversion from %s to %s", SIMD_MTYPE_Msg(desc), SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_DIV:
      sprintf (buffer, "%s /", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MPY:
      sprintf (buffer, "%s *", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_NEG:
      sprintf (buffer, "%s -", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_PARM:
    case OPR_OPTPARM:
      // [RTYPE] : f,i,z [DESC] : V
      sprintf (buffer, "%s function parameter", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_EQ:
      sprintf (buffer, "%s ==", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_NE:
      sprintf (buffer, "%s !=", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_GE:
      sprintf (buffer, "%s >=", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_GT:
      sprintf (buffer, "%s >", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_LE:
      sprintf (buffer, "%s <=", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_LT:
      sprintf (buffer, "%s <", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_ILDBITS:
    case OPR_LDBITS:
      // [RTYPE] : i [DESC] : s
      sprintf (buffer, "load of bits");
      break;

    case OPR_ILOAD:
    case OPR_LDID:
      sprintf (buffer, "%s load", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_ILOADX:
      sprintf (buffer, "%s indexed load", SIMD_MTYPE_Msg(desc));
      break;
	
    case OPR_MADD:
    case OPR_NMADD:
      sprintf (buffer, "%s combined multiplication addition", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_MSUB:
    case OPR_NMSUB:
      sprintf (buffer, "%s combined multiplication substraction", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_FIRSTPART:
      sprintf (buffer, "%s real part of a complex number", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_SECONDPART:
      sprintf (buffer, "%s imaginary part of a complex number", SIMD_MTYPE_Msg(rtype));
      // [RTYPE] : f [DESC] : V
      break;

    case OPR_INTRINSIC_OP:
      // [RTYPE] : b,f,i,M,p,s,z [DESC] : V
      sprintf (buffer, "%s intrinsic operation", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_ISTBITS:
    case OPR_STBITS:
      // [RTYPE] : V [DESC] : s
      sprintf (buffer, "store of bits");
      break;

    case OPR_ISTORE:
    case OPR_STID:
      // [RTYPE] : V [DESC] : bs,f,i,M,p,s,z
      sprintf (buffer, "%s store", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_ISTOREX:
      // [RTYPE] : V [DESC] : f
      sprintf (buffer, "%s indexed store", SIMD_MTYPE_Msg(desc));
      break;

    case OPR_MAX:
      // [RTYPE] : f,i,p [DESC] : V
      sprintf (buffer, "%s max", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MIN:
      sprintf (buffer, "%s min", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MINMAX:
      // [RTYPE] : f,i,p [DESC] : V
      sprintf (buffer, "%s composite min-max");
      break;

    case OPR_MAXPART:
      sprintf (buffer, "%s max part of a min-max pair", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MINPART:
      // [RTYPE] : f,i [DESC] : V
      sprintf (buffer, "%s min part of a min-max pair", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_MLOAD:
      // [RTYPE] : M [DESC] : V
      sprintf (buffer, "struct load");
      break;

    case OPR_MSTORE:
      sprintf (buffer, "struct store");
      break;

    case OPR_PAREN:
      sprintf (buffer, "parenthesis");
      break;

    case OPR_RECIP:
      sprintf (buffer, "%s reciprocal", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_RSQRT:
      sprintf (buffer, "%s reciprocal of square root", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_SQRT:
      sprintf (buffer, "%s square root", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_RETURN_VAL:
      // [RTYPE] : f,i,M,p,s,z [DESC] : V
      sprintf (buffer, "%s function return value", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_TAS:
      // [RTYPE] : f,i,p,s,z [DESC] : V
      sprintf (buffer, "% type cast", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_ALLOCA:
      // [RTYPE] : p [DESC] : V
      sprintf (buffer, "%s stack alloca", SIMD_MTYPE_Msg(rtype));
      break;

    case OPR_RROTATE:
      // [RTYPE] : U4,U8 [DESC] : U1,U2,U4,U8
      sprintf (buffer, "%s right rotation of bits", SIMD_MTYPE_Msg(rtype));
      break;

#ifdef TARG_XTENSA
    case OPR_TN:
      sprintf (buffer, "code generation temporary name");
      break;

#endif
    default:
      buffer [0] = 0;
      break;
  }

  return buffer;
}


static char *
SIMD_CVTL_Msg(TYPE_ID rtype, INT offset)
{
  static char cvtl_buf[64];
  if (MTYPE_is_signed(rtype)) {
      if (offset <= 8) {
	  sprintf(cvtl_buf, "truncation to char");
      } else if (offset <= 16) {
	  sprintf(cvtl_buf, "truncation to short");
      } else if (offset <= 32) {
	  sprintf(cvtl_buf, "truncation to int");
      } else {
	  sprintf(cvtl_buf, "truncation to long long");
      }
  } else {
      if (offset <= 8) {
	  sprintf(cvtl_buf, "truncation to unsigned char");
      } else if (offset <= 16) {
	  sprintf(cvtl_buf, "truncation to unsigned short");
      } else if (offset <= 32) {
	  sprintf(cvtl_buf, "truncation to unsigned int");
      } else {
	  sprintf(cvtl_buf, "truncation to long long");
      }
  }
      
  return cvtl_buf;
}

extern char *
SIMD_OPCODE_Msg (const WN *wn)
{
  if (WN_operator(wn) == OPR_CVTL)
    return SIMD_CVTL_Msg(WN_rtype(wn), WN_offset(wn));

  AT_WN_DECODE wn_decode = AT_WN_Decode_SIMD_EINFO(wn);
  TYPE_ID canonical_desc =
    (WN_kid_count(wn) > 0) ? AT_FACTORY::canonical_kid_type(wn, 0,
							    &AT_WN_Decode_SIMD_EINFO) :
    wn_decode.desc;
  return SIMD_OPCODE_Msg(WN_operator(wn),
			 wn_decode.rtype,
			 canonical_desc);
}


static bool
is_valid_id (const char *id)
{
  if (!id || !*id)
    return false;
  
  for (const char *tid = id; *tid; tid++)
  {
    char c = *tid;
    if (c == '_' || isalpha(c))
      continue;
    
    if (tid != id && isdigit(c))
      continue;
    
    return false;
  }
  
  return true;
}


extern const char *
SIMD_Base_Name_Msg (const WN *wn)
{
  if (WN_operator(wn) == OPR_ILOAD) {
    wn = WN_kid0(wn);
  } else if (WN_operator(wn) == OPR_ISTORE) {
    wn = WN_kid1(wn);
  }
  if (WN_operator(wn) == OPR_ARRAY) {
    wn = WN_array_base(wn);
  }

  if (WN_operator(wn) == OPR_LDID || 
      WN_operator(wn) == OPR_LDA) {
    ST *st = WN_st(wn);

    if (ST_class(st) != CLASS_PREG) {
      return ST_name(st);
    }
    
    PREG_NUM preg = WN_offset(wn);
    if (!Preg_Is_Dedicated(preg)) {
      const char *name = Preg_Name(preg);
      if (is_valid_id(name))
        return name;
    }
  }

  return "?";
}


void
SIMD_Msgv (AT_MSG_ID msg_id, const WN *wn, va_list vargs)
{
  if (Simd_Target == SIMD_TARG_UNKNOWN)
    return;
  
  /* If running Autotie analysis, collect all messages in the
     current SIMD_AT object so that we can later propagate them
     to Autotie. */
  bool msg_auto = (Simd_Info && Simd_Info->AT_Analysis_Phase());
  bool msg_file = (LNO_SIMD_Verbose > 0);
  
  if (!msg_auto && !msg_file)
    return;

  /* Initialize a static buffer once. Set the last character to zero
     to detect buffer overflow. */
  static char *buf = NULL;
  const int buf_size = 2048;
  if (!buf) {
    buf = (char *)MEM_POOL_Alloc(Malloc_Mem_Pool, buf_size * sizeof(char));
    buf[buf_size - 1] = 0;
  }
  
  SRCPOS srcpos = wn ? LWN_Get_Linenum(wn) : 0;
  INT lineno = wn ? (INT)Srcpos_To_Line(srcpos) : 0;
  const char *filename = wn ? SIMD_Filename(srcpos, true) : Src_File_Name;
  const char *base_filename = wn ? SIMD_Filename(srcpos, false) : Src_File_Name;

  vsprintf(buf, AT_MSG_ID_short_desc_fmt(msg_id), vargs);
  FmtAssert(buf[buf_size - 1] == 0, ("SIMD message buffer overflow."));
  
  if (msg_auto) {
    AT_MESSAGE *msg = XT_New(AT_Libauto_Pool())
      AT_MESSAGE(AT_Libauto_Pool(), msg_id, filename, lineno, buf);

    SIMD_AT *simd_at = Simd_Info->SIMD_At();
    switch (msg_id)
    {
    case AT_MSG_SIMD_NO_SNL:
    case AT_MSG_SIMD_BAD_LOOP:
    case AT_MSG_SIMD_NO_LOOP:
      /* Don't add these messages. Regions for some loops will be created
         later by CG with appropriate messages. */
      break;

    case AT_MSG_SIMD_ARRAY_ALIAS:
    case AT_MSG_SIMD_BAD_DGRAPH:
    case AT_MSG_SIMD_BAD_TIE_OP:
    case AT_MSG_SIMD_DISABLED:
    case AT_MSG_SIMD_LOOP_TOO_DEEP:
    case AT_MSG_SIMD_NON_COUNTABLE_LOOP:
    case AT_MSG_SIMD_PRAGMA_IGNORED:
    case AT_MSG_SIMD_PROC_BEGIN:
    case AT_MSG_SIMD_UNSIGNED_LOOP_UPPER_BOUND:
      /* Maintain the file number with the message. This is necessary
         so that some function level messages are associated with their
         regions later. */
      if (wn) {
        msg->Set_File_Number(SRCPOS_filenum(srcpos));
      }
      
      /* PU level messages. */
      simd_at->AT_Pu()->Messages()->Append(msg, /* unique_only */ true);
      
      break;
      
    default:
      /* Region level messages. */
      Simd_Info->SIMD_At()->Add_Message(msg);
      break;
    }
  }
  
  if (msg_file) {
    const char *msg_id_str = AT_MSG_ID_str(msg_id);
    
    if (Simd_Info &&
        (msg_id == AT_MSG_SIMD_ARRAY_ALIAS ||
         msg_id == AT_MSG_SIMD_PRAGMA_IGNORED ||
         msg_id == AT_MSG_STAT_UNALIGNED_LOAD ||
         msg_id == AT_MSG_STAT_UNALIGNED_STORE)) {
      /* Generate a message string and add it to the message string
         set. If the message is already in the set, don't emit it to
         stdout. */
      char *msg = (char *)MEM_POOL_Alloc(&SIMD_auto_pool, sizeof(char) *
                                         (strlen(buf) + strlen(filename) +
                                          strlen(msg_id_str) + 32));
      sprintf(msg, "%s|%d|%s|%s", filename, lineno, msg_id_str, buf);
      
      if (!Simd_Info->Add_Message_Str(msg))
        return;
    }

    fprintf(stdout, "%s%s:%d (%s): %s\n",
	    (msg_id == AT_MSG_SIMD_PROC_BEGIN || msg_id == AT_MSG_SIMD_LOOP_BEGIN) ? "\n" : "",
            base_filename, lineno, msg_id_str, buf);
  }
}


void
SIMD_Msg (AT_MSG_ID msg_id, const WN *wn, ...)
{
  va_list vargs;
  va_start (vargs, wn);
  SIMD_Msgv(msg_id, wn, vargs);
  va_end(vargs);
}


const char *
SIMD_Filename (SRCPOS pos, bool fullpath)
{
  const char *fname, *dirname;
  IR_Srcpos_Filename(pos, &fname, &dirname);
  return((!fname) ? NULL :
         (!dirname || !fullpath || !AT_Libauto_Pool()) ? fname :
         xt_pool_printf(AT_Libauto_Pool(), "%s/%s", dirname, fname));
}


void
SIMD_INFO::Shift_Into_Reg(SIMD_PREG *new_input, SIMD_PREG *res,
			  WN *block, WN *stmt, INT64 line_num, 
			  INT shift_amount, SIMD_PREG *target)
{
    TYPE_ID    mtype  = res->Type();
    SIMD_SELECT *simd_sel = Get_SIMD_Select_SIMD(mtype);
    Is_True(simd_sel != NULL, ("NULL SIMD select object."));
    INT64      sel_0  = simd_sel->Sel_4321(shift_amount);
    
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = Find_Function(op, mtype, MTYPE_UNKNOWN, 1);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    WN*        apr[3];
    
    /* select shift */
    apr[0] = LWN_CreateParm(mtype, new_input->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, res->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(MTYPE_U4, WN_CreateIntconst(OPC_U4INTCONST,
							sel_0),
			    MTYPE_To_TY(MTYPE_U4), 
			    WN_PARM_BY_VALUE);
    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN  *sto  = ((target) ? target : res)->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(block, stmt, sto);
    WN_linenum(sto) = line_num;
}


const char *
Simd_Target_String (SIMD_TARGET target)
{
    switch (target)
    {
    case SIMD_TARG_VECTRA_I:
	return "SIMD_TARG_VECTRA_I";

    case SIMD_TARG_VECTRA_II:
	return "SIMD_TARG_VECTRA_II";
	
    case SIMD_TARG_GENERIC:
	return "SIMD_TARG_GENERIC";

    case SIMD_TARG_AT_ANALYSIS:
	return "SIMD_TARG_AT_ANALYSIS";
	
    default:
	break;
    }
    
    return "SIMD_TARG_UNKNOWN";
}

TYPE_ID Get_Packed_Output_Type(TIE_MACRO_ID tie_macro_id)
{
    Is_True(tie_macro_id != TIE_INVALID_ID, ("Invalid TIE macro id"));
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
    return tie_macro->output_mtype(tie_info);
}

TYPE_ID Get_Packed_Output_Type(INTRINSIC intrin_id)
{
    return Get_Packed_Output_Type(Intrinsic_To_Tie_Macro_Id(intrin_id));
}


INT
Ldid_Def_Alignment (WN *ldid)
{
  const INT unknown_align = 1;
  
  if (WN_operator(ldid) != OPR_LDID)
    return unknown_align;
  
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ldid);
  if (!def_list || def_list->Incomplete() || def_list->Is_Empty()) {
    return unknown_align;
  }
  
  /* Traverse through all DEFs and find the GCD of the known
     alignments. */
  INT align = Stack_Alignment();
  DEF_LIST_ITER iter_def(def_list);
  for (DU_NODE* def_node = iter_def.First(); 
       !iter_def.Is_Empty(); 
       def_node = (DU_NODE *) iter_def.Next()) {
    WN* def = def_node->Wn();
    if (WN_operator(def) != OPR_STID) {
      return unknown_align;
    }
    
    WN* rhs = WN_kid0(def);
    OPERATOR oper = WN_operator(rhs);

    /* Check for ALLOCA operator. Stack alignment is the start assumption. */
    if (oper == OPR_ALLOCA) {
      continue;
    }
    
    /* For integer constants, find the GCD of their value and
       the current alignment. */
    if (oper == OPR_INTCONST) {
      INT64 cval = WN_const_val(rhs); 
      align = gcd(align, cval);
      continue;
    }
    
    /* Check for ALLOCA intrinsic call. */
    if (oper != OPR_LDID ||
	ST_class(WN_st(rhs)) != CLASS_PREG ||
	!Preg_Is_Dedicated(WN_offset(rhs))) {
      return unknown_align;
    }
    
    WN *prev = WN_prev(def);
    if (!prev ||
	WN_operator(prev) != OPR_INTRINSIC_CALL ||
	WN_intrinsic(prev) != INTRN_U4I4ALLOCA) {
      return unknown_align;
    }

    /* ALLOCA intrinsic call -- stack alignment is the start assumption. */
  }
  
  return align;
}


void
SIMD_Reverse_Backward_Loops (WN *wn)
{
  if (Simd_Target == SIMD_TARG_UNKNOWN || !wn)
    return;
  
  /* Find the innermost DO_LOOP. */
  for (WN *wn_inner = wn; wn_inner; wn_inner = Find_Next_Innermost_Do(wn_inner)) {
    wn = wn_inner;
  }
  
  if (WN_operator(wn) != OPR_DO_LOOP)
    return;
  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn);
  if (!dli->Is_Backward || !RV_Is_Legal(wn))
    return;
  
  if (simd_debug) {
    fprintf(TFile, "SIMD reversing backward inner loop...\n");
  }
  
  RV_Reverse_Loop(wn);
}


SIMD_LOOP *
SIMD_LOOP_New (LOOP_MODEL *lm, WN *simd_loop, INT simd_loop_level)
{
  SIMD_LOOP *sloop = NULL;
  
  switch (Simd_Target)
  {
  case SIMD_TARG_VECTRA_I:
    sloop = CXX_NEW(SIMD_LOOP(&SIMD_default_pool, lm, simd_loop, simd_loop_level),
                    &SIMD_default_pool);
    break;

  case SIMD_TARG_VECTRA_II:
    sloop = CXX_NEW(SIMD_LOOP_V2(&SIMD_default_pool, lm, simd_loop, simd_loop_level),
                    &SIMD_default_pool);
    break;
    
  case SIMD_TARG_GENERIC:
  case SIMD_TARG_AT_ANALYSIS:
    sloop = CXX_NEW(SIMD_LOOP_AT(&SIMD_default_pool, lm, simd_loop, simd_loop_level),
                    &SIMD_default_pool);
    break;
    
  default:
    FmtAssert(0, ("Unexpected SIMD target (%s)",
                  Simd_Target_String(Simd_Target)));
    break;
  }
  
  return sloop;
}


OPERATOR
SIMD_Negate_Comparison (OPERATOR oper)
{
  switch (oper)
  { 
  case OPR_LT:
    oper = OPR_GE;
    break;
    
  case OPR_LE:
    oper = OPR_GT;
    break;
    
  case OPR_EQ:
    oper = OPR_NE;
    break;

  case OPR_NE:
    oper = OPR_EQ;
    break;

  case OPR_GE:
    oper = OPR_LT;
    break;
    
  case OPR_GT:
    oper = OPR_LE;
    break;
    
  default:
    FmtAssert(0, ("Unexpected operator."));
  }
  
  return oper;
}


OPERATOR
SIMD_Swap_Comparison (OPERATOR oper)
{
  switch (oper)
  {
  case OPR_LT:
    oper = OPR_GT;
    break;

  case OPR_LE:
    oper = OPR_GE;
    break;
    
  case OPR_NE:
    oper = OPR_NE;
    break;

  case OPR_EQ:
    oper = OPR_EQ;
    break;

  case OPR_GE:
    oper = OPR_LE;
    break;
    
  case OPR_GT:
    oper = OPR_LT;
    break;

  default:
    FmtAssert(0, ("Unexpected operator."));
  }
  
  return oper;
}



// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
