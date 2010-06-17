
// simd_ti.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD target info interface routines                                       *
 *                                                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_ti.cxx $

#include "intrn_info.h"
#include "opt_du.h"
#include "simd.h"
#include "simd_imem.h"
#include "simd_ti.h"
#include "simd_loop_at.h"


extern DU_MANAGER *Du_Mgr;          	// PU DU manager


SIMD_TI::SIMD_TI (MEM_POOL *pool) : 
    SIMD_INFO(pool), _default_vl(0)
{
    if (simd_debug) {
	fprintf(TFile,"Initializing SIMD_TI...\n");
    }
    
    _vector_lengths = CXX_NEW(TYPE_BV_MAP(127, Pool()), Pool());
    _type_info_map = CXX_NEW(TYPE_INFO_MAP(127, Pool()), Pool());
    _simd_type_info_map = CXX_NEW(SIMD_TYPE_INFO_MAP(127, Pool()), Pool());

    // Initialize the TARG_SIMD_INFO data.
    Init_Libti_Types();
    Init_Xtbool_Types();
    Init_Packed_Types();    
}


bool
SIMD_TI::Add_Targ_Simd_Info (TARG_SIMD_INFO *tsi)
{
    
    if (tsi->Vector_Length()< 2 ||
	tsi->Base_Type() == MTYPE_UNKNOWN ||
	tsi->Simd_Reg_Type() == MTYPE_UNKNOWN ||
	tsi->Simd_Mem_Type() == MTYPE_UNKNOWN) {
	DevWarn("Bad TIE SIMD INFO structure ignored (%s x%d)",
		MTYPE_name(tsi->Base_Type()), tsi->Vector_Length());
	
	if (simd_debug) {
	    fprintf(TFile,"Warning: bad TIE SIMD INFO structure (ignored):\n");
	    tsi->Print(TFile);
	}

	return false;
    }
    
    BIT_VECTOR *vls = Vector_Lengths(tsi->Base_Type());
    if (vls == NULL) {
	vls = CXX_NEW(BIT_VECTOR(MAX_VECTOR_LENGTH + 1, Pool()), Pool());
	Vector_Lengths()->Enter(tsi->Base_Type(), vls);
    }
    
    FmtAssert(!vls->Test(tsi->Vector_Length()), 
	      ("Expected unique (VL,BASE_TYPE) pair."));
    
    vls->Set(tsi->Vector_Length());
    Type_Info_Map()->Enter(TARG_SIMD_INFO_KEY(tsi->Vector_Length(),
					      tsi->Base_Type()), tsi);
    
    Simd_Type_Info_Map()->Enter_If_Unique(tsi->Simd_Reg_Type(),tsi);
    Simd_Type_Info_Map()->Enter_If_Unique(tsi->Simd_Mem_Type(),tsi);
    
    if (simd_debug) {
	tsi->Print(TFile);
    }

    return true;
}


void
SIMD_TI::Init_Libti_Types (void)
{
    INT sel_bits = 0;
    if (Simd_Target == SIMD_TARG_VECTRA_II) {
	INT vsel_max_way = TI_ISA_Vsel_Max_Way();
	if (vsel_max_way > 1) {
	    sel_bits = LNO_Min_Pow_Of_Two_Exp(vsel_max_way);
	    _type1simd = vsel_max_way >> 1;
	}
	if (simd_debug) {
	    fprintf(TFile, ("Vsel way is %d, # of bits for each way is %d\n"), 
		    vsel_max_way, sel_bits);
	}
    }
    
    // Initialize all ISA_SIMD_INFO based maps.
    ISA_SIMD_TYPE_INFO isi;
    FOR_ALL_ISA_SIMD_TYPE_INFO(isi) {
	INT vl = TI_ISA_Simd_Type_Vl(isi);
	TYPE_ID base_type = TI_ISA_Simd_Base_Mtype(isi);
	TYPE_ID reg_type = TI_ISA_Simd_Vector_Register_Mtype(isi);
	TYPE_ID mem_type = TI_ISA_Simd_Vector_Memory_Mtype(isi);
	TYPE_ID align_type = TI_ISA_Alignment_Mtype(mem_type);
	
	INT elems_per_way = 1;
	if (vl > 0 && Simd_Target == SIMD_TARG_VECTRA_II)
	    elems_per_way = _type1simd / vl;
	
	INT type_sel_bits = (sel_bits == 0) ? LNO_Min_Pow_Of_Two_Exp(2 * vl) : sel_bits;
	SIMD_SELECT *simd_select = CXX_NEW(SIMD_SELECT(vl, type_sel_bits, elems_per_way),
					   Pool());
	
	TARG_SIMD_INFO *tsi = CXX_NEW(TARG_SIMD_INFO(vl, base_type,
						     reg_type, mem_type, align_type,
						     simd_select),
				      Pool());
	
	if (!Add_Targ_Simd_Info(tsi)) {
	    continue;
	}
	
	TYPE_ID vsel_type = TI_ISA_Vsel_Mtype(tsi->Simd_Reg_Type());        
	if (vsel_type != MTYPE_UNKNOWN) {

          if (!Type_Conversion_Possible(MTYPE_I4, vsel_type) &&
              !Type_Conversion_Possible(MTYPE_U4, vsel_type)) {
            /* We can't use a vector select type if we can't convert ints
               into it. A vector select type needs the proper int/unit xtox
               protos. */
            DevWarn("No type conversion from int/uint to vector select type '%s'",
                    MTYPE_name(vsel_type));
            if (simd_debug) {
              fprintf(TFile,
                      "Warning: no type conversion from int/uint to vector select type '%s'\n",
                      MTYPE_name(vsel_type));
            }
          } else  if (_vsel_type == MTYPE_UNKNOWN) {
            _vsel_type = vsel_type;
            if (simd_debug) {
              fprintf(TFile,"Add VSEL type: %s\n", MTYPE_name(vsel_type));
            }
          } else if (_vsel_type != vsel_type) {
            DevWarn("Multiple VSEL types ignored (%s,%s)",
                    MTYPE_name(vsel_type), MTYPE_name(_vsel_type));
            if (simd_debug) {
              fprintf(TFile,
                      "Warning: multiple VSEL types (ignored): (%s,%s)\n",
                      MTYPE_name(vsel_type), MTYPE_name(_vsel_type));
            }
          }
        }
    }
}


void
SIMD_TI::Init_Xtbool_Types (void)
{
    if (!Targ_Simd_Info(MTYPE_XTBOOL, 2))
	Add_Targ_Simd_Info(CXX_NEW(TARG_SIMD_INFO(2, MTYPE_XTBOOL,
						  MTYPE_XTBOOL2, MTYPE_XTBOOL2,
						  MTYPE_UNKNOWN, NULL), Pool()));

    if (!Targ_Simd_Info(MTYPE_XTBOOL, 4))
	Add_Targ_Simd_Info(CXX_NEW(TARG_SIMD_INFO(4, MTYPE_XTBOOL,
						  MTYPE_XTBOOL4, MTYPE_XTBOOL4,
						  MTYPE_UNKNOWN, NULL), Pool()));
    if (!Targ_Simd_Info(MTYPE_XTBOOL, 8))
	Add_Targ_Simd_Info(CXX_NEW(TARG_SIMD_INFO(8, MTYPE_XTBOOL,
						  MTYPE_XTBOOL8, MTYPE_XTBOOL8,
						  MTYPE_UNKNOWN, NULL), Pool()));
    
    if (!Targ_Simd_Info(MTYPE_XTBOOL, 16))
	Add_Targ_Simd_Info(CXX_NEW(TARG_SIMD_INFO(16, MTYPE_XTBOOL,
						  MTYPE_XTBOOL16, MTYPE_XTBOOL16,
						  MTYPE_UNKNOWN, NULL), Pool()));
}


void
SIMD_TI::Init_Packed_Types (void)
{
    // Enter all TIE packed types that can be vectorized.
    for (TYPE_ID vector_ptype = MTYPE_FIRST; vector_ptype <= Mtype_Last; vector_ptype++)
    {
	if (!MTYPE_is_tie_packed(vector_ptype))
	    continue;
	
	INT member_types = tie_info->num_scalar_mtypes(vector_ptype);
	INT vl = 0;
	for (INT member_type_idx = 0; member_type_idx < member_types; member_type_idx++) {
	    TYPE_ID member_type = tie_info->get_scalar_mtype(vector_ptype, member_type_idx);
	    TARG_SIMD_INFO *tsi = Targ_Simd_Info_For_Simd_Type(member_type);
	    if (tsi == NULL)
		continue;
	    
	    INT tvl = tsi->Vector_Length();
	    Is_True(tvl > 1, ("Expected vector length > 1 (%d)", tvl));
	    Is_True(vl == 0 || vl == tvl,
		    ("Expected vector length %d, not %d, for packed type member %s",
		     vl, tvl, MTYPE_name(member_type)));
	    vl = tvl;
	}
	
	// Packed type is not a vector type.
	if (vl == 0)
	    continue;

	TYPE_ID scalar_ptype = MTYPE_FIRST;
	bool scalar_found = false;
	
	// Find the scalar packed type corresponding to the vector packed type
	for (; scalar_ptype <= Mtype_Last; scalar_ptype++)
	{
	    if (!MTYPE_is_tie_packed(scalar_ptype))
		continue;
	    
	    INT member_types = tie_info->num_scalar_mtypes(scalar_ptype);
	    if (member_types != tie_info->num_scalar_mtypes(vector_ptype))
		continue;
	    
	    bool same = true;
	    for (INT member_type_idx = 0; member_type_idx < member_types; member_type_idx++) {
		TYPE_ID smember_type = tie_info->get_scalar_mtype(scalar_ptype, member_type_idx);
		TYPE_ID vmember_type = tie_info->get_scalar_mtype(vector_ptype, member_type_idx);
		if (Simd_Reg_Type_Of_Scalar(smember_type, vl) != vmember_type) {
		    same = false;
		    break;
		}
	    }
	    
	    if (same) {
		scalar_found = true;
		break;
	    }
	}
	
	if (!scalar_found) {
	    if (simd_debug) {
		fprintf(TFile, "Can't find scalar packed type for %s\n", 
			MTYPE_name(vector_ptype));
	    }
	    continue;
	}
	
	// Record the scalar to vector type mapping.
	TARG_SIMD_INFO *tsi = CXX_NEW(TARG_SIMD_INFO(vl, scalar_ptype,
						     vector_ptype, vector_ptype,
						     /* alignment type */ MTYPE_UNKNOWN,
						     /* SIMD_SELECT */ NULL),
				      Pool());

	Add_Targ_Simd_Info(tsi);
    }
}


// returns a bit vector 'bv'
// bv->Test(vl) is set iff there is a SIMD info for 'base_type' and 'vl' available
BIT_VECTOR *
SIMD_TI::Vector_Lengths(TYPE_ID base_type) {
  return Vector_Lengths()->Find(base_type);
}

// returns the TARG_SIMD_INFO structure for the (vl,base_type) pair
TARG_SIMD_INFO *
SIMD_TI::Targ_Simd_Info (TYPE_ID base_type, INT vl) {
    if (vl == 0) {
	vl = Default_Vl();
    }
    return Type_Info_Map()->Find(TARG_SIMD_INFO_KEY(vl, base_type));
}

// returns the TARG_SIMD_INFO structure that contains 'type'
TARG_SIMD_INFO *
SIMD_TI::Targ_Simd_Info_For_Simd_Type (TYPE_ID type) {
  return Simd_Type_Info_Map()->Find(type);
}

TYPE_ID
SIMD_TI::Simd_Mem_Type_Of_Scalar (TYPE_ID base_type, INT vl) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info(base_type,vl);
  if (tsi==NULL) {
    return MTYPE_UNKNOWN;
  }
  return tsi->Simd_Mem_Type();
}

TYPE_ID
SIMD_TI::Simd_Reg_Type_Of_Scalar (TYPE_ID base_type, INT vl) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info(base_type,vl);
  if (tsi==NULL) {
    return MTYPE_UNKNOWN;
  }
  return tsi->Simd_Reg_Type();
}

TYPE_ID
SIMD_TI::Alignment_Type_Of_Scalar (TYPE_ID base_type, INT vl) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info(base_type, vl);
  if (tsi == NULL) {
    return MTYPE_UNKNOWN;
  }
  return tsi->Alignment_Type();
}

SIMD_SELECT *
SIMD_TI::Simd_Select_Of_Scalar (TYPE_ID base_type, INT vl)
{
    TARG_SIMD_INFO *tsi = Targ_Simd_Info(base_type, vl);
    if (tsi == NULL) {
	return NULL;
    }
    return tsi->Simd_Select();
}


INT
SIMD_TI::Guard_Bits (TYPE_ID base_type, INT vl) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info(base_type,vl);
  Is_True(tsi!=NULL,("Expected to find (base_type,vl) pair."));
  return tsi->Guard_Bits();
}
  

const char *
SIMD_TI::Tie_Operator_Name(OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
  return TI_ISA_Operator_Intrinsic(oper,rtype,desc);
}

const char *
SIMD_TI::Tie_Operator_Name(OPERATOR oper,
			   unsigned int num_operands,
			   TI_DIR *operand_dirs,
			   TYPE_ID *operand_types,
			   INT64 *immediates)
{
    return TI_ISA_Operator_Intrinsic_Exp(oper, num_operands, operand_dirs,
					 operand_types, immediates);
}

TIE_MACRO_ID
SIMD_TI::Tie_Operator_Macro(OPERATOR oper, 
			    unsigned int num_operands,
			    TI_DIR *operand_dirs,
			    TYPE_ID *operand_types,
			    INT64 *immediates) 
{
  const char *name = Tie_Operator_Name(oper,num_operands, operand_dirs,
				       operand_types, immediates);
  if (name==NULL) {
    return TIE_INVALID_ID;
  }
  return tie_info->tie_macro_id(name);
}

TIE_MACRO_ID
SIMD_TI::Tie_Operator_Macro(OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
  const char *name = Tie_Operator_Name(oper,rtype,desc);
  if (name==NULL) {
    return TIE_INVALID_ID;
  }
  return tie_info->tie_macro_id(name);
}


INTRINSIC
SIMD_TI::Tie_Operator_Intrinsic(OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
  TIE_MACRO_ID macro_id = Tie_Operator_Macro(oper,rtype,desc);
  if (macro_id==TIE_INVALID_ID) {
    return INTRINSIC_INVALID;
  }
  return Tie_Macro_Id_To_Intrinsic(macro_id);
}


INTRINSIC
SIMD_TI::Tie_Operator_Intrinsic (OPERATOR oper, 
				 unsigned int num_operands,
				 TI_DIR *operand_dirs,
				 TYPE_ID *operand_types,
				 INT64 *immediates) {
    TIE_MACRO_ID macro_id = Tie_Operator_Macro(oper, num_operands,
					       operand_dirs, operand_types, immediates);
    if (macro_id == TIE_INVALID_ID)
	return INTRINSIC_INVALID;

    return Tie_Macro_Id_To_Intrinsic(macro_id);
}

const char *
SIMD_TI::Tie_TiOperator_Name (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
    return TI_ISA_TiOperator_Intrinsic (oper, rtype, desc);
}


TIE_MACRO_ID
SIMD_TI::Tie_TiOperator_Macro (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
  const char *name = Tie_TiOperator_Name(oper, rtype, desc);
  if (name == NULL) {
    return TIE_INVALID_ID;
  }
  return tie_info->tie_macro_id(name);
}


INTRINSIC
SIMD_TI::Tie_TiOperator_Intrinsic (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc) {
  TIE_MACRO_ID macro_id = Tie_TiOperator_Macro(oper,rtype, desc);
  if (macro_id == TIE_INVALID_ID) {
    return INTRINSIC_INVALID;
  }
  return Tie_Macro_Id_To_Intrinsic(macro_id);
}


const char *
SIMD_TI::Tie_TiOperator_Name_Exp (TI_OPERATOR oper,
				  unsigned int num_operands,
				  TI_DIR *operand_dirs,
				  TYPE_ID *operand_types,
				  INT64 *immediates) {
  return TI_ISA_TiOperator_Intrinsic_Exp(oper,num_operands,operand_dirs,
					 operand_types,immediates);
}

TIE_MACRO_ID
SIMD_TI::Tie_TiOperator_Macro_Exp (TI_OPERATOR oper,
				   unsigned int num_operands,
				   TI_DIR *operand_dirs,
				   TYPE_ID *operand_types,
				   INT64 *immediates) {
  const char *name = Tie_TiOperator_Name_Exp(oper,num_operands,operand_dirs,
					     operand_types,immediates);
  if (name==NULL) {
    return TIE_INVALID_ID;
  }
  return tie_info->tie_macro_id(name);
}

INTRINSIC
SIMD_TI::Tie_TiOperator_Intrinsic_Exp (TI_OPERATOR oper,
				       unsigned int num_operands,
				       TI_DIR *operand_dirs,
				       TYPE_ID *operand_types,
				       INT64 *immediates) {
  TIE_MACRO_ID macro_id = Tie_TiOperator_Macro_Exp(oper,num_operands,operand_dirs,
						   operand_types,immediates);
  if (macro_id==TIE_INVALID_ID) {
    return INTRINSIC_INVALID;
  }
  return Tie_Macro_Id_To_Intrinsic(macro_id);
}

const char *
SIMD_TI::Tie_NamedOp_Name_Exp (const char *oper,
			       unsigned int num_operands,
			       TI_DIR *operand_dirs,
			       TYPE_ID *operand_types,
			       INT64 *immediates) {
  return TI_ISA_Named_Intrinsic_Exp(oper,num_operands,operand_dirs,
				    operand_types,immediates);
}

TIE_MACRO_ID
SIMD_TI::Tie_NamedOp_Macro_Exp (const char *oper,
				unsigned int num_operands,
				TI_DIR *operand_dirs,
				TYPE_ID *operand_types,
				INT64 *immediates) {
  const char *name = Tie_NamedOp_Name_Exp(oper,num_operands,operand_dirs,
					  operand_types,immediates);
  if (name==NULL) {
    return TIE_INVALID_ID;
  }
  return tie_info->tie_macro_id(name);
}

INTRINSIC
SIMD_TI::Tie_NamedOp_Intrinsic_Exp (const char *oper,
				    unsigned int num_operands,
				    TI_DIR *operand_dirs,
				    TYPE_ID *operand_types,
				    INT64 *immediates) {
  TIE_MACRO_ID macro_id = Tie_NamedOp_Macro_Exp(oper,num_operands,operand_dirs,
						operand_types,immediates);
  if (macro_id==TIE_INVALID_ID) {
    return INTRINSIC_INVALID;
  }
  return Tie_Macro_Id_To_Intrinsic(macro_id);
}

INTRINSIC
SIMD_TI::Tie_Macro_To_Intrinsic(TIE_MACRO_p macro) {
  if (macro==NULL) {
    return INTRINSIC_INVALID;
  }
  TIE_MACRO_ID macro_id = macro->id();
  return Tie_Macro_Id_To_Intrinsic(macro_id);
}

/* Map from basic type to SIMD register type */
TYPE_ID
SIMD_TI::Get_SIMD_Reg_Type_Scalar(TYPE_ID scalar_type) {
  return Simd_Reg_Type_Of_Scalar(scalar_type);
}
  
/* Map from basic type to SIMD memory type */
TYPE_ID
SIMD_TI::Get_SIMD_Mem_Type_Scalar(TYPE_ID scalar_type) {
  return Simd_Mem_Type_Of_Scalar(scalar_type);
}

TYPE_ID
SIMD_TI::Get_Alignment_Type_Scalar(TYPE_ID scalar_type) {
  return Alignment_Type_Of_Scalar(scalar_type);
}


/* number of elements in a SIMD register */
INT
SIMD_TI::Get_SIMD_Width_Scalar (TYPE_ID scalar_type) {
    TARG_SIMD_INFO *tsi = Targ_Simd_Info(scalar_type);
    if (tsi == NULL) {
	return 0;
    }
    return tsi->Vector_Length();
}


INT
SIMD_TI::Get_SIMD_Width_SIMD(TYPE_ID simd_type) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info_For_Simd_Type(simd_type);
  if (tsi==NULL) {
    return 0;
  }
  return tsi->Vector_Length();
}


SIMD_SELECT *
SIMD_TI::Get_SIMD_Select_Scalar (TYPE_ID scalar_type)
{
    return Simd_Select_Of_Scalar(scalar_type);
}


SIMD_SELECT *
SIMD_TI::Get_SIMD_Select_SIMD (TYPE_ID simd_type)
{
    TARG_SIMD_INFO *tsi = Targ_Simd_Info_For_Simd_Type(simd_type);
    if (tsi == NULL) {
	return 0;
    }
    return tsi->Simd_Select();
}


/* number of bytes in a SIMD type, as respect to memory */
INT
SIMD_TI::Get_SIMD_Mem_Bytes_Scalar(TYPE_ID scalar_type) {
  TARG_SIMD_INFO *tsi = Targ_Simd_Info(scalar_type);
  if (tsi==NULL) {
    return 0;
  }
  return tsi->Simd_Mem_Byte_Size();
}
  
TYPE_ID
SIMD_TI::Get_SIMD_Xtbool(INT vl)
{
    TYPE_ID rtype = MTYPE_UNKNOWN;

    /* get the correct xtbool vector type */
    TARG_SIMD_INFO *tsi = Simd_Ti->Targ_Simd_Info(MTYPE_XTBOOL, vl);
    if (tsi) {
	rtype = tsi->Simd_Reg_Type();
    }
    return rtype;
}

/* find a SIMD function in 'type' for 'op' corresponding the 'lohi' */
TIE_MACRO_ID
SIMD_TI::Search_Vector_Macro(OPERATOR op,
			     TYPE_ID dtype, TYPE_ID rtype,
			     INT lohi, bool update) {
    
    if (simd_debug) {
	fprintf(TFile,"Search Vector Macro: %s %s %s %d %s\n",
		OPERATOR_name(op),MTYPE_name(dtype),MTYPE_name(rtype),lohi,update?"UPDATE":"");
    }
    
    TIE_MACRO_p  tie_macro = NULL;
    TIE_MACRO_ID macro_id = TIE_INVALID_ID;
    
    if (rtype==MTYPE_UNKNOWN) {
	rtype = dtype;
    }
    
    switch (op) {
    case OPR_ADD:
    case OPR_SUB:
    case OPR_ABS:
    case OPR_NEG:
      
    case OPR_RECIP:
    case OPR_RSQRT:
	
    case OPR_BNOT:
    case OPR_BXOR:
    case OPR_BAND:
    case OPR_BIOR:

    case OPR_MIN:
    case OPR_MAX:
	
    case OPR_MPY:
    case OPR_MADD:
    case OPR_MSUB:

    case OPR_GT:
    case OPR_GE:
    case OPR_EQ:
    case OPR_NE:
    case OPR_LE:
    case OPR_LT:
	macro_id = Tie_Operator_Macro(op, rtype, dtype);
        break;
	
    case OPR_ILOAD: {
	switch (lohi) {
	case 0: /* vector */
	    if (update) {
		macro_id = Tie_TiOperator_Macro(TI_OPR_LV_U, rtype, dtype);
	    } else {
		tie_macro = tie_info->mtype_loadi_macro(dtype);
		if (tie_macro) {
		    macro_id = tie_macro->id();
		}
	    }
	    break;

	case 1: /* scalar */
	    if (update) {
		macro_id = Tie_TiOperator_Macro(TI_OPR_LS_U, rtype, dtype);
	    } else {
		tie_macro = tie_info->mtype_mtor_macro(dtype, rtype);
		if (tie_macro) {
		    macro_id = tie_macro->id();
		}
	    }
	    break;

       case 3:
	   /* LS_X */
	   macro_id = Tie_TiOperator_Macro(
	       (update) ? TI_OPR_LSX_U : TI_OPR_LSX, rtype, dtype);	
	   break;

       case 4:
	   /* LV_X */
	   macro_id = Tie_TiOperator_Macro(
	       (update) ? TI_OPR_LVX_U : TI_OPR_LVX, rtype, dtype);	
	   break;
	   
        default:
	    Is_True(lohi == 2 , ("Expecting alignment load"));
	    INTRINSIC intrn = Load_Align(update ? 
					 LOADA_IMM_UPDATE : LOADA_IMM,
					 dtype, rtype);
	    if (intrn == INTRINSIC_INVALID)
		return TIE_INVALID_ID;
	    
	    macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
	}
	break;
    }
    
    case OPR_ISTORE: {
	switch (lohi) {
	case 0: /* vector */
	    if (update) {
		macro_id = Tie_TiOperator_Macro(TI_OPR_SV_U, rtype, dtype);
	    } else {
		tie_macro = tie_info->mtype_storei_macro(dtype);
		if (tie_macro) {
		    macro_id = tie_macro->id();
		}
	    }
	    break;
        case 1: /* scalar */
	    if (update) {
		macro_id = Tie_TiOperator_Macro(TI_OPR_SS_U, rtype, dtype);
	    } else {
		tie_macro = tie_info->mtype_rtom_macro(rtype, dtype);
		if (tie_macro) {
		    macro_id = tie_macro->id();
		}
	    }

        case 4: /* SV_X */
	   /* SV_X */
	   macro_id = Tie_TiOperator_Macro(
	       (update) ? TI_OPR_SVX_U : TI_OPR_SVX, rtype, dtype);	
	   break;

	default:
	    Is_True(lohi == 2 , ("Expecting alignment store"));
	    INTRINSIC intrn = Store_Align(update ? STOREA_IMM_UPDATE : STOREA_IMM,
					  dtype, rtype);
	    if (intrn == INTRINSIC_INVALID)
		return TIE_INVALID_ID;
		
	    macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
	}
	break;
    }
    
    default:
	DevWarn(("Unhandled operator"));
	return TIE_INVALID_ID;
	
    }
    return macro_id;
}

TIE_MACRO_ID
SIMD_TI::Get_Tie_Set_Vsar()
{
  if (simd_debug) {
    fprintf(TFile,"request for WVSAR \n");
  }
  
  return Tie_TiOperator_Macro(TI_OPR_WVSAR, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
}


TIE_MACRO_ID
SIMD_TI::Get_Tie_Set_Round()
{
  if (simd_debug) {
    fprintf(TFile,"request for WROUND \n");
  }
  
  return Tie_TiOperator_Macro(TI_OPR_WROUND, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
}


INTRINSIC
SIMD_TI::Tie_Sel_Intrinsic_Imm (TYPE_ID vec_type, INT64 sel_imm) {
    if (simd_debug) {
	fprintf(TFile,"request for SEL (%s, %" LLX_FMT ")\n", MTYPE_name(vec_type), sel_imm);
    }
    
    TI_DIR dirs[4];
    TYPE_ID types[4];
    INT64 imm[4];
    
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = dirs[3] = TI_DIR_IN;
    
    types[0] = types[1] = types[2] = vec_type;
    types[3] = MTYPE_UNKNOWN;
    
    imm[0] = imm[1] = imm[2] = 0;
    imm[3] = sel_imm;
    
    INTRINSIC intrn = Tie_TiOperator_Intrinsic_Exp(TI_OPR_VSEL, 4, dirs, types, imm);
    return intrn;
}


INTRINSIC
SIMD_TI::Tie_Sel_Intrinsic (TYPE_ID vec_type) {
    if (simd_debug) {
	fprintf(TFile,"request for SEL (%s)\n", MTYPE_name(vec_type));
    }

    if (Vsel_type() == MTYPE_UNKNOWN)
	return INTRINSIC_INVALID;
    
    TI_DIR dirs[4];
    TYPE_ID types[4];
    
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = dirs[3] = TI_DIR_IN;
    
    types[0] = types[1] = types[2] = vec_type;
    types[3] = Vsel_type();
    
    INTRINSIC intrn = Tie_TiOperator_Intrinsic_Exp(TI_OPR_VSEL, 4, dirs, types,
						   /* immediates */ NULL);
    return intrn;
}


INTRINSIC
SIMD_TI::Tie_Dsel_Intrinsic_Imm (TYPE_ID vec_type, INT64 dsel_imm)
{
    if (simd_debug) {
	fprintf(TFile,"request for DSEL (%s, %" LLX_FMT ")\n", MTYPE_name(vec_type), dsel_imm);
    }
    
    TI_DIR dirs[5];
    TYPE_ID types[5];
    INT64 imm[5];
    
    dirs[0] = dirs[1] = TI_DIR_OUT;
    dirs[2] = dirs[3] = dirs[4] = TI_DIR_IN;
    
    types[0] = types[1] = types[2] = types[3] = vec_type;
    types[4] = MTYPE_UNKNOWN;
  
    imm[0] = imm[1] = imm[2] = imm[3] = 0;
    imm[4] = dsel_imm;
    
    INTRINSIC intrn = Tie_TiOperator_Intrinsic_Exp(TI_OPR_VSEL, 5, dirs, types, imm);
    return intrn;
}


INTRINSIC
SIMD_TI::Tie_Dsel_Intrinsic (TYPE_ID vec_type) {
    if (simd_debug) {
	fprintf(TFile,"request for DSEL (%s)\n", MTYPE_name(vec_type));
    }
    
    if (Vsel_type() == MTYPE_UNKNOWN) {
	return INTRINSIC_INVALID;
    }
    
    TI_DIR dirs[5];
    TYPE_ID types[5];
    
    dirs[0] = dirs[1] = TI_DIR_OUT;
    dirs[2] = dirs[3] = dirs[4] = TI_DIR_IN;
    
    types[0] = types[1] = types[2] = types[3] = vec_type;
    types[4] = Vsel_type();
    
    INTRINSIC intrn = Tie_TiOperator_Intrinsic_Exp(TI_OPR_VSEL, 5, dirs, types,
						   /* immediates */ NULL);
    return intrn;
}


INTRINSIC
SIMD_TI::Tie_Shift_Intrinsic_Imm (OPERATOR oper, TYPE_ID vec_type, INT shift_amount) {
    Is_True(oper == OPR_ASHR || oper == OPR_LSHR || oper == OPR_SHL,
	    ("Unexpected operator %s", OPERATOR_name(oper)));
    
    if (simd_debug) {
	fprintf(TFile,"request for SHIFT_I (%s, %s, %d)\n",
		OPERATOR_name(oper), MTYPE_name(vec_type), shift_amount);
    }
    
    TI_DIR dirs[3];
    TYPE_ID types[3];
    INT64 imm[3];
    
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = types[1] = vec_type;
    types[2] = MTYPE_UNKNOWN;
    
    imm[0] = imm[1] = 0;
    imm[2] = shift_amount;
    
    INTRINSIC intrn = Tie_Operator_Intrinsic(oper, 3, dirs, types, imm);
    return intrn;
}


INTRINSIC
SIMD_TI::Tie_Shift_Intrinsic (OPERATOR oper, TYPE_ID vec_type) {
    Is_True(oper == OPR_ASHR || oper == OPR_LSHR || oper == OPR_SHL,
	    ("Unexpected operator %s", OPERATOR_name(oper)));
    
    if (simd_debug) {
	fprintf(TFile,"request for SHIFT (%s, %s)\n",
		OPERATOR_name(oper), MTYPE_name(vec_type));
    }
    
    TI_DIR dirs[3];
    TYPE_ID types[3];
    
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = types[1] = vec_type;
    types[2] = MTYPE_I4;
    
    INTRINSIC intrn = Tie_Operator_Intrinsic(oper, 3, dirs, types, NULL);
    return intrn;
}


bool
SIMD_TI::Tie_Sel_Possible (TYPE_ID vec_type, INT64 sel_imm)
{
    return (Tie_Sel_Intrinsic_Imm(vec_type, sel_imm) != INTRINSIC_INVALID ||
	    Tie_Sel_Intrinsic(vec_type) != INTRINSIC_INVALID);
}


bool
SIMD_TI::Tie_Dsel_Possible (TYPE_ID vec_type, INT64 dsel_imm)
{
    if (Tie_Dsel_Intrinsic_Imm(vec_type, dsel_imm) != INTRINSIC_INVALID ||
	Tie_Dsel_Intrinsic(vec_type) != INTRINSIC_INVALID) {
	return true;
    }
    
    SIMD_SELECT *simd_sel = Get_SIMD_Select_SIMD(vec_type);
    Is_True(simd_sel != NULL,
	    ("NULL SIMD select object for %s", MTYPE_name(vec_type)));
    
    INT64 sel_0, sel_1;
    simd_sel->Dsel_To_Sel(dsel_imm, sel_1, sel_0);
    return (Tie_Sel_Possible(vec_type, sel_0) &&
	    Tie_Sel_Possible(vec_type, sel_1));
}


bool
SIMD_TI::Load_Scalar_IU_Possible(TYPE_ID mem_type, TYPE_ID reg_type)
{
    return 
	(Search_Vector_Macro(OPR_ILOAD, mem_type, reg_type, 1, true) != 
	 TIE_INVALID_ID);
}

bool
SIMD_TI::Load_Scalar_XU_Possible(TYPE_ID mem_type, TYPE_ID reg_type)
{
    return (
	Search_Vector_Macro(OPR_ILOAD, mem_type, reg_type, 3, true) != 
	TIE_INVALID_ID &&
	Search_Vector_Macro(OPR_ILOAD, mem_type, reg_type, 3, false)!=
	TIE_INVALID_ID
	);
}


void
SIMD_TI::Shift_Into_Reg(SIMD_PREG *new_input, SIMD_PREG *res,
			WN *block, WN *stmt, INT64 line_num,
			INT shift_amount, SIMD_PREG *target)
{
    TYPE_ID mtype  = res->Type();
    SIMD_SELECT *simd_sel = Get_SIMD_Select_SIMD(mtype);
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    
    INT64 sel = simd_sel->Sel_4321(shift_amount);
    
    dynamic_cast<SIMD_LOOP_AT*>(Cur_Simd_Loop)->
	Generate_Sel_Imm(stmt, (target) ? target : res, new_input, res, 
			 sel, block);
}

INTRINSIC
SIMD_TI::CMov(TYPE_ID rtype, TYPE_ID bool_type)
{
    if (simd_debug) {
	fprintf(TFile,"request for MOVT (%s, %s)\n", MTYPE_name(rtype),
		MTYPE_name(bool_type));
    }
    
    TI_DIR dirs[3];
    TYPE_ID types[3];
    
    dirs[0] = TI_DIR_INOUT;
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = types[1] = rtype;
    types[2] = bool_type;
    
    INTRINSIC intrn = Tie_TiOperator_Intrinsic_Exp(TI_OPR_MOVT, 3, dirs, types,
						   /* immediates */ NULL);
    return intrn;
}

INTRINSIC
SIMD_TI::Load_Align (LOADA_KIND kind, TYPE_ID mem_type, TYPE_ID reg_type) {
  Is_True(mem_type != MTYPE_UNKNOWN && reg_type != MTYPE_UNKNOWN, 
	  ("Unknown memory or register type"));
  
  TI_OPERATOR ti_oper;
  const char *kind_str = NULL;
  
  switch (kind) {
  case LOADA_PRIME:
    ti_oper = TI_OPR_LOADAP;
    kind_str = "LOADA_PRIME";
    break;
  case LOADA_IMM:
    ti_oper = TI_OPR_LOADA;
    kind_str = "LOADA_IMM";
    break;
  case LOADA_IMM_UPDATE:
    ti_oper = TI_OPR_LOADAU;
    kind_str = "LOADA_IMM_UPDATE";
    break;
  default:
    FmtAssert(0, ("Unsupported LOADA_KIND %d", (INT)kind));
  }
  
  if (simd_debug) {
    fprintf(TFile, "request for LOADA (kind %s, mem_type %s, reg_type %s)\n",
	    kind_str, MTYPE_name(mem_type), MTYPE_name(reg_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(ti_oper, reg_type, mem_type);
  return intrn;
}


INTRINSIC
SIMD_TI::Store_Align (STOREA_KIND kind, TYPE_ID mem_type, TYPE_ID reg_type) {
  Is_True(mem_type != MTYPE_UNKNOWN && reg_type != MTYPE_UNKNOWN, 
	  ("Unknown memory or register type"));
  
  TI_OPERATOR ti_oper;
  const char *kind_str = NULL;
  
  switch (kind) {
  case STOREA_PRIME:
    ti_oper = TI_OPR_STOREAP;
    kind_str = "STOREA_PRIME";
    break;
  case STOREA_IMM:
    ti_oper = TI_OPR_STOREA;
    kind_str = "STOREA_IMM";
    break;
  case STOREA_IMM_UPDATE:
    ti_oper = TI_OPR_STOREAU;
    kind_str = "STOREA_IMM_UPDATE";
    break;
  case STOREA_FLUSH:
    ti_oper = TI_OPR_STOREAF;
    kind_str = "STOREA_IMM_UPDATE";
    break;
  default:
    FmtAssert(0, ("Unsupported STOREA_KIND %d", (INT)kind));
  }
  
  if (simd_debug) {
    fprintf(TFile, "request for STOREA (kind %s, mem_type %s, reg_type %s)\n",
	    kind_str, MTYPE_name(mem_type), MTYPE_name(reg_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(ti_oper, reg_type, mem_type);
  return intrn;
}


INTRINSIC
SIMD_TI::Radd (TYPE_ID res_type, TYPE_ID desc_type)
{
  if (simd_debug) {
    fprintf(TFile, "request for RADD (result %s, vector %s)\n",
	    MTYPE_name(res_type), MTYPE_name(desc_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(TI_OPR_RADD, res_type, desc_type);
  return intrn;
}


INTRINSIC
SIMD_TI::Rmin (TYPE_ID res_type, TYPE_ID desc_type)
{
  if (simd_debug) {
    fprintf(TFile, "request for RMIN (result %s, vector %s)\n",
	    MTYPE_name(res_type), MTYPE_name(desc_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(TI_OPR_RMIN, res_type, desc_type);
  return intrn;
}


INTRINSIC
SIMD_TI::Rmax (TYPE_ID res_type, TYPE_ID desc_type)
{
  if (simd_debug) {
    fprintf(TFile, "request for RMAX (result %s, vector %s)\n",
	    MTYPE_name(res_type), MTYPE_name(desc_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(TI_OPR_RMAX, res_type, desc_type);
  return intrn;
}


INTRINSIC
SIMD_TI::Zero (TYPE_ID reg_type)
{
  if (simd_debug) {
    fprintf(TFile, "request for ZERO (result %s)\n", MTYPE_name(reg_type));
  }
  
  INTRINSIC intrn = Tie_TiOperator_Intrinsic(TI_OPR_ZERO, reg_type, MTYPE_UNKNOWN);
  return intrn;
}


bool
SIMD_TI::Load_Align_Possible (TYPE_ID mem_type, TYPE_ID reg_type) {
  return
    Load_Align(LOADA_PRIME, mem_type, reg_type) != INTRINSIC_INVALID &&
    Load_Align(LOADA_IMM_UPDATE, mem_type, reg_type) != INTRINSIC_INVALID;
}


bool
SIMD_TI::Store_Align_Possible (TYPE_ID mem_type, TYPE_ID reg_type) {
    return
	Store_Align(STOREA_PRIME, mem_type, reg_type) != INTRINSIC_INVALID &&
	Store_Align(STOREA_IMM_UPDATE, mem_type, reg_type) != INTRINSIC_INVALID &&
	Store_Align(STOREA_FLUSH, mem_type, reg_type) != INTRINSIC_INVALID;
}


bool
SIMD_TI::Type_Conversion_Possible (TYPE_ID from_type, TYPE_ID to_type)
{
  if (simd_debug) {
    fprintf(TFile, "Request if type conversion possible from %s to %s.\n",
	    MTYPE_name(from_type), MTYPE_name(to_type));
  }
  
  Is_True(MTYPE_is_tie(from_type) || MTYPE_is_tie(to_type),
	  ("Expected at least one TIE type (%s and %s)",
	   MTYPE_name(from_type), MTYPE_name(to_type)));
  
  bool possible = (tie_info->mtype_rtor_macro(from_type, to_type) ||
		   tie_info->mtype_mtor_macro(from_type, to_type) ||
		   tie_info->mtype_rtom_macro(from_type, to_type));

  if (simd_debug) {
    fprintf(TFile, "Conversion from %s to %s: %spossible.\n",
	    MTYPE_name(from_type), MTYPE_name(to_type),
	    possible ? "" : "im");
  }
	
  return possible;
}


bool
SIMD_TI::Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type) {
  if (red_type != RED_ADD &&
      red_type != RED_MIN &&
      red_type != RED_MAX)
    return false;

  /* TIE doesn't support non-pointer int64/uint64 types so we don't
     support reduction on these types. */
  if (scalar_type == MTYPE_I8 || scalar_type == MTYPE_U8)
    return false;
  
  if (AT_Analysis_Phase())
    return true;
  
  TYPE_ID vec_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
  
  switch (red_type)
  {
  case RED_ADD:
    return (Radd(scalar_type, vec_type) != INTRINSIC_INVALID &&
	    (Zero(vec_type) != INTRINSIC_INVALID ||
	     Search_Vector_Macro(OPR_BXOR, vec_type) != TIE_INVALID_ID ||
	     (!MTYPE_is_float(vec_type) &&
              Search_Vector_Macro(OPR_SUB, vec_type) != TIE_INVALID_ID) ||
	     Type_Conversion_Possible(scalar_type, vec_type)));
    
  case RED_MIN:
    return (Rmin(scalar_type, vec_type) != INTRINSIC_INVALID &&
	    Type_Conversion_Possible(scalar_type, vec_type));

  case RED_MAX:
    return (Rmax(scalar_type, vec_type) != INTRINSIC_INVALID &&
	    Type_Conversion_Possible(scalar_type, vec_type));

  default:
    Is_True(0, ("Bad checks."));
    break;
  }

  return false;
}


WN *
SIMD_TI::Generate_LoadA_Prime (TYPE_ID scalar_type, WN *stmt, WN *addr,
			       SIMD_PREG *addr_reg, SIMD_PREG *target,
			       SIMD_PREG *align) {
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    
    INTRINSIC intrin_id = Load_Align(LOADA_PRIME, mem_type, reg_type);
    Is_True(intrin_id != INTRINSIC_INVALID,
	    ("Can't find priming alignment load for %s\n", MTYPE_name(scalar_type)));
    OPCODE intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    INT bias_offset = MTYPE_alignment(mem_type);
    WN *wn_const = WN_CreateIntconst(OPC_I4INTCONST, bias_offset);
    OPCODE opc = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
    addr = LWN_CreateExp2(opc, addr, wn_const);
    
    WN *stid = addr_reg->Simd_Preg_Stid(addr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
    WN_linenum(stid) = LWN_Get_Linenum(stmt);
    
    WN *apr[2];
    WN *align_ldid = align->Simd_Preg_Ldid();
    WN *addr_ldid = addr_reg->Simd_Preg_Ldid();
    
    apr[0] = LWN_CreateParm(align->Type(), align_ldid,
			    MTYPE_To_TY(align->Type()),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr_reg->Type(), addr_ldid, 
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    
    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 2, apr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, callNode);
    WN_linenum(callNode) = LWN_Get_Linenum(stmt);

    WN *parm_ldid = WN_Ldid(align->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = align->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    Du_Mgr->Add_Def_Use(callNode, align_ldid);
    
    parm_ldid = WN_Ldid(addr_reg->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = addr_reg->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    return callNode;
}


void
SIMD_TI::Generate_StoreA_Prime (IMEM_GROUP *ig, WN *stmt) {
    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    
    WN *prime_pt = stmt;
    WN *prime_blk = LWN_Get_Parent(prime_pt);
    
    SIMD_PREG *align = ig->Store_Align_Reg();
    
    INTRINSIC intrin_id = Store_Align(STOREA_PRIME, mem_type, reg_type);
    Is_True(intrin_id != INTRINSIC_INVALID,
	    ("Can't find priming alignment store for %s\n", MTYPE_name(scalar_type)));
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, align->Type(), MTYPE_V);
    
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 0, NULL);
    WN *align_stid = align->Simd_Preg_Stid(callNode);
    LWN_Insert_Block_Before(prime_blk, prime_pt, align_stid);
    WN_linenum(align_stid) = LWN_Get_Linenum(prime_pt);

    if (Enclosing_Do_Loop(prime_blk) != NULL) {
	// If there's an enclosing loop around the prime point, let the store flush
	// know that it should create a dependence graph vertex.
	ig->Pending_Wn().Push(NULL);
    }
}


void
SIMD_TI::Generate_StoreA_Update (IMEM_GROUP *ig, WN *stmt,
				 SIMD_PREG *val, INT inc) {
    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    Is_True(val->Type() == reg_type,
	    ("Different type of STOREA_IMM_UPDATE (%s and %s)",
	     MTYPE_name(val->Type()), MTYPE_name(reg_type)));
    
    SIMD_PREG *align = ig->Store_Align_Reg();
    SIMD_PREG *addr = ig->Store_Addr_Reg();
    
    INTRINSIC intrin_id   = Find_Function(OPR_ISTORE, mem_type, reg_type, 
					  2 /* SVA */, true /* IU */);
    OPCODE    intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    OPCODE    const_opc = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
    WN       *wn_inc       = WN_CreateIntconst(const_opc, inc);
    
    WN *addr_ldid = addr->Simd_Preg_Ldid();
    WN *apr[4];
    apr[0] = LWN_CreateParm(val->Type(), val->Simd_Preg_Ldid(),
			    MTYPE_To_TY(val->Type()), 
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
    
    WN *parm_ldid = WN_Ldid(align->Type(), -1, Tie_Output_Volatile_Preg,
			    Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    WN *parm_stid = align->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);
    
    parm_ldid = WN_Ldid(addr->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(callNode, parm_ldid);
    parm_stid = addr->Simd_Preg_Stid(parm_ldid);
    Du_Mgr->Add_Def_Use(parm_stid, addr_ldid);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, parm_stid);
    WN_linenum(parm_stid) = LWN_Get_Linenum(stmt);

    /* add 'res' to the V_Stores() for updating dependence later */
    ig->V_Stores()->AddElement(callNode);
}


void
SIMD_TI::Generate_StoreA_Flush (IMEM_GROUP *ig, WN *stmt, bool ins_after) {
    TYPE_ID scalar_type = ig->Scalar_Type();
    TYPE_ID mem_type = Get_SIMD_Mem_Type_Scalar(scalar_type);
    TYPE_ID reg_type = Get_SIMD_Reg_Type_Scalar(scalar_type);
    
    INTRINSIC intrin_id = Store_Align(STOREA_FLUSH, mem_type, reg_type);
    Is_True(intrin_id != INTRINSIC_INVALID,
	    ("Can't find flush alignment store for %s\n", MTYPE_name(scalar_type)));
    OPCODE intrin_call = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    
    WN *prime_pt = stmt;
    WN *prime_blk = LWN_Get_Parent(prime_pt);
    
    SIMD_PREG *align = ig->Store_Align_Reg();
    SIMD_PREG *addr_reg = ig->Store_Addr_Reg();
    
    WN *apr[2];
    WN *align_ldid = align->Simd_Preg_Ldid();
    WN *addr_ldid = addr_reg->Simd_Preg_Ldid();
    
    apr[0] = LWN_CreateParm(align->Type(), align_ldid,
			    MTYPE_To_TY(align->Type()),
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(addr_reg->Type(), addr_ldid, 
			    Make_Pointer_Type(MTYPE_To_TY(mem_type)),
			    WN_PARM_BY_VALUE);
    
    WN *callNode = LWN_Create_Intrinsic(intrin_call, intrin_id, 2, apr);
    
    if (ins_after) {
	if (prime_blk) {
	    LWN_Insert_Block_After(prime_blk, prime_pt, callNode);
	} else {
	    WN_next(callNode) = WN_next(prime_pt);
	    if (WN_next(prime_pt)) {
		WN_prev(WN_next(prime_pt)) = callNode;
	    }
	    WN_next(prime_pt) = callNode;
	    WN_prev(callNode) = prime_pt;
	}
    } else {
	Is_True(prime_blk != NULL, ("Expected non-null parent block"));
	LWN_Insert_Block_Before(prime_blk, prime_pt, callNode);
    }
    WN_linenum(callNode) = LWN_Get_Linenum(prime_pt);
    
    /* add dependence vertex */
    if (ig->Pending_Wn().Elements()) {
	// It should be NULL
	WN *ls = ig->Pending_Wn().Pop();
	Is_True(ig->Pending_Wn().Elements() == 0 && ls == NULL,
		("Unaligned stores are messed up."));
	Array_Dependence_Graph->Add_Vertex(callNode);
    }
}


void
SIMD_TI::Store_Packed_Op_Output (SIMD_PREG *out1, SIMD_PREG *out2, WN *res, 
				 WN *block, WN *stmt, INT64 line_num)
{
    TYPE_ID out_type = WN_rtype(res);
    Is_True(tie_info->num_scalar_mtypes(out_type) == 2, 
	    ("Output type is not compound of two types"));
    SIMD_PREG *outpart = Gen_Symbol(out_type, Simd_Info->Pool());
    
    /* store the output */
    WN *stid = outpart->Simd_Preg_Stid(res);
    LWN_Insert_Block_Before(block, stmt, stid);
    WN_linenum(stid) = line_num;
    
    /* extract the output */
    WN *part_ldid = LWN_CreateOutpart(outpart->Simd_Preg_Ldid(), 1, out1->Type());
    WN *part_stid = out1->Simd_Preg_Stid(part_ldid);
    LWN_Insert_Block_Before(block, stmt, part_stid);
    WN_linenum(part_stid) = line_num;
    
    part_ldid = LWN_CreateOutpart(outpart->Simd_Preg_Ldid(), 2, out2->Type());
    part_stid = out2->Simd_Preg_Stid(part_ldid);
    LWN_Insert_Block_Before(block, stmt, part_stid);
    WN_linenum(part_stid) = line_num;
}


void
SIMD_TI::Store_Packed_Call_Output (SIMD_PREG *out1, SIMD_PREG *out2,
				   WN *res, WN *block, WN *stmt, 
				   INT64 line_num)
{
    LWN_Insert_Block_Before(block, stmt, res);
    WN_linenum(res) = line_num;
    
    /* add a dependence vertex */
    if (Enclosing_Do_Loop(block) != NULL) {
      Array_Dependence_Graph->Add_Vertex(res);
    }
    
    WN *parm_ldid = WN_Ldid(out1->Type(), -1, Tie_Output_Volatile_Preg,
			  Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(res, parm_ldid);
    WN *parm_stid = out1->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(block, stmt, parm_stid);
    WN_linenum(parm_stid) = line_num;
    
    parm_ldid = WN_Ldid(out2->Type(), -2, Tie_Output_Volatile_Preg,
			Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(res, parm_ldid);
    parm_stid = out2->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(block, stmt, parm_stid);
    WN_linenum(parm_stid) = line_num;
}

void
SIMD_TI::Store_Op_Output (SIMD_PREG *out1, WN *res, 
			  WN *block, WN *stmt, INT64 line_num)
{
    /* store the output */
    WN *stid = out1->Simd_Preg_Stid(res);
    LWN_Insert_Block_Before(block, stmt, stid);
    WN_linenum(stid) = line_num;
}


void
SIMD_TI::Store_Call_Output (SIMD_PREG *out1,
			    WN *res, WN *block, WN *stmt, 
			    INT64 line_num)
{
    LWN_Insert_Block_Before(block, stmt, res);
    WN_linenum(res) = line_num;
    
    /* add a dependence vertex */
    if (Enclosing_Do_Loop(block) != NULL) {
      Array_Dependence_Graph->Add_Vertex(res);
    }
    
    WN *parm_ldid = WN_Ldid(out1->Type(), -1, Tie_Output_Volatile_Preg,
			  Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(res, parm_ldid);
    WN *parm_stid = out1->Simd_Preg_Stid(parm_ldid);
    LWN_Insert_Block_Before(block, stmt, parm_stid);
    WN_linenum(parm_stid) = line_num;
}

bool
SIMD_TI::Has_Paired_Mac(OPERATOR op, TYPE_ID out_type, TYPE_ID in_type)
{
    TIE_MACRO_ID macro_id = TIE_INVALID_ID;
    INT vl_r = Get_SIMD_Width_SIMD(out_type);
    INT vl_d = Get_SIMD_Width_SIMD(in_type);
    
    Is_True(op == OPR_MPY || op == OPR_MADD || op == OPR_MSUB,
	    ("SIMD_TI::Has_Paired_Mac: illegal operator"));

    if (vl_d == 2*vl_r) {
	TI_DIR   dirs[4];
	TYPE_ID  types[4];
	INT64    imm[4];
	if (op != OPR_MPY) {
	    dirs[0] = dirs[1] = TI_DIR_INOUT;
	} else {
	    dirs[0] = dirs[1] = TI_DIR_OUT;
	}	
	dirs[2] = dirs[3] = TI_DIR_IN;
	types[0] = types[1] = out_type;
	types[2] = types[3] = in_type;
	imm[0] = imm[1] = imm[2] = imm[3] = 0;
	macro_id = Tie_Operator_Macro(op, 4, dirs,types,imm);
    }
    return (macro_id != TIE_INVALID_ID);
}

INTRINSIC
SIMD_TI::Tie_Mulr_Intrinsic(TYPE_ID out_type, TYPE_ID in_type)
{
    TI_DIR   dirs[4];
    TYPE_ID  types[4];
    INT64    imm[4];
    dirs[0] = dirs[1] = TI_DIR_OUT;
    dirs[2] = dirs[3] = TI_DIR_IN;
    
    types[0] = types[1] = out_type;
    types[2] = types[3] = in_type;
    imm[0]   = imm[1]   = imm[2] = imm[3] = 0;
    INTRINSIC intrin_id = 
	Tie_TiOperator_Intrinsic_Exp(TI_OPR_MULR, 4, dirs,types,imm);
    return intrin_id;
}

INTRINSIC
SIMD_TI::Tie_Mulr_Intrinsic(TYPE_ID out_type, TYPE_ID in_type, bool even)
{
    TI_DIR   dirs[3];
    TYPE_ID  types[3];
    INT64    imm[3];
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = out_type;
    types[1] = types[2] = in_type;
    imm[0]   = imm[1]   = imm[2] = 0;
    INTRINSIC intrin_id = 
	(even) ? 
	Tie_TiOperator_Intrinsic_Exp(TI_OPR_MULR_EVEN, 3, dirs,types,imm) :
	Tie_TiOperator_Intrinsic_Exp(TI_OPR_MULR_ODD, 3, dirs,types,imm);
    return intrin_id;
}

bool
SIMD_TI::Has_Even_Odd_Mac(OPERATOR op, TYPE_ID out_type, TYPE_ID in_type)
{
    return (
	Tie_Mac_Intrinsic(op, out_type, in_type, true) != INTRINSIC_INVALID
	&&
	Tie_Mac_Intrinsic(op, out_type, in_type, false) != INTRINSIC_INVALID);
}

INTRINSIC
SIMD_TI::Tie_Mac_Intrinsic(OPERATOR op, TYPE_ID out_type, 
			   TYPE_ID in_type, bool even)
{
    TI_DIR   dirs[3];
    TYPE_ID  types[3];
    INT64    imm[3];
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = out_type;
    types[1] = types[2] = in_type;
    imm[0]   = imm[1]   = imm[2] = 0;
    TI_OPERATOR ti_opr;
    switch (op) {
	case OPR_MPY:
	    ti_opr = (even) ? TI_OPR_MPY_EVEN : TI_OPR_MPY_ODD;
	    dirs[0] = TI_DIR_OUT;
	    break;
	case OPR_MADD:
	    ti_opr = (even) ? TI_OPR_MADD_EVEN : TI_OPR_MADD_ODD;
	    dirs[0] = TI_DIR_INOUT;
	    break;
	case OPR_MSUB:
	    ti_opr = (even) ? TI_OPR_MSUB_EVEN : TI_OPR_MSUB_ODD;
	    dirs[0] = TI_DIR_INOUT;
	    break;
	default:
	    FmtAssert(0, ("SIMD_TI::Tie_Mac_Intrinsic: illegal operator"));
    }
    INTRINSIC intrin_id = 
	Tie_TiOperator_Intrinsic_Exp(ti_opr, 3, dirs,types,imm);
    return intrin_id;
}

bool
SIMD_TI::Has_Paired_Mulr(TYPE_ID out_type, TYPE_ID in_type)
{
    return (Tie_Mulr_Intrinsic(out_type, in_type) != INTRINSIC_INVALID);
}

INT
TARG_SIMD_INFO::Guard_Bits (void) const {
  INT bit_diff = Simd_Reg_Bit_Size()-Simd_Mem_Bit_Size();
  INT vl = Vector_Length();
  Is_True(vl!=0,("Non-zero vector length expected."));
  Is_True(bit_diff%vl==0,
	  ("MEM and REG types bit size difference should be multiple of VL"));
  return bit_diff/vl;
}

void
TARG_SIMD_INFO::Print (FILE *f, INT tab) {
    fprintf(f, "%*sTARG_SIMD_INFO {\n", tab, "");
    {
	tab += 2;
	fprintf(f, "%*sVector length:  %d\n", tab, "", Vector_Length());
	fprintf(f, "%*sBase type:      %s (%d)\n",
		tab, "", MTYPE_name(Base_Type()), Base_Type());
	fprintf(f, "%*sSIMD reg-type:  %s (%d)\n",
		tab, "", MTYPE_name(Simd_Reg_Type()), Simd_Reg_Type());
	fprintf(f, "%*sSIMD mem-type:  %s (%d)\n",
		tab, "", MTYPE_name(Simd_Mem_Type()), Simd_Mem_Type());
	fprintf(f, "%*sAlignment type: %s (%d)\n",
		tab, "", MTYPE_name(Alignment_Type()), Alignment_Type());
	if (Simd_Select())
	    Simd_Select()->Print(f, tab);
	tab -= 2;
    }
    fprintf(f, "%*s}\n", tab, "");
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
