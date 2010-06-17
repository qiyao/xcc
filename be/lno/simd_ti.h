// Copyright (c) 2003-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_ti.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_TI: SIMD target info interface routines                             *
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

// $Id: simd_ti.h $

#ifndef __SIMD_TI__
#define __SIMD_TI__

#include "cxx_hash.h"
#include "defs.h"
#include "libti.h"
#include "lno_bv.h"
#include "mempool.h"
#include "mtypes.h"
#include "simd_select.h"
#include "tie.h"
#include "wintrinsic.h"

// forward declarations

class SIMD_TI;
class TARG_SIMD_INFO;

//
// TARG_SIMD_INFO
//

class TARG_SIMD_INFO {
    
private:
    INT _vl;
    TYPE_ID _base_type;
    TYPE_ID _simd_reg_type;
    TYPE_ID _simd_mem_type;
    TYPE_ID _alignment_type;
    SIMD_SELECT *_simd_select;
    
public:
    TARG_SIMD_INFO (INT vl, TYPE_ID base_type,
		    TYPE_ID simd_reg_type, TYPE_ID simd_mem_type,
		    TYPE_ID alignment_type, SIMD_SELECT *simd_select) :
	_vl(vl), _base_type(base_type),
	_simd_reg_type(simd_reg_type), _simd_mem_type(simd_mem_type),
	_alignment_type(alignment_type), _simd_select(simd_select) { }
    
    INT Vector_Length (void) const { return _vl; }
    TYPE_ID Base_Type (void) const { return _base_type; }
    TYPE_ID Simd_Reg_Type (void) const { return _simd_reg_type; }
    TYPE_ID Simd_Mem_Type (void) const { return _simd_mem_type; }
    TYPE_ID Alignment_Type (void) const { return _alignment_type; }
    SIMD_SELECT *Simd_Select (void) const { return _simd_select; }
    
    void Set_Simd_Reg_Type (TYPE_ID t) { _simd_reg_type = t; }
    void Set_Simd_Mem_Type (TYPE_ID t) { _simd_mem_type = t; }
    void Set_Alignment_Type (TYPE_ID t) { _alignment_type = t; }   
    void Set_Simd_Select (SIMD_SELECT *simd_select) { _simd_select = simd_select; }

    INT Base_Bit_Size (void) const { return MTYPE_bit_size(_base_type); }
    INT Simd_Reg_Bit_Size (void) const { return MTYPE_bit_size(_simd_reg_type); }
    INT Simd_Mem_Bit_Size (void) const { return MTYPE_bit_size(_simd_mem_type); }
    
    // number of guard bits per element -- can be a negative number if
    // the register type is smaller than the memory type
    INT Guard_Bits (void) const;
    
    INT Base_Byte_Size (void) const { return Base_Bit_Size()>>3; }
    INT Simd_Reg_Byte_Size (void) const { return Simd_Reg_Bit_Size()>>3; }
    INT Simd_Mem_Byte_Size (void) const { return TY_size(MTYPE_To_TY(_simd_mem_type)); }
    
    INT Simd_Alignment (void) const { return MTYPE_alignment(_simd_mem_type); }
    
    void Print (FILE *f, INT tab =0);
}; // TARG_SIMD_INFO

//
// SIMD_TI
//

class SIMD_TI : public SIMD_INFO {

protected:
    
    //
    // TARG_SIMD_INFO_KEY
    //
    class TARG_SIMD_INFO_KEY {
	
    private:
	const INT _vl;
	const TYPE_ID _base_type;
	
    public:
	TARG_SIMD_INFO_KEY (INT vl, TYPE_ID base_type) :
	    _vl(vl), _base_type(base_type) {}
	
	INT Vector_Length (void) const { return _vl; }
	TYPE_ID Base_Type (void) const { return _base_type; }
	
	INT Hash (void) const {
	    return Vector_Length()*37+((INT)Base_Type());
	}
	bool operator == (const TARG_SIMD_INFO_KEY &key) const {
	    return Vector_Length()==key.Vector_Length() &&
		Base_Type()==key.Base_Type();
	}
	
	struct HASH {
	    INT operator () (const TARG_SIMD_INFO_KEY &key) const {
		return key.Hash();
	    }
	};
	
	struct EQUAL {
	    bool operator () (const TARG_SIMD_INFO_KEY &k1,
			      const TARG_SIMD_INFO_KEY &k2) const {
		return k1==k2;
	    }
	};
    }; // TARG_SIMD_INFO_KEY
    
private:

    INT _default_vl; // default vectorization vector length
    
    // a map from a base type to a bit vector of available vector lengths
    typedef HASH_TABLE<TYPE_ID,BIT_VECTOR *> TYPE_BV_MAP;
    TYPE_BV_MAP *_vector_lengths;
    
    // a map from (base type, vector length) to TARG_SIMD_INFO
    typedef USER_HASH_TABLE<TARG_SIMD_INFO_KEY,TARG_SIMD_INFO *,
	TARG_SIMD_INFO_KEY::HASH,TARG_SIMD_INFO_KEY::EQUAL> TYPE_INFO_MAP;
    TYPE_INFO_MAP *_type_info_map;
    
    // a map from a SIMD type to TARG_SIMD_INFO
    typedef HASH_TABLE<TYPE_ID,TARG_SIMD_INFO *> SIMD_TYPE_INFO_MAP;
    SIMD_TYPE_INFO_MAP *_simd_type_info_map;

    // Initialize the TARG_SIMD_INFOs for the simd types coming from libti.
    void Init_Libti_Types (void);
    
    // Initialize the TARG_SIMD_INFOs for the boolean types, if available.
    void Init_Xtbool_Types (void);
    
    // Initialize the TARG_SIMD_INFOs for the packed types.
    void Init_Packed_Types (void);

    // Add 'tsi' to the appropriate tables and return 'true' on success.
    bool Add_Targ_Simd_Info (TARG_SIMD_INFO *tsi);
    
protected:
    TYPE_BV_MAP *Vector_Lengths (void) { return _vector_lengths; }
    TYPE_INFO_MAP *Type_Info_Map (void) { return _type_info_map; }
    SIMD_TYPE_INFO_MAP *Simd_Type_Info_Map (void) { return _simd_type_info_map; }
    
    INTRINSIC Tie_Macro_To_Intrinsic (TIE_MACRO_p macro);
    
public:
    static const INT MAX_VECTOR_LENGTH = 64;
    
    enum LOADA_KIND { LOADA_PRIME, LOADA_IMM, LOADA_IMM_UPDATE };
    enum STOREA_KIND { STOREA_PRIME, STOREA_IMM, STOREA_IMM_UPDATE, STOREA_FLUSH };
    
    SIMD_TI (MEM_POOL *pool);
    
    void Set_Default_Vl (INT vl) { _default_vl = vl; }
    INT Default_Vl (void) const { return _default_vl; }
    
    // returns a bit vector 'bv'
    // bv->Test(vl) is set iff there is a SIMD info for 'base_type' and 'vl' available
    BIT_VECTOR *Vector_Lengths (TYPE_ID base_type);
    
    // returns the TARG_SIMD_INFO structure for the (base_type, vl) pair
    virtual TARG_SIMD_INFO *Targ_Simd_Info (TYPE_ID base_type, INT vl =0);
    
    // returns the TARG_SIMD_INFO structure that contains 'type'
    TARG_SIMD_INFO *Targ_Simd_Info_For_Simd_Type (TYPE_ID type);

    TYPE_ID Simd_Mem_Type_Of_Scalar (TYPE_ID base_type, INT vl =0);
    TYPE_ID Simd_Reg_Type_Of_Scalar (TYPE_ID base_type, INT vl =0);
    TYPE_ID Alignment_Type_Of_Scalar (TYPE_ID base_type, INT vl =0);
    SIMD_SELECT *Simd_Select_Of_Scalar (TYPE_ID base_type, INT vl =0);

    virtual INT Guard_Bits(TYPE_ID base_type, INT vl =0);
    
    /* the configuration is good */ 
    virtual bool Vectra_Ok(void) { return Type_Info_Map()->Num_Entries() != 0; }
    
    virtual bool Is_Vectra1(void){ return false; }
    
    /* Map from basic type to SIMD register type */
    virtual TYPE_ID Get_SIMD_Reg_Type_Scalar(TYPE_ID scalar_type);
    
    /* Map from basic type to SIMD memory type */
    virtual TYPE_ID Get_SIMD_Mem_Type_Scalar(TYPE_ID scalar_type);
    
    /* Map from basic type to alignment type */
    virtual TYPE_ID Get_Alignment_Type_Scalar (TYPE_ID scalar_type);
    
    /* number of elements in a SIMD register */
    virtual INT Get_SIMD_Width_Scalar(TYPE_ID scalar_type);
    virtual INT Get_SIMD_Width_SIMD(TYPE_ID simd_type);
    
    virtual SIMD_SELECT *Get_SIMD_Select_Scalar (TYPE_ID scalar_type);
    virtual SIMD_SELECT *Get_SIMD_Select_SIMD (TYPE_ID simd_type);

    /* number of bytes in a SIMD type, as respect to memory */
    virtual INT Get_SIMD_Mem_Bytes_Scalar(TYPE_ID scalar_type);

    virtual TIE_MACRO_ID Get_Tie_Set_Vsar(void);
    virtual TIE_MACRO_ID Get_Tie_Set_Round(void);

    // return the name, the TIE_MACRO_ID and the INTRINSIC for a specific OPERATOR
    const char *Tie_Operator_Name (OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    const char *Tie_Operator_Name(OPERATOR oper, unsigned int num_operands,
				  TI_DIR *operand_dirs, TYPE_ID *operand_types,
				  INT64 *immediates);
    
    TIE_MACRO_ID Tie_Operator_Macro (OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    TIE_MACRO_ID Tie_Operator_Macro(OPERATOR oper, unsigned int num_operands,
				    TI_DIR *operand_dirs, TYPE_ID *operand_types,
				    INT64 *immediates);
    
    INTRINSIC Tie_Operator_Intrinsic (OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    INTRINSIC Tie_Operator_Intrinsic (OPERATOR oper, unsigned int num_operands,
				      TI_DIR *operand_dirs, TYPE_ID *operand_types,
				      INT64 *immediates);
    
    
    // return the name, the TIE_MACRO_ID and the INTRINSIC for a specific TI_OPERATOR
    const char *Tie_TiOperator_Name (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    TIE_MACRO_ID Tie_TiOperator_Macro (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    INTRINSIC Tie_TiOperator_Intrinsic (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc);
    
    const char *Tie_TiOperator_Name_Exp (TI_OPERATOR oper,
					 unsigned int num_operands,
					 TI_DIR *operand_dirs,
					 TYPE_ID *operand_types,
					 INT64 *immediates);
    TIE_MACRO_ID Tie_TiOperator_Macro_Exp (TI_OPERATOR oper,
					   unsigned int num_operands,
					   TI_DIR *operand_dirs,
					   TYPE_ID *operand_types,
					   INT64 *immediates);
    INTRINSIC Tie_TiOperator_Intrinsic_Exp (TI_OPERATOR oper,
					    unsigned int num_operands,
					    TI_DIR *operand_dirs,
					    TYPE_ID *operand_types,
					    INT64 *immediates);
    
    // return the name, the TIE_MACRO_ID and the INTRINSIC for a specific operator
    const char *Tie_NamedOp_Name_Exp (const char *oper,
				      unsigned int num_operands,
				      TI_DIR *operand_dirs,
				      TYPE_ID *operand_types,
				      INT64 *immediates);
    TIE_MACRO_ID Tie_NamedOp_Macro_Exp (const char *oper,
					unsigned int num_operands,
					TI_DIR *operand_dirs,
					TYPE_ID *operand_types,
					INT64 *immediates);
    INTRINSIC Tie_NamedOp_Intrinsic_Exp (const char *oper,
					 unsigned int num_operands,
					 TI_DIR *operand_dirs,
					 TYPE_ID *operand_types,
					 INT64 *immediates);
    
    virtual INTRINSIC    CMov(TYPE_ID rtype, TYPE_ID bool_type);

    /* find a SIMD function in 'type' for 'op' corresponding the 'lohi' */
    virtual TIE_MACRO_ID Search_Vector_Macro(OPERATOR op,
					     TYPE_ID dtype, TYPE_ID rtype =MTYPE_UNKNOWN,
					     INT lohi = 0, bool update = false);
    
    /* alignment loads */
    virtual WN *Generate_LoadA_Prime (TYPE_ID scalar_type, WN *stmt, WN *addr,
				      SIMD_PREG *addr_reg, SIMD_PREG *target,
				      SIMD_PREG *align);
    
    /* alignment stores */
    virtual void Generate_StoreA_Prime (IMEM_GROUP *ig, WN *stmt);
    virtual void Generate_StoreA_Update (IMEM_GROUP *ig, WN *stmt,
					 SIMD_PREG *val, INT inc);
    virtual void Generate_StoreA_Flush (IMEM_GROUP *ig, WN *stmt, bool ins_after);    
    
    INTRINSIC Tie_Sel_Intrinsic_Imm (TYPE_ID vec_type, INT64 sel_imm);
    INTRINSIC Tie_Sel_Intrinsic (TYPE_ID vec_type);
    INTRINSIC Tie_Dsel_Intrinsic_Imm (TYPE_ID vec_type, INT64 dsel_imm);
    INTRINSIC Tie_Dsel_Intrinsic (TYPE_ID vec_type);
    
    INTRINSIC Tie_Mulr_Intrinsic(TYPE_ID out_type, TYPE_ID in_type);
    INTRINSIC Tie_Mulr_Intrinsic(TYPE_ID out_type, TYPE_ID in_type, bool even);
    INTRINSIC Tie_Mac_Intrinsic(OPERATOR op, TYPE_ID out_type, 
				TYPE_ID in_type, bool even);
    
    INTRINSIC Tie_Shift_Intrinsic_Imm (OPERATOR oper, TYPE_ID vec_type, INT shift_amount);
    virtual INTRINSIC Tie_Shift_Intrinsic (OPERATOR oper, TYPE_ID vec_type);

    bool Tie_Sel_Possible (TYPE_ID vec_type, INT64 sel_imm);
    bool Tie_Dsel_Possible (TYPE_ID vec_type, INT64 dsel_imm);
    
    INTRINSIC Load_Align (LOADA_KIND kind, TYPE_ID mem_type, TYPE_ID reg_type);
    INTRINSIC Store_Align (STOREA_KIND kind, TYPE_ID mem_type, TYPE_ID reg_type);
    virtual INTRINSIC Radd (TYPE_ID res_type, TYPE_ID desc_type);
    virtual INTRINSIC Rmin (TYPE_ID res_type, TYPE_ID desc_type);
    virtual INTRINSIC Rmax (TYPE_ID res_type, TYPE_ID desc_type);
    INTRINSIC Zero (TYPE_ID reg_type);
    
    bool Load_Align_Possible (TYPE_ID mem_type, TYPE_ID reg_type);
    bool Store_Align_Possible (TYPE_ID mem_type, TYPE_ID reg_type);
    bool Load_Scalar_XU_Possible(TYPE_ID mem_type, TYPE_ID reg_type);
    bool Load_Scalar_IU_Possible(TYPE_ID mem_type, TYPE_ID reg_type);

    virtual void Shift_Into_Reg(SIMD_PREG *new_in, SIMD_PREG *res,
				WN *block, WN *stmt, INT64 linenum,
				INT shift_amount = 1, SIMD_PREG *target=NULL);
    virtual bool Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type);
    bool Type_Conversion_Possible (TYPE_ID from_type, TYPE_ID to_type);

    void Store_Packed_Op_Output (SIMD_PREG *out1, SIMD_PREG *out2, WN *res, 
				 WN *block, WN *stmt, INT64 line_num);
    void Store_Packed_Call_Output (SIMD_PREG *out1, SIMD_PREG *out2,
				   WN *res, WN *block, WN *stmt,
				   INT64 line_num);
    void Store_Op_Output (SIMD_PREG *out1, WN *res, WN *block, WN *stmt, 
			  INT64 line_num);
    void Store_Call_Output (SIMD_PREG *out1, WN *res, WN *block, WN *stmt,
			    INT64 line_num);
    virtual bool Has_Paired_Mac(OPERATOR op, TYPE_ID out_type, TYPE_ID in_type);
    virtual bool Has_Paired_Mulr(TYPE_ID out_type, TYPE_ID in_type);
    virtual bool Has_Even_Odd_Mac(OPERATOR op, TYPE_ID out_type, TYPE_ID in_type);
    virtual TYPE_ID Get_SIMD_Xtbool(INT vl);
}; // SIMD_TI

#endif /* __SIMD_TI__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

