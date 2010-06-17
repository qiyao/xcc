// Copyright (c) 2003-2005 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_imem.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_IMEM: SIMD transformation IMEM related data elements and functions  *
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

// $Id: simd_imem.h $

#ifndef __SIMD_IMEM__
#define __SIMD_IMEM__

#include "simd.h"


class IMEM_Map;
class IMEM_GROUP;
class IMEM_ELEM;
class IMEM_OFFSET;
class IMEM_INFO;


typedef DYN_ARRAY <IMEM_ELEM *>   IMEM_ELEMS;
typedef DYN_ARRAY <IMEM_OFFSET *> IMEM_OFFSETS;

typedef DYN_ARRAY<IMEM_INFO *> IMEM_Array;
typedef DYN_ARRAY<IMEM_GROUP *> IMEM_GROUP_Array;

//
//  VSEL_IMEM: For odd cases vsel of array of struct
//
//  The registers are numbered from inputs to outputs,
//  i.e., #outputs[0] = total_number_of(inputs)
//
#define MAX_VEC_WIDTH 16
struct VSEL_OP {
    INT input_0;     // input0 register number, (-1 terminate)
    INT input_1;     // input1 register number
    INT output_0;    // output0 register number
/*     INT output_1;    // output1 register number (-1 if not needed) */

    INT sel_0[MAX_VEC_WIDTH]; // select for each location
/*    INT sel_1[MAX_VEC_WIDTH]; // (sel_1[0]==(-1)) if not needed */
};

class VSEL_IMEM {
public:
    INT       field_count;    // number of fields
    INT       vec_width;      // width of vector
    VSEL_OP  *vsel_ops;       // -1 terminated array
    VSEL_IMEM(INT f, INT vl, VSEL_OP *vsel): 
	field_count(f), vec_width(vl), vsel_ops(vsel) {}
};

typedef DYN_ARRAY<VSEL_IMEM *> VSEL_IMEMS;
extern VSEL_OP * Find_Vsel_Op(VSEL_IMEMS &table, INT fc, INT vl);

//
// IMEM_Map
//
// IMEM_Map combines all IMEM_GROUPs (and, respectively, IMEM_INFOs)
//   IMEM_GROUP combines several IMEM_ELEMs
//     IMEM_ELEM combines several IMEM_OFFSETs
//       IMEM_OFFSET corresponds to a unique IMEM_INFO
//

class IMEM_Map {
    
private:

  IMEM_Array _array;
  IMEM_GROUP_Array _groups;
  MEM_POOL *_pool;
  SIMD_LOOP *_simd_loop;
  WN_ARRAY  *_all_lod_sto;
  VSEL_IMEMS _vsel_r_table;
  VSEL_IMEMS _vsel_w_table;

public:
    IMEM_Map(SIMD_LOOP *simd_loop, MEM_POOL *pool);
    ~IMEM_Map();
    
    SIMD_LOOP *Simd_Loop()   { return _simd_loop; }
    MEM_POOL *Pool()         { return _pool;      }
    IMEM_Array& IMem_Array() { return _array;     }    
    VSEL_IMEMS& Vsel_R_Table() { return _vsel_r_table;}
    VSEL_IMEMS& Vsel_W_Table() { return _vsel_w_table;}
    
    void Initialize_Vsel_Imems(MEM_POOL *pool);
    
    void Clear(void) { _array.Resetidx(); _groups.Resetidx(); }
    
    void Enter_Group (IMEM_GROUP *ig) { _groups.AddElement(ig); }

    IMEM_INFO *Find(WN *expr);
    
    void       Enter(IMEM_INFO *imem) { _array.AddElement(imem); }
    
    // return the maximum number of offsets(fields) within a single element
    INT          Get_Max_Offset_Count(void);
    
    /* clear SIMD_EINFO */
    void       Clear_Simd_EInfo(void);
    void       Reset_AT_Vl (void);
    
    bool       Setup_Access_Properties(bool before_transform);
    void       Order_Specific_Setup(LOOP_MODEL *lm, INT num_loops,
				    SIMD_LOOP *simd_loop, bool before_transform,
				    INT inner, bool msg);
    
    /* collect load/store per each IMEM_GROUP */
    void         Collect_Load_Store(void);
    
    /* Add dependence for pending iload/istore merging */
    void       Add_Store_Dependence(MEM_POOL *pool);
    
    // Bring together IMEMs that have the same base and same indexes or
    // indexes differing by a constant, and build the IMEM_GROUP/ELEM/OFFSET
    // structures.
    void Group (void);
    IMEM_GROUP_Array &Groups(void) { return _groups; }
    void Print_Groups (FILE *out_file = stderr, INT indent = 0);

    // Split reuse group if the reuse is no at the inner most level
    void Split_Group(void);

    // Split reuse group if there is not Vsel for the reuse
    void Split_Group_No_Vsel(void);
    BOOL Need_Split_Group_No_Vsel(void);
    
    void Setup_Index_Value (void);
    void Delete_Index_Value (void);
    void Allocate_Regs(void);
    void Add_Dependence_Vertices(void);
    INT  Update_Dependence(void);
    
    INT  Write_Group_Count(void);
    INT  Read_Group_Count(void);
    INT  Unaligned_Write_Group_Count(void);
    INT  Unaligned_Read_Group_Count(void);
    
    IMEM_INFO *Only_Write_Imem_Info(void);
    void       Add_Lod_Sto(WN *wn) { _all_lod_sto->AddElement(wn); }
    WN_ARRAY  *All_Lod_Sto(void)   { return _all_lod_sto; }
};


//
// IMEM_GROUP:
//
// Combines several relatively close array accesses (e.g. a[i] and a[i+1],
// given unit step) to avoid redundant loads (as well as running out of
// alignment registers)
//

class IMEM_GROUP {
    
private:
    
    enum FLAG {
	NONE           = 0x00,
	LOADED         = 0x01,
	IS_VECTOR      = 0x02,
	UPDATING_LOAD  = 0x04,
	PRIME_IN       = 0x08, /* unaligned store primed inside loop */
	BAD_ELEM       = 0x10, /* has bad element                    */
	VAR_STRIDE     = 0x20, /* variable stride                    */
	ADDR_SHARED    = 0x40, /* addr register is shared            */
	ADDR_UPDATED   = 0x80, /* addr is updated                    */
	IS_REDUCTION   = 0x100,/* reduction                          */
	GAPS           = 0x200,/* access gaps                        */
    };
    
    UINT         _flags;
    UINT8        _at_vl;       /* processed Auto Tie vector length   */
    UINT         _reuse_length;/* number of reuse registers          */
    
    IMEM_ELEMS *_elems;
    
    MEM_POOL   *_mem_pool;
    
    // result registers from/to memory
    SIMD_PREGS *_res_regs;         /* result registers                */
    
    // address registers
    SIMD_PREG  *_load_addr_reg;    /* load address register           */
    SIMD_PREG  *_store_addr_reg;   /* store address register          */
    SIMD_PREG  *_index_addr_reg;   /* index register                  */

    WN         *_index_value;      /* index register value            */    
    
    // unaligned load
    SIMD_PREG  *_load_align_reg;   /* load alignment register         */
    
    // unaligned store (with or without alignment register)
    SIMD_PREG  *_store_copy;       /* pending unaligned store w/o SVA */
    SIMD_PREG  *_store_sel_reg;    /* select for unaligned store      */
    SIMD_PREG  *_store_index_reg;  /* index to str_align              */
    SIMD_PREG  *_store_align_reg;  /* store alignment register        */
    
    INT         _non_const_level;  /* highest level where the stride 1
				      dimension is loop variant       */
    
    WN         *_base_addr;        /* base load/store address for the group */
    
    WN         *_invar_point;      /* insertion point for moving out loads */
    
    WN_ARRAY   *_stores;           /* all stores for this group */
    WN_ARRAY   *_loads;            /* all loads for this group */
    
    WN_ARRAY   *_v_stores;         /* vector stores            */
    WN_ARRAY   *_v_loads;          /* vector loads             */
    
    VINDEX16_ARRAY *_v_load_vertices; /* vector load vertices  */  
    VINDEX16_ARRAY *_v_store_vertices;/* vector store vertices */
    
    TYPE_ID     _scalar_type;      /* type of bottom level elements */
    EXPR_Stack  _pending_wn;       /* pending loads/stores for DGraph update */

    void         Allocate_Res_Regs (void);
    void         Load_Res_Reg (WN *load, WN *stmt, INT idx, WN *t_loop);
    void         Load_Variable_Stride_Reg(WN *load, WN *stmt, INT idx, 
					  SIMD_PREG *var_reg, WN *orig_ls,
					  SIMD_PREG *addr_preg, 
					  SIMD_PREG *idx_preg,
					  bool update);
    bool         Use_Before_Def(void);
    bool         Def_Before_Use(void);
    
public:
    IMEM_GROUP (MEM_POOL *pool);
    IMEM_GROUP (IMEM_GROUP *ig, MEM_POOL *pool);
    ~IMEM_GROUP ();
    
    MEM_POOL*    Mem_Pool (void)         { return _mem_pool;  }
    
    UINT         Flags(void)             { return _flags;     }
    void         Set_Flags(UINT f)       { _flags = f;        } 
    
    void         Set_Loaded(void)        { _flags |= LOADED;  }
    void         Reset_Loaded(void)      { _flags &= ~LOADED; }
    bool         Loaded(void)            { return (_flags & LOADED)!=0; }
    
    void         Set_Is_Vector(void)     { _flags |= IS_VECTOR;  }
    void         Reset_Is_Vector(void)   { _flags &= ~IS_VECTOR; }
    bool         Is_Vector(void)         { return (_flags & IS_VECTOR)!=0; }

    void         Set_Is_Reduction(void)  { _flags |= IS_REDUCTION;  }
    void         Reset_Is_Reduction(void){ _flags &= ~IS_REDUCTION; }
    bool         Is_Reduction(void)      { return (_flags & IS_REDUCTION)!=0; }
    
    void         Set_Updating_Load(void)   { _flags |= UPDATING_LOAD;  }
    void         Reset_Updating_Load(void) { _flags &= ~UPDATING_LOAD; }
    bool         Updating_Load(void)       { return (_flags & UPDATING_LOAD)!=0; }
    
    void         Set_Prime_In(void)        { _flags |= PRIME_IN;  }
    void         Reset_Prime_In(void)      { _flags &= ~PRIME_IN; }
    bool         Prime_In(void)            { return (_flags & PRIME_IN)!=0; }
    
    void         Set_Has_Bad_Elem(void)  { _flags |= BAD_ELEM;  }
    void         Reset_Has_Bad_Elem(void){ _flags &= ~BAD_ELEM; }
    bool         Has_Bad_Elem(void)      { return (_flags & BAD_ELEM)!=0; }
    
    void         Set_Variable_Stride(void)  { _flags |= VAR_STRIDE;  }
    void         Reset_Variable_Stride(void){ _flags &= ~VAR_STRIDE; }
    bool         Variable_Stride(void)      { return (_flags & VAR_STRIDE)!=0; }
    void         Set_Addr_Shared(void)    { _flags |= ADDR_SHARED;  }
    void         Reset_Addr_Shared(void)  { _flags &= ~ADDR_SHARED; }
    bool         Addr_Shared(void)        { return (_flags & ADDR_SHARED)!=0; }

    void         Set_Addr_Updated(void)  { _flags |= ADDR_UPDATED;  }
    void         Reset_Addr_Updated(void){ _flags &= ~ADDR_UPDATED; }
    bool         Addr_Updated(void)      { return (_flags & ADDR_UPDATED)!=0; }
    
    void         Set_Has_Gaps(void)      { _flags |= GAPS;  }
    void         Reset_Has_Gaps(void)    { _flags &= ~GAPS; }
    bool         Has_Gaps(void)          { return (_flags & GAPS)!=0; }
    
    INT          AT_Vl(void)             { return _at_vl;                }
    void         Set_AT_Vl(INT vl)       { _at_vl = vl;                  }
    INT          Reuse_Length(void)      { return _reuse_length;         }
    void         Set_Reuse_Length(INT l) { _reuse_length = l;            }
    
    bool         Is_Def(void)            { return _stores->Elements()>0; }
    bool         Is_Use(void)            { return _loads->Elements()>0;  }
    bool         Is_Aligned(void); 
    WN_ARRAY *   Stores(void)            { return _stores;               }
    WN_ARRAY *   Loads(void)             { return _loads;                }
    WN_ARRAY *   V_Stores(void)          { return _v_stores;             }
    WN_ARRAY *   V_Loads(void)           { return _v_loads;              }
    
    VINDEX16_ARRAY* V_Load_Vertices(void)  { return _v_load_vertices;   }
    VINDEX16_ARRAY* V_Store_Vertices(void) { return _v_store_vertices;  }
    
    IMEM_ELEMS*  Elems (void)       { return _elems; }
    IMEM_ELEM*   Elem (INT i)       { return Elems()->Get(i); }
    void         Set_Elem (INT i, IMEM_ELEM *ie);
    void         Add_Elem (IMEM_ELEM *ie);
    INT          Elem_Count (void)   { return Elems()->Elements(); }
    
    SIMD_PREGS*  Res_Regs(void)      { return _res_regs;               }
    SIMD_PREG*   Res_Reg(INT i)      { return Res_Regs()->Get(i);      }
    INT          Res_Reg_Count(void) { return Res_Regs()->Elements();  }
    void         Set_Res_Reg(INT i, SIMD_PREG *r) { Res_Regs()->Set(i,r); }
    void         Add_Res_Reg(SIMD_PREG *r) { Res_Regs()->AddElement(r);   }
    
    SIMD_PREG*   Load_Align_Reg(void)     { return _load_align_reg;      }
    void         Set_Load_Align_Reg(SIMD_PREG *e) { _load_align_reg = e; }
    
    SIMD_PREG*   Load_Addr_Reg(void){ return _load_addr_reg;         }
    void         Set_Load_Addr_Reg(SIMD_PREG *e) { _load_addr_reg = e; }
    
    SIMD_PREG*   Store_Addr_Reg(void){ return _store_addr_reg;         }
    void         Set_Store_Addr_Reg(SIMD_PREG *e) { _store_addr_reg = e; }

    SIMD_PREG*   Index_Addr_Reg(void)  { return _index_addr_reg;         }
    void         Set_Index_Addr_Reg(SIMD_PREG *e) { _index_addr_reg = e; }

    WN*          Index_Value(void)        { return _index_value;         }
    void         Set_Index_Value(WN *e)   { _index_value = e;            }
    
    SIMD_PREG*   Store_Align_Reg(void){ return _store_align_reg;       }
    void         Set_Store_Align_Reg(SIMD_PREG *e) { _store_align_reg = e; }
    
    SIMD_PREG*   Store_Copy(void)    { return _store_copy;       }
    void         Set_Store_Copy(SIMD_PREG *e) { _store_copy = e; }
    
    SIMD_PREG*   Store_Sel_Reg(void) { return _store_sel_reg;          }
    void         Set_Store_Sel_Reg(SIMD_PREG *e) { _store_sel_reg = e; }
    
    SIMD_PREG*   Store_Index_Reg(void) { return _store_index_reg;          }
    void         Set_Store_Index_Reg(SIMD_PREG *e) { _store_index_reg = e; }
    
    INT          Non_Const_Level (void) { return _non_const_level;       }
    void         Set_Non_Const_Level (INT l) { _non_const_level = l;     }
    
    WN *         Invar_Point (void)  { return _invar_point;            }
    void         Set_Invar_Point (WN *ip) { _invar_point = ip;         }
    
    WN *         Base_Addr(void) { return _base_addr; }
    void         Set_Base_Addr(WN *base_addr) { _base_addr = base_addr; }
    void         Recompute_Base_Addr(void);

    TYPE_ID      Scalar_Type (void) const      { return _scalar_type;    }
    void         Set_Scalar_Type(TYPE_ID type) { _scalar_type=type;      }
    
    EXPR_Stack&  Pending_Wn(void)          { return _pending_wn;         }
    
    void Setup_Index_Value(void);
    void Delete_Index_Value (void);
    void Allocate_Regs(void);
    
    /* collect load/store from IMEM_INFO the loads, stores array */
    void         Collect_Load_Store(void);
    
    /* setup has_exp_part_use, has_part_def, has_dif_size properties */
    void         Setup_Property(void);
    
    WN *         Compute_Index_Value(WN *t_loop);

    /* generate store align index register */
    void         Generate_Store_Offset_Index(SIMD_PREG *store_addr_reg,
					     WN *stmt, SIMD_LOOP *do_info);
    
    /* merge unaligned store */
    WN*          Generate_Unaligned_Store_Merge(WN *lod_val,
						SIMD_PREG *store_copy);
    
    /* update store copy for unaligned store */
    void         Generate_Unaligned_Store_Copy(WN *new_val, WN* stmt);
    
    WN *         Generate_Store_Select(WN *stmt, INT mul, SIMD_PREG *sto_index_reg, 
				       SIMD_PREG *sel);
    
    /* add dependence vertices for vector loads/stores */
    void         Add_Dependence_Vertices(void);
    
    /* update dependence */
    INT          Update_Dependence();
    INT          Update_Dependence(WN_ARRAY *, VINDEX16_ARRAY *);
    
    /* Add dependence for pending iload/istore merging */
    void         Add_Store_Dependence(MEM_POOL *pool);
    
    void         Transform_Load (WN *load, WN *stmt);
    void         Transform_Store (WN *store, WN *stmt);
    void         Transform_Variable_Stride_Load(WN *load, WN *stmt);
    void         Transform_Indexed_Load(WN *load, WN *stmt, WN *t_loop);
    void         Transform_Reuse_Load (WN *load, WN *stmt);
    /* Distance from the last element to the first element */
    INT          Distance(void);
    
    // Split reuse group if the reuse is no at the inner most level
    void         Split_Group(IMEM_Map *imap);

    // Split reuse group if thers is no Vsel for the reuse
    void Split_Group_No_Vsel(IMEM_Map *im);
    BOOL Need_Split_Group_No_Vsel(IMEM_Map *im);
    
    /* first IMEM_INFO */
    IMEM_INFO   *First_Imem_Info(void);
    
    void         Get_Vector_Load_Types(TYPE_ID &res_type, TYPE_ID &desc_type);
    bool         Index_Const_Fit_Immediate();
    
    // Return the maximum number of offsets (fields) within a single element
    // in this group.
    INT          Get_Max_Offset_Count (void);

    void         Print(FILE *out_file = stderr, int indent = 0);
};


//
// IMEM_ELEM
//
// Combines the offset accesses within the loop stride (e.g. a[i].x and
// a[i].y, or a[2*i] and a[2*i+1])
//
// Combines the reuse within the same loop (e.g. a[i], a[i+1], a[i].x

class IMEM_ELEM {
    
private:
    
    enum FLAG {
	NONE          = 0x00,
	LOADED        = 0x01,
	PART_DEF      = 0x02,          /* has partial write               */
	EXP_PART_USE  = 0x04,          /* has exposed partial use         */
	DIF_SIZE      = 0x08,          /* different size in IMEM_OFFSETs  */
	BAD_FIELD_CNT = 0x10,          /* unhandled number of fields      */
	VAR_STRIDE    = 0x20,          /* variable stride                 */
    };
    
    UINT         _flags;
    
    IMEM_GROUP   *_parent_group;
    IMEM_OFFSETS *_offsets;
    MEM_POOL     *_mem_pool;
    
    // the result registers are parallel to the registers in SIMD_EINFO
    SIMD_PREGS   *_res_regs;           /* result registers                */
    
    INT          _elem_byte_size;      /* size of array element           */

    void         Allocate_Res_Regs (void);

    void         Transform_Odd_Even_Shuffle(WN *stmt,
					    SIMD_PREGS *inputs, 
					    SIMD_PREGS *output_0, 
					    SIMD_PREGS *output_1);
    
    void         Transform_Interleave_Shuffle(WN *stmt,
					      SIMD_PREGS *inputs, 
					      SIMD_PREGS *outputs);
    
    bool         Test_Sel_Fields_By_Table (TYPE_ID vec_type, INT num_fields,
					   bool is_read);

    bool         Test_Sel_Fields (bool pairwise, bool is_read);
    
public:
    
    IMEM_ELEM (MEM_POOL *pool);
    
    MEM_POOL*    Mem_Pool (void)    { return _mem_pool; }
    
    IMEM_GROUP*  Parent_Group(void) { return _parent_group; }
    void         Set_Parent_Group(IMEM_GROUP *pg = NULL)
                                     { _parent_group = pg; }
    
    void         Set_Loaded(void)        { _flags |= LOADED;  }
    void         Reset_Loaded(void)      { _flags &= ~LOADED; }
    bool         Loaded(void)            { return (_flags & LOADED)!=0; }

    void         Set_Has_Part_Def(void)  { _flags |= PART_DEF;  }
    void         Reset_Has_Part_Def(void){ _flags &= ~PART_DEF; }
    bool         Has_Part_Def(void)      { return (_flags & PART_DEF)!=0; }

    void         Set_Has_Exp_Part_Use(void)  { _flags |= EXP_PART_USE;  }
    void         Reset_Has_Exp_Part_Use(void){ _flags &= ~EXP_PART_USE; }
    bool         Has_Exp_Part_Use(void)  { return (_flags & EXP_PART_USE)!=0; }

    void         Set_Has_Dif_Size(void)  { _flags |= DIF_SIZE;  }
    void         Reset_Has_Dif_Size(void){ _flags &= ~DIF_SIZE; }
    bool         Has_Dif_Size(void)      { return (_flags & DIF_SIZE)!=0; }

    void         Set_Bad_Field_Count(void)  { _flags |= BAD_FIELD_CNT;  }
    void         Reset_Bad_Field_Count(void){ _flags &= ~BAD_FIELD_CNT; }
    bool         Bad_Field_Count(void) { return (_flags & BAD_FIELD_CNT)!=0; }

    void         Set_Variable_Stride(void)  { _flags |= VAR_STRIDE;  }
    void         Reset_Variable_Stride(void){ _flags &= ~VAR_STRIDE; }
    bool         Variable_Stride(void) { return (_flags & VAR_STRIDE)!=0; }

    IMEM_OFFSETS* Offsets (void)     { return _offsets; }
    IMEM_OFFSET* Offset (INT i)      { return Offsets()->Get(i); }
    void         Set_Offset (INT i, IMEM_OFFSET *io);
    void         Add_Offset (IMEM_OFFSET *io);
    INT          Offset_Count (void) { return Offsets()->Elements(); }
    
    INT          Elem_Byte_Size (void) { return _elem_byte_size;       }
    void         Set_Elem_Byte_Size (int _size)
                                       { _elem_byte_size = _size; }

    SIMD_PREGS*  Res_Regs(void)      { return _res_regs;               }
    SIMD_PREG*   Res_Reg(INT i)      { return Res_Regs()->Get(i);      }
    INT          Res_Reg_Count(void) { return Res_Regs()->Elements();  }
    void         Set_Res_Reg(INT i, SIMD_PREG *r)
                                     { Res_Regs()->Set(i,r);           }
    void         Add_Res_Reg(SIMD_PREG *r)
                                     { Res_Regs()->AddElement(r);      }

    /* setup has_exp_part_use, has_part_def, has_dif_size properties */
    void         Setup_Property(void);

    void         Allocate_Regs(void);

    void         Read_Reuse_Into_Regs(WN *stmt);

    void         Read_Into_Regs(WN *stmt, SIMD_PREGS *inputs,
				SIMD_PREGS *outputs, bool pairwise);
    void         Read_Into_Regs(WN *stmt, SIMD_PREGS *inputs,
				SIMD_PREGS *outputs);

    void         Read_Into_Regs(WN *stmt);
    
    void         Write_Out_Regs_P2(WN *stmt, SIMD_PREGS *inputs,
				   SIMD_PREGS *outputs);
    void         Write_Out_Regs(WN *stmt, SIMD_PREGS *inputs,
				SIMD_PREGS *outputs);
    void         Write_Out_Regs(WN *stmt);

    void         Transform_By_Table(WN *stmt, SIMD_PREGS *inputs, 
				    SIMD_PREGS *outputs, bool is_read);

    void         Transform_Load (WN *load, WN *stmt);
    void         Transform_Store (WN *store, WN *stmt);    
    
    bool         Has_Table_For_Fields(INT vl=2);
    bool         Test_Sel_Read_Into_Regs (void);
    bool         Test_Sel_Write_Out_Regs (void);
    
    void         Print(FILE *out_file = stderr, int indent = 0);
};


//
// IMEM_INFO
//
// TODO: there is basically no difference between IMEM_INFO and IMEM_OFFSET,
// so we should probably combine them at some point

class IMEM_INFO
{
private:
  enum FLAG {
    NONE          = 0x00,
    IS_DEF        = 0x01,            /* has DEF            */
    IS_USE        = 0x02,            /* has load           */
    IS_VECTOR     = 0x04,            /* is vector          */
    HAS_REUSE     = 0x08,            /* has inner reuse    */
    IS_ALIGNED    = 0x10,            /* is aligned         */
    REDUCTION     = 0x20,            /* is part of a reduction */
    TRANS         = 0x40,            /* tranformed        */
    NO_PRIME      = 0x80,            /* cannot prime outside of the loop */
    VAR_STRIDE    = 0x100,           /* variable stride in the loop */
    ALIGNED_2D    = 0x200,           /* 2 D aligned       */
    TOO_MESSY     = 0x400,           /* messy array access */
  };
  
  UINT32         _flag;                /* flags           */
  
  EXPR_Stack     _lodsto;              /* indirect memory   *
                                       /* lod/sto/apr       */
  SYMBOL        *_array;               /* base of the stream*/
  WN_OFFSET      _offset;              /* WN_offset         */
  ACCESS_ARRAY  *_access;              /* access array      */
  TYPE_ID        _scalar_type;         /* type of bottom level data */
    
  INT            _data_byte_size;      /* size of accessed data */
  INT            _element_byte_size;   /* size of array element */
  
  INT            _non_const_level;     /* highest level where
                                          the stride 1 dimension is
                                          loop variant      */
  WN            *_to_loop;             /* invariant move to loop */
  SIMD_EINFO    *_eInfo;               /* most recent SIMD_EINFO */
  IMEM_INFO     *_prev_lod;            /* previous load      */
  
  IMEM_OFFSET   *_imem_offset;         /* the leaf in the group data structure */
  IMEM_Map      *_imem_map;            /* map of all IMEM_INFOs/IMEM_GROUPs */
  SIMD_PREG_Stack _if_conv;            /* for If_Conv                      */
  
  MEM_POOL      *_pool;
  
  void Init_Access_Array (WN *array_wn);
  
public:
    
  /* Constructor/Destructor */
  IMEM_INFO(WN *ls, IMEM_Map *imem_map, MEM_POOL *pool);
  
  /* Member access */
  IMEM_Map *   Imem_Map(void)      { return _imem_map;               }
  SIMD_LOOP *  Simd_Loop(void)     { return _imem_map->Simd_Loop();  }
  EXPR_Stack&  Lod_Sto(void)       { return _lodsto;                 }
  SYMBOL*      Array_Base(void)    { return _array;                  }
  WN_OFFSET    WN_Offset(void)     { return _offset;                 }
  ACCESS_ARRAY* Access_Array(void) { return _access;                 }
  TYPE_ID      Scalar_Type(void)   { return _scalar_type;            }
  INT          Data_Byte_Size(void)    { return _data_byte_size;     }
  INT          Element_Byte_Size(void) { return _element_byte_size;  }
  SIMD_EINFO*  E_Info(void)        { return _eInfo;                  }
  void         Set_E_Info(SIMD_EINFO* e) { _eInfo = e;               }
  INT          Non_Const_Level(void) { return _non_const_level;      }
  void         Set_Non_Const_Level(INT l) { _non_const_level = l;    }
  
  WN*          Orig_Wn (void) { return Lod_Sto().Bottom_nth(0); }

  WN*          To_Loop(void)       { return _to_loop;                }
  void         Set_To_Loop(WN *l)  { _to_loop = l;                   }
  
  /* Flags */
  void         Set_Is_Def(void)    { _flag |= IS_DEF;                }
  bool         Is_Def(void)        { return (_flag & IS_DEF) != 0;   }
  void         Set_Is_Use(void)    { _flag |= IS_USE;                }
  bool         Is_Use(void)        { return (_flag & IS_USE) != 0;   }
  void         Set_Is_Vector(void) { _flag |= IS_VECTOR;             }
  bool         Is_Vector(void)     { return (_flag & IS_VECTOR) != 0;}
  void         Set_Has_Reuse(void) { _flag |= HAS_REUSE;             }
  void         Reset_Has_Reuse(void) { _flag &= ~HAS_REUSE;          }
  bool         Has_Reuse(void)     { return (_flag & HAS_REUSE) != 0;}
  void         Reset_Is_Aligned(void){ _flag &= ~IS_ALIGNED;         }
  void         Set_Is_Aligned(void){ _flag |= IS_ALIGNED;            }
  bool         Is_Aligned(void)    { return (_flag & IS_ALIGNED)!= 0;}
  void         Set_2D_Aligned(void){ _flag |= ALIGNED_2D;            }
  bool         Is_2D_Aligned(void) { return (_flag & ALIGNED_2D)!= 0;}
  void         Set_Reduction(void) { _flag |= REDUCTION;             }
  bool         Is_Reduction(void)  { return (_flag & REDUCTION)!= 0; }
  void         Set_Transformed(void){ _flag |= TRANS;                }
  bool         Transformed(void)   { return (_flag & TRANS)!= 0;     }
  void         Set_No_Prime(void)  { _flag |= NO_PRIME;              }
  bool         No_Prime(void)      { return (_flag & NO_PRIME)!= 0;  }
  void         Set_Variable_Stride(void)  { _flag |= VAR_STRIDE;     }
  bool         Variable_Stride(void) { return (_flag & VAR_STRIDE)!= 0;  }
  void         Set_Too_Messy(void)  { _flag |= TOO_MESSY;     }
  bool         Too_Messy(void) { return (_flag & TOO_MESSY) != 0;  }
  
  UINT32       Flag(void)          { return _flag;                   }
  void         Set_Flag(UINT32 flag){ _flag = flag;                  }
  
  IMEM_INFO   *Prev_Load(void)     { return _prev_lod;               } 
  void         Set_Prev_Load(IMEM_INFO *l) { _prev_lod = l;          }
  
  IMEM_OFFSET *Imem_Offset (void) { return _imem_offset; }
  void         Set_Imem_Offset (IMEM_OFFSET *io=NULL) { _imem_offset = io; }
  
  INT          Stride_At_Loop(INT loop_level);
  void         Set_Alignment(SIMD_LOOP *simd, WN *loop=NULL,INT loop_level=-1,
                             WN *loop_inner=NULL, INT loop_level_inner=-1);
  bool         Check_Stride(void);
  bool         Is_First_Imem_In_Group(void);
  
  /* check for same array and same access */
  bool         Is_Same_Array_Access(WN *wn);
  
  void         Setup_Vector_Property(SIMD_LOOP *);
  void         Setup_Load_Property (LOOP_MODEL *lm, INT num_loops,
                                    SIMD_LOOP *doinfo, bool before_transform,
                                    INT inner, bool msg);
  
  /* Add a new lod/sto to the stack. */
  void Add_Lod_Sto(WN *ls);
  
  /* Print */
  void         Print(FILE *outFile = stderr, int indent = 0);
  
  // Compare array access base (TRUE if equal)
  bool Same_Imem_Base (IMEM_INFO *ii);
  
  // Compare array indexes assuming equal bases (TRUE if differ only in the
  // constant offset, which is returned in diff if it is not NULL)
  bool Compare_Imem_Index (IMEM_INFO *ii, INT *diff = NULL);
  
  // Compare the offsets of the memory accesses (returns the difference)
  INT Diff_Imem_Offset (IMEM_INFO *ii);
  
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
  WN* Get_IStore(void);
}; // IMEM_INFO


//
// IMEM_OFFSET
//
// All information about the specific memory access. Combines same memory
// accesses within a loop (e.g. two uses of a[i].x).
//

class IMEM_OFFSET {
    
private:
    
    enum FLAG {
	NONE          = 0x00,
	LOADED        = 0x01,
	EXP_USE       = 0x02,         // has exposed use
	DEF           = 0x04,         // has def
	VAR_STRIDE    = 0x10,         // variable stride
    };

    UINT         _flags;
    
    IMEM_ELEM    *_parent_elem;
    IMEM_INFO    *_imem_info;
    
    MEM_POOL     *_mem_pool;

    // the result registers are parallel to the registers in SIMD_EINFO
    SIMD_PREGS   *_res_regs;           /* result registers                */
   
    SIMD_PREG    *_var_reg;            /* variable stride register        */

    void         Allocate_Res_Regs (void);
    
public:
    
    IMEM_OFFSET (IMEM_INFO *imem_info, MEM_POOL *pool);
    ~IMEM_OFFSET ();
    
    MEM_POOL*    Mem_Pool (void) { return _mem_pool; }
    
    IMEM_ELEM*   Parent_Elem (void)  { return _parent_elem; }
    void         Set_Parent_Elem (IMEM_ELEM *ps = NULL) { _parent_elem = ps; }
    
    IMEM_GROUP*  Parent_Group (void) { return Parent_Elem()->Parent_Group(); }
    
    IMEM_INFO*   Imem_Info (void)                     { return _imem_info; }
    void         Set_Imem_Info (IMEM_INFO *ii = NULL) { _imem_info = ii; }

    WN*          Orig_Wn(void) { return Imem_Info()->Orig_Wn(); }

    void         Set_Loaded(void)        { _flags |= LOADED;  }
    void         Reset_Loaded(void)      { _flags &= ~LOADED; }
    bool         Loaded(void)            { return (_flags & LOADED)!=0; }

    void         Set_Has_Def(void)       { _flags |= DEF;  }
    void         Reset_Has_Def(void)     { _flags &= ~DEF; }
    bool         Has_Def(void)           { return (_flags & DEF)!=0; }

    void         Set_Variable_Stride(void)  { _flags |= VAR_STRIDE;  }
    void         Reset_Variable_Stride(void){ _flags &= ~VAR_STRIDE; }
    bool         Variable_Stride(void)      { return (_flags & VAR_STRIDE)!=0;}

    void         Set_Has_Exp_Use(void)   { _flags |= EXP_USE;  }
    void         Reset_Has_Exp_Use(void) { _flags &= ~EXP_USE; }
    bool         Has_Exp_Use(void)       { return (_flags & EXP_USE)!=0; }

    SIMD_PREGS*  Res_Regs(void)      { return _res_regs;               }
    SIMD_PREG*   Res_Reg(INT i)      { return Res_Regs()->Get(i);      }
    INT          Res_Reg_Count(void) { return Res_Regs()->Elements();  }
    void         Set_Res_Reg(INT i, SIMD_PREG *r)
                                     { Res_Regs()->Set(i,r);           }
    void         Add_Res_Reg(SIMD_PREG *r)
                                     { Res_Regs()->AddElement(r);      }

    SIMD_PREG*   Var_Reg(void)      { return _var_reg;                 }
    void         Set_Var_Reg(SIMD_PREG *r) { _var_reg = r;             }

    WN *         Invar_Point(void) { return Parent_Group()->Invar_Point(); }

    void         Allocate_Regs(void);

    void         Transform_Load (WN *load, WN *stmt);
    void         Transform_Store (WN *store, WN *stmt);

    void         Print(FILE *out_file = stderr, int indent = 0);
};

#endif /* __SIMD_IMEM__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

