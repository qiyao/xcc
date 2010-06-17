
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* generate symtab references for compiler-generated runtime calls */

#include "defs.h"
#include "errors.h"
#include "symtab.h"
#include "strtab.h"

ST *Throw_Runtime_ST (void)
{
  TY_IDX func_ty_idx = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  PU_IDX pu_idx;
  PU&    pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ST * st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str("__throw"), CLASS_FUNC, SCLASS_EXTERN,
		       EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  return st;
}
  

ST *Rethrow_Runtime_ST (void)
{
  TY_IDX func_ty_idx = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  PU_IDX pu_idx;
  PU&    pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ST * st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str("__rethrow"), CLASS_FUNC, SCLASS_EXTERN,
		       EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  return st;
}
  

ST *Terminate_Runtime_ST (void)
{
  TY_IDX func_ty_idx = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  PU_IDX pu_idx;
  PU&    pu = New_PU(pu_idx);
  PU_Init(pu, func_ty_idx, GLOBAL_SYMTAB + 1);
  Set_PU_cxx_lang(pu);
  ST * st = New_ST(GLOBAL_SYMTAB);
  ST_Init(st, Save_Str("__terminate_Fv"), CLASS_FUNC, SCLASS_EXTERN,
		       EXPORT_PREEMPTIBLE, TY_IDX(pu_idx));
  return st;
}
  

  
  


