/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//
//  Generate ISA properties information
///////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. Operator attributes descriptors
//   2. Exception classification descriptors
//   3. Other operator descriptors (mostly for global optimization).
//
// Within each ISA_PROPERTY instructions are listed in alphabetical order and
// as shown in the ISA manual
/////////////////////////////////////
//  $Revision: 1.38 $
//  $Date: 2000/04/12 17:14:08 $
//  $Author: lesniak $
//  $Source: /osprey.src/osprey1.0/common/targ_info/isa/ia64/RCS/isa_properties.cxx,v $


#include <stddef.h>
#include "topcode.h"
#include "isa_properties_gen.h"

main()
{
  ISA_PROPERTY
    move,   		/* Move operator */
    load,   		/* Memory load operator */
    store, 		/* Memory store operator */
    prefetch,		/* Prefetch operator */
    xfer, 		/* Control transfer operator */
    call, 		/* Subprogram call operator */
    cond, 		/* Call/xfer is conditional */
    likely, 		/* Cond call/xfer is likely */
    unalign_ld, 	/* Unaligned load operator */
    unalign_store,	/* Unaligned store operator */
    unknown_addr,	/* Memory operator potentially access any memory location */
    unknown_memdata,	/* Memory operator potentially alter data loaded/stored */
    cond_move, 		/* conditional move */
    uniq_res, 		/* Result must not be opnd */
    same_res,		/* Result must be same as opnd */
    noop, 		/* No-op operator */
    generic, 		/* a generic operator which can be specialized */
    select, 		/* Operator is a machine level select */
    dummy, 		/* No-op doesn't get emitted */
    iadd, 		/* Integer add operator */
    isub, 		/* Integer subtract operator */
    imul, 		/* Integer multiply operator */
    idiv,		/* Integer divide operator */
    flop, 		/* Any proper floating point op */
    fadd, 		/* FP add operator */
    fsub,		/* FP subtract operator */
    fmul, 		/* FP multiply operator */
    fmisc,              /* FP miscellaneous class type */
    madd,		/* The kind that do two at once */
    mmalu,              /* Multimedia ALU operator */
    mmmul,              /* Multimedia multiply operator */
    mmshf,              /* Multimedia shift operator */
    itrap,		/* Integer trap potential */
    safe,		/* Never traps -- always safe */
    ftrap,		/* Floating point trap potential */
    fdiv,		/* Floating point divides */
    sqrt,		/* Square root operator */
    memtrap,		/* Memory trap potential */
    unsafe,		/* Unsafe always */
    defs_fp,		/* Operator defines FP reg */
    defs_fcc,		/* Operator defines FP CC reg */
    defs_fcr,		/* Operator defines FCR reg */
    refs_fcr,		/* Operator uses FCR reg */
    defs_fpu_int,	/* Operator defs int val in FP reg */
    ior,		/* Integer logical OR operator */
    jump,		/* Jump operator */
    ijump,		/* Indirect jump operator */
    ixor,		/* Integer logical exclusive OR operator */
    iand,		/* Integer logical AND operator */
    icmp,		/* Integer compare operator */
    f_group,            /* Instruction must be first in an instruction group */
    l_group,            /* Instruction must be last in an instruction group */
    privileged,         /* Instruction is a privileged instruction */
    simulated,		/* Instruction is simulated, i.e. a macro */
    predicated,		/* Instruction is predicated */
    access_reg_bank,	/* Instruction accesses rotating register banks */
    side_effects,	/* Instruction has side effects */
    extern_effects,	/* Instruction has different kind of external effects other than volatile mem */
    branch_predict,	/* Branch prediction (but not actual xfer) */
    mem_fill_type,      /* Memory instructions which are fill/spill type */
    var_opnds,		/* Variable number of operands AND/OR results */
    base_update,	/* Instruction updates 'base' operand */
    loop_start,         /* Instruction marks the start of a zero-cost loop */
    use_shared_resource,/* Instruction uses at least one shared resources */
    has_multireg_operand;
			/* Instruction has at least 1 multireg operand */

  ISA_Properties_Begin ("xtensa");

/* ====================================================================
 *              Operator attributes descriptors
 * ====================================================================
 */

/* ===== Move operator ====== */
  move = ISA_Property_Create ("move", true);
  Instruction_Group (move,
                     TOP_mov_n,
                     TOP_UNDEFINED);

/* ===== Memory load operator ====== */
  load = ISA_Property_Create ("load", true);
  Instruction_Group (load,
                     TOP_l16si,
                     TOP_l16ui,
                     TOP_l32ai,
                     TOP_l32e,
                     TOP_l32i,
                     TOP_l32i_n,
                     TOP_l32r,
                     TOP_l32si,
                     TOP_s32c1i,
                     TOP_l8ui,
                     TOP_UNDEFINED);

/* ===== Memory store operator ====== */
  store = ISA_Property_Create ("store", true);
  Instruction_Group (store,
                     TOP_s16i,
                     TOP_s32c1i,
                     TOP_s32e,
                     TOP_s32i,
                     TOP_s32i_n,
                     TOP_s32ri,
                     TOP_s8i,
                     TOP_UNDEFINED);

/* ===== Prefetch operator ====== */
  prefetch = ISA_Property_Create ("prefetch", false);
  Instruction_Group (prefetch,
                     TOP_UNDEFINED);

/* ===== Memory fill/spill type instructions ====== */
  mem_fill_type = ISA_Property_Create ("mem_fill_type", false);
  Instruction_Group (mem_fill_type,
		     TOP_UNDEFINED);

/* ===== Control transfer operator ====== */
  xfer = ISA_Property_Create ("xfer", true);
  Instruction_Group (xfer,
                     TOP_ball,
                     TOP_bany,
                     TOP_bbc,
                     TOP_bbci,
                     TOP_bbs,
                     TOP_bbsi,
                     TOP_beq,
                     TOP_beqi,
                     TOP_beqt,
                     TOP_beqz,
                     TOP_beqzt,
                     TOP_beqz_n,
                     TOP_bf,
                     TOP_bge,
                     TOP_bgei,
                     TOP_bgeu,
                     TOP_bgeui,
                     TOP_bgez,
                     TOP_blt,
                     TOP_blti,
                     TOP_bltu,
                     TOP_bltui,
                     TOP_bltz,
                     TOP_bnall,
                     TOP_bne,
                     TOP_bnei,
                     TOP_bnet,
                     TOP_bnez,
                     TOP_bnezt,
                     TOP_bnez_n,
                     TOP_bnone,
                     TOP_bt,                     
                     TOP_call0,
                     TOP_call12,
                     TOP_call4,
                     TOP_call8,
                     TOP_callx0,
                     TOP_callx12,
                     TOP_callx4,
                     TOP_callx8,
                     TOP_j,
                     TOP_jx,
		     TOP_loop,
		     TOP_loopgtz,
		     TOP_loopnez,
		     TOP_loop_end,
		     TOP_retw,
		     TOP_retw_n,
		     TOP_ret,
		     TOP_ret_n,
		     TOP_simcall,
		     TOP_syscall,
                     TOP_UNDEFINED);

/* ===== Subprogram call operator ====== */
  call = ISA_Property_Create ("call", true);
  Instruction_Group (call,
                     TOP_call0,
                     TOP_call12,
                     TOP_call4,
                     TOP_call8,
                     TOP_callx0,
                     TOP_callx12,
                     TOP_callx4,
                     TOP_callx8,
                     TOP_simcall,
                     TOP_syscall,
                     TOP_UNDEFINED);

/* ===== Call/xfer is conditional ====== */
  cond = ISA_Property_Create ("cond", true);
  Instruction_Group (cond,
                     TOP_ball,
                     TOP_bany,
                     TOP_bbc,
                     TOP_bbci,
                     TOP_bbs,
                     TOP_bbsi,
                     TOP_beq,
                     TOP_beqi,
                     TOP_beqt,
                     TOP_beqz,
                     TOP_beqzt,
                     TOP_beqz_n,
                     TOP_bf,
                     TOP_bge,
                     TOP_bgei,
                     TOP_bgeu,
                     TOP_bgeui,
                     TOP_bgez,
                     TOP_blt,
                     TOP_blti,
                     TOP_bltu,
                     TOP_bltui,
                     TOP_bltz,
                     TOP_bnall,
                     TOP_bne,
                     TOP_bnei,
                     TOP_bnet,
                     TOP_bnez,
                     TOP_bnezt,
                     TOP_bnez_n,
                     TOP_bnone,
                     TOP_bt,                     
		     TOP_loop,
		     TOP_loopgtz,
		     TOP_loopnez,
		     TOP_loop_end,
                     TOP_UNDEFINED);

/* ===== Cond call/xfer is likely ====== */
  likely = ISA_Property_Create ("likely", false);
  Instruction_Group (likely,
                     TOP_UNDEFINED);

/* ===== Result def is conditional ====== */
  cond_move = ISA_Property_Create ("cond_move", true);
  Instruction_Group (cond_move,
                     TOP_moveqz,
                     TOP_movf,
                     TOP_movgez,
                     TOP_movltz,
                     TOP_movnez,
                     TOP_movt,
                     TOP_UNDEFINED);

/* ===== Result must not be opnd ====== */
  uniq_res = ISA_Property_Create ("uniq_res", false);
  Instruction_Group (uniq_res,
                     TOP_UNDEFINED);

/* ===== Result must be same as opnd ====== */
  same_res = ISA_Property_Create ("same_res", true);
  Instruction_Group (same_res,
                     TOP_moveqz,
                     TOP_movf,
                     TOP_movgez,
                     TOP_movltz,
                     TOP_movnez,
                     TOP_movt,
		     //const16hi doesn't have an input register operand
		     //TOP_const16hi,
		     TOP_const16lo,
		     TOP_const16,
		     /* Because asm macros can share the same operand between
		        inputs and outputs, we conservatively mark this 
			property so that the compiler doesn't try to separate
			any common operands. */
                     TOP_asm, 
                     TOP_s32c1i,
                     TOP_UNDEFINED);

/* ===== Operator is a machine level select ====== */
  select = ISA_Property_Create ("select", false);
  Instruction_Group (select,
                     TOP_UNDEFINED);

/* ===== Unaligned load operator ====== */
  unalign_ld = ISA_Property_Create ("unalign_ld", false);
  Instruction_Group (unalign_ld,
                     TOP_UNDEFINED);

/* ===== Unaligned store operator ====== */
  unalign_store = ISA_Property_Create ("unalign_store", true);
  Instruction_Group (unalign_store,
		     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_memdata", true);
  Instruction_Group (unknown_memdata,
		     TOP_UNDEFINED);

/* ===== Unknown addr operator ====== */
  unknown_addr = ISA_Property_Create ("unknown_addr", true);
  Instruction_Group (unknown_addr,
                     TOP_l32ai,
                     TOP_s32c1i,
                     TOP_s32ri,
		     TOP_UNDEFINED);

/* ===== Integer add operator ====== */
  iadd = ISA_Property_Create ("iadd", false);
  Instruction_Group (iadd,
                     TOP_add,
                     TOP_add_n,
                     TOP_addi,
                     TOP_addi_n,
                     TOP_addmi,
                     TOP_UNDEFINED);

/* ===== Integer subtract operator ====== */
  isub = ISA_Property_Create ("isub", true);
  Instruction_Group (isub,
                     TOP_sub,
                     TOP_UNDEFINED);

/* ===== Integer multiply operator ====== */
  // Not sure we really have one of these.  All our multiply operators
  // are wierd.  Anyway, it's not used in any machine independent way.
  imul = ISA_Property_Create ("imul", false);
  Instruction_Group (imul,
                     TOP_UNDEFINED);

/* ===== Integer divide operator ====== */
  idiv = ISA_Property_Create ("idiv", false);
  Instruction_Group (idiv,
		     TOP_quou,
		     TOP_quos,
		     TOP_remu,
		     TOP_rems,
                     TOP_UNDEFINED);

/* ===== Any proper floating point op ====== */
  flop = ISA_Property_Create ("flop", false);
  Instruction_Group (flop,
                     TOP_UNDEFINED);

/* ===== FP add operator ====== */
  fadd = ISA_Property_Create ("fadd", false);
  Instruction_Group (fadd,
                     TOP_UNDEFINED);

/* ===== FP subtract operator ====== */
  fsub = ISA_Property_Create ("fsub", false);
  Instruction_Group (fsub,
                     TOP_UNDEFINED);

/* ===== FP multiply operator ====== */
  fmul = ISA_Property_Create ("fmul", false);
  Instruction_Group (fmul,
                     TOP_UNDEFINED);

/* ===== FP miscellaneous operator ====== */
  fmisc = ISA_Property_Create ("fmisc", false);
  Instruction_Group (fmisc,
		     TOP_UNDEFINED);

/* ===== The kind that do two at once ====== */
  // Used in isa/expand.cxx only and not for any purpose we could use.
  madd = ISA_Property_Create ("madd", false);
  Instruction_Group (madd,
                     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia ALU type ====== */
  mmalu = ISA_Property_Create ("mmalu", false);
  Instruction_Group (mmalu,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia shift (MMSHF) type ====== */
  mmshf = ISA_Property_Create ("mmshf", false);
  Instruction_Group (mmshf,
		     TOP_UNDEFINED);

/* ===== Instructions belonging to Multimedia multiply (MMMUL) type ====== */
  mmmul = ISA_Property_Create ("mmmul", false);
  Instruction_Group (mmmul,
		     TOP_UNDEFINED);

  noop = ISA_Property_Create ("noop", true);
  Instruction_Group (noop,
                     TOP_nop,
                     TOP_nop_n,
                     TOP_noop,
                     TOP_UNDEFINED);

  generic = ISA_Property_Create ("generic", true);
  Instruction_Group (generic,
                     TOP_UNDEFINED);

  dummy = ISA_Property_Create ("dummy", true);
  Instruction_Group (dummy,
		     TOP_begin_pregtn,
		     TOP_end_pregtn,
		     TOP_fwd_bar,
		     TOP_bwd_bar,
		     TOP_label,
		     TOP_noop,
		     TOP_UNDEFINED);

/* ====================================================================
 *              Exception classification descriptors
 * ====================================================================
 */

  /* ===== Integer trap potential ====== */
  itrap = ISA_Property_Create ("itrap", false);
  Instruction_Group (itrap,
                     TOP_UNDEFINED);

  /* ===== Never traps -- always safe ====== */
  safe = ISA_Property_Create ("safe", false);
  Instruction_Group (safe,
			// TODO
                     TOP_UNDEFINED);

  /* ===== Unsafe always ====== */
  unsafe = ISA_Property_Create ("unsafe", true);
  Instruction_Group (unsafe,
                     TOP_fwd_bar, TOP_bwd_bar,
                     TOP_UNDEFINED);

  /* ===== Floating point trap potential ====== */
  ftrap = ISA_Property_Create ("ftrap", false);
  Instruction_Group (ftrap,
                     TOP_UNDEFINED);

  /* ===== Floating point divides ====== */
  fdiv = ISA_Property_Create ("fdiv", false);
  Instruction_Group (fdiv,
                     TOP_UNDEFINED);

  /* ===== Square roots ====== */
  sqrt = ISA_Property_Create ("sqrt", false);
  Instruction_Group (sqrt,
                     TOP_UNDEFINED);

  /* ===== Memory trap potential ====== */
  memtrap = ISA_Property_Create ("memtrap", true);
  Instruction_Group (memtrap,
                     TOP_l16si,
                     TOP_l16ui,
                     TOP_l32ai,
                     TOP_l32e,
                     TOP_l32i,
                     TOP_l32i_n,
                     TOP_l32r,
                     TOP_l32si,
                     TOP_l8ui,
                     TOP_s16i,
                     TOP_s32e,
                     TOP_s32c1i,
                     TOP_s32i,
                     TOP_s32ri,
                     TOP_s32i_n,
                     TOP_s8i,
                     TOP_UNDEFINED);

  /* ===== Instruction must be first in an instruction group ====== */
  f_group = ISA_Property_Create ("f_group", false);
  Instruction_Group (f_group,
		     TOP_UNDEFINED);

  /* ===== Instruction must be last in an instruction group ====== */
  l_group = ISA_Property_Create ("l_group", false);
  Instruction_Group (l_group,
		     TOP_UNDEFINED);

  /* ===== Instruction is a privileged instruction ====== */
  privileged = ISA_Property_Create ("privileged", false);
  Instruction_Group (privileged,
		     TOP_UNDEFINED);

/* ====================================================================
 * Other operator descriptors (mostly for global optimization).
 * TODO: These descriptors should actually be determined from mips_operands.
 * ====================================================================
 */

/* ===== Operator defines FP CC reg ====== */
  defs_fcc = ISA_Property_Create ("defs_fcc", false);
  Instruction_Group (defs_fcc,
                     TOP_UNDEFINED);

/* ===== Operator defines FCR reg ====== */
  defs_fcr = ISA_Property_Create ("defs_fcr", false);
  Instruction_Group (defs_fcr,
                     TOP_UNDEFINED);

/* ===== Operator uses FCR reg ====== */
  refs_fcr = ISA_Property_Create ("refs_fcr", false);
  Instruction_Group (refs_fcr,
                     TOP_UNDEFINED);

/* ===== Operator defs int val in FP reg ====== */
  defs_fpu_int = ISA_Property_Create ("defs_fpu_int", false);
  Instruction_Group (defs_fpu_int,
                     TOP_UNDEFINED);

/* ===== Operator defines FP reg ====== */
  defs_fp = ISA_Property_Create ("defs_fp", false);
  Instruction_Group (defs_fp,
                     TOP_UNDEFINED);

/* ===== Logical OR operator ====== */
  ior = ISA_Property_Create ("ior", true);
  Instruction_Group (ior,
                     TOP_or,
                     TOP_orb,
                     TOP_UNDEFINED);

/* ===== Jump operator ====== */
  jump = ISA_Property_Create ("jump", true);
  Instruction_Group (jump,
                     TOP_j,
                     TOP_UNDEFINED);

/* ===== Indirect jump operator ====== */
  ijump = ISA_Property_Create ("ijump", true);
  Instruction_Group (ijump,
                     TOP_jx,
                     TOP_UNDEFINED);

/* ===== Logical exclusive OR operator ====== */
  ixor = ISA_Property_Create ("ixor", true);
  Instruction_Group (ixor,
                     TOP_xor,
                     TOP_xorb,
                     TOP_UNDEFINED);

/* ===== Logical AND operator ====== */
  iand = ISA_Property_Create ("iand", true);
  Instruction_Group (iand,
                     TOP_and,
                     TOP_andb,
                     TOP_UNDEFINED);

/* ===== Integer compare operator ====== */
  icmp = ISA_Property_Create ("icmp", false);
  Instruction_Group (icmp,
                     TOP_UNDEFINED);

/* ===== Simulated instructions ====== */
  simulated = ISA_Property_Create ("simulated", true);
  Instruction_Group (simulated,
                     TOP_asm,
		     TOP_const16hi,
		     TOP_const16lo,
                     TOP_extw_pseudo,
                     TOP_memw_pseudo,
                     TOP_spadjust,
                     TOP_intrncall,
                     TOP_noop,
		     TOP_loop_end,
		     TOP_load_const,
		     TOP_br_to_ar,
		     TOP_br2_to_ar,
		     TOP_br4_to_ar,
		     TOP_br8_to_ar,
		     TOP_br16_to_ar,
		     TOP_ar_to_br,
		     TOP_ar_to_br2,
		     TOP_ar_to_br4,
		     TOP_ar_to_br8,
		     TOP_ar_to_br16,
		     TOP_get_tmp_ar,
		     TOP_movbr,
		     TOP_movbr2,
		     TOP_movbr4,
		     TOP_movbr8,
		     TOP_movbr16,
		     TOP_wsr_br,
		     TOP_rsr_br,
		     TOP_wsr,
		     TOP_rsr,
		     TOP_xsr,
                     TOP_UNDEFINED);

/* ===== Predicated instructions ====== */
  predicated = ISA_Property_Create ("predicated", false);
  Instruction_Group (predicated,
                     TOP_UNDEFINED);

/* ===== Instructions access rotating register banks ====== */
  access_reg_bank = ISA_Property_Create ("access_reg_bank", false);
  Instruction_Group (access_reg_bank,
		     TOP_UNDEFINED);

/* ===== Instructions with side effects ====== */
  side_effects = ISA_Property_Create ("side_effects", true);
  /* IMPORTANT!

     There is a corresponding list of these in the assembler
     please be sure it is updated if you update this list. */


  Instruction_Group (side_effects,
		     /* Because asm macros can trash memory, we conservatively 
			mark this property so that the compiler doesn't move
			instructions around it. */
                     TOP_asm, 
		     TOP_break,
		     TOP_break_n,
		     TOP_chk,
		     TOP_dhi,
		     TOP_dhu,
		     TOP_dhwb,
		     TOP_dhwbi,
		     TOP_diwb,
		     TOP_diwbi,
		     TOP_dii,
		     TOP_diu,
		     TOP_dpfl,
		     TOP_dpfr,
		     TOP_dpfro,
		     TOP_dpfw,
		     TOP_dpfwo,
		     TOP_dsync,
		     TOP_entry,
		     TOP_esync,
		     TOP_excw,
		     TOP_extw,
		     TOP_extw_pseudo,
		     TOP_idtlb,
		     TOP_iitlb,
		     TOP_ihi,
		     TOP_ihu,
		     TOP_iii,
		     TOP_iiu,
                     TOP_ill, TOP_ill_n, 
		     TOP_inval,
		     TOP_ipf,
		     TOP_ipfl,
		     TOP_isync,
		     TOP_l32ai,
		     TOP_l32e,
		     TOP_ldct,
		     TOP_lict,
		     TOP_licw,
		     TOP_memw,
		     TOP_memw_pseudo,
		     TOP_movsp,
		     TOP_pdtlb,
		     TOP_pitlb,
		     TOP_rdtlb0,
		     TOP_rdtlb1,
		     TOP_ritlb0,
		     TOP_ritlb1,
		     TOP_rfdd,
		     TOP_rfde,
		     TOP_rfdo,
		     TOP_rfe,
		     TOP_rfi,
		     TOP_rfme,
		     TOP_rfue,
		     TOP_rfwo,
		     TOP_rfwu,
		     TOP_rotw,
		     TOP_rsil,
		     TOP_rsync,
                     TOP_s32c1i,
		     TOP_s32e,
		     TOP_s32ri,
		     TOP_sdct,
		     TOP_sict,
		     TOP_sicw,
		     TOP_simcall,
		     TOP_syscall,
		     TOP_waiti,
		     TOP_wdtlb,
		     TOP_witlb,
		     TOP_UNDEFINED);

/* ===== Instructions with side effects ====== */
  extern_effects = ISA_Property_Create ("extern_effects", true);
  Instruction_Group (extern_effects,
		     TOP_UNDEFINED);

/* ===== Instructions with branch predictions ====== */
  branch_predict = ISA_Property_Create ("branch_predict", false);
  Instruction_Group (branch_predict,
		     TOP_UNDEFINED);

/* ===== Instructions with variable number of operands/results ====== */
  var_opnds = ISA_Property_Create ("var_opnds", true);
  Instruction_Group (var_opnds,
                     TOP_asm,
		     TOP_UNDEFINED);

/* ===== Instructions that update 'base' operand ====== */
  base_update = ISA_Property_Create ("base_update", true);
  Instruction_Group (base_update,
		     TOP_UNDEFINED);

/* ===== Instructions that mark the head of a zero-cost loop ====== */
  loop_start = ISA_Property_Create ("loop_start", true);
  Instruction_Group (loop_start,
		     TOP_loop,
		     TOP_loopgtz,
		     TOP_loopnez,
		     TOP_UNDEFINED);

/* ===== Instructions that use at least one shared resource ====== */
  loop_start = ISA_Property_Create ("use_shared_resource", true);
  Instruction_Group (use_shared_resource,
		     TOP_UNDEFINED);

/* ===== Instructions that have multireg operands ====== */
  has_multireg_operand = ISA_Property_Create ("has_multireg_operand", true);
  Instruction_Group (has_multireg_operand,
		     TOP_all4,
		     TOP_all8,
		     TOP_any4,
		     TOP_any8,
		     TOP_br2_to_ar,
		     TOP_br4_to_ar,
		     TOP_br8_to_ar,
		     TOP_br16_to_ar,
		     TOP_ar_to_br2,
		     TOP_ar_to_br4,
		     TOP_ar_to_br8,
		     TOP_ar_to_br16,
		     TOP_movbr2,
		     TOP_movbr4,
		     TOP_movbr8,
		     TOP_movbr16,
		     TOP_UNDEFINED);

/* ===== Set the size for each opcode ====== */  
  for (int i = 0; i < TOP_count; ++i)
  {
    TOP top = (TOP)i;
    switch (top)
    {
    default:
      Instruction_Size(3, top, TOP_UNDEFINED);
      break;

    case TOP_add_n:
    case TOP_addi_n:
    case TOP_beqz_n:
    case TOP_bnez_n:
    case TOP_break_n:
    case TOP_l32i_n:
    case TOP_mov_n:
    case TOP_movi_n:
    case TOP_nop_n:
    case TOP_ret_n:
    case TOP_retw_n:
    case TOP_s32i_n:
      Instruction_Size(2, top, TOP_UNDEFINED);
      break;
    }
  }

  ISA_Properties_End();
  return 0;
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
