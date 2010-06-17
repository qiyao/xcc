
// Copyright (c) 2003-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.
//
// Module: cg_special_op.h
//
// Description:
//
//   External interface to the Specialization/Relaxation of opcodes.
//   A special opcode is a concrete opcode while a generic opcode
//   can be realized with one of the special opcodes. A generic opcode
//   normally has less restrict bundling constraint. Therefore, it is
//   beneficial to relax the special opcodes before scheduling phases
//   and then specialize after the bundling is done. Note however, it is
//   better to limit the exposure of generic opcode since many CG passes
//   have code that matches specific opcode (e,g, EBO) and they will not
//   do well with generic opcodes.
//
// Utilities:
//
//   extern void CG_Relax_Special_Ops()
//     Scan all ops in the current region and replace special ops with
//     generic ones.
//
//   extern void CG_Relax_Special_Op(OP* op);
//     Replace the opcode with one of the generic opcode if possible.
//
//   extern void CG_Specialize_Op(OP* op);
//     Replace the opcode with one of the special opcode assuming the
//     format and slot has been determined, i.e., bundling is done.
//
//   extern TOP CG_Get_Generic_Top(TOP top);
//     Return the generic top for the special 'top'
//     Return the input 'top' if there is no generic version
//
//   extern bool Is_Generic_Addi_Op(OP* op);
//     Return true if the topcode of 'op' is a generic addi

#include "op.h"
#include "tn.h"
#include "bb.h"
#include "libti.h"

extern void CG_Relax_Special_Ops();
extern void CG_Relax_Special_Op(OP* op);
extern void CG_Specialize_Op(OP* op);
extern void CG_Convert_To_Predicted_Branch(BB* bb);
extern TOP CG_Get_Generic_Top(TOP special_top);
extern bool Is_Generic_Addi_Op(OP* op);

