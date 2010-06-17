/*

  Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.

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
/*
 * Functions to test Xtensa immediate operand validity.
 */

/* $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/targ_info/isa/xtensa/xtensa-immed.c#1 $ */


/* This code is automatically generated -- DO NOT EDIT */

int xtensa_msalp32(v)
  int v;
{
  switch (v) {
    case 32:
    case 31:
    case 30:
    case 29:
    case 28:
    case 27:
    case 26:
    case 25:
    case 24:
    case 23:
    case 22:
    case 21:
    case 20:
    case 19:
    case 18:
    case 17:
    case 16:
    case 15:
    case 14:
    case 13:
    case 12:
    case 11:
    case 10:
    case 9:
    case 8:
    case 7:
    case 6:
    case 5:
    case 4:
    case 3:
    case 2:
    case 1:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_simm8x256(v)
  int v;
{
  return (v & 255) == 0 && (v >= -32768 && v <= 32512);
}

int xtensa_ai4const(v)
  int v;
{
  switch (v) {
    case -1:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_soffsetx4(v)
  int v;
{
  return (v & 3) == 0 && (v >= -524288 && v <= 524284);
}

int xtensa_lsi4x4(v)
  int v;
{
  return (v & 3) == 0 && (v >= 0 && v <= 60);
}

int xtensa_simm4(v)
  int v;
{
  return v >= -8 && v <= 7;
}

int xtensa_uimm12x8(v)
  int v;
{
  return (v & 7) == 0 && (v >= 0 && v <= 32760);
}

int xtensa_uimm16x4(v)
  int v;
{
  return (v & 3) == 0 && (v >= -262144 && v <= -4);
}

int xtensa_simm7(v)
  int v;
{
  return v >= -32 && v <= 95;
}

int xtensa_uimm6(v)
  int v;
{
  return v >= 0 && v <= 63;
}

int xtensa_simm8(v)
  int v;
{
  return v >= -128 && v <= 127;
}

int xtensa_simm16(v)
  int v;
{
  return v >= -32768 && v <= 32767;
}

int xtensa_simm32(v)
  int v;
{
  return 1;
}

int xtensa_uimm8(v)
  int v;
{
  return v >= 0 && v <= 255;
}

int xtensa_const16(v)
  int v;
{
  //the const16 takes only 16 bits, but the assembler syntax for
  //is const16 a0, 32_bit_immed@hi
  //is const16 a0, 32_bit_immed@lo
  //and the assembler strips out the right bits for you
  return 1;
}

int xtensa_b4constu(v)
  int v;
{
  switch (v) {
    case 32768:
    case 65536:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
    case 16:
    case 32:
    case 64:
    case 128:
    case 256:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_op2p1(v)
  int v;
{
  switch (v) {
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_x(v)
  int v;
{
  switch (v) {
    case 0:
    case 1:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_y(v)
  int v;
{
  switch (v) {
    case 2:
    case 3:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_simm12(v)
  int v;
{
  return v >= -2048 && v <= 2047;
}

int xtensa_not_simm12(v)
  int v;
{
  return !xtensa_simm12(v);
}

int xtensa_soffset(v)
  int v;
{
  return v >= -131072 && v <= 131071;
}

int xtensa_uimm8x2(v)
  int v;
{
  return (v & 1) == 0 && (v >= 0 && v <= 510);
}

int xtensa_tp7(v)
  int v;
{
  switch (v) {
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
    case 14:
    case 15:
    case 16:
    case 17:
    case 18:
    case 19:
    case 20:
    case 21:
    case 22:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_uimm4x16(v)
  int v;
{
  return (v & 15) == 0 && (v >= 0 && v <= 240);
}

int xtensa_uimm8x4(v)
  int v;
{
  return (v & 3) == 0 && (v >= 0 && v <= 1020);
}

int xtensa_neg_uimm8x4(v)
  int v;
{
  return (v & 3) == 0 && (v >= -1024 && v <= -4);
}

int xtensa_b4const(v)
  int v;
{
  switch (v) {
    case -1:
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
    case 16:
    case 32:
    case 64:
    case 128:
    case 256:
    {
      return 1;
    }
    break;
  }
  return 0;
}

int xtensa_uimm4(v)
  int v;
{
  return v >= 0 && v <= 15;
}

int xtensa_uimm5(v)
  int v;
{
  return v >= 0 && v <= 31;
}


