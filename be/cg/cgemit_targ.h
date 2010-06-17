
/* 
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
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


// whether to use the base st for the reloc.
extern BOOL CGEMIT_Use_Base_ST_For_Reloc (INT reloc, ST *st);

//Build complete name for symbol
extern char * Build_Qualified_Name (ST *st, char *buf, UINT sz);

// put reloc info into asm buffer, returning # parens to close with.
extern INT CGEMIT_Relocs_In_Asm (TN *t, ST *st, vstring *buf, INT64 *val);
extern void CGEMIT_Relocs_In_Asm_Suffix (TN *t, vstring *buf);
// put reloc info into object file.
extern void CGEMIT_Relocs_In_Object (
	TN *t, ST *st, INT32 PC, pSCNINFO PU_section, INT64 *val);

// add events and relocs as needed for call.
extern void CGEMIT_Add_Call_Information (
	OP *op, BB *bb, INT32 PC, pSCNINFO PU_section);

/* Emit assembly to change from section 'cur_section' to section 'st' 
   'cur_section'. Section attibutes are extracted from 'st'. */
void
CGEMIT_Change_Section_In_Asm (ST *st, ST *cur_section);

/* Emit assembly to change to section 'scn_name'. Section attibutes
   are explicitly specified by 'scn_type', 'scn_flags', 'scn_entsize',
   and 'scn_align'. 

   Note that this does absolutely *nothing* to get out of the current
   section correctly. If you call this function directly, you must
   be careful to do that by hand. You really should avoid using this
   function and use "CGEMIT_Change_Section_In_Asm" instead unless you 
   have a very good reason not to.
*/
extern void CGEMIT_Prn_Scn_In_Asm (FILE *asm_file,
				   const char *scn_name,
				   Elf64_Word  scn_type,
				   Elf64_Word  scn_flags,
				   Elf64_Xword scn_entsize,
				   Elf64_Word  scn_align);

/* Emit assembly to change origin in section 'st' to 'offset',
   aligning to 'align' */
extern void CGEMIT_Change_Origin_In_Asm (ST *st, INT64 offset, INT32 align);

/* Return true if section 'scn_name' should be aligned only once per
   assembly file. */
BOOL CGEMIT_Align_Section_Once (const char *scn_name);

extern void
CGEMIT_Prn_Line_Dir_In_Asm (USRCPOS usrcpos);
extern void
CGEMIT_Prn_File_Dir_In_Asm(USRCPOS usrcpos,
                        const char *pathname,
                        const char *filename);

// Generate the .frame, .mask and the .fmask directives for the assembler.
extern void CGEMIT_Gen_Asm_Frame (INT64 frame_len);

// Prepass before emit to fix up fp registers for IA-32.
// Noop for every other target.
extern void STACK_FP_Fixup_PU();

// Generate the entry (.proc) directive.
extern void CGEMIT_Prn_Ent_In_Asm (ST *pu);

// generate weak alias directive.
extern void CGEMIT_Weak_Alias (ST *sym, ST *strongsym);

// generate alias directive.
extern void CGEMIT_Alias (ST *sym, ST *strongsym);

/* Initialize/finalize target for emission of text for 'pu'. Called 
   before/after each processing unit is emitted. */
extern void CGEMIT_Targ_Text_Initialize (ST *pu);
extern void CGEMIT_Targ_Text_Finalize (ST *pu);
void CGEMIT_Write_Literal_TCON(ST *lit_st, TCON tcon);
void CGEMIT_Write_Literal_Label (ST *lit_st, LABEL_IDX lab);
void CGEMIT_Write_Literal_Symbol (ST *lit_st, ST *sym, 
				  Elf64_Sxword sym_ofst);
