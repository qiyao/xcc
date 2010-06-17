/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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


/* ====================================================================
 *
 * Module: cgemit_targ.c
 * $Revision: 1.29 $
 * $Date: 2000/04/06 01:50:47 $
 * $Author: mtibuild $
 * $Source: /osprey.src/osprey1.0/be/cg/ia64/RCS/cgemit_targ.cxx,v $
 *
 * Description:
 *
 * Target-specific cgemit code.
 *
 * ====================================================================
 * ====================================================================
 */


#include <elf.h>

#define	USE_STANDARD_TYPES 1
#include "defs.h"

#include <alloca.h>

#include "targ_const.h"
#include "targ_const_private.h"
#include "vstring.h"
#include "config_asm.h"
#include "em_elf.h"
#include "symtab.h"
#include "tn.h"
#include "data_layout.h"
#include "bb.h"
#include "op.h"
#include "iface_scn.h"
#include "cg_flags.h"
#include "glob.h"
#include "sections.h"
#include "erglob.h"
#include "errors.h"
#include "cgemit.h"
#include "cgemit_targ.h"

static BOOL
Non_Default_Text_Section (ST *pu)
{
  if (!pu || !ST_base(pu))
    return FALSE;

  return ((ST_sclass(ST_base(pu)) == SCLASS_TEXT) && 
	  strcmp(ST_name(ST_base(pu)), ".text"));
}


void
CGEMIT_Targ_Text_Initialize (ST *pu)
{
  fprintf(Asm_File, "\t.literal_position\n");
}


void
CGEMIT_Targ_Text_Finalize (ST *pu)
{
}


void 
CGEMIT_Targ_End_Section (ST * sec)
{
  if (!sec || !ST_base(sec))
    return;

  if ((ST_sclass(ST_base(sec)) == SCLASS_TEXT))
    CGEMIT_Targ_Text_Finalize(sec);
}


BOOL
CGEMIT_Align_Section_Once (const char *scn_name)
{
  return strcmp(scn_name, ".literal") && strcmp(scn_name, ".text");
}


#ifdef _WIN32

// Replace "\" with "\\"
// Assumes new_name is big enough to hold new string
static void double_back(const char *old_name, char *new_name)
{
  int old_index=0;
  int new_index=0;
  while (old_name[old_index]) {
    if (old_name[old_index] == '\\') {
      new_name[new_index++] = '\\';
    } 
    new_name[new_index++] = old_name[old_index++];
  } 
  new_name[new_index] = 0;
}

#include "cxx_memory.h"

void
CGEMIT_Prn_File_Dir_In_Asm(USRCPOS usrcpos,
			   const char *pathname,
			   const char *filename)
{
  char *fixpath = CXX_NEW_ARRAY(char, 2*strlen(pathname)+1, &MEM_phase_pool);
  char *fixfile = CXX_NEW_ARRAY(char, 2*strlen(filename)+1, &MEM_phase_pool);
  double_back(pathname, fixpath);
  double_back(filename, fixfile);
  if (USRCPOS_filenum(usrcpos) == 0) {
    fprintf (Asm_File, "\t%s\t\"%s\"\n", AS_FILE, fixfile);
  } else {
    fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	     AS_FILE, USRCPOS_filenum(usrcpos), fixpath, fixfile);
  }
  CXX_DELETE_ARRAY(fixpath, &MEM_phase_pool);
  CXX_DELETE_ARRAY(fixfile, &MEM_phase_pool);
}

#else /* !_WIN32 */

void
CGEMIT_Prn_File_Dir_In_Asm(USRCPOS usrcpos,
			   const char *pathname,
			   const char *filename)
{
  if (USRCPOS_filenum(usrcpos) == 0) {
    fprintf (Asm_File, "\t%s\t\"%s\"\n", AS_FILE, filename);
  } else {
    fprintf (Asm_File, "\t%s\t%d\t\"%s/%s\"\n",
	     AS_FILE, USRCPOS_filenum(usrcpos), pathname, filename);
  }
}

#endif /* !_WIN32 */

extern void
CGEMIT_Prn_Line_Dir_In_Asm (USRCPOS usrcpos)
{
  if( !CG_emit_asm_dwarf) { 
    fprintf (Asm_File, " # "); //turn the rest into comment
  }
  fprintf (Asm_File, "\t.loc\t%d\t%d\t%d\n", 
	   USRCPOS_filenum(usrcpos),
	   USRCPOS_linenum(usrcpos),
	   USRCPOS_column(usrcpos));    
}


/* This is the function to use for most things. You should only use
   CGEMIT_Prn_Scn_In_Asm if you really know what you are doing. */

void
CGEMIT_Change_Section_In_Asm (ST *st, ST *cur_section)
{
  const char * scn_name = ST_name(st);

  // literal sections are archaic T1040 technology.
  // the ".literal" directive and .literal_prefix 
  // now handles all the special section stuff associated 
  // with literals
  assert (strcmp(scn_name, ".literal") != 0);

  CGEMIT_Prn_Scn_In_Asm(Asm_File,
			ST_name(st),
			Get_Section_Elf_Type(STB_section_idx(st)),
			Get_Section_Elf_Flags(STB_section_idx(st)),
			Get_Section_Elf_Entsize(STB_section_idx(st)),
			STB_align(st));
}


/* see the note about this function in the header. */
void
CGEMIT_Prn_Scn_In_Asm (FILE       *asm_file,
		       const char *scn_name,
		       Elf64_Word  scn_type,
		       Elf64_Word  scn_flags,
		       Elf64_Xword scn_entsize,
		       Elf64_Word  scn_align)
{
  if (!strcmp(scn_name, ".rodata") && (scn_flags & SHF_WRITE)) {
    ErrMsg(EC_Asm_Rodata);
  } 
  
  if (!strcmp(scn_name, ".data") || !strcmp(scn_name, ".text"))
  {
    fprintf (asm_file, "\n\t%s", scn_name);
  }
  else
  {
    char scn_flags_string[5];
    char *p = &scn_flags_string[0];
    
    fprintf (asm_file, "\n\t%s\t%s,", AS_SECTION, scn_name);
    if (scn_flags & SHF_WRITE) *p++ = 'w';
    if (scn_flags & SHF_ALLOC) *p++ = 'a';
    if (scn_flags & SHF_EXECINSTR) *p++ = 'x';
    if (scn_flags & SHF_MERGE) *p++ = 'M';
    if (scn_flags & SHF_STRINGS) *p++ = 'S';
    *p = '\0'; // null terminate the string.
    fprintf (asm_file, " \"%s\"", scn_flags_string);
    if (scn_flags & SHF_STRINGS) {
      fprintf(asm_file, " ,1 ");
    }
  }

  fprintf (asm_file, "\n");
}


void
CGEMIT_Change_Origin_In_Asm (ST *st, INT64 offset, INT32 align)
{
  /* Make sure these match what is used in eh_region.cxx (with "t"
     changed to "e" or "h"). */
#define EH_REGION_LINKONCE_PREFIX ".gnu.linkonce.e."
#define EH_DESC_LINKONCE_PREFIX ".gnu.linkonce.h."
    
  /* We don't want to emit literals, because they are no longer 
     handled as sections.

     We don't want to emit .org for exception region or descriptors
     since the section contains both .xt_except_table/.xt_except_desc
     and .gnu.linkonce.e.* / .gnu.linkonce.h.* sections. We don't need
     the .org for these because there are no alignment issues since
     all INITVs in the section are 4 bytes, and the section start is 4
     byte aligned. */

  if ((ST_sclass(st) != SCLASS_MERGE_STRING) &&
      strcmp(ST_name(st), ".literal") /* &&
      strcmp(ST_name(st), ".xt_except_table") &&
      strcmp(ST_name(st), ".xt_desc_table") &&
      strncmp(ST_name(st), EH_REGION_LINKONCE_PREFIX,
	      strlen(EH_REGION_LINKONCE_PREFIX)) &&
      strncmp(ST_name(st), EH_DESC_LINKONCE_PREFIX,
      strlen(EH_DESC_LINKONCE_PREFIX))*/)
  {
    fprintf (Asm_File, "\t%s 0x%" LLX_FMT "\n", AS_ORIGIN, offset);
    /* generate a '.align 0' to make sure we don't autoalign, do we
       need this for xtensa's gas? */
    fprintf ( Asm_File, "\t%s\t%d\n", AS_ALIGN, align );
  }
}


// whether to use the base st for the reloc
extern BOOL
CGEMIT_Use_Base_ST_For_Reloc (INT reloc, ST *st)
{
  FmtAssert(((reloc == TN_RELOC_HIGH16) || (reloc == TN_RELOC_LOW16)), \
    ("Illegal relocation"));
  return false;
}

extern INT
CGEMIT_Relocs_In_Asm (TN *t, ST *st, vstring *buf, INT64 *val)
{
  INT paren = 1;  // num parens
  // only add in GP_DISP if based on gprel section
  // not if based on ipa-generated extern.
  if (ST_class(st) == CLASS_BLOCK && STB_section(st)) {
    *val -= GP_DISP;
  }
  switch (TN_relocs(t)) {
  case TN_RELOC_HIGH16:
  case TN_RELOC_LOW16:
    break;
  default:
    FmtAssert (FALSE, ("relocs_asm: illegal reloc TN"));
    /*NOTREACHED*/
  }
  *buf = vstr_concat (*buf, "(" );
  UINT sz = ((ST_name(st) && *(ST_name(st)) != '\0') ?
	     strlen(ST_name(st)) : strlen(ST_name(ST_base(st)))) + 30;
  char *tbuf = (char *) alloca(sz);
  Build_Qualified_Name(st, tbuf, sz);
  *buf = vstr_concat(*buf, tbuf);
  if (*Symbol_Name_Suffix != '\0')
    *buf = vstr_concat(*buf, Symbol_Name_Suffix);
  return paren;
}

	  
extern void
CGEMIT_Relocs_In_Asm_Suffix (TN *t, vstring *buf)
{
	switch (TN_relocs(t)) {
	case TN_RELOC_HIGH16:
        	*buf = vstr_concat (*buf, "@h");
		break;
	case TN_RELOC_LOW16:
        	*buf = vstr_concat (*buf, "@l");
		break;

    	default:
    		FmtAssert (FALSE, ("relocs_asm: illegal reloc TN"));
		/*NOTREACHED*/
	}
}


extern void
CGEMIT_Relocs_In_Object (TN *t, ST *st, INT32 PC, pSCNINFO PU_section, INT64 *val)
{
    FmtAssert(FALSE, ("Xtensa does not support direct object file writes"));
    
#if TENSILICA_Object_Code
	// only add in GP_DISP if based on gprel section
	// not if based on ipa-generated extern.
	if (ST_class(st) == CLASS_BLOCK && STB_section(st)) {
		*val -= GP_DISP;
	}
	switch (TN_relocs(t)) {
	case TN_RELOC_IA_GPREL22:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_GPREL22, PC, *val, PU_section);
	      	*val = 0;
		break;
	case TN_RELOC_IA_LTOFF22:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_LTOFF22, PC, *val, PU_section);
		*val = 0;
		break;
	case TN_RELOC_IA_LTOFF_FPTR:
		Em_Add_New_Rela (EMT_Put_Elf_Symbol (st), 
			R_IA_64_LTOFF_FPTR22, PC, *val, PU_section);
		*val = 0;
		break;
	default:
	      #pragma mips_frequency_hint NEVER
	      FmtAssert (FALSE, ("relocs_object: illegal reloc TN"));
	}
#endif
} 

// add events and relocs as needed for call
extern void 
CGEMIT_Add_Call_Information (OP *op, BB *bb, INT32 PC, pSCNINFO PU_section)
{
	ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_CALLINFO);
	ST *call_sym = CALLINFO_call_st(ANNOT_callinfo(ant));
    	Elf_Event_Kind event_type;

	if (call_sym == NULL) return;
	if (ST_is_export_local(call_sym)) {
		event_type = EK_FCALL_LOCAL;
	}
	else {
		event_type = EK_FCALL_EXTERN;
      	}
	Em_Add_New_Event (event_type, PC, EMT_Put_Elf_Symbol(call_sym),
			0, 0, PU_section);
      
	// TODO: if indirect call add plt reloc

	// do pcrel relocation for all calls,
	// as even statics may be forward refs so don't know pc.
	// Ld will generate a stub if needed.
	Em_Add_New_Rela (EMT_Put_Elf_Symbol(call_sym), 
		R_IA_64_PCREL21B, PC, 0, PU_section);

      	if (EMIT_interface_section) {
		Interface_Scn_Add_Call( call_sym, 
			CALLINFO_call_wn(ANNOT_callinfo(ant)));
      	}
}


void
CGEMIT_Gen_Asm_Frame (INT64 frame_len)
{
  /* Xtensa doesn't use .frame anymore. */
}


// Generate the entry (.proc) directive.
void 
CGEMIT_Prn_Ent_In_Asm (ST *pu)
{
  /* Xtensa doesn't have a .proc... */
}


// Preprocess FP registers before emit.  Needed only for IA-32.
void
STACK_FP_Fixup_PU()
{}

void
CGEMIT_Weak_Alias (ST *sym, ST *strongsym) 
{
        fprintf ( Asm_File, "\t%s\t%s\n", AS_WEAK, ST_name(sym));
        fprintf ( Asm_File, "\t.set %s, %s\n", ST_name(sym), ST_name(strongsym));
}

void
CGEMIT_Alias (ST *sym, ST *strongsym) 
{
        fprintf ( Asm_File, "\t.set %s, %s\n", ST_name(sym), ST_name(strongsym));
}

void CGEMIT_Write_Literal_TCON(ST *lit_st, TCON tcon)
{
  INT64 val;
  if (TCON_ty(tcon) == MTYPE_F4)
    val = TCON_word0(tcon);
  else if ((TCON_ty(tcon) == MTYPE_I4) || (TCON_ty(tcon) == MTYPE_U4))
    val = TCON_v0(tcon);
  else
    FmtAssert(FALSE, ("Invalid literal value"));
  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  if ((val >= INT32_MIN) && (val <= INT32_MAX)) 
    fprintf(Asm_File, ", %" LLD_FMT "\n", val);
  else
    fprintf(Asm_File, ", 0x%" LLX_FMT "\n", val);
  
}

void CGEMIT_Write_Literal_Label (ST *lit_st, LABEL_IDX lab)
{
  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  fprintf(Asm_File, ", %s\n", LABEL_name(lab));
}

void CGEMIT_Write_Literal_Symbol (ST *lit_st, ST *sym, 
				  Elf64_Sxword sym_ofst)
{
  ST *basesym;
  basesym = sym;
  INT64 base_ofst = 0;

  if (Has_Base_Block(sym) && ST_is_export_local(sym) && ST_class(sym) != CLASS_FUNC) {
    Base_Symbol_And_Offset (sym, &basesym, &base_ofst);
  }
  base_ofst += sym_ofst;

  fprintf ( Asm_File, "\t%s\t", ".literal");
  EMT_Write_Qualified_Name(Asm_File, lit_st);
  fprintf ( Asm_File, ", ");
  if (ST_class(sym) == CLASS_CONST ) {
    EMT_Write_Qualified_Name (Asm_File, basesym);
    if (base_ofst == 0)
      fprintf (Asm_File, "\n");
    else
      fprintf (Asm_File, " %+"LLD_FMT"\n", base_ofst);
  }
  else {
    EMT_Write_Qualified_Name (Asm_File, sym);
    if (sym_ofst == 0)
      fprintf (Asm_File, "\n");
    else
      fprintf (Asm_File, " %+"LLD_FMT"\n", sym_ofst);
  }
}

// Local Variables:
// mode: c++
// c-file-style: "mongoose"
// End:
