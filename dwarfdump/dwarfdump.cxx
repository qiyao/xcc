
/* 
   Copyright (C) 2001-2004 Tensilica, Inc.  All Rights Reserved.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <libelf/libelf.h>
#include <stdio.h>
#include <elf.h>
#include <dwarf.h>
#include <libdwarf.h>
#include "../common/com/defs.h"


// This program uses libdwarf to dump dwarf2 debug information

char step_name[256];		// string buffer
Dwarf_Debug dbg=NULL;
int die_id=0;
int cu_id=0;
int verbose=0;
int fd=0;
Dwarf_Unsigned cu_offset=0;

// called at beginning of each step
void New_Step(const char* s) {
  printf("====== %s ======\n", s);
  sprintf(step_name,"%s", s);
}

// default error handler
void er_handler(Dwarf_Error error, Dwarf_Ptr errarg) {

  fprintf(stderr, "ERROR %s: %s\n",
		  dwarf_errmsg(error),
		  (char*)errarg);
}

// clean up before exit or abort
void finish() {

  close(fd);
  dwarf_finish(dbg,NULL);
}

// call finish then abort
void dd_abort() {
  finish();
  abort();
}

// give no entry message and abort
void no_entry_abort(char* obj_name) {
  fprintf(stderr, "ERROR No entry for %s, abort\n", obj_name);
  dd_abort();
}

// give no entry message and continue
void no_entry_msg(char* obj_name) {
  printf("No entry for %s\n", obj_name);
}

// handle location attribute value
void print_loc(Dwarf_Loc* loc_p) {

  if (loc_p==NULL)
    return;

  Dwarf_Small lr_atom = loc_p->lr_atom;
  Dwarf_Unsigned lr_number = loc_p->lr_number;
  Dwarf_Unsigned lr_number2 = loc_p->lr_number2;
  Dwarf_Unsigned lr_offset = loc_p->lr_offset;

  switch (lr_atom) {

    case DW_OP_addr: printf("OP_addr"); printf("(0x%Lx)",lr_number); break;
    case DW_OP_deref: printf("OP_deref"); break;
    case DW_OP_const1u: printf("OP_const1u"); printf("(%Lu)",lr_number); break;
    case DW_OP_const1s: printf("OP_const1s"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_const2u: printf("OP_const2u"); printf("(0x%Lx)",lr_number); break;
    case DW_OP_const2s: printf("OP_const2s"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_const4u: printf("OP_const4u"); printf("(0x%Lx)",lr_number); break;
    case DW_OP_const4s: printf("OP_const4s"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_const8u: printf("OP_const8u"); printf("(0x%Lx)",lr_number); break;
    case DW_OP_const8s: printf("OP_const8s"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_constu: printf("OP_constu"); printf("(%Lu)",lr_number); break;
    case DW_OP_consts: printf("OP_consts"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_dup: printf("OP_dup"); break;
    case DW_OP_drop: printf("OP_drop"); break;
    case DW_OP_over: printf("OP_over"); break;
    case DW_OP_pick: printf("OP_pick"); printf("(0x%d)",(signed char)lr_number); break;
    case DW_OP_swap: printf("OP_swap"); break;
    case DW_OP_rot: printf("OP_rot"); break;
    case DW_OP_xderef: printf("OP_xderef"); break;
    case DW_OP_abs: printf("OP_abs"); break;
    case DW_OP_and: printf("OP_and"); break;
    case DW_OP_div: printf("OP_div"); break;
    case DW_OP_minus: printf("OP_minus"); break;
    case DW_OP_mod: printf("OP_mod"); break;
    case DW_OP_mul: printf("OP_mul"); break;
    case DW_OP_neg: printf("OP_neg"); break;
    case DW_OP_not: printf("OP_not"); break;
    case DW_OP_or: printf("OP_or"); break;
    case DW_OP_plus: printf("OP_plus"); break;
    case DW_OP_plus_uconst: printf("OP_plus_uconst"); printf("(%Lu)",lr_number); break;
    case DW_OP_shl: printf("OP_shl"); break;
    case DW_OP_shr: printf("OP_shr"); break;
    case DW_OP_shra: printf("OP_shra"); break;
    case DW_OP_xor: printf("OP_xor"); break;
    case DW_OP_skip: printf("OP_skip"); printf("(0x%d)",(signed short)lr_number); break;
    case DW_OP_bra: printf("OP_bra"); printf("(0x%d)",(signed short)lr_number); break;
    case DW_OP_eq: printf("OP_eq"); break;
    case DW_OP_ge: printf("OP_ge"); break;
    case DW_OP_gt: printf("OP_gt"); break;
    case DW_OP_le: printf("OP_le"); break;
    case DW_OP_lt: printf("OP_lt"); break;
    case DW_OP_ne: printf("OP_ne"); break;
    case DW_OP_lit0: printf("OP_lit0"); break;
    case DW_OP_lit1: printf("OP_lit1"); break;
    case DW_OP_lit2: printf("OP_lit2"); break;
    case DW_OP_lit3: printf("OP_lit3"); break;
    case DW_OP_lit4: printf("OP_lit4"); break;
    case DW_OP_lit5: printf("OP_lit5"); break;
    case DW_OP_lit6: printf("OP_lit6"); break;
    case DW_OP_lit7: printf("OP_lit7"); break;
    case DW_OP_lit8: printf("OP_lit8"); break;
    case DW_OP_lit9: printf("OP_lit9"); break;
    case DW_OP_lit10: printf("OP_lit10"); break;
    case DW_OP_lit11: printf("OP_lit11"); break;
    case DW_OP_lit12: printf("OP_lit12"); break;
    case DW_OP_lit13: printf("OP_lit13"); break;
    case DW_OP_lit14: printf("OP_lit14"); break;
    case DW_OP_lit15: printf("OP_lit15"); break;
    case DW_OP_lit16: printf("OP_lit16"); break;
    case DW_OP_lit17: printf("OP_lit17"); break;
    case DW_OP_lit18: printf("OP_lit18"); break;
    case DW_OP_lit19: printf("OP_lit19"); break;
    case DW_OP_lit20: printf("OP_lit20"); break;
    case DW_OP_lit21: printf("OP_lit21"); break;
    case DW_OP_lit22: printf("OP_lit22"); break;
    case DW_OP_lit23: printf("OP_lit23"); break;
    case DW_OP_lit24: printf("OP_lit24"); break;
    case DW_OP_lit25: printf("OP_lit25"); break;
    case DW_OP_lit26: printf("OP_lit26"); break;
    case DW_OP_lit27: printf("OP_lit27"); break;
    case DW_OP_lit28: printf("OP_lit28"); break;
    case DW_OP_lit29: printf("OP_lit29"); break;
    case DW_OP_lit30: printf("OP_lit30"); break;
    case DW_OP_lit31: printf("OP_lit31"); break;
    case DW_OP_reg0: printf("OP_reg0"); break;
    case DW_OP_reg1: printf("OP_reg1"); break;
    case DW_OP_reg2: printf("OP_reg2"); break;
    case DW_OP_reg3: printf("OP_reg3"); break;
    case DW_OP_reg4: printf("OP_reg4"); break;
    case DW_OP_reg5: printf("OP_reg5"); break;
    case DW_OP_reg6: printf("OP_reg6"); break;
    case DW_OP_reg7: printf("OP_reg7"); break;
    case DW_OP_reg8: printf("OP_reg8"); break;
    case DW_OP_reg9: printf("OP_reg9"); break;
    case DW_OP_reg10: printf("OP_reg10"); break;
    case DW_OP_reg11: printf("OP_reg11"); break;
    case DW_OP_reg12: printf("OP_reg12"); break;
    case DW_OP_reg13: printf("OP_reg13"); break;
    case DW_OP_reg14: printf("OP_reg14"); break;
    case DW_OP_reg15: printf("OP_reg15"); break;
    case DW_OP_reg16: printf("OP_reg16"); break;
    case DW_OP_reg17: printf("OP_reg17"); break;
    case DW_OP_reg18: printf("OP_reg18"); break;
    case DW_OP_reg19: printf("OP_reg19"); break;
    case DW_OP_reg20: printf("OP_reg20"); break;
    case DW_OP_reg21: printf("OP_reg21"); break;
    case DW_OP_reg22: printf("OP_reg22"); break;
    case DW_OP_reg23: printf("OP_reg23"); break;
    case DW_OP_reg24: printf("OP_reg24"); break;
    case DW_OP_reg25: printf("OP_reg25"); break;
    case DW_OP_reg26: printf("OP_reg26"); break;
    case DW_OP_reg27: printf("OP_reg27"); break;
    case DW_OP_reg28: printf("OP_reg28"); break;
    case DW_OP_reg29: printf("OP_reg29"); break;
    case DW_OP_reg30: printf("OP_reg30"); break;
    case DW_OP_reg31: printf("OP_reg31"); break;
    case DW_OP_breg0: printf("OP_breg0"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg1: printf("OP_breg1"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg2: printf("OP_breg2"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg3: printf("OP_breg3"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg4: printf("OP_breg4"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg5: printf("OP_breg5"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg6: printf("OP_breg6"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg7: printf("OP_breg7"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg8: printf("OP_breg8"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg9: printf("OP_breg9"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg10: printf("OP_breg10"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg11: printf("OP_breg11"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg12: printf("OP_breg12"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg13: printf("OP_breg13"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg14: printf("OP_breg14"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg15: printf("OP_breg15"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg16: printf("OP_breg16"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg17: printf("OP_breg17"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg18: printf("OP_breg18"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg19: printf("OP_breg19"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg20: printf("OP_breg20"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg21: printf("OP_breg21"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg22: printf("OP_breg22"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg23: printf("OP_breg23"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg24: printf("OP_breg24"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg25: printf("OP_breg25"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg26: printf("OP_breg26"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg27: printf("OP_breg27"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg28: printf("OP_breg28"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg29: printf("OP_breg29"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg30: printf("OP_breg30"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_breg31: printf("OP_breg31"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_regx: printf("OP_regx"); printf("(%Lu)",lr_number); break;
    case DW_OP_fbreg: printf("OP_fbreg"); printf("(%Ld)",(Dwarf_Signed)lr_number); break;
    case DW_OP_bregx: printf("OP_bregx");
		      printf("(%Lu,%Ld)",lr_number,(Dwarf_Signed)lr_number2); break;
    case DW_OP_piece: printf("OP_piece"); printf("(0x%Lx)",lr_number); break;
    case DW_OP_deref_size: printf("OP_deref_size"); printf("(%d)",(signed char)lr_number); break;
    case DW_OP_xderef_size: printf("OP_xderef_size"); printf("(%d)",(signed char)lr_number); break;
    case DW_OP_nop: printf("OP_nop"); break;
    case DW_OP_lo_user: printf("OP_lo_user"); break;
    case DW_OP_hi_user: printf("OP_hi_user"); break;

    default: fprintf(stderr,"ERROR Unhandled location opcode (0x%x)\n", lr_atom); dd_abort();
  }

}

/*
    these will be used for dumping line number opcode in the future

    case DW_LNS_copy: printf("LNS_copy"); break;
    case DW_LNS_advance_pc: printf("LNS_advance_pc"); break;
    case DW_LNS_advance_line: printf("LNS_advance_line"); break;
    case DW_LNS_set_file: printf("LNS_set_file"); break;
    case DW_LNS_set_column: printf("LNS_set_column"); break;
    case DW_LNS_negate_stmt: printf("LNS_negate_stmt"); break;
    case DW_LNS_set_basic_block: printf("LNS_set_basic_block"); break;
    case DW_LNS_const_add_pc: printf("LNS_const_add_pc"); break;
    case DW_LNS_fixed_advance_pc: printf("LNS_fixed_advance_pc"); break;
    case DW_LNE_end_sequence: printf("LNE_end_sequence"); break;
    case DW_LNE_set_address: printf("LNE_set_address"); break;
    case DW_LNE_define_file: printf("LNE_define_file"); break;
*/

// handle DW_AT_accessibility attribute value
void print_access(Dwarf_Off code) {

  switch (code) {

    case DW_ACCESS_public: printf("ACCESS_public"); break;
    case DW_ACCESS_protected: printf("ACCESS_protected"); break;
    case DW_ACCESS_private: printf("ACCESS_private"); break;

    default: fprintf(stderr,"ERROR Unhandled access code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_visibility attribute value
void print_visibility(Dwarf_Off code) {

  switch (code) {

    case DW_VIS_local: printf("VIS_local"); break;
    case DW_VIS_exported: printf("VIS_exported"); break;
    case DW_VIS_qualified: printf("VIS_qualified"); break;

    default: fprintf(stderr,"ERROR Unhandled visibility code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_virtuality attribute value
void print_virtuality(Dwarf_Off code) {

  switch (code) {

    case DW_VIRTUALITY_none: printf("VIRTUALITY_none"); break;
    case DW_VIRTUALITY_virtual: printf("VIRTUALITY_virtual"); break;
    case DW_VIRTUALITY_pure_virtual: printf("VIRTUALITY_pure_virtual"); break;

    default: fprintf(stderr,"ERROR Unhandled virtuality code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_identifier_case attribute value
void print_identifier_case(Dwarf_Off code) {

  switch (code) {

    case DW_ID_case_sensitive: printf("ID_case_sensitive"); break;
    case DW_ID_up_case: printf("ID_up_case"); break;
    case DW_ID_down_case: printf("ID_down_case"); break;
    case DW_ID_case_insensitive: printf("ID_case_insensitive"); break;

    default: fprintf(stderr,"ERROR Unhandled identifier_case code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_calling_convention attribute value
void print_calling_convention(Dwarf_Off code) {

  switch (code) {

    case DW_CC_normal: printf("CC_normal"); break;
    case DW_CC_program: printf("CC_program"); break;
    case DW_CC_nocall: printf("CC_nocall"); break;
    case DW_CC_lo_user: printf("CC_lo_user"); break;
    case DW_CC_hi_user: printf("CC_hi_user"); break;

    default: fprintf(stderr,"ERROR Unhandled calling_convention code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_inline attribute value
void print_inline(Dwarf_Off code) {

  switch (code) {

    case DW_INL_not_inlined: printf("INL_not_inlined"); break;
    case DW_INL_inlined: printf("INL_inlined"); break;
    case DW_INL_declared_not_inlined: printf("INL_declared_not_inlined"); break;
    case DW_INL_declared_inlined: printf("INL_declared_inlined"); break;

    default: fprintf(stderr,"ERROR Unhandled inline code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_ordering attribute value
void print_ordering(Dwarf_Off code) {

  switch (code) {

    case DW_ORD_row_major: printf("ORD_row_major"); break;
    case DW_ORD_col_major: printf("ORD_col_major"); break;

    default: fprintf(stderr,"ERROR Unhandled ordering code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_discr_list attribute value
void print_discr_list(Dwarf_Off code) {

  switch (code) {

    case DW_DSC_label: printf("DSC_label"); break;
    case DW_DSC_range: printf("DSC_range"); break;

    default: fprintf(stderr,"ERROR Unhandled discr_list code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_encoding attribute value
void print_encoding(Dwarf_Off code) {

  switch (code) {

    case DW_ATE_address: printf("ATE_address"); break;
    case DW_ATE_boolean: printf("ATE_boolean"); break;
    case DW_ATE_complex_float: printf("ATE_complex_float"); break;
    case DW_ATE_float: printf("ATE_float"); break;
    case DW_ATE_signed: printf("ATE_signed"); break;
    case DW_ATE_signed_char: printf("ATE_signed_char"); break;
    case DW_ATE_unsigned: printf("ATE_unsigned"); break;
    case DW_ATE_unsigned_char: printf("ATE_unsigned_char"); break;
    case DW_ATE_lo_user: printf("ATE_lo_user"); break;
    case DW_ATE_hi_user: printf("ATE_hi_user"); break;

    default: fprintf(stderr,"ERROR Unhandled encoding code (%d)\n", code); dd_abort();

  }

}

// handle DW_AT_language attribute value
void print_lang(Dwarf_Off code) {

  switch (code) {
    case DW_LANG_C89: printf("LANG_C89"); break;
    case DW_LANG_C: printf("LANG_C"); break;
    case DW_LANG_Ada83: printf("LANG_Ada83"); break;
    case DW_LANG_C_plus_plus: printf("LANG_C_plus_plus"); break;
    case DW_LANG_Cobol74: printf("LANG_Cobol74"); break;
    case DW_LANG_Cobol85: printf("LANG_Cobol85"); break;
    case DW_LANG_Fortran77: printf("LANG_Fortran77"); break;
    case DW_LANG_Fortran90: printf("LANG_Fortran90"); break;
    case DW_LANG_Pascal83: printf("LANG_Pascal83"); break;
    case DW_LANG_Modula2: printf("LANG_Modula2"); break;
    case DW_LANG_Mips_Assembler: printf("LANG_Mips_Assembler"); break;

    default: fprintf(stderr,"ERROR Unhandled language code (%d)\n", code); dd_abort();

  }

}

// print C type declaraion
// needs more enhancement
void pretty_print_type(Dwarf_Off type_die_off) {

  Dwarf_Die type_die;
  dwarf_offdie(dbg, type_die_off, &type_die, NULL);
  Dwarf_Half tag;
  int ret;

  if ((ret=dwarf_tag(type_die, &tag, NULL))==DW_DLV_OK) {

    if (tag==DW_TAG_pointer_type) {
      Dwarf_Attribute type_attr;
      if ((ret=dwarf_attr(type_die, DW_AT_type, &type_attr, NULL))==DW_DLV_OK) {
	Dwarf_Off type_die_off;
	dwarf_formref(type_attr, &type_die_off, NULL);
	type_die_off += cu_offset;
	printf("(");
	pretty_print_type(type_die_off);
	printf("*)");
      }
    } else {

      Dwarf_Attribute name_attr;
      ret=dwarf_attr(type_die, DW_AT_name, &name_attr, NULL);
      if (ret != DW_DLV_NO_ENTRY && ret != DW_DLV_ERROR) {
        char *str_tmp;
        dwarf_formstring(name_attr, &str_tmp, NULL);
        printf("%s", str_tmp);
        dwarf_dealloc(dbg, str_tmp, DW_DLA_STRING);
      } else if (ret != DW_DLV_NO_ENTRY) {
	printf("(0x%04x)", type_die_off);
      }
    }
  }
}

// handle tag name
void print_tag(Dwarf_Half tag) {
  switch (tag) {

    case DW_TAG_array_type: printf("TAG_array_type"); break;
    case DW_TAG_class_type: printf("TAG_class_type"); break;
    case DW_TAG_entry_point: printf("TAG_entry_point"); break;
    case DW_TAG_enumeration_type: printf("TAG_enumeration_type"); break;
    case DW_TAG_formal_parameter: printf("TAG_formal_parameter"); break;
    case DW_TAG_imported_declaration: printf("TAG_imported_declaration"); break;
    case DW_TAG_label: printf("TAG_label"); break;
    case DW_TAG_lexical_block: printf("TAG_lexical_block"); break;
    case DW_TAG_member: printf("TAG_member"); break;
    case DW_TAG_pointer_type: printf("TAG_pointer_type"); break;
    case DW_TAG_reference_type: printf("TAG_reference_type"); break;
    case DW_TAG_compile_unit: printf("TAG_compile_unit"); break;
    case DW_TAG_string_type: printf("TAG_string_type"); break;
    case DW_TAG_structure_type: printf("TAG_structure_type"); break;
    case DW_TAG_subroutine_type: printf("TAG_subroutine_type"); break;
    case DW_TAG_typedef: printf("TAG_typedef"); break;
    case DW_TAG_union_type: printf("TAG_union_type"); break;
    case DW_TAG_unspecified_parameters: printf("TAG_unspecified_parameters"); break;
    case DW_TAG_variant: printf("TAG_variant"); break;
    case DW_TAG_common_block: printf("TAG_common_block"); break;
    case DW_TAG_common_inclusion: printf("TAG_common_inclusion"); break;
    case DW_TAG_inheritance: printf("TAG_inheritance"); break;
    case DW_TAG_inlined_subroutine: printf("TAG_inlined_subroutine"); break;
    case DW_TAG_module: printf("TAG_module"); break;
    case DW_TAG_ptr_to_member_type: printf("TAG_ptr_to_member_type"); break;
    case DW_TAG_set_type: printf("TAG_set_type"); break;
    case DW_TAG_subrange_type: printf("TAG_subrange_type"); break;
    case DW_TAG_with_stmt: printf("TAG_with_stmt"); break;
    case DW_TAG_access_declaration: printf("TAG_access_declaration"); break;
    case DW_TAG_base_type: printf("TAG_base_type"); break;
    case DW_TAG_catch_block: printf("TAG_catch_block"); break;
    case DW_TAG_const_type: printf("TAG_const_type"); break;
    case DW_TAG_constant: printf("TAG_constant"); break;
    case DW_TAG_enumerator: printf("TAG_enumerator"); break;
    case DW_TAG_file_type: printf("TAG_file_type"); break;
    case DW_TAG_friend: printf("TAG_friend"); break;
    case DW_TAG_namelist: printf("TAG_namelist"); break;
    case DW_TAG_namelist_item: printf("TAG_namelist_item"); break;
    case DW_TAG_packed_type: printf("TAG_packed_type"); break;
    case DW_TAG_subprogram: printf("TAG_subprogram"); break;
    case DW_TAG_template_type_param: printf("TAG_template_type_param"); break;
    case DW_TAG_template_value_param: printf("TAG_template_value_param"); break;
    case DW_TAG_thrown_type: printf("TAG_thrown_type"); break;
    case DW_TAG_try_block: printf("TAG_try_block"); break;
    case DW_TAG_variant_part: printf("TAG_variant_part"); break;
    case DW_TAG_variable: printf("TAG_variable"); break;
    case DW_TAG_volatile_type: printf("TAG_volatile_type"); break;

    default: fprintf(stderr,"ERROR Unknown tag code (%d)\n", tag); dd_abort();

  }
  printf("\n");
}

// entry point to print attribute name and value
void print_attribute(Dwarf_Attribute attr) {

  int ret;
  Dwarf_Half form_code;
  Dwarf_Half attr_code;
  char* code_name;

  if ((ret=dwarf_whatform(attr, &form_code, NULL))!=DW_DLV_OK ||
      (ret=dwarf_whatattr(attr, &attr_code, NULL))!=DW_DLV_OK)
    return;

  if (verbose)
    printf("attr=0x%x form=0x%x\n", attr_code, form_code);

  Dwarf_Addr addr;
  Dwarf_Off  off;
  Dwarf_Unsigned udata;
  Dwarf_Signed sdata;
  Dwarf_Block *block;
  Dwarf_Bool b;
  char* str;

  int is_addr=0;
  int is_off=0;
  int is_udata=0;
  int is_sdata=0;
  int is_block=0;
  int is_bool=0;
  int is_str=0;
  int is_at_type=0;

  switch (form_code) {
    case DW_FORM_addr: dwarf_formaddr(attr, &addr, NULL); is_addr=1;
	 break;
    case DW_FORM_block2:
    case DW_FORM_block4: dwarf_formblock(attr, &block, NULL); is_block=1;
	 break;
    case DW_FORM_data2:
    case DW_FORM_data4:
    case DW_FORM_data8: dwarf_formudata(attr, &udata, NULL); is_udata=1;
	 break;
    case DW_FORM_string: dwarf_formstring(attr, &str, NULL); is_str=1;
	 break;
    case DW_FORM_block:
    case DW_FORM_block1: dwarf_formblock(attr, &block, NULL); is_block=1;
	 break;
    case DW_FORM_data1: dwarf_formudata(attr, &udata, NULL); is_udata=1;
	 break;
    case DW_FORM_flag: dwarf_formflag(attr, &b, NULL); is_bool=1;
	 break;
    case DW_FORM_sdata: dwarf_formsdata(attr, &sdata, NULL); is_sdata=1;
	 break;
    case DW_FORM_strp: dwarf_formstring(attr, &str, NULL); is_str=1;
	 break;
    case DW_FORM_udata: dwarf_formudata(attr, &udata, NULL); is_udata=1;
	 break;
    case DW_FORM_ref_addr:
    case DW_FORM_ref1:
    case DW_FORM_ref2:
    case DW_FORM_ref4:
    case DW_FORM_ref8:
    case DW_FORM_ref_udata: dwarf_formref(attr, &off, NULL); is_off=1;
	 off += cu_offset;
	 break;
    case DW_FORM_indirect: 
	 fprintf(stderr,"ERROR Unhandled form FORM_indirect (%d)\n",
		 form_code);
	 dd_abort();
	 break;

    default: fprintf(stderr,"ERROR Unknown form code (%d)\n", form_code); dd_abort();
  }

  switch (attr_code) {

    case DW_AT_sibling: printf("\tAT_sibling("); break;

    case DW_AT_location: printf("\tAT_location("); goto process_location;
    case DW_AT_data_member_location: printf("\tAT_data_member_location(");
		goto process_location;
    case DW_AT_variable_parameter: printf("\tAT_variable_parameter(");
		goto process_location;
    case DW_AT_string_length: printf("\tAT_string_length(");
		goto process_location;
    case DW_AT_use_location: printf("\tAT_use_location(");
		goto process_location;
    case DW_AT_return_addr: printf("\tAT_return_addr(");
		goto process_location;
    case DW_AT_const_value: printf("\tAT_const_value(");
		goto process_location;
    case DW_AT_frame_base: printf("\tAT_frame_base(");
		goto process_location;
    case DW_AT_static_link: printf("\tAT_static_link(");
		goto process_location;
    case DW_AT_vtable_elem_location: printf("\tAT_vtable_elem_location(");

process_location:
	  {
		/* only block form is handled here */
		if (is_block==0)
		  break;

		Dwarf_Locdesc *locs;
		Dwarf_Signed num_locs;
		dwarf_loclist(attr, &locs, &num_locs, NULL);
		for (int i=0; i<num_locs; i++) {
		  printf("[0x%x:0x%x]<", locs[i].ld_lopc, locs[i].ld_hipc);
		  for (int j=0; j<locs[i].ld_cents; j++) {
		    print_loc(&locs[i].ld_s[j]);
		    if (j+1<locs[i].ld_cents) printf(" ");
		    dwarf_dealloc(dbg, locs[i].ld_s, DW_DLA_LOC_BLOCK);
		  }
		  printf(">");
		  dwarf_dealloc(dbg, locs, DW_DLA_LOCDESC);
		}
		printf(")\n");
		return;
	  }
    case DW_AT_name: printf("\tAT_name("); break;
    case DW_AT_ordering: printf("\tAT_ordering("); break;
    case DW_AT_subscr_data: printf("\tAT_subscr_data("); break;
    case DW_AT_byte_size: printf("\tAT_byte_size("); break;
    case DW_AT_bit_offset: printf("\tAT_bit_offset("); break;
    case DW_AT_bit_size: printf("\tAT_bit_size("); break;
    case DW_AT_element_list: printf("\tAT_element_list("); break;
    case DW_AT_stmt_list: printf("\tAT_stmt_list("); break;
    case DW_AT_low_pc: printf("\tAT_low_pc("); break;
    case DW_AT_high_pc: printf("\tAT_high_pc("); break;
    case DW_AT_language: printf("\tAT_language(");
		print_lang(udata); printf(")\n");
	        return;
    case DW_AT_member: printf("\tAT_member("); break;
    case DW_AT_discr: printf("\tAT_discr("); break;
    case DW_AT_discr_value: printf("\tAT_discr_value("); break;
    case DW_AT_visibility: printf("\tAT_visibility(");
		print_visibility(udata); printf(")\n");
	        return;
    case DW_AT_import: printf("\tAT_import("); break;
    case DW_AT_common_reference: printf("\tAT_common_reference("); break;
    case DW_AT_comp_dir: printf("\tAT_comp_dir("); break;
    case DW_AT_containing_type: printf("\tAT_containing_type("); break;
    case DW_AT_default_value: printf("\tAT_default_value("); break;
    case DW_AT_inline: printf("\tAT_inline(");
		print_inline(udata); printf(")\n");
	        return;
    case DW_AT_is_optional: printf("\tAT_is_optional("); break;
    case DW_AT_lower_bound: printf("\tAT_lower_bound("); break;
    case DW_AT_producer: printf("\tAT_producer("); break;
    case DW_AT_prototyped: printf("\tAT_prototyped("); break;
    case DW_AT_start_scope: printf("\tAT_start_scope("); break;
    case DW_AT_stride_size: printf("\tAT_stride_size("); break;
    case DW_AT_upper_bound: printf("\tAT_upper_bound("); break;
    case DW_AT_abstract_origin: printf("\tAT_abstract_origin("); break;
    case DW_AT_accessibility: printf("\tAT_accessibility(");
		print_access(udata); printf(")\n");
		return;
    case DW_AT_address_class: printf("\tAT_address_class("); break;
    case DW_AT_artificial: printf("\tAT_artificial("); break;
    case DW_AT_base_types: printf("\tAT_base_types("); break;
    case DW_AT_calling_convention: printf("\tAT_calling_convention(");
		print_calling_convention(udata); printf(")\n");
	        return;
    case DW_AT_count: printf("\tAT_count("); break;
    case DW_AT_decl_column: printf("\tAT_decl_column("); break;
    case DW_AT_decl_file: printf("\tAT_decl_file("); break;
    case DW_AT_decl_line: printf("\tAT_decl_line("); break;
    case DW_AT_declaration: printf("\tAT_declaration("); break;
    case DW_AT_discr_list: printf("\tAT_discr_list(");
		print_discr_list(udata); printf(")\n");
	        return;
    case DW_AT_encoding: printf("\tAT_encoding(");
		print_encoding(udata); printf(")\n");
	        return;
    case DW_AT_external: printf("\tAT_external("); break;
    case DW_AT_friend: printf("\tAT_friend("); break;
    case DW_AT_identifier_case: printf("\tAT_identifier_case(");
		print_identifier_case(udata); printf(")\n");
	        return;
    case DW_AT_macro_info: printf("\tAT_macro_info("); break;
    case DW_AT_namelist_items: printf("\tAT_namelist_items("); break;
    case DW_AT_priority: printf("\tAT_priority("); break;
    case DW_AT_segment: printf("\tAT_segment("); break;
    case DW_AT_specification: printf("\tAT_specification("); break;
    case DW_AT_type:
		{
		   printf("\tAT_type(");
		   pretty_print_type(off);
		   printf("@");
		   break;
		}
    case DW_AT_virtuality: printf("\tAT_virtuality(");
		print_virtuality(udata); printf(")\n");
	        return;

    case DW_AT_lo_user:
	printf("WARNING Unhandled user extension attribute");
	printf(" code (0x%x)\n", attr_code);
	return;

    case DW_AT_MIPS_fde: code_name = "AT_MIPS_fde";
			goto unhandled_mips;
    case DW_AT_MIPS_loop_begin: code_name = "AT_MIPS_loop_begin";
			goto unhandled_mips;
    case DW_AT_MIPS_tail_loop_begin: code_name = "AT_MIPS_tail_loop_begin";
			goto unhandled_mips;
    case DW_AT_MIPS_epilog_begin: code_name = "AT_MIPS_epilog_begin";
			goto unhandled_mips;
    case DW_AT_MIPS_loop_unroll_factor: code_name ="AT_MIPS_loop_unroll_factor";
			goto unhandled_mips;
    case DW_AT_MIPS_software_pipeline_depth: code_name="AT_MIPS_pipeline_depth";
			goto unhandled_mips;
    case DW_AT_MIPS_linkage_name: code_name = "AT_MIPS_linkage_name";
			printf("\tAT_MIPS_linkage_name(");
			break;
    case DW_AT_MIPS_stride: code_name = "AT_MIPS_stride";
			goto unhandled_mips;
    case DW_AT_MIPS_abstract_name: code_name = "AT_MIPS_abstract_name";
			goto unhandled_mips;
    case DW_AT_MIPS_clone_origin: code_name = "AT_MIPS_clone_origin";
			goto unhandled_mips;
    case DW_AT_MIPS_has_inlines: code_name = "AT_MIPS_has_inlines";
			printf("\tAT_MIPS_has_inline(");
			break;
    case DW_AT_MIPS_stride_byte: code_name = "AT_MIPS_stride_byte";
			goto unhandled_mips;
    case DW_AT_MIPS_stride_elem: code_name = "AT_MIPS_stride_elem";
			goto unhandled_mips;
    case DW_AT_MIPS_ptr_dopetype: code_name = "AT_MIPS_ptr_dopetype";
			goto unhandled_mips;
    case DW_AT_MIPS_allocatable_dopetype:
			code_name = "AT_MIPS_allocatable_dopetype";
			goto unhandled_mips;
    case DW_AT_MIPS_assumed_shape_dopetype:
			code_name = "AT_MIPS_assumed_shape_dopetype";
			goto unhandled_mips;
    case DW_AT_MIPS_assumed_size: code_name = "AT_MIPS_assumed_size";

unhandled_mips:
	printf("WARNING Unhandled MIPS extension attribute");
	printf(" %s (0x%x)\n", code_name, attr_code);
	return;

/* GNU extensions, currently not used in dwarf2 by egcs
   Mostly dwarf1 extensions not needed in dwarf2?
*/
    case DW_AT_sf_names: code_name="AT_sf_names";
			goto unhandled_gnu;
    case DW_AT_src_info: code_name="AT_src_info";
			goto unhandled_gnu;
    case DW_AT_mac_info: code_name="AT_mac_info";
			goto unhandled_gnu;
    case DW_AT_src_coords: code_name="AT_src_coords";
			goto unhandled_gnu;
    case DW_AT_body_begin: code_name="AT_body_begin";
			goto unhandled_gnu;
    case DW_AT_body_end: code_name="AT_body_end";
			goto unhandled_gnu;

unhandled_gnu:
	printf("WARNING Unhandled GNU extension attribute");
	printf(" %s (0x%x)\n", code_name, attr_code);
	return;

    default: fprintf(stderr,"ERROR Unknown attribute code (%d)\n", attr_code); dd_abort();

  }

  switch (form_code) {
    case DW_FORM_addr:
	 printf("0x%x)", addr);
	 break;
    case DW_FORM_block2:
	 printf("0x%x)", *(unsigned short*)block->bl_data);
	 dwarf_dealloc(dbg, block, DW_DLA_BLOCK);
	 break;
    case DW_FORM_block4:
	 printf("0x%x)", *(unsigned int*)block->bl_data);
	 dwarf_dealloc(dbg, block, DW_DLA_BLOCK);
	 break;
    case DW_FORM_data2:
	 printf("%u)", (int)udata);
	 break;
    case DW_FORM_data4:
	 printf("%u)", (int)udata);
	 break;
    case DW_FORM_data8:
	 printf("%" LLU_FMT ")", (long long)udata);
	 break;
    case DW_FORM_string:
	 printf("%s)", str);
	 dwarf_dealloc(dbg, str, DW_DLA_STRING);
	 break;
    case DW_FORM_block:
	 printf("0x%x)", *(unsigned int*)block->bl_data);
	 dwarf_dealloc(dbg, block, DW_DLA_BLOCK);
	 break;
    case DW_FORM_block1:
	 printf("0x%x)", *(unsigned char*)block->bl_data);
	 dwarf_dealloc(dbg, block, DW_DLA_BLOCK);
	 break;
    case DW_FORM_data1:
	 printf("%u)", (int)udata);
	 break;
    case DW_FORM_flag:
	 printf("%s)", (b?"true":"false"));
	 break;
    case DW_FORM_sdata:
	 printf("%Ld)", sdata);
	 break;
    case DW_FORM_strp:
	 printf("%s)", str);
	 dwarf_dealloc(dbg, str, DW_DLA_STRING);
	 break;
    case DW_FORM_udata:
	 printf("%Lu)", udata);
	 break;
    case DW_FORM_ref_addr:
    case DW_FORM_ref1:
    case DW_FORM_ref2:
    case DW_FORM_ref4:
    case DW_FORM_ref8:
    case DW_FORM_ref_udata:
	 printf("0x%x)", off);
	 break;
    case DW_FORM_indirect: 
	 printf("not handled");
	 break;

    default: fprintf(stderr,"ERROR Unknown form code (%d)\n", form_code); dd_abort();
  }

  printf("\n");
}

// traverse the die tree and print out tags and their attributes
// this is called recursively
void die_traversal(Dwarf_Die die) {

  int ret;
  Dwarf_Die save_die;
  int sibling_count=0;

traverse_sibling:

  save_die=die;

  if  (die!=NULL) {
    Dwarf_Half tag;
    Dwarf_Off off;
    Dwarf_Unsigned size;
    if ((ret=dwarf_tag(die, &tag, NULL))==DW_DLV_OK &&
	(ret=dwarf_dieoffset(die, &off, NULL))==DW_DLV_OK) {

	Dwarf_Attribute* attrs;
	Dwarf_Signed num_attrs;

	// print a die's tag name then list all its attributes
	printf("0x%04x: ", off);

	print_tag(tag);

	if ((ret=dwarf_attrlist(die, &attrs, &num_attrs, NULL))==DW_DLV_OK) {

	  int i;
	  for (i=0; i<num_attrs; i++) {
	    print_attribute(attrs[i]);
	    dwarf_dealloc(dbg, attrs[i], DW_DLA_ATTR);
	  }
	  dwarf_dealloc(dbg, attrs, DW_DLA_LIST);
	}
	printf("\n");

	// for a compile unit, we also dump the file and line number info
	if (tag==DW_TAG_compile_unit) {
	  char **files;
	  Dwarf_Signed num_files;
	  Dwarf_Line *lines;
	  Dwarf_Signed num_lines;

	  if ((ret=dwarf_srcfiles(die, &files, &num_files, NULL))==DW_DLV_OK){
	    printf("file    id: file_name\n");
	    for (int i=0; i<num_files; i++) {
	      printf("       %3d: %s\n", i+1, files[i]);
	      dwarf_dealloc(dbg, files[i], DW_DLA_STRING);
	    }
	    dwarf_dealloc(dbg, files, DW_DLA_LIST);
	    printf("\n");
	  }
	  if ((ret=dwarf_srclines(die, &lines, &num_lines, NULL))==DW_DLV_OK){
	    printf("line    id: line_num\tcol\t      addr bs es bb src\n");
	    for (int i=0; i<num_lines; i++) {
	      Dwarf_Bool begin_stmt_b;
	      Dwarf_Bool end_sequence_b;
	      Dwarf_Bool begin_block_b;
	      Dwarf_Unsigned lineno;
	      Dwarf_Addr addr;
	      Dwarf_Signed lineoff;
	      char* linesrc;
	      char begin_stmt, end_sequence, begin_block;

	      (void)dwarf_linebeginstatement(lines[i], &begin_stmt_b, NULL);
	      (void)dwarf_lineendsequence(lines[i], &end_sequence_b, NULL);
	      (void)dwarf_lineblock(lines[i], &begin_block_b, NULL);
	      (void)dwarf_lineno(lines[i], &lineno, NULL);
	      (void)dwarf_lineaddr(lines[i], &addr, NULL);
	      (void)dwarf_lineoff(lines[i], &lineoff, NULL);
	      (void)dwarf_linesrc(lines[i], &linesrc, NULL);

	      begin_stmt=(begin_stmt_b!=0)?'Y':'N';
	      end_sequence=(end_sequence_b!=0)?'Y':'N';
	      begin_block=(begin_block_b!=0)?'Y':'N';

	      printf("       %3d: %-9d\t", i+1, (int)lineno);
	      printf("%3d\t0x%08x %c  %c  %c ",
	      (int)lineoff, (unsigned int)addr,
	      begin_stmt, end_sequence, begin_block);
	      printf("%s\n", linesrc);
	      dwarf_dealloc(dbg, linesrc, DW_DLA_STRING);
	      dwarf_dealloc(dbg, lines[i], DW_DLA_LINE);
	    }
	    dwarf_dealloc(dbg, lines, DW_DLA_LIST);
	    printf("\n");
	  } else
	    no_entry_msg("line");
	}
    }
    die_id++;

    // continue on the kids
    if ((ret=dwarf_child(save_die, &die, NULL))==DW_DLV_OK){
      die_traversal(die);
    }
  }

  // continue on the siblings
  // when the original die is NULL, the first die is returned
  if ((ret=dwarf_siblingof(dbg, save_die, &die, NULL))==DW_DLV_OK){
    sibling_count++;
    goto traverse_sibling;
  }

}

// dump info in the following order
// .debug_typenames
// .debug_pubnames
// .debug_varnames
// .debug_funcnames
// .debug_aranges
// .debug_frame
// CU information in .debug_info

int main(int argc, char* argv[]) {

  Dwarf_Error d_err;
  char *buf;
  char *elf_file=NULL;
  int ret,i;

  Dwarf_Func *funcs;
  Dwarf_Signed num_funcs;

  Dwarf_Global *globals;
  Dwarf_Signed num_globals;

  Dwarf_Type *types;
  Dwarf_Signed num_types;

  Dwarf_Var *vars;
  Dwarf_Signed num_vars;

  Dwarf_Fde *fdes;
  Dwarf_Signed num_fdes;

  Dwarf_Cie *cies;
  Dwarf_Signed num_cies;

  Dwarf_Arange *aranges;
  Dwarf_Signed num_aranges;
  int cu;

  if (argc<2)
    fprintf(stderr, "Usage: dwarfdump elf_file_name\n");
  elf_file=argv[1];
  fd = open(elf_file, O_RDONLY);

  if (argc>=3) {
    verbose=1;
    printf("VERBOSE on\n");
  }

  cu= -1;
  if (argc==4) {
    cu=atoi(argv[3]);
    printf("dump for cu %d only\n", cu);
  }

  New_Step("Init");

  if ((ret=dwarf_init(fd,DW_DLC_READ,er_handler,step_name,&dbg,NULL))==
       DW_DLV_NO_ENTRY) no_entry_abort("dbg");

  New_Step("Types");
  if ((ret=dwarf_get_types(dbg, &types, &num_types, NULL))==DW_DLV_OK){
    printf("type  id:        name     die_offset\t\tcu_offset\n");
    for (i=0; i<num_types; i++) {
      Dwarf_Off die_offset;
      Dwarf_Off cu_offset;
      if ((ret=dwarf_type_name_offsets(
		types[i], &buf, &die_offset, &cu_offset, NULL))==DW_DLV_OK) {
	printf("       %3d: %-15s ", i+1, buf);
	printf("0x%x\t\t0x%x\n", die_offset, cu_offset);
        dwarf_dealloc(dbg, buf, DW_DLA_STRING);
      }
      dwarf_dealloc(dbg, types[i], DW_DLA_TYPE);
    }
    dwarf_dealloc(dbg, types, DW_DLA_LIST);
  } else
    no_entry_msg("type");

  New_Step("Globals");
  if ((ret=dwarf_get_globals(dbg, &globals, &num_globals, NULL))==DW_DLV_OK){
    printf("global  id:        name     die_offset\t\tcu_offset\n");
    for (i=0; i<num_globals; i++) {
      Dwarf_Off die_offset;
      Dwarf_Off cu_offset;
      if ((ret=dwarf_global_name_offsets(
		globals[i], &buf, &die_offset, &cu_offset, NULL))==DW_DLV_OK) {
	printf("       %3d: %-15s ", i+1, buf);
	printf("0x%04x\t\t0x%x\n", die_offset, cu_offset);
        dwarf_dealloc(dbg, buf, DW_DLA_STRING);
      }
      dwarf_dealloc(dbg, globals[i], DW_DLA_GLOBAL);
    }
    dwarf_dealloc(dbg, globals, DW_DLA_LIST);
  } else
    no_entry_msg("global");

  New_Step("Variables");
  if ((ret = dwarf_get_vars(dbg, &vars, &num_vars, NULL))==DW_DLV_OK) {
    printf("num_vars=%Ld\n", num_vars);
    for (i=0; i<num_vars; i++) {
      if ((ret=dwarf_varname(vars[i], &buf, NULL))==DW_DLV_OK) {
	printf("var %3d: %10s\n", i+1, buf);
        dwarf_dealloc(dbg, buf, DW_DLA_STRING);
      }
      dwarf_dealloc(dbg, vars[i], DW_DLA_VAR);
    }
    dwarf_dealloc(dbg, vars, DW_DLA_LIST);
  } else
    no_entry_msg("var");

  New_Step("Static Functions");
  if ((ret=dwarf_get_funcs(dbg, &funcs, &num_funcs, NULL))==DW_DLV_OK){
    printf("num_funcs=%Ld\n", num_funcs);
    for (i=0; i<num_funcs; i++) {
      if ((ret=dwarf_funcname(funcs[i], &buf, NULL))==DW_DLV_OK) {
	printf("func %3d: %10s\n", i+1, buf);
        dwarf_dealloc(dbg, buf, DW_DLA_STRING);
      }
      dwarf_dealloc(dbg, funcs[i], DW_DLA_FUNC);
    }
    dwarf_dealloc(dbg, funcs, DW_DLA_LIST);
  } else
    no_entry_msg("func");

  New_Step("Address Ranges");
  if ((ret=dwarf_get_aranges(dbg, &aranges, &num_aranges, NULL))==DW_DLV_OK){
    printf("arange  id:   start\tlength\t\tcu_offset\n");
    for (i=0; i<num_aranges; i++) {
      Dwarf_Addr start;
      Dwarf_Unsigned length;
      Dwarf_Off cu_die_offset;
      if ((ret=dwarf_get_arange_info(
	       aranges[i], &start, &length, &cu_die_offset,NULL))==DW_DLV_OK) {
	printf("       %3d: 0x%08x\t0x%08llx\t0x%09x\n",
		i+1, (unsigned)start, length, (unsigned int)cu_die_offset);
      }
      dwarf_dealloc(dbg, aranges[i], DW_DLA_ARANGE);
    }
    dwarf_dealloc(dbg, aranges, DW_DLA_LIST);
  } else
    no_entry_msg("arange");

  New_Step("Frames");
  if ((ret=dwarf_get_fde_list(dbg, &cies, &num_cies, &fdes, &num_fdes, NULL))==DW_DLV_OK){
    printf("num_fdes=%Ld\n", num_fdes);
    for (i=0; i<num_fdes; i++) {

      Dwarf_Addr low_pc;
      Dwarf_Unsigned func_length;
      Dwarf_Ptr fde_bytes;
      Dwarf_Unsigned fde_byte_length;
      Dwarf_Off cie_offset;
      Dwarf_Signed cie_index;
      Dwarf_Off fde_offset;

      if ((ret=dwarf_get_fde_range(
	       fdes[i], &low_pc, &func_length, &fde_bytes, &fde_byte_length,
	       &cie_offset, &cie_index, &fde_offset, NULL))==DW_DLV_OK) {
	printf("frame %3d: low_pc=0x%x\n", i+1, low_pc);
      }
      dwarf_dealloc(dbg, fdes[i], DW_DLA_FDE);
    }
    for (i=0; i<num_cies; i++) {
      dwarf_dealloc(dbg, cies[i], DW_DLA_CIE);
    }
    dwarf_dealloc(dbg, fdes, DW_DLA_LIST);
    dwarf_dealloc(dbg, cies, DW_DLA_LIST);
  } else
    no_entry_msg("frame");

  New_Step("CU");
  Dwarf_Unsigned cu_header_length;
  Dwarf_Half version_stamp;
  Dwarf_Off abbrev_offset;
  Dwarf_Half address_size;
  Dwarf_Unsigned next_cu_header_offset;
  cu_id=0;
  cu_offset = 0;
  while ((ret=dwarf_next_cu_header(dbg, &cu_header_length, &version_stamp, &abbrev_offset, &address_size, &next_cu_header_offset, NULL))==DW_DLV_OK){
    if (cu== -1 || cu == cu_id) {

      printf("----------------------------------------------------\n");
      printf("cu %3d: cu_header_length=%d, version_stamp=%d\n",
	     cu_id, cu_header_length, version_stamp);
      printf("cu %3d: abbrev_offset=0x%x, address_size=%d\n",
	     cu_id, abbrev_offset, address_size);
      printf("cu %3d: next_cu_header_offset=0x%x\n",
	     cu_id, next_cu_header_offset);
      printf("----------------------------------------------------\n");
      Dwarf_Die die=NULL;
      die_id=0;
      die_traversal(die);
    }

    cu_offset = next_cu_header_offset;
    cu_id++;
  }

  New_Step("Finish");

  finish();

  return 0;

}


