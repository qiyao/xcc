/*
   Copyright (C) 2001-2006 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#ifndef cg_prof_INCLUDED
#define cg_prof_INCLUDED

#include <stdio.h>

extern void profile_info_init(const char* src_file_name, const char* pu_name, bool is_linkonce);
extern void profile_info_add_block(const char* block_label_at_emit,
				   int block_id, int unrolliing=0);
extern void profile_info_generate_and_finish(FILE* asm_file);
extern void profile_info_record_checksum(unsigned long long checksum);

extern void Process_Profile_Data_File(const char* file_name);

extern void* Get_Cur_PU_Profile_Data(const char* soruce_file_name,
				     const char* source_pu_name,
				     unsigned long long* checksum);

extern long long Get_Profile_Data(void* cur_pu_profile_data, int bb_id);



#endif

