/*
   Copyright (C) 2001-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#include <stdio.h>
#include "defs.h"
#include "glob.h"
#include "config.h"
#include "cxx_template.h"
#include "cxx_memory.h"

class PC_RANGE;

static char* current_src_pu_name = NULL;
static char* current_src_name = NULL;
static char* current_pu_name = NULL;
static bool mem_pool_initialized = false;
static DYN_ARRAY<PC_RANGE>* pc_ranges = NULL;
static int pu_count = 0;
static bool is_linkonce = false;

static MEM_POOL profile_info_pool;
static unsigned long long checksum;

const int INVALID_ID = -1;

typedef struct {
  int			_id;
  int			_unrolling;
} Block;

class PC_RANGE {

  const char*		_start_label;
  DYN_ARRAY<Block>	_blocks;
  int			_id;

  static int		_next_id;

  const char* save_string(const char* src)
		{
			char* dst=CXX_NEW_ARRAY(char,strlen(src)+1,&profile_info_pool);
			strcpy(dst, src);
			return dst;
		}

  PC_RANGE(const PC_RANGE& pr);
  PC_RANGE operator=(const PC_RANGE& pr);

  int block_unrolling(int i) const { return _blocks[i]._unrolling; }

public:

  PC_RANGE(): _start_label(NULL), _id(_next_id++)
		{
			_blocks.Set_Mem_Pool(&profile_info_pool);
		}
  ~PC_RANGE() { _blocks.Free_array(); }

  void init(const char* start_label)
		{
  			_id=_next_id++;
			_blocks.Set_Mem_Pool(&profile_info_pool);
			_blocks.Alloc_array(1);
			_blocks.Resetidx();
			_start_label = save_string(start_label);
		}

  int id() const { return _id; }
  const char* start_label() const { return _start_label; }
  int num_blocks() const { return _blocks.Elements(); }

  static void reset_id() { _next_id = 1; }
  void set_start_label(const char* start_label)
		{ _start_label = save_string(start_label); }
  int block_id(int i) const { return _blocks[i]._id; }
  void add_block(int block_id, int unrolling)
		{
			int num_elements = _blocks.Elements();
			for (int i=0; i<num_elements; i++) {
			  if (_blocks[i]._id == block_id &&
			      _blocks[i]._unrolling == unrolling)
			    return;
			}

			int nidx = _blocks.Newidx();
			_blocks[nidx]._id = block_id;
			_blocks[nidx]._unrolling = unrolling;
		}

};

int PC_RANGE::_next_id=1;
static int current_range_id = INVALID_ID;

void profile_info_record_checksum(unsigned long long new_checksum) {
  checksum = new_checksum;
}

void profile_info_init(const char* src_fname, const char* pu_name, bool linkonce) {

  if (mem_pool_initialized == false) {
    MEM_POOL_Initialize(&profile_info_pool,"Profile_Info_Pool",FALSE);
    mem_pool_initialized = true;
  }

  MEM_POOL_Push(&profile_info_pool);
  current_src_name = CXX_NEW_ARRAY(char, strlen(src_fname) + 1, &profile_info_pool);
  current_pu_name = CXX_NEW_ARRAY(char, strlen(pu_name) + 1, &profile_info_pool);
  current_src_pu_name = CXX_NEW_ARRAY(char, strlen(src_fname) + strlen("/") + strlen(pu_name)
                    + 1, &profile_info_pool);

  is_linkonce = linkonce;

  strcpy(current_src_name,src_fname);
  strcpy(current_pu_name,pu_name);
  strcpy(current_src_pu_name,src_fname);
  strcat(current_src_pu_name,":");
  strcat(current_src_pu_name,pu_name);

  pc_ranges = CXX_NEW(DYN_ARRAY<PC_RANGE>(&profile_info_pool), &profile_info_pool);

  PC_RANGE::reset_id();
  pu_count++;

  current_range_id = INVALID_ID;
}

void profile_info_add_block(const char* block_label_at_emit,
			    int block_id,
			    int unrolling) {

  if (block_label_at_emit &&
      (current_range_id==INVALID_ID ||
       strcmp(pc_ranges->Get(current_range_id).start_label(),
	      block_label_at_emit)))
  {
    current_range_id = pc_ranges->Newidx();
    pc_ranges->Get(current_range_id).init(block_label_at_emit);
  }

  if (block_id!=0 && current_range_id != INVALID_ID)
    pc_ranges->Get(current_range_id).add_block(block_id, unrolling);
}

void profile_info_generate_and_finish(FILE* asm_file) {

  static const char* xt_range_section_name = ".xt.profile_ranges";
  static const char* xt_file_section_name = ".xt.profile_files";

  static const char* xt_range_section_linkonce_prefix = ".gnu.linkonce";
  static const char* xt_file_section_linkonce_prefix = ".gnu.linkonce";

  static const char* xt_file_name_label = ".Lprof_file_name_label";

  if (is_linkonce)
    fprintf(asm_file,"\n\t.section\t%s%s.%s, \"\"\n\n",
	xt_range_section_linkonce_prefix,xt_range_section_name,current_pu_name);
  else
    fprintf(asm_file,"\n\t.section\t%s, \"\"\n\n", xt_range_section_name);

  int num_ranges = pc_ranges->Elements();
  for (int i=0; i<num_ranges; i++) {

    PC_RANGE& pr = pc_ranges->Get(i);
    fprintf(asm_file,"\n\t# profile range %d\n", pr.id());
    int num_blocks = pr.num_blocks();

    if (num_blocks==0)
      continue;

    fprintf(asm_file,"\t.long\t%s,%d,%s_%d\n\t.long\t",
		pr.start_label(), num_blocks, xt_file_name_label, pu_count);
    int j=0;
    for (; j<num_blocks-1; j++) {
      fprintf(asm_file,"%d,", pr.block_id(j));
    }
    fprintf(asm_file,"%d\n", pr.block_id(j));
  }

  if (is_linkonce)
    fprintf(asm_file,"\n\t.section\t%s%s.%s, \"\"\n%s_%d:\n\t.quad\t%llu\n\t.string\t\"%s\"\n",
	    xt_file_section_linkonce_prefix, xt_file_section_name, current_pu_name,
	    xt_file_name_label, pu_count, checksum, current_src_pu_name);
  else
    fprintf(asm_file,"\n\t.section\t%s, \"\"\n%s_%d:\n\t.quad\t%llu\n\t.string\t\"%s\"\n",
	    xt_file_section_name, xt_file_name_label, pu_count, checksum, current_src_pu_name);

  fprintf(asm_file, "\n\t.text\n", checksum);
  MEM_POOL_Pop(&profile_info_pool);
  pc_ranges = NULL;
}

typedef struct {
  int           	block_id;
  long long     	exec_count;
} Profile_block_data;

typedef struct {
  const char*   	pu_name;
  Profile_block_data*	block;
  int           	num_blocks;
  unsigned long long	checksum;
} Profile_pu_data;

static inline void
byteswap4(void *four_byte_word)
{
  char *p = (char *) four_byte_word;
  char tmp[4] = { p[3], p[2], p[1], p[0] };
  p[0] = tmp[0];
  p[1] = tmp[1];
  p[2] = tmp[2];
  p[3] = tmp[3];
}

static inline void
byteswap8(void *eight_byte_word)
{
  char *p = (char *) eight_byte_word;
  char tmp[8] = { p[7], p[6], p[5], p[4],
		  p[3], p[2], p[1], p[0] };
  p[0] = tmp[0];
  p[1] = tmp[1];
  p[2] = tmp[2];
  p[3] = tmp[3];
  p[4] = tmp[4];
  p[5] = tmp[5];
  p[6] = tmp[6];
  p[7] = tmp[7];
}

static Profile_pu_data* Profile_PU_Data = NULL;
static char* Profile_Src_File_Name = "";
static int Profile_Num_PUs = 0;

void Process_Profile_Data_File(const char* file_name) {

  if (file_name==NULL) {
    return;
  }

  FILE* prof_f = fopen(file_name,"rb");

  if (prof_f==NULL) {
    return;
  }

  char name_buffer[strlen(Src_File_Name)+1];

  unsigned int buffer[2];
  unsigned int byte_count = 0;

  if (fread(buffer, 4, 2, prof_f)==2) {
    unsigned int profile_pu_section = buffer[0];
    unsigned int profile_block_section = buffer[1];
    if (!Same_Byte_Sex) {
      byteswap4(&profile_pu_section);
      byteswap4(&profile_block_section);
    }
    byte_count += 8;
    bool found = false;
    unsigned int pu_offset;
    int num_pus;
    while (byte_count < profile_pu_section) {
      unsigned file_name_length;
      if (fread(&file_name_length, 4, 1, prof_f)==1) {
	if (!Same_Byte_Sex) {
	  byteswap4(&file_name_length);
	}
	if (strlen(Src_File_Name)<file_name_length)
	  break;
	else if (strlen(Src_File_Name)>file_name_length) {
	  unsigned int skip_size = file_name_length + 8;
	  if (fread(name_buffer, 1, skip_size, prof_f)!= skip_size)
	    break;
	  byte_count += skip_size;
	  continue;
	}

	if (fread(name_buffer, 1, file_name_length, prof_f) !=
		  file_name_length ||
	    strncmp(name_buffer,Src_File_Name,file_name_length)) {

	  if (fread(name_buffer, 1, 8, prof_f)!= 8)
	    break;
	  byte_count += 8;

	  continue;
	}

	if (fread(&pu_offset, 4, 1, prof_f)!=1)
	  break;
	if (fread(&num_pus, 4, 1, prof_f)!=1)
	  break;
	byte_count += 8;

	if (!Same_Byte_Sex) {
	  byteswap4(&pu_offset);
	  byteswap4(&num_pus);
	}
	found = true;
	break;
      }
    }

    if (found && fseek(prof_f, profile_pu_section+pu_offset, SEEK_SET)==0) {
      Profile_PU_Data = CXX_NEW_ARRAY(Profile_pu_data, num_pus,
			      MEM_src_nz_pool_ptr);
      unsigned int pu_block_offset = 0;
      bool pu_block_offset_set = false;
      bool ok = true;
      for (int i=0; i<num_pus && ok; i++) {
	char* pu_name = NULL;
	unsigned int first_block_offset;
	int num_blocks;
	unsigned int pu_name_length;
	unsigned long long checksum;

        if (fread(&pu_name_length, 4, 1, prof_f)==1) {
	  if (!Same_Byte_Sex) {
	    byteswap4(&pu_name_length);
	  }
	  pu_name = CXX_NEW_ARRAY(char, pu_name_length+1, MEM_src_nz_pool_ptr);
	} else ok = false;

        if (ok && fread(pu_name, 1, pu_name_length, prof_f)==pu_name_length) {
	    pu_name[pu_name_length] = '\0';
	    Profile_PU_Data[i].pu_name = pu_name;
	} else ok = false;

        if (ok &&
	    fread(&checksum, 8, 1, prof_f)==1 &&
	    fread(&first_block_offset, 4, 1, prof_f)==1 &&
	    fread(&num_blocks, 4, 1, prof_f)==1) {
	  if (!Same_Byte_Sex) {
	    byteswap8(&checksum);
	    byteswap4(&first_block_offset);
	    byteswap4(&num_blocks);
	  }
	  if (pu_block_offset_set==false) {
	    pu_block_offset = first_block_offset;
	    pu_block_offset_set = true;
	  }

	  Profile_PU_Data[i].block =
		CXX_NEW_ARRAY(Profile_block_data, num_blocks,
			      MEM_src_nz_pool_ptr);

	  Profile_PU_Data[i].num_blocks = num_blocks;
	  Profile_PU_Data[i].checksum = checksum;
	} else ok = false;
      }
      if (ok && pu_block_offset_set &&
	  fseek(prof_f, profile_block_section+pu_block_offset, SEEK_SET)==0) {
        bool ok = true;
        for (int i=0; i<num_pus && ok; i++) {

	  int num_blocks = Profile_PU_Data[i].num_blocks;
	  int block_id = -1;
	  long long exec_count;

	  for (int j=0; j<num_blocks && ok; j++) {
	    if (fread(&block_id, 4, 1, prof_f)==1 &&
		fread(&exec_count, 8, 1, prof_f)==1) {
	      if (!Same_Byte_Sex) {
		byteswap4(&block_id);
		byteswap8(&exec_count);
	      }
	      Profile_PU_Data[i].block[j].block_id = block_id;
	      Profile_PU_Data[i].block[j].exec_count = exec_count;
	    } else ok = false;
	  }

	  if (ok) {
	    int len = strlen(Src_File_Name)+1;
	    Profile_Src_File_Name =
		CXX_NEW_ARRAY(char, len, MEM_src_nz_pool_ptr);
	    strcpy(Profile_Src_File_Name, Src_File_Name);
	    Profile_Num_PUs = num_pus;
	  }
	}
      }
    }
  }

  fclose(prof_f);

}

void* Get_Cur_PU_Profile_Data(const char* source_file_name,
		       const char* source_pu_name,
		       unsigned long long* challenge) {

  if (strcmp(source_file_name,Profile_Src_File_Name))
    return NULL;

  for (int i=0; i<Profile_Num_PUs; i++) {
    if (!strcmp(Profile_PU_Data[i].pu_name,source_pu_name)) {

      /* further check if there is any positive exec count */
      Profile_pu_data* pu_ptr = &(Profile_PU_Data[i]);
      int num_blocks = pu_ptr->num_blocks;
      Profile_block_data* block = pu_ptr->block;
      for (int j=0; j<num_blocks; j++) {
	if (block[j].exec_count>0) {
	  *challenge = pu_ptr->checksum;
	  return pu_ptr;
	}
      }
      return NULL;
    }
  }
  return NULL;
}

static int cmp_profile_block(const void* p1, const void* p2) {

  Profile_block_data* b1 = (Profile_block_data*)p1;
  Profile_block_data* b2 = (Profile_block_data*)p2;

  return b1->block_id - b2->block_id;

}

long long Get_Profile_Data(void* cur_pu_profile_data, int bb_id) {

  Profile_pu_data* pu_ptr = (Profile_pu_data*)cur_pu_profile_data;

  if (pu_ptr) {
    Profile_block_data b, *p;
    b.block_id = bb_id;
    p = (Profile_block_data*)bsearch(&bb_id, pu_ptr->block, pu_ptr->num_blocks,
		sizeof(Profile_block_data), cmp_profile_block);
    if (p==NULL)
      return -1LL;
    else if (p->exec_count==0)
      return -1LL;
    else
      return p->exec_count;
  }

  return -1LL;
}


