/* 
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
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


#include "defs.h"                       // std. mongoose definitions
#include "cmplrs/host.h"
#include <limits.h>                     // for PATH_MAX
#include <unistd.h>                     // for read(2)/write(2)
#ifdef _WIN32
#include <sys/utime.h>
#else
#include <utime.h>                      // for utime
#endif
#include <fcntl.h>                      // for open(2)
#include <sys/types.h>
#include <sys/stat.h>                   // for chmod(2)
#include <string.h>
#include <errno.h>                      // for sys_errlist
#include <vector>                       // for STL vector container
#include <ext/hash_map>
#ifndef _WIN32
#include <libgen.h>                     // for basename()
#endif
#include <time.h>                       // for time()
#include <cmplrs/rcodes.h>              // for RC_SYSTEM_ERROR

#include "linker.h"                     // std. linker's headers
#include "ipa_process.h"                // for tmpdir, etc.
#include "main.h"                       // for arg_vectors
#include "ipc_weak.h"

#include "errors.h"                     // for ErrMsg
#include "erglob.h"                     // error code
#include "glob.h"                       // for Tlog_File_Name
#include "tracing.h"                    // for Set_Trace_File
#include "cxx_memory.h"                 // for CXX_NEW
#include "opcode.h"                     // needed by wn_core.h
#include "wn_core.h"                    // needed by ir_bread.h
#include "pu_info.h"                    // needed by ir_bread.h
#include "ir_bread.h"                   // for WN_get_section_base ()

#include "dwarf_DST_mem.h"              // needed by ipc_file.h
#include "ipc_file.h"                   // for IP_FILE_HDR
#include "ipa_option.h"                 // ipa option flags
#include "ipc_link.h"                   // for ipa_link_link_argv
#include "fake.h"
#include <sys/wait.h>

#pragma weak tos_string
#pragma weak outfilename

using std::vector;
using __gnu_cxx::hash_map;

#if defined(__linux__) || defined(TARG_XTENSA)
#define _USE_GNU_MAKE_
#define TARGET_DELIMITER " : "
#else
#define TARGET_DELIMITER " ! "
#endif

#ifdef _WIN32
#define DIR_SEP "/"
#define DIR_SEP_CHAR '/'
  char* pl_name = "/perl/bin/MSWin32-x86/perl.exe";
#else
#define DIR_SEP "/"
#define DIR_SEP_CHAR '/'
  char* pl_name = "/bin/perl";
#endif


// To create an executable, we have to 
// -- Compile the symtab file to a .o file (an elf symtab)
// -- Compile the symtab to a .G file
// -- Using the .G file, compile each of the regular whirl files
//    (each file that contains a pu) to an elf .o file
// -- Link all of the .o files.

// For each of the regular whirl files, we keep track of 
// -- its name
// -- the command line we'll use to compile it
// -- a list of zero of more lines that will be added to the makefile
//    as comments.  Normally these will be the pu's contained in the file.

// How to use these functions:
// (1) Initialize with ipa_compile_init.
// (2) Call ipacom_process_symtab, where the argument is the symtab file
//     name.  (e.g. "symtab.I")
// (3) For each other whirl file:
//     (a) call ipacom_process_file
//     (b) call ipacom_add_comment zero or more times, using the 
//         index that ipacom_process_file returned.
// (4) call ipacom_doit.  ipacom_doit never returns.


// --------------------------------------------------------------------------
// File-local variables

// cmdfile_pl is a full pathname, as is outfiles_fullpath, 
// all of the others are basename only.

static char* cmdfile_name = 0;         // name of the cmdfile
static FILE* cmdfile = 0; 

static vector<const char*>* infiles = 0;
static vector<const char*>* outfiles = 0;
static vector<const char*>* outfiles_fullpath = 0;
static vector<const char*>* commands = 0;
static vector<UINT32>* ProMP_Idx = 0;
static vector<vector<const char*> >* comments = 0;

// Name of the symtab file as written by the ipa.  (e.g. symtab.I)
static char input_symtab_name[PATH_MAX] = "";

// Name of the symtab file that will be used to compile other whirl
// files. (e.g. symtab.G)
static char whirl_symtab_name[PATH_MAX] = "";

// Name of the elf symtab file. (e.g. symtab.o)
static char elf_symtab_name[PATH_MAX] = "";

// Command line for producing the whirl and elf symtab files.  
static const char* symtab_command_line = 0;
static const char* symtab_extra_args = 0;


// Map from short forms of command names (e.g. "cc") to full 
// forms (e.g. "/usr/bin/cc").

namespace {
  struct eqstr {
    bool operator()(const char* s1, const char* s2) const
      { return strcmp(s1, s2) == 0; }
  };
  typedef __gnu_cxx::hash_map<const char*, const char*, __gnu_cxx::hash<const char*>, eqstr>
          COMMAND_MAP_TYPE;
}

static COMMAND_MAP_TYPE* command_map;

// --------------------------------------------------------------------------

// Overview of what happens post-ipa.  We create a cmdfile in the
// tmpdir.  We spawn a shell to invoke make.  Make will compile each
// of the .I files in tmpdir, and will then do the final link in the
// current working directory.  After make terminates, the shell
// deletes all of the in the tmp_file_list, including the cmdfile
// itself and the shell script itself, moves all of the other files in
// tmpdir into the current working directory, and delete the tmpdir.
// Upon interrupt (using sh's builtin trap command), it does the same
// thing.

static const char* get_extra_args(const char* ipaa_filename);
static const char* get_extra_symtab_args(const ARGV&);
static void exec_smake(char* pl_cmdfile_name);
static void write_cleanup_function();
static void write_setup_signal_handling();
static void write_compile_file_commands(const char * tmpdir_name);
static void write_compile_symtab_command(const char * tmpdir_name);
static const char *  write_autotie_compile_file_commands(const char * tmpdir_name);
static void write_link_command(const char * tmpdir_name);

    /*
    	This is here because the gnu basename() doesn't strip
	off multiple slashes.
    */
static char*ipa_basename(char *name){

#ifndef _WIN32
    char *bname = basename(name);
#else
    char *bname = strrchr(name, DIR_SEP_CHAR);
    if (bname==NULL) return name;
#endif
    
    while (*bname == DIR_SEP_CHAR)
    	bname++;
    return bname;
}

static const char* abi()
{
  return ""; // Only one ABI for Xtensa
}

namespace {

// Returns true if path refers to an ordinary file.
bool file_exists(const char* path)
{
  if (!path || strlen(path) == 0)
    return false;

  struct stat buf;
  return stat(path, &buf) == 0 && S_ISREG(buf.st_mode);
}

} // Close unnamed namespace

#ifdef _WIN32
#define MAKE_EXECUTABLE "bin/make.exe"
#else
#define MAKE_EXECUTABLE "bin/make"
#endif

#define MAKE_STRING get_make_path()
char * get_make_path()
{
  static char * path = NULL;
  if (path == NULL) {
     /* one for the dir separator and one for the null terminator */
     char * tool_path = getenv("XTENSA_TOOLS");
     if (tool_path == NULL) {
        ErrMsg(EC_No_X, "Unable to find path to make.\n");
        exit(1);
     }
     path = (char *) malloc(strlen(tool_path) + strlen(MAKE_EXECUTABLE) + 2);

     strcpy(path, tool_path);
     strcat(path, DIR_SEP);
     strcat(path, MAKE_EXECUTABLE);
  }
  return path;
}

#ifdef _WIN32
#define XCC_EXECUTABLE "bin/xt-xcc.exe"
#else
#define XCC_EXECUTABLE "bin/xt-xcc"
#endif

char * get_xcc_path()
{
  static char * path = NULL;
  if (path == NULL) {
     /* one for the dir separator and one for the null terminator */
     char * tool_path = getenv("XTTOOLS");
     if (tool_path == NULL) {
        ErrMsg(EC_No_X, "Unable to find path to xcc.\n");
        exit(1);
     }
     path = (char *) malloc(strlen(tool_path) + strlen(XCC_EXECUTABLE) + 2);

     strcpy(path, tool_path);
     strcat(path, DIR_SEP);
     strcat(path, XCC_EXECUTABLE);
  }
  return path;
}


extern "C" void
ipa_compile_init ()
{ 
  Is_True(tmpdir, ("no IPA temp. directory"));

  Is_True(infiles == 0 && outfiles == 0 && commands == 0 && comments == 0
          && cmdfile_name == 0 && cmdfile == 0 && command_map == 0,
          ("ipa_compile_init already initialized"));

  infiles           = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  outfiles          = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  outfiles_fullpath = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  commands          = CXX_NEW (vector<const char*>,          Malloc_Mem_Pool);
  comments          = CXX_NEW (vector<vector<const char*> >, Malloc_Mem_Pool);

  if (infiles == 0 || outfiles == 0 || outfiles_fullpath == 0 ||
      commands == 0 || comments == 0)
    ErrMsg (EC_No_Mem, "ipa_compile_init");

  if (ProMP_Listing)
      ProMP_Idx = CXX_NEW (vector<UINT32>, Malloc_Mem_Pool);

  char name_buffer[256];
  sprintf(name_buffer, "%s.pl", tmpdir);
  cmdfile_name = (char *) malloc(strlen(name_buffer) + 1);
  if (!cmdfile_name)
    ErrMsg (EC_Ipa_Open, name_buffer, strerror(errno));

  strcpy(cmdfile_name, name_buffer);

  cmdfile = fopen(cmdfile_name, "w");
  if (cmdfile == 0)
    ErrMsg (EC_Ipa_Open, cmdfile_name, strerror(errno));

#ifndef _WIN32
  fprintf(cmdfile, "BEGIN {\n");
  fprintf(cmdfile, "\tmap s|^.*(/lib/perl5/.*)|$1|, @INC;\n");
  fprintf(cmdfile, "\t@new_array = map{\"%s\".$_} @INC;\n",
	  getenv("XTENSA_TOOLS"));
  fprintf(cmdfile, "\t@INC = @new_array\n}\n");
#endif

  fprintf(cmdfile, "use File::Copy;\n");
  fprintf(cmdfile, "use Cwd;\n");
  fprintf(cmdfile, "use strict;\n");

  write_cleanup_function();
  write_setup_signal_handling();

  command_map = CXX_NEW(COMMAND_MAP_TYPE, Malloc_Mem_Pool);
  if (command_map == 0)
    ErrMsg (EC_No_Mem, "ipa_compile_init");

  (*command_map)["cc"] = get_xcc_path();
  (*command_map)[MAKE_STRING] = get_make_path();
}


// Generate a command line to compile an ordinary whirl file into an object
// file.
static void
get_command_line (const IP_FILE_HDR& hdr, ARGV& argv, const char* inpath,
                  const char* outpath) 
{
  char* base_addr = (char*)
    WN_get_section_base (IP_FILE_HDR_input_map_addr (hdr), WT_COMP_FLAGS);

  if (base_addr == (char*) -1)
    ErrMsg (EC_IR_Scn_Read, "command line", IP_FILE_HDR_file_name (hdr));

  Elf64_Word argc = *((Elf64_Word *) base_addr);
  Elf64_Word* args = (Elf64_Word *) (base_addr + sizeof(Elf64_Word));

  // args[0] is the command, so we need to treat it specially.  If
  // TOOLROOT is set, and if args[0] isn't already an absolute pathname,
  // we need to construct an absolute pathname using TOOLROOT.

  Is_True(command_map != 0
            && command_map->find("cc") != command_map->end()
            && (*command_map)["cc"] != 0
            && strlen((*command_map)["cc"]) != 0,
          ("Full pathname for cc not set up"));

  if (argc > 0) {
    char* command = base_addr + args[0];
    
    // Look up the command name in the map; we may need to turn into into
    // a full pathname.
    if (command_map->find(command) == command_map->end()) {
      char* toolroot = getenv("XTTOOLS");
      if (toolroot == 0 || strchr(command, DIR_SEP_CHAR) != 0) 
        (*command_map)[command] = command;
      else {
        int len = strlen(toolroot) + strlen(command) + 116;
        char* buf = static_cast<char*>(malloc(len));
        if (!buf)
          ErrMsg (EC_No_Mem, "get_command_line");
        strcpy(buf, toolroot);
        strcat(buf, DIR_SEP "bin" DIR_SEP);
        strcat(buf, command);
        (*command_map)[command] = buf;
      }
    }
    Is_True(command_map->find(command) != command_map->end()
            && (*command_map)[command] != 0
            && strlen((*command_map)[command]) != 0,
            ("Full pathname for %s not found in command map", command));

    argv.push_back((*command_map)[command]);

    for (INT i = 1; i < argc; ++i) {
      argv.push_back (base_addr + args[i]);
    }
  }
  else {
    argv.push_back ((*command_map)["cc"]);
    argv.push_back ("-c");
  }

  argv.push_back(abi());

  argv.push_back (inpath);
  argv.push_back ("-o");
  argv.push_back (outpath);
  argv.push_back ("-c");
    
} // get_command_line


// temp. kludge:  ipacom_process_file should really take
// const IP_FILE_HDR& as argument instead of const PU_Info *
#include "ipc_symtab_merge.h"
static const IP_FILE_HDR&
get_ip_file_hdr (const PU_Info *pu)
{
  ST_IDX st_idx = PU_Info_proc_sym (pu);
  PU_IDX pu_idx = ST_pu (St_Table[st_idx]);
  return *AUX_PU_file_hdr (Aux_Pu_Table[pu_idx]);
}

extern "C" void
ipacom_process_symtab (char* symtab_file)
{

  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0,
          ("ipacom_process_symtab: ipacom not yet initialized"));

  Is_True(strlen(input_symtab_name) == 0 &&
          strlen(whirl_symtab_name) == 0 &&
          strlen(elf_symtab_name) == 0 &&
          symtab_command_line == 0,
          ("ipacom_process_symtab: symtab already initialized"));

  char* output_file = create_unique_file (symtab_file, 'o');

  const char* input_base = ipa_basename(symtab_file);
  const char* output_base = ipa_basename(output_file);

  // Save the three symtab file names in global variables.
  strcpy(input_symtab_name, input_base);
  strcpy(elf_symtab_name,   output_base);
  strcpy(whirl_symtab_name, output_base);
  whirl_symtab_name[strlen(whirl_symtab_name) - 1] = 'G';

  // Generate a command line to create the .G and .o files.
  char buf[3*PATH_MAX + 64];

  Is_True(command_map != 0
            && command_map->find("cc") != command_map->end()
            && (*command_map)["cc"] != 0
            && strlen((*command_map)["cc"]) != 0,
          ("Full pathname for cc not set up"));

  sprintf(buf, "%s -c %s %s -o %s -TENV:emit_global_data=%s %s",
            (*command_map)["cc"],
            abi(),
            input_symtab_name,
            elf_symtab_name,
            whirl_symtab_name,
            IPA_Enable_AutoGnum?"-Gspace 0":"");


  char* cmd = static_cast<char*>(malloc(strlen(buf) + 1));
  if (!cmd)
    ErrMsg (EC_No_Mem, "ipacom_process_symtab");

  strcpy(cmd, buf);
  symtab_command_line = cmd;

  Is_True(strlen(input_symtab_name) != 0 &&
          strlen(whirl_symtab_name) != 0 &&
          strlen(elf_symtab_name) != 0 &&
          symtab_command_line != 0,
          ("ipacom_process_symtab: initialization failed"));

} // ipacom_process_symtab

// The return value is the index of this file in the vectors.
extern "C" 
size_t ipacom_process_file (char* input_file,
                            const PU_Info* pu, UINT32 ProMP_id)
{
  Is_True(infiles != 0 && outfiles_fullpath != 0 && commands != 0 &&
          comments != 0,
          ("ipacom_process_file: ipacom not initialized"));

  Is_True(strlen(input_symtab_name) != 0 &&
          strlen(whirl_symtab_name) != 0 &&
          strlen(elf_symtab_name) != 0 &&
          symtab_command_line != 0,
          ("ipacom_process_file: symtab not initialized"));

  if (ProMP_Listing) {
      Is_True (ProMP_Idx != 0,
	       ("ipacom_process_file: ipacom not initialized"));
      ProMP_Idx->push_back (ProMP_id);
  }

  char* output_file = create_unique_file (input_file, 'o');

  const char* input_base = ipa_basename (input_file);
  const char* output_base = ipa_basename (output_file);

  infiles->push_back(input_base);
  outfiles->push_back(output_base);
  outfiles_fullpath->push_back(output_file);

  // Assemble the command line.

  ARGV argv;                          // vector<const char*>
  get_command_line (get_ip_file_hdr (pu), argv, input_base, output_base);

  char* str = (char*) malloc(2 * PATH_MAX + 64);
  sprintf(str, "-TENV:ipa_ident=%d -TENV:read_global_data=%s %s",
          time(0),
          whirl_symtab_name,
          IPA_Enable_AutoGnum?"-Gspace 0":"");

  argv.push_back(str);
#if 0
  if (ProMP_Listing) {
    char* str = static_cast<char*>(malloc(64));
    sprintf(str, "-PROMP:=ON -PROMP:next_id=%lu", (unsigned long) ProMP_id);
    argv.push_back(str);
  }
    
#ifdef TODO
  if (gspace_size) {
    WRITE_STRING("-Gspace", argv->argv[i]);
    sprintf(str, "%d", gspace_size);
    WRITE_STRING(str, argv->argv[++i]);
  }
#else
  static bool reported = false;
  if (!reported) {
    reported = true;
    DevWarn ("TODO: implement gspace_size command file");
  }
#endif
#endif

  if (IPA_Enable_Array_Sections)
    argv.push_back("-LNO:ipa");

  if (Profile_File_Name) {
    char* buffer;
    int length = strlen("-prof_opt ")+strlen(Profile_File_Name)+1;
    buffer = (char*)malloc(sizeof(char)*length);
    sprintf(buffer, "-prof_opt %s", Profile_File_Name);
    argv.push_back(buffer);
  }

  // Piece the command line together and push it onto the list.
  size_t cmdline_length = 0;
  ARGV::const_iterator i;

  for (i = argv.begin(); i != argv.end(); ++i)
    cmdline_length += strlen(*i) + 1;

  char* cmdline = static_cast<char*>(malloc(cmdline_length + 1));
  if (!cmdline)
    ErrMsg (EC_No_Mem, "ipacom_process_file");    

  cmdline[0] = '\0';
    
  for (i = argv.begin(); i != argv.end(); ++i) {
    strcat(cmdline, *i);
    strcat(cmdline, " ");
  }

  commands->push_back(cmdline);
    
  // Add an empty vector for this file's comments.
  comments->push_back(vector<const char*>());

  Is_True (infiles->size() > 0 &&
           infiles->size() == outfiles->size() &&
           infiles->size() == outfiles_fullpath->size() &&
           infiles->size() == commands->size() &&
           infiles->size() == comments->size(),
           ("ipacom_process_file: inconsistent vector sizes"));

  // Set up extra args for compiling symtab, if necessary.
  if (!symtab_extra_args)
    symtab_extra_args = get_extra_symtab_args(argv);

  return infiles->size() - 1;

} // ipacom_process_file

// Each file has a list of zero or more comments that will appear in the
// cmdfile.  (Usually, each comment will be the name of a pu.)  
// This function adds a comment to the n'th file's list.
extern "C"
void ipacom_add_comment(size_t n, const char* comment)
{
  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0,
          ("ipacom_add_comment: ipacom not initialized"));

  Is_True(comments->size() >= n + 1,
          ("ipacom_add_comment: invalid index %ld, max is %ld",
           n, comments->size()));

  Is_True(comment != 0, ("ipacom_add_comment: argument is a null pointer"));

  char* tmp = static_cast<char*>(malloc(strlen(comment) + 1));
  if (!tmp)
    ErrMsg (EC_No_Mem, "ipacom_add_commend");
  strcpy(tmp, comment);

  (*comments)[n].push_back(tmp);
}

char* ipc_copy_of (char *str)
{
  register int len;
  register char *p;

  len = strlen(str) + 1;
  p = (char *) MALLOC (len);
  MALLOC_ASSERT (p);
  memcpy (p, str, len);
  return p;
} /* ipc_copy_of */


extern "C"
void ipacom_doit (const char* ipaa_filename)
{
  Is_True(infiles != 0 && outfiles != 0 && outfiles_fullpath != 0 &&
          commands != 0 && comments != 0 && cmdfile != 0,
          ("ipacom_doit: ipacom not yet initialized"));
  Is_True(infiles->size() == outfiles->size() &&
          infiles->size() == outfiles_fullpath->size() &&
          infiles->size() == commands->size() &&
          infiles->size() == comments->size(),
          ("ipacom_doit: vectors are inconsistent"));


  if (infiles->size() > 0) {
    Is_True(strlen(input_symtab_name) != 0 &&
            strlen(whirl_symtab_name) != 0 &&
            strlen(elf_symtab_name) != 0 &&
            symtab_command_line != 0,
            ("ipacom_doit: symtab not initialized"));
  }

  write_compile_symtab_command(tmpdir);
  write_compile_file_commands(tmpdir);
  const char * output_tdk = write_autotie_compile_file_commands(tmpdir);
  write_link_command(tmpdir);

  fprintf(cmdfile, "\n");
  fprintf(cmdfile, "handle_signals();\n");
  fprintf(cmdfile, "compile_symtab();\n");
  if (IPA_Autotie) {
    fprintf(cmdfile, "compile_files(\"-xpres\");\n");
    fprintf(cmdfile, "compile_xpress_files();\n");
  }
  if (!IPA_Autotie || output_tdk) {
    char * xt_xtensa_params_link = NULL;
    char * xt_xtensa_params = NULL;
    if (output_tdk)
      {
        xt_xtensa_params_link = concat_names("--xtensa-params=", (char *)output_tdk);
        if (output_tdk[0] == '/')
          xt_xtensa_params = xt_xtensa_params_link;
        else
          xt_xtensa_params = concat_names("--xtensa-params=../", (char *)output_tdk);
      }
    
    fprintf(cmdfile, "compile_files(\"%s\");\n", xt_xtensa_params ? xt_xtensa_params : "");
    if (IPA_Enable_final_link)
      fprintf(cmdfile, "link_files(\"%s\");\n", xt_xtensa_params_link ? xt_xtensa_params_link : "");
  }

  fprintf(cmdfile, "cleanup();\n");
  fprintf(cmdfile, "exit(0);\n");

  fclose(cmdfile);

  if (Tlog_File_Name) {
    fclose (Tlog_File);  
  }

  exec_smake(cmdfile_name);
} // ipacom_doit


static void write_cleanup_function(void)
{
  // Define a perl function for the cleanup.

  fprintf(cmdfile, "sub cleanup {\n");

  if (!ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag) {

    // Remove temporary file: *.o *.M *.G *.save
    fprintf(cmdfile, "\tunlink <%s/*.o>;\n", tmpdir);
    fprintf(cmdfile, "\tunlink <%s/*.M>;\n", tmpdir);
    fprintf(cmdfile, "\tunlink <%s/*.M.acl>;\n", tmpdir);
    fprintf(cmdfile, "\tunlink <%s/*.G>;\n", tmpdir);

    fprintf(cmdfile, "\tmy $file;\n");

    // Move any files that we don't know about to cwd  
    fprintf(cmdfile, "\tforeach $file (<%s/*>) { copy ($file, cwd()) }\n", tmpdir);
    fprintf(cmdfile, "\tunlink <%s/*>;\n", tmpdir);

    // Remove the directory
    fprintf(cmdfile, "\trmdir (\"%s\");\n", tmpdir);

    // Remove command file
    fprintf(cmdfile, "\tunlink (\"%s\");\n", cmdfile_name);
  }
  fprintf(cmdfile, "};\n\n");   // End of cleanup function.
}

static void write_setup_signal_handling()
{
  fprintf(cmdfile, "sub handle_signals {\n");

  // Establish a signal handler so that cleanup always gets called.
  // Nice perl bonus is that even if the signal doesn't exist on
  // the system, perl will let us set up a signal hanlder for it
  fprintf(cmdfile, "\t$SIG{ABRT} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{EMT} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{SYS} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{POLL} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{HUP} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{INT} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{QUIT} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{ILL} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{TRAP} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{FPE} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{KILL} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{BUS} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{PIPE} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{TERM} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{USR1} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{USR2} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{IO} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{VTALRM} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{PROF} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{XCPU} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{XFSZ} = \\&cleanup;\n");
  fprintf(cmdfile, "\t$SIG{SEGV} = \\&cleanup;\n");
  fprintf(cmdfile, "}\n\n");

  //end signal handling stuff
} 


#if !defined(_WIN32)
#define PARALLEL 1
#endif

void write_compile_file_commands(const char * tmpdir_name)
{
  const char* extra_args = get_extra_args(0);
  fprintf(cmdfile, "sub compile_files {\n");
  fprintf(cmdfile, "\tmy ($extra_cflags) = @_;\n");
#if PARALLEL
  fprintf(cmdfile, "\tmy $failed = 0;\n");
  fprintf(cmdfile, "\tmy $waits = 0;\n");
  fprintf(cmdfile, "\tmy $pid = 0;\n");
#endif
  // For each whirl file, tell how to create the corresponding elf file.
  for (size_t i = 0; i < infiles->size(); ++i) {
    if (ld_ipa_opt[LD_IPA_SHOW].flag) {
      fprintf(cmdfile, "\tprintf(\"cd %s && %s %s $extra_cflags\\n\");\n",
	      tmpdir_name, (*commands)[i], extra_args);
    }
#if PARALLEL
    /* multi-task the sub compiles.  */
    fprintf(cmdfile, "\tmy $origdir = cwd();\n");
    fprintf(cmdfile, "\tchdir(\"%s\");\n", tmpdir_name); 
    fprintf(cmdfile, "\t$pid = fork ();\n");
    fprintf(cmdfile, "\tif (defined $pid) {\n");
    fprintf(cmdfile, "\t\t if ($pid == 0) {\n");
    fprintf(cmdfile, "\t\texec(\"%s %s $extra_cflags\")\n",  (*commands)[i], extra_args);
    fprintf(cmdfile, "\tor die(\"Failed to exec\");\n");
    fprintf(cmdfile, "\t}\n");
    fprintf(cmdfile, "\telse {\n");
    fprintf(cmdfile, "\t\t$waits++;\n");
    fprintf(cmdfile, "\t\t}\n\t}\n");
    fprintf(cmdfile, "\telse {\n");
    fprintf(cmdfile, "\t\t$failed++;\n");
    fprintf(cmdfile, "\t\t}\n");
    fprintf(cmdfile, "\tchdir($origdir);\n");
    fprintf(cmdfile, "\tif ($waits == %d) { \n", IPA_Max_Jobs);
    fprintf(cmdfile, "\t\t$pid = wait();\n");
    fprintf(cmdfile, "\t\tif ($? != 0) { $failed++; }\n");
    fprintf(cmdfile, "\t\t$waits--;\n");
    fprintf(cmdfile, "\t}\n");
#else
    fprintf(cmdfile, "\tmy $origdir = cwd();\n");
    fprintf(cmdfile, "\tmy $ok = chdir(\"%s\") && system(\"%s %s $extra_cflags\") == 0;\n",
            tmpdir_name, (*commands)[i], extra_args);
    fprintf(cmdfile, "\tchdir($origdir);\n");
    fprintf(cmdfile, "\tif (! $ok) {\n\t\tcleanup();\n\t\texit(1);\n\t}\n\n");
#endif
    const vector<const char*>& com = (*comments)[i];
    for (vector<const char*>::const_iterator it = com.begin();
         it != com.end();
         ++it)
      fprintf(cmdfile, "\t## %s\n", *it);
    fputs("\n", cmdfile);
  }
  
#if PARALLEL
  fprintf(cmdfile, "\twhile ($waits != 0) { \n");
  fprintf(cmdfile, "\t\t$pid = wait();\n");
  fprintf(cmdfile, "\t\tif ($? != 0) { $failed++;}\n");
  fprintf(cmdfile, "\t\t$waits--;\n");
  fprintf(cmdfile, "\t}\n");
  
  fprintf(cmdfile, "\tif ($failed != 0) {\n");
  fprintf(cmdfile, "\t\tcleanup();\n"); 
  fprintf(cmdfile, "\t\texit(1);\n\t}\n");
#endif

  fprintf(cmdfile, "}\n\n");
}


static void write_compile_symtab_command(const char * tmpdir_name)
{  
  const char* extra_args = get_extra_args(0);

  fprintf(cmdfile, "sub compile_symtab {\n");
  fprintf(cmdfile, "\tmy ($extra_cflags) = @_;\n");
  // This generates both the .o symtab and the .G symtab.
  if (strlen(elf_symtab_name) != 0) {
    Is_True(strlen(input_symtab_name) != 0 &&
            strlen(whirl_symtab_name) != 0 &&
            symtab_command_line != 0 && strlen(symtab_command_line) != 0,
            ("ipacom_doit: symtab not initialized"));

    if (ld_ipa_opt[LD_IPA_SHOW].flag) {
      fprintf(cmdfile, "\tprintf(\"cd %s && %s %s\\n\");\n\n",
	      tmpdir_name, symtab_command_line, extra_args);
    }
    fprintf(cmdfile, "\tmy $origdir = cwd();\n");
    fprintf(cmdfile, "\tmy $ok = chdir(\"%s\") && system(\"%s %s\") == 0;\n",
            tmpdir_name, symtab_command_line, extra_args);
    fprintf(cmdfile, "\tchdir($origdir);\n");
    fprintf(cmdfile, "\tif (! $ok) {\n\t\tcleanup();\n\t\texit(1);\n\t}\n\n");
  }

  fprintf(cmdfile, "}\n\n");
}


static void write_link_command(const char * tmpdir_name)
{
  // link commands
  ARGV* link_line = ipa_link_line_argv (outfiles_fullpath, 
					tmpdir, 
					elf_symtab_name);
  Is_True(link_line->size() > 1, ("Invalid link line ARGV vector"));

  fprintf(cmdfile, "sub link_files {\n");
  fprintf(cmdfile, "\tmy ($extra_flags) = @_;\n");
  if (ld_ipa_opt[LD_IPA_SHOW].flag) {
    for (ARGV::const_iterator i = link_line->begin();
	 i != link_line->end(); ++i) {
      fprintf(cmdfile, "\t\tprintf(\"%s \\\\\\n\");\n", *i);
    }
    fprintf(cmdfile, "\t\tprintf(\"$extra_flags \\\\\\n\");\n");
  }
  
  fprintf(cmdfile, "\tmy @sysargs = (\n");
  for (ARGV::const_iterator i = link_line->begin();
       i != link_line->end(); ++i) {
    fprintf(cmdfile, "\t\t\"%s\",\n", *i);
  }
  fprintf(cmdfile, "\t);\n");
  fprintf(cmdfile, "\tif ($extra_flags ne \"\") { push(@sysargs, $extra_flags);}\n");

  fprintf(cmdfile, "\tmy $rc = system(@sysargs);\n");
  fprintf(cmdfile, "\tif ($rc != 0) {\n\t\tcleanup();\n\t\texit(1);\n\t}\n}\n\n");
}

const char * write_autotie_compile_file_commands(const char * tmpdir_name)
{
  const ARGV* autotie_line = ipa_autotie_line_argv();
  ARGV::const_iterator iter = autotie_line->begin();
  const char *tie_target = "";
  const char *tdks_dir = NULL;
  const char *output_tdk = NULL;

  /* The base name to use for generated tdk and tie files. */
  const char *tie_base_name = strrchr(outfilename, '/');
  tie_base_name = ((tie_base_name) ? tie_base_name + 1 : outfilename);

  /* Hack around our testing system by removing any .nostdlib extension. */
  char *nsl_ext = strrchr(tie_base_name, '.');
  if (nsl_ext && !strcmp(nsl_ext, ".nostdlib"))
    {
      tie_base_name = strdup(tie_base_name);
      nsl_ext = strrchr(tie_base_name, '.');
      *nsl_ext = '\0';
    }
  
   for (; iter != autotie_line->end(); ++iter) {
    const char *str = *iter;
    
    if (!strncmp(str, "-XP:tdk=", strlen("-XP:tdk=")))
      output_tdk = str + strlen("-XP:tdk=");
    
    // if a tdk_dir is specified, then we form the full tdk path
    // from that directory and the executable target name
    if (!strncmp(str, "-XP:tdk_dir=", strlen("-XP:tdk_dir=")))
      {
	tdks_dir = str + strlen("-XP:tdk_dir=");
	output_tdk = concat_names((char *)tdks_dir, "/");
	output_tdk = concat_names((char *)output_tdk, (char *)tie_base_name);
	output_tdk = concat_names((char *)output_tdk, "_tdk");
      }
  }
  
  fprintf(cmdfile, "sub compile_xpress_files {\n");
  fprintf(cmdfile, "\tmy $rc = system(\n");
  fprintf(cmdfile, "\t\t\"%s %s -xpres", get_xcc_path(), ld_ipa_opt[LD_IPA_SHOW].flag ? "-v" : "");

  /* Add all the -XP flags, and find the output tie file, if any. */
  const char *output_tie = NULL;
  iter = autotie_line->begin();
  for (; iter != autotie_line->end(); ++iter) {
    const char *str = *iter;
    
    if (!strncmp(str, "-XP:tie=", strlen("-XP:tie=")))
      output_tie = str + strlen("-XP:tie=");
    
    /* We don't pass -XP:tdk=... or -XP:tdk_dir=... to auto. */
    if (!strncmp(str, "-XP:tdk=", strlen("-XP:tdk=")) ||
	!strncmp(str, "-XP:tdk_dir=", strlen("-XP:tdk_dir="))) {
      continue;
    }
    
    fprintf(cmdfile, " %s", str);
  }

  /* If output tie filename is not specified, then put it in the
     directory containing the tdks. */
  if (!output_tie) {
    if (tdks_dir)
      {
	output_tie = concat_names((char *)tdks_dir, "/");
	output_tie = concat_names((char *)output_tie, (char *)tie_base_name);
	output_tie = concat_names((char *)output_tie, ".tie");
      }
    else
      {
	output_tie = "out.tie";
      }
    
    fprintf(cmdfile, " -XP:tie=%s", output_tie);
  }
  
  for (vector<const char*>::iterator i = outfiles->begin();
       i != outfiles->end();
       ++i)
    fprintf(cmdfile, " %s/%s ", tmpdir, *i);
  
  fprintf(cmdfile, "\");\n");
  
  fprintf(cmdfile, "\tif ($rc != 0) {\n\t\tcleanup();\n\t\texit(1);\n\t}\n\n");
  
  /* If no tdk was specified, then don't run tc. */
  if (output_tdk)
    {
      fprintf(cmdfile, "\tunlink (\"");
      for (vector<const char*>::iterator i = outfiles->begin();
	   i != outfiles->end();
	   ++i)
	fprintf(cmdfile, " %s/%s", tmpdir, *i);
  
      fprintf(cmdfile, "\");\n");
      fprintf(cmdfile, "\tunlink(\"%s\");\n", output_tdk);
      fprintf(cmdfile, "\t$rc = system(\"tc -d %s %s\");\n\n", output_tdk, output_tie);
      fprintf(cmdfile, "\tif ($rc != 0) {\n\t\tcleanup();\n\t\texit(1);\n\t}\n\n");
      fprintf(cmdfile, "\tunlink(\"%s\");\n", output_tie);
    }
  fprintf(cmdfile, "}\n\n");

  return output_tdk;
}


// Helper function for get_extra_args.
static void escape_char (char *str)
{
  char *p = str + 1;

  do {
    *str++ = *p++;
  } while (*str != 0);
} /* escape_char */


// Collect any extra arguments that we will tack onto the command line.
// First collect them as a vector of strings, then concatenate them all
// together into a single string.

//FIXME: it appears that this function is always called with ipaa_filename==NULL

static const char* get_extra_args(const char* ipaa_filename)
{
  vector<const char*> args;
  args.reserve(16);
#ifndef TARG_XTENSA  
  switch (ld_ipa_opt[LD_IPA_SHARABLE].flag) {
  case F_MAKE_SHARABLE:
    args.push_back("-pic2");
    break;
  case F_CALL_SHARED:
  case F_CALL_SHARED_RELOC:
    args.push_back("-pic1");
    break;
  case F_NON_SHARED:
    args.push_back("-non_shared");
    break;
  case F_RELOCATABLE:
    if (IPA_Enable_Relocatable_Opt == TRUE)
      args.push_back("-pic1");
    break;
  }
#endif 
  // -IPA:keeplight:=ON, which is the default, means that we keep only
  // the .I files, not the .s files.
  if (ld_ipa_opt[LD_IPA_KEEP_TEMPS].flag && !IPA_Enable_Keeplight)
    args.push_back("-keep");

  if (ld_ipa_opt[LD_IPA_SHOW].flag)
    args.push_back("-show");

  /* If there's an IPAA intermediate file, let WOPT know: */
  if (ipaa_filename) {
    char* buf = (char*) malloc(strlen(ipaa_filename) + 32);
    if (!buf)
      ErrMsg (EC_No_Mem, "extra_args");

    sprintf(buf, "-WOPT:ipaa:ipaa_file=%s", ipaa_filename );
    args.push_back(buf);
  }

  /* If there are -WB,... options, pull them out and add them to the
     * list.  Strip the '-WB,', and treat non-doubled internal commas
     * as delimiters for new arguments (undoubling the doubled ones):
     */
  if (WB_flags) {
    string p = ipc_copy_of (WB_flags);
    while (*p) {
      args.push_back(p);
      while (*p) {
#if __STRIP_COMMAS__
        if (*p == ',') {
          if (p[1] != ',') {
            *p++ = 0;
            break;
          }
          else
            escape_char(p);
        }
        else if (p[0] == '\\' && p[1] != 0)
          escape_char (p);
#endif
        p++;
      }
    }
  }

  /* If there are -Yx,... options, pull them out and add them to the
     * list.  Several may be catenated with space delimiters:
     */
  vector<char*> space_ptr; // for restoring spaces overwritten by zero
  if (Y_flags) {
    char* p = Y_flags;
    while (*p) {
      args.push_back(p);
      while (*p) {
        if (*p == ' ') {
          space_ptr.push_back(p);
          *p++ = 0;
          break;
        }
        else if (p[0] == '\\')
          escape_char (p);
        p++;
      }
    }
  }

  size_t len = 0;
  vector<const char*>::const_iterator i;

  for (i = args.begin(); i != args.end(); ++i)
    len += strlen(*i) + 1;

  char* result = (char*) malloc(len + 1);
  if (!result)
    ErrMsg (EC_No_Mem, "extra_args");    

  result[0] = '\0';

  for (i = args.begin(); i != args.end(); ++i) {
    strcat(result, *i);
    strcat(result, " ");
  }

  // now restore spaces in Y_flags that were overwritten by zeros
  for (size_t idx = 0; idx < space_ptr.size(); idx++) {
    Is_True(*space_ptr[idx] == 0, ("space_ptr must point to 0"));
    *space_ptr[idx] = ' ';
  }

  return result;
} /* get_extra_args */

static const char* get_extra_symtab_args(const ARGV& argv)
{
  const char* result = get_extra_args(0);

  for (ARGV::const_iterator i = argv.begin(); i != argv.end(); ++i) {
    const char* const debug_flag = "-DEBUG";
    const char* const G_flag = "-G";
    const char* const TARG_flag = "-TARG";
    const char* const OPT_flag = "-OPT";
    const int debug_len = 6;
    const int G_len = 2;
    const int TARG_len = 5;
    const int OPT_len = 4;
    bool flag_found = false;

    // The link line contains -r.  That means we don't have enough information
    // from the link line alone to determine whether the symtab should be
    // compiled shared or nonshared.  We have to look at how one of the other
    // files was compiled.
    if (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_RELOCATABLE &&
                IPA_Enable_Relocatable_Opt != TRUE) {
      const char* const non_shared_flag = "-non_shared";
      if (strcmp(*i, non_shared_flag) == 0)
        flag_found = true;
    }

    if ((strncmp(*i, debug_flag, debug_len) == 0) ||
            (strncmp(*i, G_flag, G_len) == 0) ||
            (strncmp(*i, OPT_flag, OPT_len) == 0) ||
            (strncmp(*i, TARG_flag, TARG_len) == 0))

      flag_found = true;

    if (flag_found == true) {
      char* buf = static_cast<char*>(malloc(strlen(result) +
                                            strlen(*i) + 2));
      if (!buf)
        ErrMsg (EC_No_Mem, "extra_symtab_args");
      strcpy(buf, result);
      strcat(buf, " ");
      strcat(buf, *i);
      free(const_cast<char*>(result));
      result = buf;
     }
  }

  return result;
}

static void exec_smake (char* pl_cmdfile_name)
{
  /* Clear the trace file: */
  Set_Trace_File ( NULL );

#ifdef _OPENMP
  /* The exec process will cause mp slaves, if any, to
       be killed.  They will in turn send a signal to the
       master process, which is the new process.  This process
       in turn has a signal handler that dies when it finds out
       that its slaves die.  This way, if we ever compile -mp,
       we kill the child processes before doing the exec */
  mp_destroy_();
#endif

  // Call the shell.
  char * tool_path = getenv("XTENSA_TOOLS");
  if (tool_path == NULL) 
    tool_path = "";

  const int argc = 2;
  char* argv[argc+1];
  argv[0] = (char *) malloc(strlen(tool_path) + strlen(pl_name) + 2);
  strcpy(argv[0], tool_path);
  strcat(argv[0], pl_name);
  argv[1] = pl_cmdfile_name;
  argv[2] = 0;
  
#if defined(__linux__) || defined(sun)
  fflush(NULL);
  // There's a bug in Solaris that causes SIGALRM to kill 
  // execve'd process if it's not forked first
  pid_t forkpid = fork();

  if (forkpid == -1) {
    perror("fork failed");
    exit(RC_SYSTEM_ERROR);
  }
  else if (forkpid != 0) {
    pid_t waitpid;
    int waitstatus;
    while ((waitpid = wait(&waitstatus)) != forkpid) {
      if (waitpid == -1) {
        perror("wait failed");
        exit(RC_SYSTEM_ERROR);
      }
    }
    if (WIFEXITED(waitstatus)) {
      exit(WEXITSTATUS(waitstatus));
    }
    else {
      exit(RC_SYSTEM_ERROR);
    }
  }
  /*printf("executing %s\n", argv[0]);*/
  /*printf("argv[1] = %s\n", argv[1]);*/
  execve (argv[0], argv, environ_vars);

#else /*_WIN32*/
  _flushall();
  /* FIXME: If we use execvpe here, then windows will 
     think ipa_link.exe has exited, and that it should
     produce a command line right now. And then it won't
     produce one later. This is probably also bad for Make
     too, because make will think the process is done
     when it isn't. This method works for now, but it 
     would be nice to release the memory. */
  int ret = _spawnvpe(_P_WAIT, argv[0], argv, environ_vars);
  exit(ret);
#endif

  Fail_FmtAssertion ("ipa: exec perl failed");
}


/* Create a unique temporary file.  */
static string
make_tmp_file (string name, char suffix)
{
  int len = strlen (name);
  if (len + 4 >= PATH_MAX) {
    fprintf(stderr,"%s %s\n","path name too long:", name);
    exit(1);
  }

  char path[PATH_MAX];
  strcpy (path, name);

  if (suffix && len >= 2) {
    /* remove the original suffix */
    if (path[len-2] == '.') {
      len -= 2;
      path[len] = 0;
    }
  }

  if (suffix) {
    path[len] = '.';
    path[len+1] = suffix;
    path[len+2] = 0;
  }

  if (access (path, F_OK) != 0)
    return strdup (path);

  int count = 1;
  do {
    if (suffix)
      sprintf (&(path[len]), ".%d.%c", count, suffix);
    else
      sprintf (&(path[len]), ".%d", count);
    count++;
  } while (access (path, F_OK) == 0);

  return strdup (path);
}


/* Create a new file with a unique name.  */
string
create_unique_file (const string path, char suffix)
{
  /* Length of tmpdir + basename of path and '/' between the dir
     and the basename + null terminator. */
  string base = ipa_basename (path);
  string p = (string) MEM_POOL_Alloc(Malloc_Mem_Pool, 
		                     strlen(tmpdir)+strlen(base)+5);
  strcpy (p, tmpdir);
  strcat (p, DIR_SEP);
  strcat (p, base);
  string new_path = make_tmp_file(p, suffix);
  free (p);

  return new_path;
}

void add_to_tmp_file_list (string path) 
{}
