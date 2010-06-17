
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


//   si_gen
/////////////////////////////////////
//
//  Description:
//
//      Digest the description of a particular hardware implementation's
//      scheduling information and generate a c file that describes the
//      features.  The interface is extensively described in si_gen.h.
//
/////////////////////////////////////

//  $Revision: 1.29 $
//  $Date: 2000/04/06 02:33:26 $
//  $Author: mtibuild $
//  $Source: /isms/cmplrs.src/osprey1.0/common/targ_info/generate/RCS/si_gen.cxx,v $


#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdarg.h>
#include <map>

using namespace std;

#include "topcode.h"
#include "targ_isa_properties.h"
#include "targ_isa_subset.h"
#include "targ_isa_operands.h"
#include "gen_util.h"
#include "si_gen.h"

#ifdef SI_GEN_DYNAMIC
#include "ti_si.h"
template<> int TI_SI_RRW::_length = 1;
template<> bool TI_SI_RRW::_length_set = false;
template<> int TI_SI_RESOURCE_ID_SET::_length = 1;
template<> bool TI_SI_RESOURCE_ID_SET::_length_set = false;
static MEM_POOL *sched_pool = NULL;
#define SCHEDNEW(x) CXX_NEW(x, sched_pool)
#define SCHEDNEW_ARRAY(x, c) CXX_NEW_ARRAY(x, c, sched_pool)
#define TOP_NAME(x) TI_TOP_Name(x)
#define TOP_COUNT TI_TOP_Count()
#define TOP_HAS_PROPERTY(t, p) TI_ISA_Property_Set(p, t)
#define MAX_OPERANDS TI_ISA_Operand_Max()
#define MAX_RESULTS TI_ISA_Result_Max()
#else
#define SCHEDNEW(x) new x
#define SCHEDNEW_ARRAY(x, c) new x[c]
#define TOP_NAME(x) TOP_Name(x)
#define TOP_COUNT TOP_count
#define TOP_HAS_PROPERTY(t, p) TOP_has_property(t, p)
#define MAX_OPERANDS ISA_OPERAND_max_operands
#define MAX_RESULTS ISA_OPERAND_max_results
#define TI_SI_INVALID_FORMAT_ID (-1)
template <int type_id> class TI_SI_RRV;
typedef TI_SI_RRV<1> TI_SI_RRW;
typedef TI_SI_RRV<2> TI_SI_RESOURCE_ID_SET;
#endif

TI_SI_RRW* format_resource_overuse_mask;

typedef struct format_mask {
  int format_id;
  unsigned long long mask;
  int shift;
  struct format_mask* next;
} format_mask_t;

format_mask_t* format_resource_overuse_masks=NULL;

static ISA_SUBSET machine_isa;

// Parameters:
const int bits_per_long = 32;
const int bits_per_long_long = 64;
const bool use_long_longs = true;       // For now always

/////////////////////////////////////
int Mod( int i, int j )
/////////////////////////////////////
//  Mathematically correct integer modulus function.  Unlike C's
//  builtin remainder function, this correctly handles the case where
//  one of the two arguments is negative.
/////////////////////////////////////
{
  int rem;

  if ( j == 0 )
    return i;

  rem = i % j;

  if ( rem == 0 )
    return 0;

  if ( (i < 0) != (j < 0) )
    return j + rem;
  else
    return rem;
}

/////////////////////////////////////
static void Maybe_Print_Comma(FILE* fd, bool& is_first)
/////////////////////////////////////
// Print a "," to <fd> if <is_first> is false.  Update <is_first> to false.
// Great for printing C initializers.
/////////////////////////////////////
{
  if ( is_first )
    is_first = false;
  else
    fprintf(fd,",");
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class GNAME {
/////////////////////////////////////
// A generated name for a variable in the generated file.  This supports a
// unified method for naming and getting the names of the objects we generate.
/////////////////////////////////////
public:
  GNAME();
  // Generate a unique name.  Don't care about prefix.
  GNAME(const char* prefix);
  // Generate a unique name.  Force a particular prefix.
  GNAME(GNAME& other);
  // Generate a name that is a copy of <other>.  The name will not be unique.
  // Really only useful when <other> is about to be destructed, but we still
  // need to refer to it.
  const char* Gname();
  // Return the name.  This is the name under which the object is defined.
  const char* Addr_Of_Gname();
  // Return a pointer to the named object.
  void Stub_Out();
  // We've decided not to define the object after all but we may still want a
  // pointer to it.  After this call, Addr_Of_Gname will return 0.

private:
  char gname[16];       // Where to keep the name.  (This could be more
                        //   hi-tech, but why?
  bool stubbed;         // Stubbed-out?
  static int count;     // For generating the unique names.
};

int GNAME::count = 0;

GNAME::GNAME() : stubbed(false) {
  sprintf(gname,"&gname%d",count++);
}

GNAME::GNAME(const char* prefix) : stubbed(false) {
  assert(strlen(prefix) <= 8);
  sprintf(gname,"&%s%d",prefix,count++);
}

GNAME::GNAME(GNAME& other) : stubbed(false) {
  sprintf(gname,"%s",other.gname);
}

const char* GNAME::Gname() {
  if (stubbed)
    return "0";
  else
    return gname + 1;
}

const char* GNAME::Addr_Of_Gname() {
  if (stubbed)
    return "0";
  else
    return gname;
}

void GNAME::Stub_Out() {
  stubbed = true;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES_WORD {
/////////////////////////////////////
// A machine word in the resource reservation table.  We use an encoding of
// resources that allows very effecient checking for resources with more than
// a single instance.  Shifted arithmetic is used to check for resource
// availability and to reserve resources.  Each resource has a reserved field
// in the RES_WORD.  The field for a given resource r is log2(count(r)) + 1
// bits wide.  This field is wide enough to hold the count of members of r and
// one extra bit to the left of the count.  This bit is called the "overuse
// bit".  The field is initialized to be 2**field_width - (count + 1).  This
// means that we can add count elements to the field without effecting the
// overuse bit, but adding more elements to the field will set the overuse
// bit.  So we can can check for resource availability of all the resources
// reqpresented in a word with an an add (of the counts required) and a mask
// of the overuse bits.  If the result is non-zero, there is a resource
// overuse (conflict).
//
// In theory this could be any natural sized integer supported on the host
// architecture, but for now we will always use long longs so we don't have to
// worry about generating/checking more than one word/cycle.  This could be
// extended, but it would mean moving the actual resource checking to the
// generated side of the compile time interface (since it would need to know
// the format of the resource reservation table which could change...

public:
  static void Find_Word_Allocate_Field(int width, int min_width,
				       int count,
                                       int &word, int &bit);
  // Allocate the first available resource field with <width> bits to hold
  // <count> resources.  A new resource word is allocated if required.  On
  // return, <word> and <bit> hold the word index and bit index of the
  // allocated word. <min_width> specifies the minimum bit field width.
  // This is needed in order to leave room for further count up/down
  // even passing the overuse bit. Currently only resources created for
  // issue format requires <min_width> >= <width>. For other resources,
  // <min_width> should be equal to <width>.

  static void Output_All(FILE* fd);
  // Write resource word descriptions to output.

#ifdef SI_GEN_DYNAMIC
  static bool Generate_All(TI_SI_RRW **RRW_initializer, TI_SI_RRW **RRW_overuse_mask, TI_SI_RRW** RRW_format_resource_overuse_mask);
  // Generate resource word descriptions.
#endif
  
private:
  int bit_inx;                          // Index of first next free bit
  const int word_inx;                   // My index in table
  long long initializer;                // Value when no resources used
  long long overuse_mask;               // Bits to check
                                        //   for overuse after adding new
                                        //   resources
  RES_WORD *next;                       // next RES_WORD in order
  
  static RES_WORD *res_words;           // list of all res_words in order
  static RES_WORD *res_words_tail;      // 
  static int count;                     // Of all resource words
                                        //   resource words?

  RES_WORD()
    : bit_inx(0),
      word_inx(count++),
      initializer(0),
      overuse_mask(0),
      next(NULL)
  {
    if (res_words == NULL)
      res_words = this;
    else
      res_words_tail->next = this;

    res_words_tail = this;
  }
  bool Allocate_Field(int width, int min_width, int count, int &word, int &bit);
};

RES_WORD *RES_WORD::res_words = NULL;
RES_WORD *RES_WORD::res_words_tail = NULL;
int RES_WORD::count = 0;

/////////////////////////////////////
bool RES_WORD::Allocate_Field(int width, int min_width, int count,
			      int &word, int &bit)
/////////////////////////////////////
// Allocate a field <min_width> bits wide to hold <count> elements.  Return true
// to indicate success with <word> set to my word_inx and <bit> set to the
// the bit index of the start of the field. The initializer and the overuse_mask
// are calculated based on <width>.
/////////////////////////////////////
{
  int new_inx = bit_inx + min_width;

  if (    use_long_longs && new_inx > bits_per_long_long
       || !use_long_longs && new_inx > bits_per_long
  ) {
    return false;
  }

  word = word_inx;
  bit = bit_inx;
  initializer |= ((1ULL << (width - 1)) - (count + 1)) << bit_inx;
  overuse_mask |= (1ULL << (width - 1)) << bit_inx;
  bit_inx += min_width;
  return true;
}

void RES_WORD::Find_Word_Allocate_Field(int width, int min_width, int count,
                                        int &word, int &bit)
{
  for (RES_WORD *rscan = res_words; rscan != NULL; rscan = rscan->next) {
    if ( rscan->Allocate_Field(width,min_width,count,word,bit) )
      return;
  }

  RES_WORD* new_res_word = SCHEDNEW(RES_WORD());

  if ( ! new_res_word->Allocate_Field(width,min_width,count,word,bit) ) {
    fprintf(stderr,"### Cannot allocate field for %d resources\n",count);
    exit(EXIT_FAILURE);
  }
}

void RES_WORD::Output_All(FILE* fd)
{
  if ( count == 0 )
    fprintf(stderr,"ERROR: no resource words allocated.\n");
  else if ( count > 1 ) {
    fprintf(stderr,"ERROR: cannot handle %d > 1 long long worth of "
                   "resource info.\n",
                   count);
  }
  else {
    // Important special case.  We don't need a vector of resource words at all
    // and can just use a scalar.
    fprintf(fd,"TI_SI_RRW TSI_RRW_initializer = 0x%" LL_FORMAT "x;\n",
               res_words->initializer);
    fprintf(fd,"TI_SI_RRW TSI_RRW_overuse_mask = 0x%" LL_FORMAT "x;\n",
               res_words->overuse_mask);

    fprintf(fd,"TI_SI_RRW TSI_RRW_format_resource_overuse_mask[1] = {\n");
    fprintf(fd,"\t0x0\n};\n");
  }
}

#ifdef SI_GEN_DYNAMIC
bool RES_WORD::Generate_All (TI_SI_RRW **RRW_initializer, TI_SI_RRW **RRW_overuse_mask, TI_SI_RRW** RRW_format_resource_overuse_mask)
{
  if ( count == 0 )
    {
      DevWarn("no resource words allocated.");
      return false;
    }
  
  TI_SI_RRW::set_length(count*sizeof(unsigned long long)*8);

  unsigned long long *initializer = SCHEDNEW_ARRAY(unsigned long long, count);
  unsigned long long *overuse_mask = SCHEDNEW_ARRAY(unsigned long long, count);
  RES_WORD* rw = res_words;
  int i;
  for (i=0; i<count; i++, rw=rw->next) {
    initializer[i] = rw->initializer;
    overuse_mask[i] = rw->overuse_mask;
  }

  *RRW_initializer = SCHEDNEW(TI_SI_RRW(initializer));

  *RRW_overuse_mask = SCHEDNEW(TI_SI_RRW(overuse_mask));

  format_resource_overuse_mask =
		SCHEDNEW_ARRAY(TI_SI_RRW, sizeof(TI_SI_FORMAT_ID_SET)*8);
  format_mask_t* p = format_resource_overuse_masks;
  while (p) {
    format_resource_overuse_mask[p->format_id].add_bit_mask(p->mask, p->shift);
    p=p->next;
  }
  *RRW_format_resource_overuse_mask = format_resource_overuse_mask;

  return true;
}
#endif

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES {
/////////////////////////////////////
// A machine level resource.
/////////////////////////////////////

public:
  RES(const char *name,int count,int format_id);
  // <name> is used for documentation and debugging.  <count> is the number of
  // elements in the class.

  static RES* Get(int id);
  // Find and return the resource with the given <id>.

  const char* Name() const { return name; }
  // Return debugging name.

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return name of pointer to this resource object (in generated code).

  unsigned int Count() const { return count; }
  // How may members?

  int Word() const { return word; }
  // Index of word in resource reservation table.  (We have sort of allowed
  // there to be more than one, but this is probably not fully working.)

  int Id() const { return id; }
  // Unique ID of resource.  Index into table of pointers to resources in the
  // generated file.

  unsigned int Shift_Count() const { return shift_count; }
  // Bit index of the field in the resource reservation word.

  int Format_Id() const { return format_id; }
  // Xtensa format id (>=0) or TI_SI_INVALID_FORMAT_ID if not a resource
  // for format constraints

  static void Output_All( FILE* fd );
  // Write out all the resource info to <fd>.

#ifdef SI_GEN_DYNAMIC
  static bool Generate_All(INT *resource_count, TI_SI_RESOURCE ***resources);
  // Generate all resource info.

  TI_SI_RESOURCE *generated_resource;
  // Resource information for this RES.
#endif

private:
  const int count;          // Available per cycle
  const char* name;         // For documentation and debugging
  GNAME gname;              // Generated symbolic name
  int word;                 // Which word in the table?
  int field_width;          // How wide the field?
  int shift_count;          // How much to shift (starting pos of the low
                            //   order bit
  const int id;             // Unique numerical identifier
  int format_id;	    // Xtensa format id (>=0) or
			    // TI_SI_INVALID_FORMAT_ID if not a resource
			    // for format constraints
  static int total;         // Total number of different RESs (not the the
                            //   total of their counts, 1 for each RES)
  static RES **resources;   // Array of all resources, indexed by their Id's
  static int resources_allocated;
  static int max_slot_resource_copies;	// largest number of copies for
					// issue slot resources
  
  void Calculate_Field_Width();
  void Calculate_Field_Pos();

  static void Calculate_Fields();
  // Calculate fields for all resources.  This can only be done at the very
  // end becaue we may not know for sure that there are no multiple resources
  // until then.

  void Output( FILE* fd );
};

int  RES::total = 0;
int  RES::resources_allocated = 0;
int  RES::max_slot_resource_copies = 0;
RES **RES::resources = NULL;

RES::RES(const char *name, int count, int format_id)
// constructor maintains list of all resources.
  : count(count), name(name), id(total++), gname("resource"),
    format_id(format_id)
{
  if (id >= resources_allocated)
    {
      int new_res_allocated = ((resources_allocated == 0) ? 128 : resources_allocated * 2);
      RES **new_res = SCHEDNEW_ARRAY(RES *, new_res_allocated);
      memcpy(new_res, resources, sizeof(RES*)*resources_allocated);
      resources_allocated = new_res_allocated;
      resources = new_res;
    }
  
  resources[id] = this;
}

RES* RES::Get(int i)
{
  assert(total > 0 && i >= 0 && i < total);
  return resources[i];
}

void RES::Output_All( FILE* fd )
{
  int i;

  Calculate_Fields();

  for ( i = 0; i < total; ++i )
    resources[i]->Output(fd);

  fprintf(fd,"int TSI_resource_count = %d;\n",total);
  fprintf(fd,"TI_SI_RESOURCE *TSI_resources[] = {");

  bool is_first = true;
  for ( i = 0; i < total; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s",resources[i]->gname.Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");
}

#ifdef SI_GEN_DYNAMIC
bool RES::Generate_All (INT *res_count, TI_SI_RESOURCE ***res)
{
  Calculate_Fields();

  TI_SI_RESOURCE_ID_SET::set_length(total);

  TI_SI_RESOURCE **tres = SCHEDNEW_ARRAY(TI_SI_RESOURCE *, total);
  TI_SI_RESOURCE *res_array = SCHEDNEW_ARRAY(TI_SI_RESOURCE, total);
  
  for ( int i = 0; i < total; ++i )
    {
      TI_SI_RESOURCE *new_res = res_array + i;
      new_res->name = resources[i]->name;
      new_res->id = resources[i]->id;
      new_res->avail_per_cycle = resources[i]->count;
      new_res->word_index = resources[i]->word;
      new_res->bit_index = resources[i]->shift_count;
      new_res->format_id = resources[i]->format_id;

      tres[i] = new_res;
      resources[i]->generated_resource = new_res;
    }

  *res_count = total;
  *res = tres;
  
  return true;
}
#endif

/////////////////////////////////////
void RES::Calculate_Field_Width()
/////////////////////////////////////
//  Calculate the number of bits for my field and set <field_width>
//  accordingly.
/////////////////////////////////////
{
  int i;

  assert(count > 0);

  for ( i = 31 ; i >= 0 ; --i ) {
    if ((( (int) 1) << i) & count) {
      field_width = i + 2;
      break;
    }
  }
}

void RES::Calculate_Field_Pos()
{
  unsigned long long format_overuse_mask;

  Calculate_Field_Width();
  int min_width=field_width;

  if (format_id != TI_SI_INVALID_FORMAT_ID) {

#ifdef SI_GEN_DYNAMIC
    assert(TI_ISA_Num_Slots(format_id)<=TI_TIE_SLOTS_MAX);
    assert(max_slot_resource_copies > 0);
#endif

    if (max_slot_resource_copies<=1) {

      min_width = 2;

      // this allows 1 op be put into a bundle without overflowing
      // into the next bit field. Basically,
      // count	overuse_mask	initializer
      //     1          10	00
      // so 2 bits are big enough to hold the value of any initializer + 1.

      format_overuse_mask = 1ULL;

    } else if (max_slot_resource_copies<=3) {

      min_width = 3;

      // this allows 3 ops be put into a bundle without overflowing
      // into the next bit field. Basically,
      // count	overuse_mask	initializer
      //     1          110	000
      //     2          100	001
      //     3          100	000
      // so 3 bits are big enough to hold the value of any initializer + 3.

      if (count==1) format_overuse_mask = 3ULL;
      else format_overuse_mask = 1ULL;

    } else if (max_slot_resource_copies<=7) {

      min_width = 4;

      // this allows 7 ops be put into a bundle without overflowing
      // into the next bit field. Basically,
      // count	overuse_mask	initializer
      //     1          1110	0000
      //     2          1100	0001
      //     3          1100	0000
      //     4          1000	0011
      //     5          1000	0010
      //     6          1000	0001
      //     7          1000	0000
      // so 4 bits are big enough to hold the value of any initializer + 7.

      if (count==1) format_overuse_mask = 7ULL;
      else if (count==2 || count==3) format_overuse_mask = 3ULL;
      else format_overuse_mask = 1ULL;

    } else if (max_slot_resource_copies<=15) {

      min_width = 5;

      // this allows 15 ops be put into a bundle without overflowing
      // into the next bit field. Basically,
      // count	overuse_mask	initializer
      //     1         11110	0000
      //     2         11100	0001
      //     3         11100	0000
      //     4         11000	0011
      //     5         11000	0010
      //     6         11000	0001
      //     7         11000	0000
      //     8         10000	0111
      //     9         10000	0110
      //    10         10000	0101
      //    11         10000	0100
      //    12         10000	0011
      //    13         10000	0010
      //    14         10000	0001
      //    15         10000	0000
      // so 5 bits are big enough to hold the value of any initializer + 15.

      if (count==1) format_overuse_mask = 15ULL;
      else if (count==2 || count==3) format_overuse_mask = 7ULL;
      else if (4<=count && count<=7) format_overuse_mask = 3ULL;
      else format_overuse_mask = 1ULL;

    } else if (max_slot_resource_copies<=31) {

      min_width = 6;

      // this allows 31 ops be put into a bundle without overflowing
      // into the next bit field. Basically,
      // count	overuse_mask	initializer
      //     1        111110	0000
      //     2        111100	0001
      //     3        111100	0000
      //     4        111000	0011
      //     5        111000	0010
      //     6        111000	0001
      //     7        111000	0000
      //     8        110000	0111
      //     9        110000	0110
      //    10        110000	0101
      //    11        110000	0100
      //    12        110000	0011
      //    13        110000	0010
      //    14        110000	0001
      //    15        110000	0000
      //    16        100000	1111
      //    17        100000	1110
      //    18        100000	1101
      //    19        100000	1100
      //    20        100000	1011
      //    21        100000	1010
      //    22        100000	1001
      //    23        100000	1000
      //    24        100000	0111
      //    25        100000	0110
      //    26        100000	0101
      //    27        100000	0100
      //    28        100000	0011
      //    29        100000	0010
      //    30        100000	0001
      //    31        110000	0000
      // so 6 bits are big enough to hold the value of any initializer + 31.

      if (count==1) format_overuse_mask = 31ULL;
      else if (count==2 || count==3) format_overuse_mask = 15ULL;
      else if (4<=count && count<=7) format_overuse_mask = 7ULL;
      else if (8<=count && count<=15) format_overuse_mask = 3ULL;
      else format_overuse_mask = 1ULL;

    } else {
#ifdef SI_GEN_DYNAMIC
      FmtAssert(0,("Unsupported max slot number (%d)", max_slot_resource_copies));
#endif
    }
  }

  RES_WORD::Find_Word_Allocate_Field(field_width,min_width,count,word,shift_count);

  if (format_id != TI_SI_INVALID_FORMAT_ID) {
    // update the format_overuse mask for format
    // the following has to match RES_WORD::Allocate_Field() and generates
    // the format_overuse mask above
#ifdef SI_GEN_DYNAMIC
    format_mask_t* tmp = SCHEDNEW(format_mask_t);
    tmp->format_id = format_id;
    tmp->mask = format_overuse_mask;
    tmp->shift = 64 * word + field_width-1+shift_count;
    tmp->next = format_resource_overuse_masks;
    format_resource_overuse_masks = tmp;
#endif
  }
}

/////////////////////////////////////
void RES::Calculate_Fields()
/////////////////////////////////////
//  See interface description.
//  Description
/////////////////////////////////////
{
  for ( int i = 0; i < total; ++i )
    if (resources[i]->format_id != TI_SI_INVALID_FORMAT_ID &&
	resources[i]->count>max_slot_resource_copies)
      max_slot_resource_copies = resources[i]->count;

  for ( int i = 0; i < total; ++i )
    resources[i]->Calculate_Field_Pos();
}

/////////////////////////////////////
void RES::Output( FILE* fd )
/////////////////////////////////////
// Allocate my field in the resource reservation table.
/////////////////////////////////////
{
  fprintf(fd,"TI_SI_RESOURCE %s = {\"%s\",%d,%d,%d,%d,-1};\n",
             gname.Gname(),
             name,
             id,
             count,
             word,
             shift_count /*, format_id */);
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES_REQ {
/////////////////////////////////////
// A resource requirement.  Represents all the resources needed to perform an
// instruction.
/////////////////////////////////////

public:
  RES_REQ();

  bool Add_Resource(const RES* res, int cycle);
  // Require an additional resource <res> at the given <cycle> relative to
  // my start.  Return indication of success.  If adding the resource would
  // create an overuse, don't add it and return false.

  int Get_Resource_Count(const RES* res, int cycle);
  // Return the count required for <res> at <cycle>.

  void Output(FILE* fd);
  // Output my definition and initialization.

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return name of pointer to me (in generated code).

  const char* Gname() { return gname.Gname(); }
  // Return my name (in generated code).

  bool Compute_Maybe_Output_II_RES_REQ(int ii, FILE* fd,
                                       GNAME*& res_req_gname,
                                       GNAME*& resource_id_set_gname );
  // When software pipelining, we want to check all the resources for a given
  // cycle of the schedule at once.  Because the resource requirement may be
  // longer than the II into which we are trying to schedule it, we statically
  // combine the resource requirements for each II shorter than the number of
  // cycles in the request.  This function does the combining and returns a
  // bool to indicate whether the resource requirement can be scheduled in the
  // given <ii>.  If it can, the combined a definition and initialization of
  // resource requirement is output to <fd> under the GNAME <res_req_gname>.
  // A cycle indexed set of resource id's used is also output under the GNAME
  // <resource_id_set_gname>.

  int Max_Res_Cycle() { return max_res_cycle; }
  // Return the cycle (relative to my start) of the latest resource I
  // require.  (Used to know how many II relative resource requirements need
  // to be computed/output.)

  void Compute_Output_Resource_Count_Vec(FILE* fd);
  // Count up all the resources of each kind that I require (in all my cycles)
  // and output a definition and initialization.

  const char* Res_Count_Vec_Gname() { return res_count_vec_gname.Gname(); }
  // Return name of pointer to start of my resource count vector.

  int Res_Count_Vec_Size() const { return res_count_vec_size; }
  // Return length of my resource count vector.

  const char* Res_Id_Set_Gname() { return res_id_set_gname.Gname(); }
  // Return name of pointer to start of vector of resource id sets, one per
  // cycle.

#ifdef SI_GEN_DYNAMIC
  bool Compute_Generate_Resource_Count_Vec(int *res_count, TI_SI_RESOURCE_TOTAL **res_total);
  void Generate(TI_SI_RRW **res_req, TI_SI_RESOURCE_ID_SET **res_id_set);
#endif
    
private:

  /////////////////////////////////////
  class CYCLE_RES {
  /////////////////////////////////////
  // A cycle and resource (id) combined into a single object.  Used as a key
  // into a map so we can find out how may of the given resources are required
  // in the given cycle.
  /////////////////////////////////////

  public:
    CYCLE_RES(int cycle, const RES* res) : cycle(cycle), res_id(res->Id()) {}
    // Construct the <cycle,res> combination.

    int Cycle() const { return cycle; }
    // Return cycle component.

    RES* Res() const { return RES::Get(res_id); }
    // Return resource component.

    friend bool operator < (const CYCLE_RES a, const CYCLE_RES b)
    // Ordering for map.
    {  // I didn't want to put this inline, but mongoose C++ forced me to.
      return    a.cycle< b.cycle
             || a.cycle == b.cycle && a.res_id < b.res_id;
    }

    CYCLE_RES()
    // Horrible useless required constructor required by STL map.
     : cycle(0), res_id(0)
    {  // Also forced inline by mongoose C++.
      fprintf(stderr,"### Default initializer for CYCLE_RES"
              " shouldn't happen.\n");
    }

  private:
    const short cycle;
    const short res_id;
  };

  typedef map < CYCLE_RES,int,less <CYCLE_RES> > CYCLE_RES_COUNT_MAP;
  // For keeping track of the number of resources of a given type in a given
  // cycle.  <cycle,res> => count

  int max_res_cycle;
  // Latest cycle with a resource requirement

  CYCLE_RES_COUNT_MAP cycle_res_count;
  // <cycle,res> -> count required

  GNAME gname;
  // My symbolic name.

  GNAME res_count_vec_gname;
  // Symbolic name of my resource count vector.

  GNAME res_id_set_gname;
  // Symbolic name of vector of resource id sets

  int res_count_vec_size;
  // How big it is.

  bool Compute_II_RES_REQ(int ii, RES_REQ& ii_res_req);
};

RES_REQ::RES_REQ()
  : max_res_cycle(-1),
    gname("res_req")
{}

bool RES_REQ::Add_Resource(const RES* res, int cycle)
{
  assert(cycle >= 0);
  if ( cycle > max_res_cycle ) max_res_cycle = cycle;

  // a NULL res is used to set the max_res_cycle only
  // with no real resource required
  if (res==NULL)
    return true;

  CYCLE_RES cr = CYCLE_RES(cycle,res);
  int count = cycle_res_count[cr];

  if ( count >= res->Count() )
    return false;

  cycle_res_count[cr] = ++count;
  return true;
}

int RES_REQ::Get_Resource_Count(const RES* res, int cycle)
{
  assert(cycle >= 0);
  if ( cycle > max_res_cycle ) return 0;

  CYCLE_RES cr = CYCLE_RES(cycle,res);
  return cycle_res_count[cr];
}

/////////////////////////////////////
bool RES_REQ::Compute_II_RES_REQ(int ii, RES_REQ& ii_res_req)
/////////////////////////////////////
//  Compute my <ii> relative resourse requirement info <ii_res_req> and return
//  a bool to indicate whether it is possible to issue me in a loop with <ii>
//  cycles.
/////////////////////////////////////
{
  CYCLE_RES_COUNT_MAP::iterator mi;
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();
    RES* res = (*mi).first.Res();
    int count = (*mi).second;

    for (int i = 0; i < count; ++i) {
      if ( ! ii_res_req.Add_Resource(res,Mod(cycle,ii)) )
        return false;
    }
  }

  return true;
}

bool RES_REQ::Compute_Maybe_Output_II_RES_REQ(int ii, FILE* fd,
                                              GNAME*& res_req_gname,
                                              GNAME*& res_id_set_gname_ref )
{
  RES_REQ ii_res_req;

  if ( ! Compute_II_RES_REQ(ii,ii_res_req) )
    return false;

  ii_res_req.Output(fd);
  res_req_gname = SCHEDNEW(GNAME(ii_res_req.gname));
  res_id_set_gname_ref = SCHEDNEW(GNAME(ii_res_req.res_id_set_gname));
  return true;
}

void RES_REQ::Compute_Output_Resource_Count_Vec(FILE* fd)
{
  CYCLE_RES_COUNT_MAP::iterator mi;
  map<int,int,less<int> > res_inx_count;  // res_id => count

  // Sum up the number of each required
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    RES* res = (*mi).first.Res();
    int count = (*mi).second;

    res_inx_count[res->Id()] += count;
  }

  res_count_vec_size = res_inx_count.size();

  if ( res_count_vec_size == 0 ) {
    res_count_vec_gname.Stub_Out();
    return;
  }

  // Print it out
  fprintf(fd,"static TI_SI_RESOURCE_TOTAL %s[] = {",
             res_count_vec_gname.Gname());

  bool is_first = true;
  map<int,int,less<int> >::iterator mj;
  for (mj = res_inx_count.begin(); mj != res_inx_count.end(); ++mj) {
    RES* res = RES::Get((*mj).first);  // You'd think STL would allow
    int count = (*mj).second;          // something less ugly!  But no.

    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  {%s,%d} /* %s */",
               RES::Get(res->Id())->Addr_Of_Gname(),count,res->Name());
  }
  fprintf(fd,"\n};\n");
}

void RES_REQ::Output(FILE* fd)
{
  int i;
  CYCLE_RES_COUNT_MAP::iterator mi;
  unsigned long long *res_vec = SCHEDNEW_ARRAY(unsigned long long, max_res_cycle + 1);
  unsigned long long *res_used_set = SCHEDNEW_ARRAY(unsigned long long, max_res_cycle + 1);

  memset(res_vec, 0, (max_res_cycle + 1) * sizeof(unsigned long long));
  memset(res_used_set, 0, (max_res_cycle + 1) * sizeof(unsigned long long));
  
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();  // You'd think this could be abstracted,
    RES* res = (*mi).first.Res();     // but I couldn't even explain the
    unsigned long long  count = (*mi).second;  // the concept to Alex S.

    res_vec[cycle] += count << res->Shift_Count();
    res_used_set[cycle] |= 1ULL << res->Id();
  }

  fprintf(fd,"static TI_SI_RRW %s[] = {\n  %d",
             gname.Gname(),
             max_res_cycle + 1);

  for ( i = 0; i <= max_res_cycle; ++i )
    fprintf(fd,",\n  0x%" LL_FORMAT "x",res_vec[i]);

  fprintf(fd,"\n};\n");

  if ( max_res_cycle < 0 ) {
    res_id_set_gname.Stub_Out();
    return;
  }

  fprintf(fd,"static TI_SI_RESOURCE_ID_SET %s[] = {",
             res_id_set_gname.Gname());

  bool is_first = true;
  for ( i = 0; i <= max_res_cycle; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  0x%" LL_FORMAT "x",res_used_set[i]);
  }

  fprintf(fd,"\n};\n");
}

#ifdef SI_GEN_DYNAMIC
bool RES_REQ::Compute_Generate_Resource_Count_Vec(int *res_count, TI_SI_RESOURCE_TOTAL **res_total)
{
  CYCLE_RES_COUNT_MAP::iterator mi;
  map<int,int,less<int> > res_inx_count;  // res_id => count

  // Sum up the number of each required
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    RES* res = (*mi).first.Res();
    int count = (*mi).second;

    res_inx_count[res->Id()] += count;
  }

  res_count_vec_size = res_inx_count.size();
  TI_SI_RESOURCE_TOTAL *ttotal = 0;
  if ( res_count_vec_size != 0 )
    {
      unsigned int cnt = 0;
      const unsigned int size = res_inx_count.size();
      ttotal = SCHEDNEW_ARRAY(TI_SI_RESOURCE_TOTAL, size);

      map<int,int,less<int> >::iterator mj;
      for (mj = res_inx_count.begin(); mj != res_inx_count.end(); ++mj)
	{
	  RES* res = RES::Get((*mj).first);  // You'd think STL would allow
	  int count = (*mj).second;          // something less ugly!  But no.

	  if (cnt >= size)
	    return false;
	  
	  ttotal[cnt].resource = res->generated_resource;
	  ttotal[cnt].total_used = count;
	  cnt++;
	}
    }

  *res_count = res_count_vec_size;
  *res_total = ttotal;

  return true;
}

void RES_REQ::Generate(TI_SI_RRW **res_req, TI_SI_RESOURCE_ID_SET **res_id_set)
{
  int i;
  CYCLE_RES_COUNT_MAP::iterator mi;
  TI_SI_RRW* res_vec = SCHEDNEW_ARRAY(TI_SI_RRW, max_res_cycle + 1);
  TI_SI_RESOURCE_ID_SET* res_used_set = SCHEDNEW_ARRAY(TI_SI_RESOURCE_ID_SET, max_res_cycle + 1);

  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();  // You'd think this could be abstracted,
    RES* res = (*mi).first.Res();     // but I couldn't even explain the
    unsigned long long  count = (*mi).second;  // the concept to Alex S.

    res_vec[cycle].add_bit_mask(
	count, res->Word()*(sizeof(unsigned long long))*8+res->Shift_Count());
    res_used_set[cycle].add_bit_mask(1ULL, res->Id());
  }

  TI_SI_RRW *trr = SCHEDNEW_ARRAY(TI_SI_RRW, max_res_cycle + 2);
  trr[0].set_int_value(max_res_cycle + 1);

  for ( i = 0; i <= max_res_cycle; ++i )
    trr[i + 1]=res_vec[i];

  TI_SI_RESOURCE_ID_SET *tidset = 0;
  if ( max_res_cycle >= 0 )
    {
      tidset = SCHEDNEW_ARRAY(TI_SI_RESOURCE_ID_SET, max_res_cycle + 1);
      for ( i = 0; i <= max_res_cycle; ++i )
	tidset[i] = res_used_set[i];
    }

  *res_req = trr;
  *res_id_set = tidset;
}
#endif

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class ISLOT {
/////////////////////////////////////
// An issue slot.  This is for modeling the horrible beast skewed pipe and we
// hope that it will be useful enough to support experimentation with related
// ideas.
/////////////////////////////////////

public:
  ISLOT(const char* name, int skew, int avail_count);
  // <name> is for documentation and debugging.  <skew> gives a latency skew
  // instructions issued in me.

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return pointer to my name in generated.

  static int Count() { return count; }
  // How may instructions can issue in me.

  static void Output_All(FILE* fd);
  // Output all the issue slots and a vector of pointers to them all.

#ifdef SI_GEN_DYNAMIC
  static bool Generate_All(INT *issue_slot_count, TI_SI_ISSUE_SLOT ***issue_slots);
  // Generate all issue slots and a vector of pointers to them.
#endif

private:
  const char* const name;             // User supplied for documentation & debugging
  const int skew;               // Latency skew
  const int avail_count;        // How many instructions can happen in it
  GNAME gname;                  // Symbolic name in generated
  ISLOT *next;                  // next islot
  
  static ISLOT *islots;         // All the created islot
  static ISLOT *islots_tail;    
  static int count;             // How many issue slots total?
};

ISLOT *ISLOT::islots = NULL;
ISLOT *ISLOT::islots_tail = NULL;
int ISLOT::count = 0;

ISLOT::ISLOT(const char* name, int skew, int avail_count)
  : name(name),
    skew(skew),
    avail_count(avail_count)
{
  if (islots == NULL)
    islots = islots_tail = this;
  else
    islots_tail->next = this;
  
  this->next = NULL;
  islots_tail = this;
  
  ++count;
}

void ISLOT::Output_All(FILE* fd)
{
  fprintf(fd,"int TSI_issue_slot_count = %d;\n",count);

  for ( ISLOT *iscan = islots; iscan != NULL; iscan = iscan->next) {
    fprintf(fd,"static TI_SI_ISSUE_SLOT %s = { \"%s\",%d,%d};\n",
            iscan->gname.Gname(),
            iscan->name,
            iscan->skew,
            iscan->avail_count);
  }

  if ( count == 0 )
    fprintf(fd,"TI_SI_ISSUE_SLOT * TSI_issue_slots[1] = {0};\n");
  else {
    fprintf(fd,"TI_SI_ISSUE_SLOT * TSI_issue_slots[%d] = {",count);

    bool is_first = true;
    for ( ISLOT *iscan = islots; iscan != NULL; iscan = iscan->next) {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %s",iscan->Addr_Of_Gname());
    }

    fprintf(fd,"\n};\n");
  }
}

#ifdef SI_GEN_DYNAMIC
bool ISLOT::Generate_All(INT *issue_slot_count, TI_SI_ISSUE_SLOT ***issue_slots)
{
  TI_SI_ISSUE_SLOT **tislots;

  if (count == 0)
    {
      tislots = SCHEDNEW_ARRAY(TI_SI_ISSUE_SLOT *, 1);
      tislots[0] = 0;
    }
  else
    {
      int cnt = 0;
      tislots = SCHEDNEW_ARRAY(TI_SI_ISSUE_SLOT *, count);
      TI_SI_ISSUE_SLOT *ts_array = SCHEDNEW_ARRAY(TI_SI_ISSUE_SLOT, count);

      for (ISLOT *islot = islots; islot != NULL; islot = islot->next) {
	if (cnt >= count)
	  return false;
	
	TI_SI_ISSUE_SLOT *ts = ts_array + cnt;
	ts->name = islot->name;
	ts->skew = islot->skew;
	ts->avail_per_cycle = islot->avail_count;

	tislots[cnt++] = ts;
      }
    }

  *issue_slot_count = count;
  *issue_slots = tislots;
  
  return true;
}
#endif

/////////////////////////////////////
struct ISLOT_CNT {
/////////////////////////////////////
// Container for an ISLOT
/////////////////////////////////////

  ISLOT *islot;
  ISLOT_CNT *next;
  
  ISLOT_CNT(ISLOT *l, ISLOT_CNT *n) :
    islot(l), next(n) { }
};

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class LATENCY_INFO {
/////////////////////////////////////
// Describes latency information for an instruction group's operands or
// results.
/////////////////////////////////////

public:
  LATENCY_INFO(int max_elements);
  // <max_elements> is the maximum number either of operands or results.

  void Set_Any_Time(int time);
  // Any (all) of the operands or results have <time> as access or available
  // time.

  int Get_Any_Time();
  // access or available time for any (all) of the operands or results.
  // should check Any_Time_Is_Set() first.

  bool Any_Time_Is_Defined();
  // true if access or available time for any (all) of the operands or results
  // is set but not individual operand or result.

  void Set_Time(int index, int time);
  // <index>'th operand or result has <time> as access or available time.

  int Get_Time(int index);
  // access or available time for <index>'th operand or result.

  bool Time_Is_Defined(int index);
  // true if access or available time for <index>'th operand or result
  // is set.

  void Output(FILE* fd);
  // Output latency vector to <fd>.

  const char* Gname() { return gname.Gname(); }
  // Return name of pointer to me in generated file.

#ifdef SI_GEN_DYNAMIC
  bool Generate(mUINT8 **latency);
#endif

  private:
  GNAME gname;                  // Name in generated
  const int max_elements;       // Maximum number of operands or results
  bool any_time_defined;        // Overriding time defined
  int any_time;                 // And here it is
  bool *times_defined;          // Times for each operands defined?
  int *times;                   // And here they are
};

LATENCY_INFO::LATENCY_INFO(int max_elements)
  : gname("latency"),
    max_elements(max_elements),
    any_time_defined(false)
{
  times_defined = SCHEDNEW_ARRAY(bool, max_elements);
  times = SCHEDNEW_ARRAY(int, max_elements);

  memset(times_defined, 0, sizeof(bool) * max_elements);
}

void LATENCY_INFO::Set_Any_Time(int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### Warning any_time redefined for %s.  "
                   "Was %d.  Is %d\n",
                   gname.Gname(),
                   any_time,
                   time);
  }

  any_time_defined = true;
  any_time = time;
}

int LATENCY_INFO::Get_Any_Time()
{
  if ( any_time_defined == false ) {
    fprintf(stderr,"### Warning getting undefined any_time for %s \n",
                   gname.Gname());
  }

  return any_time;
}

bool LATENCY_INFO::Any_Time_Is_Defined()
{
  return any_time_defined;
}

void LATENCY_INFO::Set_Time(int index, int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### WARNING: %s setting specific time after any time.  "
                   "Any %d.  Specific %d\n",
                   gname.Gname(),
                   any_time,
                   time);
  }

  assert(index < max_elements);

  if ( times_defined[index] ) {
    fprintf(stderr,"### WARNING: Resetting %s time.  "
                   "Was %d. Now is %d\n",
                   gname.Gname(),
                   time,
                   times[index]);
  }

  times_defined[index] = true;
  times[index] = time;
}

int LATENCY_INFO::Get_Time(int index)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### WARNING: %s getting specific time after any time.  "
                   "Any %d.",
                   gname.Gname(),
                   any_time);
  }

  assert(index < max_elements);

  if ( times_defined[index] == false ) {
    fprintf(stderr,"### WARNING: Getting undefined time for %s\n",
                   gname.Gname());
  }

  return times[index];
}

bool LATENCY_INFO::Time_Is_Defined(int index)
{
  return times_defined[index];
}

void LATENCY_INFO::Output(FILE* fd)
{
  fprintf(fd,"static mUINT8 %s[] = {",gname.Gname());

    bool is_first = true;
    for ( int i = 0; i < max_elements; ++i ) {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"%d",any_time_defined ? any_time : times[i]);
    }

    fprintf(fd,"};\n");
}

#ifdef SI_GEN_DYNAMIC
bool LATENCY_INFO::Generate(mUINT8 **latency)
{
  const unsigned int size = max_elements;
  unsigned int cnt = 0;

  mUINT8 *lats = SCHEDNEW_ARRAY(mUINT8, size);

  for ( int i = 0; i < max_elements; ++i )
    {
      if (cnt >= size)
	return false;
      
      lats[cnt++] = (any_time_defined) ? any_time : times[i];
    }

  *latency = lats;
  return true;
}
#endif

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class INSTRUCTION_GROUP {
/////////////////////////////////////
// Represents one of the instruction groups, a common piece of scheduling
// information for a set of instructions.
/////////////////////////////////////

public:
  // These functions correspond exactly with the defined C client interface.
  INSTRUCTION_GROUP(const char* name);
  void Set_Any_Operand_Access_Time(int time);
  void Set_Operand_Access_Time(int operand_index, int time);
  void Set_Any_Result_Available_Time(int time);
  void Set_Result_Available_Time(int result_index, int time);
  bool Any_Operand_Access_Time_Is_Defined();
  int Get_Any_Operand_Access_Time();
  bool Operand_Access_Time_Is_Defined(int operand_index);
  int Get_Operand_Access_Time(int operand_index);
  bool Any_Result_Available_Time_Is_Defined();
  int Get_Any_Result_Available_Time();
  bool Result_Available_Time_Is_Defined(int result_index);
  int Get_Result_Available_Time(int result_index);
  void Set_Load_Access_Time( int time );
  void Set_Last_Issue_Cycle( int time );
  void Set_Store_Available_Time( int time );
  void Add_Resource_Requirement(const RES* res, int cycle);
  int Get_Resource_Requirement_Count(const RES* res, int cycle);
  int Get_Max_Resource_Cycle();
  void Add_Valid_ISLOT(ISLOT* islot);
  void Set_Write_Write_Interlock();

  static void Output_All(FILE* fd);
  // Write them all out

#ifdef SI_GEN_DYNAMIC
  void Add_Valid_Format(int fmt_id);
  static bool Generate_All(INT *ID_count, TI_SI ***ID_si);
  TI_SI *generated_ig;
#endif

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Name of pointer to me in generated file.

private:
  int id;                               // Index in vector of same
  GNAME gname;                          // Variable name in generated    
  const char* const name;                     // User supplied name for documentation
  RES_REQ res_requirement;              // Required to issue

  ISLOT_CNT *valid_islots;              // If there are any issue slots at all
  ISLOT_CNT *valid_islots_tail;         //
  int valid_count;                      // How many issue slots total?

  GNAME islot_vec_gname;                // Variable name of above in generated

#ifdef SI_GEN_DYNAMIC
  TI_SI_FORMAT_ID_SET valid_formats;    // If there are any issue formats at all
#endif

  LATENCY_INFO operand_latency_info;    // When operands latch
  LATENCY_INFO result_latency_info;     // When results available

  int load_access_time;                 // When loads access memory
  int last_issue_cycle;			// Last issue cycle in simulated insts
  int store_available_time;             // When stores make value available in
                                        //   memory 

  bool write_write_interlock;           // For simulator

  GNAME ii_res_req_gname;
  // Generated name of vector of resource requirements for each II less than
  // the total number of cycles in res_requirement (one based).

  GNAME ii_res_id_set_gname;
  // Generate name of vector of resource id sets for each II less than
  // the total number of cycles in res_requirement (one based).

  unsigned long long bad_iis[2];
  // Tells whether it is possible to schedule at all at a given II.  This
  // could be a more flexible data structure.  As it is, it is limited to 128
  // bad IIs, but I think this will be enough.

  static INSTRUCTION_GROUP *instruction_groups;
  static INSTRUCTION_GROUP *instruction_groups_tail;
  INSTRUCTION_GROUP *next;
  // All the defined instruction groups.

  static int count;
  // Of all instruction groups.  Used to generate ids.

  int II_Info_Size() { return res_requirement.Max_Res_Cycle(); }
  // Latest cycle in which I need a resource

  void Output_II_Info(FILE* fd);
  void Output_Latency_Info(FILE* fd);
  void Output_Issue_Slot_Info(FILE* fd);
  void Output(FILE* fd);
};


INSTRUCTION_GROUP *INSTRUCTION_GROUP::instruction_groups;
INSTRUCTION_GROUP *INSTRUCTION_GROUP::instruction_groups_tail;
int INSTRUCTION_GROUP::count = 0;

INSTRUCTION_GROUP::INSTRUCTION_GROUP(const char* name)
    : id(count++),
      name(name),
      valid_islots(NULL),
      valid_islots_tail(NULL),
      valid_count(0),
#ifdef SI_GEN_DYNAMIC
      valid_formats(0),
#endif
      operand_latency_info(MAX_OPERANDS),
      result_latency_info(MAX_RESULTS),
      load_access_time(0),
      last_issue_cycle(0),
      store_available_time(0),
      write_write_interlock(false),
      ii_res_req_gname("ii_rr")
{
  bad_iis[0] = 0;
  bad_iis[1] = 0;

  if (instruction_groups == NULL)
    instruction_groups = instruction_groups_tail = this;
  else
    instruction_groups_tail->next = this;
  
  this->next = NULL;
  instruction_groups_tail = this;
}

void INSTRUCTION_GROUP::Set_Any_Operand_Access_Time(int time)
{
  operand_latency_info.Set_Any_Time(time);
}

void INSTRUCTION_GROUP::Set_Operand_Access_Time(int operand_index, int time)
{
  operand_latency_info.Set_Time(operand_index,time);
}

void INSTRUCTION_GROUP::Set_Any_Result_Available_Time(int time)
{
  result_latency_info.Set_Any_Time(time);
}

void INSTRUCTION_GROUP::Set_Result_Available_Time(int result_index, int time)
{
  result_latency_info.Set_Time(result_index,time);
}

bool INSTRUCTION_GROUP::Any_Operand_Access_Time_Is_Defined()
{
  return operand_latency_info.Any_Time_Is_Defined();
}

int INSTRUCTION_GROUP::Get_Any_Operand_Access_Time()
{
  return operand_latency_info.Get_Any_Time();
}

bool INSTRUCTION_GROUP::Operand_Access_Time_Is_Defined(int index)
{
  return operand_latency_info.Time_Is_Defined(index);
}

int INSTRUCTION_GROUP::Get_Operand_Access_Time(int operand_index)
{
  return operand_latency_info.Get_Time(operand_index);
}

bool INSTRUCTION_GROUP::Any_Result_Available_Time_Is_Defined()
{
  return result_latency_info.Any_Time_Is_Defined();
}

int INSTRUCTION_GROUP::Get_Any_Result_Available_Time()
{
  return result_latency_info.Get_Any_Time();
}

bool INSTRUCTION_GROUP::Result_Available_Time_Is_Defined(int index)
{
  return result_latency_info.Time_Is_Defined(index);
}

int INSTRUCTION_GROUP::Get_Result_Available_Time(int result_index)
{
  return result_latency_info.Get_Time(result_index);
}

void INSTRUCTION_GROUP::Set_Load_Access_Time( int time )
{
  load_access_time = time;
}

void INSTRUCTION_GROUP::Set_Last_Issue_Cycle( int time )
{
  last_issue_cycle = time;
}

void INSTRUCTION_GROUP::Set_Store_Available_Time( int time )
{
  store_available_time = time;
}

void INSTRUCTION_GROUP::Add_Resource_Requirement(const RES* res, int cycle)
{
  if (! res_requirement.Add_Resource(res,cycle)) {
    fprintf(stderr,"### ERROR: Impossible resource request for "
                    "instruction group %s.\n",
                    name);
    fprintf(stderr,"###    %s at cycle %d.\n",res->Name(),cycle);
  }
}

int INSTRUCTION_GROUP::Get_Resource_Requirement_Count(const RES* res, int cycle)
{
  return res_requirement.Get_Resource_Count(res,cycle);
}

int INSTRUCTION_GROUP::Get_Max_Resource_Cycle()
{
  return res_requirement.Max_Res_Cycle();
}

void INSTRUCTION_GROUP::Add_Valid_ISLOT(ISLOT* islot)
{
  ISLOT_CNT *icnt = SCHEDNEW(ISLOT_CNT(islot, NULL));
  
  if (valid_islots == NULL)
    valid_islots = valid_islots_tail = icnt;
  else
    valid_islots_tail->next = icnt;
  
  valid_islots_tail = icnt;
  valid_count++;
}

#ifdef SI_GEN_DYNAMIC
void INSTRUCTION_GROUP::Add_Valid_Format(int fmt_id)
{
  
  if (fmt_id <0)
    fprintf(stderr,"### ERROR: Negative format ID (%d) for "
                    "instruction group %s.\n", fmt_id, name);
  
  if (fmt_id >= sizeof(TI_SI_FORMAT_ID_SET)*8)
    fprintf(stderr,"### ERROR: Format ID (%d) too large (>=%d) for ",
                    "instruction group %s.\n", fmt_id,
		    sizeof(TI_SI_FORMAT_ID_SET)*8, name);
  
  valid_formats |= (1ULL << fmt_id);
}
#endif

void INSTRUCTION_GROUP::Set_Write_Write_Interlock()
{
  write_write_interlock = true;
}

void INSTRUCTION_GROUP::Output_II_Info(FILE* fd)
{
  int i;
  bool is_first;
  const int ii_vec_size = II_Info_Size();
  const int max_num_bad_iis = sizeof(bad_iis) * 8;
  // We need ii relative information for ii's in the range
  // 1..cycle_with_final_res_requirement.  An ii of 0 makes no sense and an II
  // enough cycles that the request doesn't need to wrap are the outside bounds.

  if ( ii_vec_size <= 0 ) {
    ii_res_req_gname.Stub_Out();
    ii_res_id_set_gname.Stub_Out();
    return;
  }

  GNAME **ii_res_req_gname_vector = SCHEDNEW_ARRAY(GNAME *, ii_vec_size);
  GNAME **ii_resources_used_gname_vector = SCHEDNEW_ARRAY(GNAME *, ii_vec_size);
  bool *ii_can_do_vector = SCHEDNEW_ARRAY(bool, ii_vec_size);

  int greatest_bad_ii = 0;

  for ( i = 0; i < res_requirement.Max_Res_Cycle(); ++i ) {
    if ( res_requirement.Compute_Maybe_Output_II_RES_REQ(
           i+1,fd,
           ii_res_req_gname_vector[i],
           ii_resources_used_gname_vector[i])
    ) {
      ii_can_do_vector[i] = true;
    }
    else {
      ii_can_do_vector[i] = false;
      greatest_bad_ii = i;
      if ( i > max_num_bad_iis ) {
        fprintf(stderr,"### Error: bad II %d > %d.  "
                "Need a more flexible representation.\n",
                i, max_num_bad_iis);
      }
    }
  }

  for ( i = 0; i < sizeof(bad_iis) / sizeof(bad_iis[0]); ++i ) {
    bad_iis[i] = 0ULL;
  }

  for ( i = 0; i <= greatest_bad_ii; ++i ) {
    if ( ! ii_can_do_vector[i] ) {
      bad_iis[i / bits_per_long_long] |= (1ULL << (i % bits_per_long_long));
    }
  }
  
  // Print vector of pointers to the II relative resource requirements

  fprintf(fd,"static TI_SI_RR %s[] = {",
          ii_res_req_gname.Gname());

  is_first = true;
  for ( i = 0; i < ii_vec_size; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    if ( ii_can_do_vector[i] )
      fprintf(fd,"\n  %s",ii_res_req_gname_vector[i]->Gname());
    else
      fprintf(fd,"\n  0");
  }

  fprintf(fd,"\n};\n");

  // Print vector of pointers to the II relative resoruce id sets

  fprintf(fd,"static TI_SI_RESOURCE_ID_SET * %s[] = {",
          ii_res_id_set_gname.Gname());

  is_first = true;
  for ( i = 0; i < ii_vec_size; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    if ( ii_can_do_vector[i] ) {
      fprintf(fd,"\n  %s",
              ii_resources_used_gname_vector[i]->Gname());
    }
    else
      fprintf(fd,"\n  0");
  }

  fprintf(fd,"\n};\n");

}

void INSTRUCTION_GROUP::Output_Latency_Info(FILE* fd)
{
  operand_latency_info.Output(fd);
  result_latency_info.Output(fd);
}

void INSTRUCTION_GROUP::Output_Issue_Slot_Info(FILE* fd)
{
  if ( valid_islots == NULL ) {

/* Comment out the warning until the beast skewed support is implemented;
 * it's currently a post 7.2 affair.
 *
 *  if ( ISLOT::Count() > 0 )
 *    fprintf(stderr,"### Issue slots defined but none defined for %s\n",name);
*/
    islot_vec_gname.Stub_Out();
    return;
  }

  fprintf(fd,"static TI_SI_ISSUE_SLOT * %s[] = {",islot_vec_gname.Gname());

  bool is_first = true;
  for (ISLOT_CNT *iscan = valid_islots; iscan != NULL; iscan = iscan->next) {
    ISLOT* islot = iscan->islot;

    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s",islot->Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");
}

void INSTRUCTION_GROUP::Output(FILE* fd)
{
  int i;

  fprintf(fd,"\n/* Instruction group %s */\n",name);
  res_requirement.Output(fd);
  res_requirement.Compute_Output_Resource_Count_Vec(fd);
  Output_II_Info(fd);
  Output_Latency_Info(fd);
  Output_Issue_Slot_Info(fd);

  fprintf(fd,"static TI_SI %s = {\n",gname.Gname());
  fprintf(fd,"  \"%s\",\n",name);
  fprintf(fd,"  %-15d, /* id */\n",id);
  fprintf(fd,"  %-15s, /* operand latency */\n",
             operand_latency_info.Gname());
  fprintf(fd,"  %-15s, /* result latency */\n",
             result_latency_info.Gname());
  fprintf(fd,"  %-15d, /* load access time */\n",
             load_access_time);
  fprintf(fd,"  %-15d, /* last issue cycle */\n",
             last_issue_cycle);
  fprintf(fd,"  %-15d, /* store available time */\n",
             store_available_time);
  fprintf(fd,"  %-15s, /* resource requirement */\n",
             res_requirement.Gname());
  fprintf(fd,"  %-15s, /* res id used set vec */\n",
             res_requirement.Res_Id_Set_Gname());
  fprintf(fd,"  %-15d, /* II info size */\n",
             II_Info_Size() >= 0 ? II_Info_Size() : 0);
  fprintf(fd,"  %-15s, /* II resource requirement vec */\n",
             ii_res_req_gname.Gname());
  fprintf(fd,"  %-15s, /* II res id used set vec */\n",
             ii_res_id_set_gname.Gname());
  fprintf(fd,"  {{");
  for ( i = 0; i < sizeof(bad_iis) / sizeof(bad_iis[0]); ++i ) {
    fprintf(fd, "0x%" LL_FORMAT "x", bad_iis[i]);
    if ( i < sizeof(bad_iis) / sizeof(bad_iis[0]) - 1 ) fprintf(fd, ",");
  }
  fprintf(fd, "}}    , /* Bad IIs */\n");
  fprintf(fd,"  %-15d, /* valid issue slots vec size */\n",
             valid_count);
  fprintf(fd,"  %-15s, /* valid issue slots vec */\n",
             islot_vec_gname.Gname());
  fprintf(fd,"  0x01, /* valid issue formats */\n");
  fprintf(fd,"  %-15d, /* resource count vec size */\n",
             res_requirement.Res_Count_Vec_Size());
  fprintf(fd,"  %-15s, /* resource count vec */\n",
             res_requirement.Res_Count_Vec_Gname());
  fprintf(fd,"  %-15s  /* write-write interlock */\n",
             write_write_interlock ? "1" : "0");
  fprintf(fd,"};\n");
}

void INSTRUCTION_GROUP::Output_All(FILE* fd)
{
  for (INSTRUCTION_GROUP *igscan = instruction_groups;
       igscan != NULL; igscan = igscan->next) {
    igscan->Output(fd);
  }

  fprintf(fd,"TI_SI * TSI_ID_si[] = {");

  bool is_first = true;
  for (INSTRUCTION_GROUP *igscan = instruction_groups;
       igscan != NULL; igscan = igscan->next) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s", igscan->Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");

  fprintf(fd,"int TSI_ID_count = %d;\n",count);

  fprintf(fd,"\n"); // One extra new line to separate from what follows.

}

#ifdef SI_GEN_DYNAMIC
bool INSTRUCTION_GROUP::Generate_All(INT *ID_count, TI_SI ***ID_si)
{
  TI_SI **tidsi = SCHEDNEW_ARRAY(TI_SI *, count);
  TI_SI *tid_array = SCHEDNEW_ARRAY(TI_SI, count);

  unsigned int cnt = 0;
  for (INSTRUCTION_GROUP *ig = instruction_groups;
       ig != NULL; ig = ig->next) {

    if (cnt >= count)
      return false;
    
    TI_SI *tid = tid_array + cnt;

    TI_SI_RRW *res_req;
    TI_SI_RESOURCE_ID_SET *res_id_set;
    ig->res_requirement.Generate(&res_req, &res_id_set);

    int res_count;
    TI_SI_RESOURCE_TOTAL *res_total;
    if (!ig->res_requirement.Compute_Generate_Resource_Count_Vec(&res_count, &res_total))
      return false;
    
    /* Once we get software pipelining, will need to initialize this
       as well as fields marked "sp" below. */
    //Output_II_Info(fd);
    TI_SI_BAD_II_SET badiis = { 0x0, 0x0 };
    
    mUINT8 *res_latency, *operand_latency;
    if (!ig->operand_latency_info.Generate(&operand_latency) ||
	!ig->result_latency_info.Generate(&res_latency))
      return false;
    
    /* Once we have LIW might need to fix this and entries marked
       "is" below. */
    //Output_Issue_Slot_Info(fd);
    
    tid->name = ig->name;
    tid->id = ig->id;
    tid->operand_access_times = operand_latency;
    tid->result_available_times = res_latency;
    tid->load_access_time = ig->load_access_time;
    tid->last_issue_cycle = ig->last_issue_cycle;
    tid->store_available_time = ig->store_available_time;
    tid->rr = res_req;
    tid->resources_used = res_id_set;
    tid->ii_info_size = 0; //sp
    tid->ii_rr = 0; // sp
    tid->ii_resources_used = 0; // sp
    tid->bad_iis = badiis; // sp
    tid->valid_issue_slot_count = 0; // is
    tid->valid_issue_slots = 0; // is
    tid->valid_issue_formats = ig->valid_formats; // is
    tid->resource_total_vector_size = res_count;
    tid->resource_total_vector = res_total;
    tid->write_write_interlock = ig->write_write_interlock ? 1 : 0;
    
    tidsi[cnt++] = tid;
    ig->generated_ig = tid;
  }
  
  *ID_count = count;
  *ID_si = tidsi;

  return true;
}
#endif

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class TOP_SCHED_INFO_MAP {
/////////////////////////////////////
// Keeps track of which TOPs need are in which INSTRUCTION_GROUPs (and thus
// map to which TOP_SCHED_INFOs.
/////////////////////////////////////

public:
  static void Add_Entry( TOP top, INSTRUCTION_GROUP* ig );
  // Add entry to the map.  <top> uses <ig>'s scheduling information.

  static INSTRUCTION_GROUP* Find_Entry( TOP top );
  // Find instruction group entry in the map for <top>.

  static void Output( FILE* fd );
  // Write out the map.

#ifdef SI_GEN_DYNAMIC
  static bool Generate(TI_SI ***top_si);
  // Generate the map.
#endif

  static void Create_Dummies( void );
  // Create schedling info for the "dummy" instructions.

private:
  static INSTRUCTION_GROUP **top_sched_info_ig_map;  // The map itself.
  static bool *top_sched_info_defined;               // Which elements defined
};

INSTRUCTION_GROUP **TOP_SCHED_INFO_MAP::top_sched_info_ig_map;
bool *TOP_SCHED_INFO_MAP::top_sched_info_defined;

void TOP_SCHED_INFO_MAP::Create_Dummies( void )
{
  INSTRUCTION_GROUP *dummies = NULL;

  top_sched_info_ig_map = SCHEDNEW_ARRAY(INSTRUCTION_GROUP *, TOP_COUNT);
  top_sched_info_defined = SCHEDNEW_ARRAY(bool, TOP_COUNT);

  memset(top_sched_info_ig_map, 0, sizeof(INSTRUCTION_GROUP *) * TOP_COUNT);
  memset(top_sched_info_defined, 0, sizeof(bool) * TOP_COUNT);

  for ( int i = 0; i < TOP_COUNT; ++i ) {
    if ( TOP_HAS_PROPERTY((TOP)i, PROP_dummy) ) {
      if ( !dummies ) {
	dummies = SCHEDNEW(INSTRUCTION_GROUP("Dummy instructions"));
	dummies->Set_Any_Operand_Access_Time(0);
	dummies->Set_Any_Result_Available_Time(0);
      }
      top_sched_info_ig_map[i] = dummies;
    }
  }
}

void TOP_SCHED_INFO_MAP::Add_Entry( TOP top, INSTRUCTION_GROUP* ig )
{
  if ( top_sched_info_defined[(int) top] ) {
    fprintf(stderr,"### Warning: scheduling information for %s redefined.\n",
            TOP_NAME(top));
  }

  top_sched_info_ig_map[(int) top] = ig;
  top_sched_info_defined[(int) top] = true;
}

INSTRUCTION_GROUP* TOP_SCHED_INFO_MAP::Find_Entry( TOP top )
{
  if ( top_sched_info_defined[(int) top] == false) {
    fprintf(stderr,"### Warning: scheduling information for %s not defined.\n",
            TOP_NAME(top));
  }

  return top_sched_info_ig_map[(int) top];
}

void TOP_SCHED_INFO_MAP::Output( FILE* fd )
{
  int i;

  fprintf(fd,"TI_SI * TSI_top_si[%d] = {",TOP_COUNT);

  bool err = false;
  bool is_first = true;
  for ( i = 0; i < TOP_COUNT; ++i ) {
    bool isa_member = ISA_SUBSET_Member(machine_isa, (TOP)i);
    bool is_dummy = TOP_HAS_PROPERTY((TOP)i, PROP_dummy);

    Maybe_Print_Comma(fd,is_first);

    fprintf(fd,"\n  %-10s  /* %s */",
	    (top_sched_info_ig_map[i]) ? top_sched_info_ig_map[i]->Addr_Of_Gname() : "0",
	    TOP_NAME((TOP)i));

    if ( top_sched_info_defined[i] ) {
      if ( ! isa_member ) {
	fprintf(stderr,"### Warning: scheduling info for non-%s ISA opcode %s\n",
                       ISA_SUBSET_Name(machine_isa), TOP_NAME((TOP)i));
      } else if ( is_dummy ) {
	fprintf(stderr,"### Warning: scheduling info for dummy opcode %s\n",
                       TOP_NAME((TOP)i));
      }
    } else {
      if ( isa_member && ! is_dummy && (i != TOP_UNDEFINED)) {  
	fprintf(stderr,"### Error: no scheduling info for opcode %s\n",
                       TOP_NAME((TOP)i));
	err = true;
      }
    }
  }
  fprintf(fd,"\n};\n");
  if (err) exit(EXIT_FAILURE);
}

#ifdef SI_GEN_DYNAMIC
bool TOP_SCHED_INFO_MAP::Generate(TI_SI ***top_si)
{
  int i;

  TI_SI **tsi = SCHEDNEW_ARRAY(TI_SI *, TOP_COUNT);
  
  bool err = false;
  bool is_first = true;
  for ( i = 0; i < TOP_COUNT; ++i ) {
    bool isa_member = ISA_SUBSET_Member(machine_isa, (TOP)i);
    bool is_dummy = TOP_HAS_PROPERTY((TOP)i, PROP_dummy);

    INSTRUCTION_GROUP *ig = top_sched_info_ig_map[i];
    tsi[i] = (ig) ? ig->generated_ig : NULL;

    if ( top_sched_info_defined[i] ) {
      if ( is_dummy ) {
	DevWarn("scheduling info for dummy opcode %s",
		TOP_NAME((TOP)i));
      }
    } else {
      if ( isa_member && ! is_dummy && (i != TOP_UNDEFINED)) {  
	/* As things are now, missing schedule information means one
           of two things. 1) actually missing schedule for an opcode,
           2) the confuration does not have some core option
           (i.e. mac16) so libcc/libisa don't have schedule
           information for it, but we still have a topcode for it
           since we include everything in the isa_subset. The correct
           fix is to get the subsets working so that, based on
           commandline flags, we include only the topcodes we acually
           support. Until then we just suppress this error. */
#if 0  /* fixme */
	DevWarn("no scheduling info for opcode %s\n", TOP_NAME((TOP)i));
	err = true;
#else
	Lmt_DevWarn(1, ("missing scheduling info, possibly from optional core opcode (need to conditionalize this based on core option flags)"));
#endif
      }
    }
  }

  *top_si = tsi;
  
  return !err;
}
#endif

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// The client interface functions

static INSTRUCTION_GROUP* current_instruction_group;

/*ARGSUSED*/
void Machine(const char* name, ISA_SUBSET isa, int argc, char** argv)
{
  machine_isa = isa;

  TOP_SCHED_INFO_MAP::Create_Dummies();

}

RESOURCE RESOURCE_Create(const char* name, int count, int format_id)
{
  return SCHEDNEW(RES(name,count,format_id));
}

ISSUE_SLOT ISSUE_SLOT_Create(const char* name, int skew, int count)
{
  return SCHEDNEW(ISLOT(name,skew,count));
}

void Instruction_Group(const char* name,...)
{
  va_list ap;
  TOP opcode;

  current_instruction_group = SCHEDNEW(INSTRUCTION_GROUP(name));

  va_start(ap,name);

  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED )
    TOP_SCHED_INFO_MAP::Add_Entry(opcode,current_instruction_group);

  va_end(ap);
}

void Any_Operand_Access_Time( int time )
{
  current_instruction_group->Set_Any_Operand_Access_Time(time);
}

void Operand_Access_Time( int operand_index, int time )
{
  current_instruction_group->Set_Operand_Access_Time(operand_index,time);
}

void Any_Result_Available_Time( int time )
{
  current_instruction_group->Set_Any_Result_Available_Time(time);
}

void Result_Available_Time( int result_index, int time )
{
  current_instruction_group->Set_Result_Available_Time(result_index,time);
}

bool Any_Operand_Access_Time_Is_Defined( int opcode )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Any_Operand_Access_Time_Is_Defined();
}

int Get_Any_Operand_Access_Time( int opcode )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Any_Operand_Access_Time();
}

bool Operand_Access_Time_Is_Defined( int opcode, int operand_index )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Operand_Access_Time_Is_Defined(operand_index);
}

int Get_Operand_Access_Time( int opcode, int operand_index )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Operand_Access_Time(operand_index);
}

bool Any_Result_Available_Time_Is_Defined( int opcode )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Any_Result_Available_Time_Is_Defined();
}

int Get_Any_Result_Available_Time( int opcode )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Any_Result_Available_Time();
}

bool Result_Available_Time_Is_Defined( int opcode, int result_index )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Result_Available_Time_Is_Defined(result_index);
}

int Get_Result_Available_Time( int opcode, int result_index )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Result_Available_Time(result_index);
}

void Load_Access_Time( int time )
{
  current_instruction_group->Set_Load_Access_Time(time);
}

void Last_Issue_Cycle( int time )
{
  current_instruction_group->Set_Last_Issue_Cycle(time);
}

void Store_Available_Time( int time )
{
  current_instruction_group->Set_Store_Available_Time(time);
}

void Resource_Requirement( RESOURCE resource, int time )
{
  current_instruction_group->Add_Resource_Requirement(resource,time);
}

int Get_Resource_Requirement_Count( int opcode, RESOURCE resource, int time )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Resource_Requirement_Count(resource,time);
}

int Get_Max_Resource_Cycle( int opcode )
{
  INSTRUCTION_GROUP* instruction_group =
	  			TOP_SCHED_INFO_MAP::Find_Entry((TOP)opcode);
  return instruction_group->Get_Max_Resource_Cycle();
}

void Valid_Issue_Slot( ISSUE_SLOT slot )
{
  current_instruction_group->Add_Valid_ISLOT(slot);
}

#ifdef SI_GEN_DYNAMIC
void Valid_Issue_Format( int fmt_id )
{
  current_instruction_group->Add_Valid_Format(fmt_id);
}
#endif

void Write_Write_Interlock()
{
  current_instruction_group->Set_Write_Write_Interlock();
}

void Machine_Done( const char* filename )
{
  FILE* fd = fopen(filename,"wb");

  if ( fd == NULL ) {
    fprintf(stderr,"### Error: couldn't write %s\n",filename);
    return;
  }

  fprintf(fd,"#include \"ti_si.h\"\n");
  fprintf(fd,"#include \"libti.h\"\n");
  RES::Output_All(fd);
  RES_WORD::Output_All(fd);
  ISLOT::Output_All(fd);
  INSTRUCTION_GROUP::Output_All(fd);
  TOP_SCHED_INFO_MAP::Output(fd);

  //  Print_End_Boiler_Plate(fd);

  fclose(fd);
}

#ifdef SI_GEN_DYNAMIC
/* Instead of outputing a C file that defines the data structures
   describing the scheduling information, we generate them
   directly. */
bool Machine_Done (INT *resource_count,
		   TI_SI_RESOURCE ***resources,
		   TI_SI_RRW **RRW_initializer,
		   TI_SI_RRW **RRW_overuse_mask,
		   TI_SI_RRW **RRW_format_resource_overuse_mask,
		   INT *issue_slot_count,
		   TI_SI_ISSUE_SLOT ***issue_slots,
		   TI_SI ***top_si,
		   INT *ID_count,
		   TI_SI ***ID_si)
{
  if (!RES::Generate_All(resource_count, resources) ||
      !RES_WORD::Generate_All(RRW_initializer, RRW_overuse_mask,
			      RRW_format_resource_overuse_mask) ||
      !ISLOT::Generate_All(issue_slot_count, issue_slots) ||
      !INSTRUCTION_GROUP::Generate_All(ID_count, ID_si) ||
      !TOP_SCHED_INFO_MAP::Generate(top_si))
    return false;

  return true;
}
#endif
