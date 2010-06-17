/*
   Copyright (C) 2001-2005 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#ifndef ti_res_vector_INCLUDED
#define ti_res_vector_INCLUDED

#include "defs.h"
#include "errors.h"
#include "stdio.h"

#include "topcode.h"
#include "mempool.h"
#include "cxx_memory.h"

template <int type_id>
class TI_SI_RRV {
  static int _length;	// number of (unsigned long long) words in the vector
  static bool _length_set;	// whether _length is initialized
  unsigned long long* _v;	// array of unsigned long long words
  MEM_POOL* _pool;	// pool from which to allocate/deallocate
public:
  TI_SI_RRV() {
	  Is_True(_length_set,("Bad length"));
	  _pool = Default_Mem_Pool? Default_Mem_Pool: Malloc_Mem_Pool;
	  _v = CXX_NEW_ARRAY(unsigned long long, _length, _pool);
	  memset(_v, 0, sizeof(unsigned long long)*_length);
  }

  ~TI_SI_RRV() { CXX_DELETE_ARRAY(_v,_pool); }

#ifndef SI_GEN_DYNAMIC
  // this should only be used for static array initialization in xtensa.cxx
  TI_SI_RRV(int v) {
	  Is_True(!_length_set,("Bad length"));
	  _pool = Default_Mem_Pool? Default_Mem_Pool: Malloc_Mem_Pool;
	  _v = CXX_NEW_ARRAY(unsigned long long, _length, _pool);
	  _v[0] = v;
  }
#endif

  TI_SI_RRV(const TI_SI_RRV& v1) {
	  Is_True(_length_set,("Bad length"));
	  _pool = Default_Mem_Pool? Default_Mem_Pool: Malloc_Mem_Pool;
	  _v = CXX_NEW_ARRAY(unsigned long long, _length, _pool);
	  memcpy(_v, v1._v, sizeof(unsigned long long)*_length);
  }
  TI_SI_RRV(unsigned long long v[]) {
	  Is_True(_length_set,("Bad length"));
	  _pool = Default_Mem_Pool? Default_Mem_Pool: Malloc_Mem_Pool;
	  _v = CXX_NEW_ARRAY(unsigned long long, _length, _pool);
	  memcpy(_v, v, sizeof(unsigned long long)*_length);
  }

  void init(const TI_SI_RRV& v1, MEM_POOL* pool) {
	  Is_True(_length_set,("Bad length"));
	  _pool = pool;
	  _v = CXX_NEW_ARRAY(unsigned long long, _length, _pool);
	  memcpy(_v, v1._v, sizeof(unsigned long long)*_length);
  }

  void init(unsigned long long v[]) {
	  memcpy(_v, v, sizeof(unsigned long long)*_length);
  }

  void copy(const TI_SI_RRV& v1) {
	  memcpy(_v, v1._v, sizeof(unsigned long long)*_length);
  }

  TI_SI_RRV& operator=(const TI_SI_RRV& v1) {
	  memcpy(_v, v1._v, sizeof(unsigned long long)*_length);
	  return *this;
  }
  TI_SI_RRV& operator+=(const TI_SI_RRV& v1) {
	  for (int i=0; i<_length; i++) _v[i] += v1._v[i];
	  return *this;
  }
  TI_SI_RRV& operator-=(const TI_SI_RRV& v1) {
	  for (int i=0; i<_length; i++) _v[i] -= v1._v[i];
	  return *this;
  }
  TI_SI_RRV& operator&=(const TI_SI_RRV& v1) {
	  for (int i=0; i<_length; i++) _v[i] &= v1._v[i];
	  return *this;
  }
  TI_SI_RRV& operator|=(const TI_SI_RRV& v1) {
	  for (int i=0; i<_length; i++) _v[i] |= v1._v[i];
	  return *this;
  }
  TI_SI_RRV& operator>>=(const int d) {
	  int words = d / 64;
	  int bit_shift = d % 64;

	  for (int i=0; i<_length-words; i++)
	    _v[i] = _v[i+words];
	  for (int i=_length-words; i<_length; i++)
	    _v[i] = 0;

	  for (int i=0; i<_length-1; i++) {
	    _v[i]  = (_v[i+1]  << bit_shift) | (_v[i] >> bit_shift);
	  }
	  _v[_length-1] >>= bit_shift;
	  return *this;
  }
  TI_SI_RRV& operator<<=(const int d) {
	  int words = d / 64;
	  int bit_shift = d % 64;

	  for (int i=_length-1; i>=words; i--)
	    _v[i] = _v[i-words];
	  for (int i=words; i>=0; i--)
	    _v[i] = 0;

	  for (int i=_length-1; i>0; i--) {
	    _v[i]  = (_v[i] << bit_shift) | (_v[i-1]  >>  bit_shift);
	  }
	  _v[0] <<= bit_shift;
	  return *this;
  }
  TI_SI_RRV operator~(void) const {
	  TI_SI_RRV res;
	  for (int i=0; i<_length; i++) res._v[i] = ~_v[i];
	  return res;
  }

  bool operator==(const TI_SI_RRV& v1) const {
	  for (int i=0; i<_length; i++)
	    if (_v[i] != v1._v[i]) return FALSE;
	  return TRUE;
  }
  bool operator!=(const TI_SI_RRV& v1) const {
	  return !(*this == v1);
  }

  static void set_length(int bit_length) {
	  int unsigned_long_long_bits = sizeof(unsigned long long)*8;
	  _length = (bit_length+unsigned_long_long_bits-1)/unsigned_long_long_bits;
	  _length_set = true;
  }

  bool is_zero() const {
	  for (int i=0; i<_length; i++)
	    if (_v[i] != 0) return FALSE;
	  return TRUE;
  }

  void reset(void) {
	  for (int i=0; i<_length; i++) _v[i] == 0;
  }

  bool has_bit(int i) const {
    int word = i / 64; // sizeof(unsigned long long) * 8
    int bit = i % 64; // sizeof(unsigned long long) * 8
    unsigned long long bit_mask = (1ULL << bit);
    return word < _length ? (_v[word] & bit_mask) : false;
  }

  void reset_bit(int i) {
    int word = i / 64; // sizeof(unsigned long long) * 8
    Is_True(word<_length, ("Bad position"));
    if (word<_length) {
      int bit = i % 64; // sizeof(unsigned long long) * 8
      unsigned long long bit_mask = (1ULL << bit);
      _v[word] &= ~(bit_mask);
    }
  }

  void add_bit_mask(unsigned long long mask, int pos) {
    int word = pos / 64; // sizeof(unsigned long long) * 8
    Is_True(word<_length, ("Bad mask and position"));
    if (word<_length) {
      int bit = pos % 64; // sizeof(unsigned long long) * 8
      unsigned long long bit_mask = (mask << bit);
      Is_True((bit_mask >> bit) == mask, ("Bad mask and position"));
      Is_True((_v[word] & bit_mask) == 0, ("Bad mask and position"));
      _v[word] |= (bit_mask);
    }
  }

  void dump_hex(FILE* f) const {
    for (int i=_length-1; i>=0; i--) fprintf(f, "0x%" LLX_FMT " ", _v[i]);
  }

  // the following is needed in order to work with a hack that uses the first
  // entry of a TI_SI_RR to record the length of the reservation table
  // we do not want a type conversion operator which can accidently be used
  // where type checking IS needed
  INT int_value() const { return _v[0]; }
  INT set_int_value(int i) { _v[0]=i; }
};

template <int type_id>
DLL_SHARED inline TI_SI_RRV<type_id> operator+(
	const TI_SI_RRV<type_id>& v1,
	const TI_SI_RRV<type_id>& v2) {
	TI_SI_RRV<type_id> res = v1; return res += v2;
}
template <int type_id>
DLL_SHARED inline TI_SI_RRV<type_id> operator-(
	const TI_SI_RRV<type_id>& v1,
	const TI_SI_RRV<type_id>& v2) {
	TI_SI_RRV<type_id> res = v1; return res -= v2;
}
template <int type_id>
DLL_SHARED inline TI_SI_RRV<type_id> operator|(
	const TI_SI_RRV<type_id>& v1,
	const TI_SI_RRV<type_id>& v2) {
	TI_SI_RRV<type_id> res = v1; return res |= v2;
}
template <int type_id>
DLL_SHARED inline TI_SI_RRV<type_id> operator&(
	const TI_SI_RRV<type_id>& v1,
	const TI_SI_RRV<type_id>& v2) {
	TI_SI_RRV<type_id> res = v1; return res &= v2;
}

#endif /* ti_res_vector_INCLUDED */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
