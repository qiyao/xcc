
/*

  Copyright (C) 2001-2005 Tensilica, Inc.  All Rights Reserved.

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
// UTL_Map
//////////////////////////////////////////////////
//
// A template class providing a map from key -> value. The map is implemented
// as a chained hash table of configurable size. Users of the class provide the
// type for the key and value. The key and value can be any type, but the map
// is most efficient if the key is easily hashed, and if the value is a pointer
// type.
//
//
// template class UTL_Map
// ----------------------
//
// Template class for map objects. There are three template arguments:
//
//     'K': Key type, preferably easy to hash into an unsigned int
//
//     'V': Value type, preferably a pointer
//
//     'H': Hash type. The hash type must provide a method called "hash" that
//     takes a key of type K and returns an unsigned int. The returned int will
//     be used to access the hash table. The hash type must also provide a
//     method call "test" that takes two keys of type K and returns true if the
//     keys are equal.  The following hash classes are provided for use when
//     the key is either an integer, pointer, string or case-insensitive
//     string:
//
//     class UTL_Map_IntHash {
//         unsigned int hash (int key) const;
//         bool test (int key1, int key2) const;
//     };
//
//     class UTL_Map_PtrHash {
//         unsigned int hash (void *key) const;
//         bool test (void *key1, void *key2) const;
//     };
//
//     class UTL_Map_StringHash {
//         unsigned int hash (char *key) const;
//         bool test (char *key1, char *key2) const;
//     };
//
//     class UTL_Map_StringNoCaseHash {
//         unsigned int hash (char *key) const;
//         bool test (char *key1, char *key2) const;
//     };
//
//     For example, to instantiate a map from ints to void pointers, the
//     provided UTL_Map_IntHash class can be used:
//
//        UTL_Map<int, void *, UTL_Map_IntHash> map;
//
//
//   UTL_Map methods
//   ---------------
//
//   Constructing and destructing:
//
//     UTL_Map<K,V,H> (MEM_POOL *pool, const unsigned int size =1024)
//
//         Create an empty map from 'K' type keys to 'V' type values. Hash keys
//         using an object of type 'H'. 'size' specifies the size of the hash
//         table used to implement the map. Map allocates hash table and other
//         internal storage from 'pool'.
//
//     ~UTL_Map<K,V,H> ()
//
//         Destroy the map, destroying all contained key-value pairs. If the
//         key or value is a pointer type, they are *not* destroyed.
//
//
//   Public methods:
//
//     void insert (K key, V val)
//
//         Insert key-value pair 'key'/'value' into the map, and return a
//         pointer to the inserted pair. Error if 'key' is already in the map.
//
//
//     void setValue(K key, V val)
//
//          Set the value of 'key' to be 'val' in the map.  This works even
//          if 'key' already has a value in the map -- it just changes it.
//
//
//     bool remove (K key)
//
//         Remove key-value pair associated with 'key' from the map. Return
//         true if 'key' was found and removed from the map, false if 'key' was
//         not found in the map.
//
//
//     bool find (K key, V *val =NULL) const
//     bool find (K key, V& val) const
//
//         Search for key-value pair associated with 'key'. Return true if
//         'key' is in the map, false if not in the map. For the pointer form,
//         if 'val' is non-NULL, use it to return the value associated with
//         'key'.  For the by reference form, the value is returned in 'val' if
//         the key is found, otherwise 'val' is not changed.
//
//
//
//   Public types
//   ------------
//
//     class UTL_Map<K,V,H>::keyValuePair
//
//         Simple type used to associate a key with a value in the map.
//
//
//         Public Methods:
//
//           K key (void) const
//
//               Return the key from the pair.
//
//
//           V& value (void)
//
//               Return a reference to the value from the pair.
//
//
//   Public iterators
//   ----------------
//
//   All iterators are used as follows:
//
//      keyValuePair_p scan;
//      UTL_MAP<K,V,H>::iter iter(map);
//      while ((scan = iter.next()) != NULL) {
//          ...
//      }
//
//   UTL_Map<K,V,H>::iter (UTL_Map<K,V,H> *map)
//
//       Iterate over the key-value pairs in the map.
//
//
// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/util/xtmap.h#1 $

#ifndef _XTMAP_H_
#define _XTMAP_H_

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include "mempool.h"
#include "cxx_memory.h"

//
// UTL_Map_IntHash
// UTL_Map_Int64Hash
// UTL_Map_PtrHash
// UTL_Map_StringHash
// UTL_Map_StringNoCaseHash
//
// Default hash objects for common key types.
//
class UTL_Map_IntHash {
public:
  unsigned int hash (const int key) const { return key; }
  bool test (const int key1, const int key2) const { return key1 == key2; }
};

class UTL_Map_Int64Hash {
public:
  unsigned int hash (const long long key) const { return key; }
  bool test (const long long key1, const long long key2) const { return key1 == key2; }
};

class UTL_Map_PtrHash {
public:
  unsigned int hash (const void *key) const { return((unsigned int)key); }
  bool test (const void *key1, const void *key2) const { return key1 == key2; }
};

class UTL_Map_StringHash {
public:
  unsigned int hash (const char *key) const {
    assert(key != NULL);
    unsigned int h = 0;
    while (*key != '\0') {
      h = (h << 2) + *key;
      key++;
    }
	
    return h;
  }

  bool test (const char *key1, const char *key2) const {
    return strcmp(key1, key2) == 0;
  }
};

class UTL_Map_StringNoCaseHash {
public:
  unsigned int hash (const char *key) const {
    assert(key != NULL);
    unsigned int h = 0;
    while (*key != '\0') {
      h = (h << 2) + tolower(*key);
      key++;
    }
	
    return h;
  }

  bool test (const char *key1, const char *key2) const {
    return strcasecmp(key1, key2) == 0;
  }
};

//
// UTL_Map
//
template <class K, class V, class H> class UTL_Map {
public:
  class iter;

  class keyValuePair {
    friend class UTL_Map<K,V,H>;
    friend class iter;

  private:
    const K ikey;
    V ivalue;
    keyValuePair *next;

    keyValuePair (const K k, const V v, keyValuePair *n) :
      ikey(k), ivalue(v), next(n) { }

  public:
    K key (void) const { return(ikey); }
    V& value (void) { return(ivalue); }
  };

  typedef class keyValuePair *keyValuePair_p;
  typedef const class keyValuePair *keyValuePair_cp;


private:
  MEM_POOL *pool;
  H hasher;
  keyValuePair_p *tbl;
  const unsigned int size;
  bool _delete;
  
  unsigned int getIndex (K key) const {
    const unsigned int ind = hasher.hash(key) % size;
    return(ind);
  }


public:
  class iter {
  private:
    UTL_Map<K,V,H> *map;
    unsigned int ind;
    keyValuePair_p ptr;

  public:
    iter (UTL_Map<K,V,H> *m) : map(m), ind(0), ptr(NULL) {
      if (map->size > 0)
	ptr = map->tbl[0];
    }
    iter (UTL_Map<K,V,H>& m) : map(&m), ind(0), ptr(NULL) {
      if (map->size > 0)
	ptr = map->tbl[0];
    }

    keyValuePair_p next (void) {
      if (ptr == NULL) {
	if ((ind+1) >= map->size)
	  return(NULL);

	ptr = map->tbl[++ind];
	return(next());
      }

      keyValuePair_p t = ptr;
      ptr = ptr->next;
      return(t);
    }
  };
  friend class iter;

  typedef class iter *iter_p;
  typedef const class iter *iter_cp;


  UTL_Map (MEM_POOL *p, const unsigned int iSz =1023, bool d =false) :
    pool(p), size(iSz), _delete(d)
  {
    tbl = CXX_NEW_ARRAY(keyValuePair_p, size, pool);
    memset(tbl, 0, size*sizeof(keyValuePair_p));
  }

  ~UTL_Map () {
    if (_delete)
    {
      keyValuePair_p kv;
      typename UTL_Map<K,V,H>::iter aiter(this);
      while ((kv = aiter.next()) != NULL)
	CXX_DELETE(kv, pool);
      CXX_DELETE_ARRAY(tbl, pool);
    }
  }

  bool findPair(K key, keyValuePair_p& pair) const
  {
    const unsigned int ind = getIndex(key);
    for (keyValuePair_p scan = tbl[ind]; scan != NULL; scan = scan->next) {
      if (hasher.test(scan->key(), key)) {
	pair = scan;
	return true;
      }
    }

    return false;
  }


  bool find (K key, V *val =NULL) const
  {
    keyValuePair_p pair;

    if (findPair(key,pair)) {
      if (val != NULL)
	*val = pair->value();
      return true;
    }

    return false;
  }

  bool find (K key, V& val) const
  {
    keyValuePair_p pair;
    if (findPair(key,pair)) {
      val = pair->value();
      return true;
    }
    return false;
  }

  void insert (K key, V val) {
    const unsigned int ind = getIndex(key);
    tbl[ind] = CXX_NEW(keyValuePair(key, val, tbl[ind]), pool);
  }

  void setValue(K key, V val)
  {
    keyValuePair_p pair;

    if (findPair(key,pair))
      pair->value() = val;
    else
      insert(key,val);
  }

  bool remove (K key) {
    const unsigned int ind = getIndex(key);
    if (hasher.test(tbl[ind]->key(), key)) {
      keyValuePair_p t = tbl[ind];
      tbl[ind] = tbl[ind]->next;
      if (_delete)
	CXX_DELETE(t, pool);
      return(true);
    }
    else {
      keyValuePair_p scan = tbl[ind];
      while (scan->next != NULL) {
	if (hasher.test(scan->next->key(), key)) {
	  keyValuePair_p t = scan->next;
	  scan->next = scan->next->next;
	  if (_delete)
	    CXX_DELETE(t, pool);
	  return(true);
	}

	scan = scan->next;
      }
    }

    return(false);
  }
};

#endif /* _XTMAP_H_ */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
