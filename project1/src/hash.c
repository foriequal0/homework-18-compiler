/***************************************************************
 * File Name    : hash.c
 * Description
 *      This is an implementation file for the open hash table.
 *
 ****************************************************************/

#include <stdlib.h>
#include <string.h>
#include "subc.h"

#define  HASH_TABLE_SIZE   101

typedef struct nlist {
	struct nlist *next;
	id *data;
} nlist;

static nlist *hashTable[HASH_TABLE_SIZE];

/* Java string hash algorithm
 * sum i=0..n (s[i] * 31 ^ (n-1-i)) */
unsigned hash(const char *name) {
  /* Unsigned integer overflow is well defined behaviour,
   * while signed one is undefined behaviour */
	unsigned h = 0;
	for(const char *p = name; *p; p++) {
    h = (h * 31) + *p;
	}
	return h;
}

id *enter(int tokenType, const char *name, int length) {
  unsigned h = hash(name) % HASH_TABLE_SIZE;

  /* Find existing entry, and exit early if it is found */
  for(nlist* entry = hashTable[h];entry; entry=entry->next) {
    if (strncmp(entry->data->name, name, length+1) == 0) {
      return entry->data;
    }
  }

  id* data = (id*)malloc(sizeof(id));
  char *name_copy = (char*)calloc(1, strnlen(name, length)+1);
  data->name = strncpy(name_copy, name, length+1);
  data->name[length] = 0;
  data->tokenType = tokenType;
  data->count = 0;

  /* Maintain linked list for collided entries */
  nlist* entry = (nlist*)malloc(sizeof(nlist));
  entry->data = data;
  entry->next = hashTable[h];
  hashTable[h] = entry;
  return data;
}
