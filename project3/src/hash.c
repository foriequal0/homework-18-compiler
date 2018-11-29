/***************************************************************
 * File Name    : hash.c
 * Description
 *      This is an implementation file for the open hash table.
 *
 ****************************************************************/

#include "subc.h"
#include <string.h>
#include <stdlib.h>

#define  HASH_TABLE_SIZE   101

struct nlist {
   struct nlist *next;
   struct id *data;
};

static struct nlist *hashTable[HASH_TABLE_SIZE];

/* Java string hash algorithm
 * sum i=0..n (s[i] * 31 ^ (n-1-i)) */
unsigned hash(const char *name) {
  unsigned h = 0;
  for(const char *p = name; *p; p++) {
    h = (h * 31) + *p;
  }
  return h;
}

struct id *enter(int lextype, const char *name) {
  unsigned h = hash(name) % HASH_TABLE_SIZE;

  /* Find existing entry, and exit early if it is found */
  struct id* existing = lookup(name);
  if (existing) {
    return existing;
  }

  struct id* data = (struct id*) calloc(1, sizeof(struct id));
  data->name = strdup(name); // copy yytext because it is modified while parsing.
  data->lextype = lextype;

  struct nlist* entry = (struct nlist*) malloc(sizeof(struct nlist));
  entry->data = data;
  entry->next = hashTable[h];
  hashTable[h] = entry;
  return data;
}

struct id *lookup(const char *name) {
  unsigned h = hash(name) % HASH_TABLE_SIZE;
  for (struct nlist* entry = hashTable[h]; entry; entry = entry->next) {
    if (strcmp(entry->data->name, name) == 0) {
      return entry->data;
    }
  }
  return NULL;
}

