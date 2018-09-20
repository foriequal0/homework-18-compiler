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

unsigned hash(const char *name) {
	unsigned h = 0;
	for(const char *p = name; *p; p++) {
	    h = (h * 31) + *p;
	}
	return h;
}

id *enter(int tokenType, const char *name, int length) {
    unsigned h = hash(name) % HASH_TABLE_SIZE;

    for(nlist* entry = hashTable[h];entry; entry=entry->next) {
        if (strncmp(entry->data->name, name, length) == 0) {
            return entry->data;
        }
    }

    id* data = (id*)malloc(sizeof(id));
    char *name_copy = (char*)malloc(strnlen(name, length)+1);
    data->name = strncpy(name_copy, name, length);
    data->tokenType = tokenType;
    data->count = 0;

    nlist* entry = (nlist*)malloc(sizeof(nlist));
    entry->data = data;
    entry->next = hashTable[h];

    hashTable[h] = entry;
    return data;
}