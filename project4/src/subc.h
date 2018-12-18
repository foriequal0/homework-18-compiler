/******************************************************
 * File Name   : subc.h
 * Description
 *    This is a header file for the subc program.
 ******************************************************/

#ifndef __SUBC_H__
#define __SUBC_H__

#include <stdio.h>
#include <strings.h>
#include "gen.h"

/* structure for ID */
struct id {
      char *name;
      int lextype;
};

/* For hash table */
unsigned hash(const char *name);
struct id *enter(int lextype, const char *name);
struct id *lookup(const char *name);

void print_error(const char *fmt, ...);

void print_command(const char* fmt, ...);
void print_jump_label(struct label label);
void print_data_label(struct label label, const char* fmt, ...);

#endif

