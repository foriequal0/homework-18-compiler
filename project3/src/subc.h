/******************************************************
 * File Name   : subc.h
 * Description
 *    This is a header file for the subc program.
 ******************************************************/

#ifndef __SUBC_H__
#define __SUBC_H__

#include <stdio.h>
#include <strings.h>

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

#endif

