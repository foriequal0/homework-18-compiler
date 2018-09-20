/******************************************************
 * File Name   : subc.h
 * Description
 *    This is a header file for the subc program.
 ******************************************************/

#ifndef __SUBC_H__
#define __SUBC_H__

#include <stdio.h>
#include <strings.h>

enum TokenType {
	KEYWORD, IDENTIFIER, OPERATOR, FLOAT, INTEGER
};

typedef struct id {
	int tokenType;
	char *name;
	int count;
} id;

/* For hash table */
unsigned hash(const char *name);
id *enter(int tokenType, const char *name, int length);

#endif
