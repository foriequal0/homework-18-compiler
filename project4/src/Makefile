#
# This is a makefile for the skeleton program for project 3.
#

CC = gcc
LEX = flex
YACC = bison
COMPILE_OPTION = -g -Wall -Wextra -fsanitize=undefined
LINK_OPTION = -lfl -g -Wall -Wextra -fsanitize=undefined
YACC_OPTION = -vd

all : subc

clean :
	rm -f *.o lex.yy.c subc.tab.c subc.tab.h subc.output subc

subc : lex.yy.o hash.o subc.tab.o gen.o
	${CC} -o subc lex.yy.o hash.o subc.tab.o gen.o ${LINK_OPTION}

subc.tab.o : subc.tab.c subc.h
	${CC} -c ${COMPILE_OPTION} subc.tab.c

hash.o : hash.c subc.h
	${CC} -c ${COMPILE_OPTION} hash.c

gen.o : gen.c gen.h
	${CC} -c ${COMPILE_OPTION} gen.c

lex.yy.o : lex.yy.c subc.tab.h subc.h
	${CC} -c ${COMPILE_OPTION} lex.yy.c

lex.yy.c : subc.l
	${LEX} subc.l

subc.tab.h subc.tab.c : subc.y
	${YACC} ${YACC_OPTION} subc.y

