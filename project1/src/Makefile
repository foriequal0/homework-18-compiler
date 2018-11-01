CC=gcc -g -std=c99 -Wall -fsanitize=address -fsanitize=undefined

subc: lex.yy.o hash.o
	$(CC) -o subc lex.yy.o hash.o -lfl

lex.yy.o: lex.yy.c
	$(CC) -c lex.yy.c

lex.yy.c: subc.l
	flex subc.l

hash.o: hash.c subc.h
	$(CC) -c hash.c

clean:
	rm lex.yy.c
	rm *.o
	rm subc
