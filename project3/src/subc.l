%{
/*
 * File Name   : subc.l
 * Description : a skeleton flex input
 */

#include <stdarg.h>
#include <stdlib.h>
#include "subc.h"
#include "subc.tab.h"

static int comment_depth = 0;
static int lineno = 1;
static const char *filename = NULL;

void init_type();
%}

/* regular definitions */
letter		[A-Za-z_]
digit		[0-9]
id		{letter}({letter}|{digit})*
integer_const	([1-9]{digit}*)|"0"
white-space		[ \t]+
special-char	\\[nt]


%x COMMENT
%%
{id}	{
   const struct id * existing = lookup(yytext);
   if (existing) {
      yylval.string = existing->name;
      return existing->lextype;
   }
   struct id *entered  = enter(ID, yytext);
   yylval.string = entered->name;
   return entered->lextype;
}
{integer_const}	{
   yylval.integer = atoi(yytext);
   return INTEGER_CONST;
}
\"({special-char}|[^\\\"\n])*\"	{
   /* String Variable */
   yylval.string = yytext;
   return STRING;
}
'({special-char}|[^\\'\n])'	{
   /* Character Variable */
   yylval.ch = yytext[0];
   return CHAR_CONST;
}
"||" { return LOGICAL_OR; }
"&&" { return LOGICAL_AND; }
"<"|"<="|">"|">=" {
    yylval.string = yytext;
    return RELOP;
}
"=="|"!=" {
    yylval.string = yytext;
    return EQUOP;
}
"++" { return INCOP; }
"--" { return DECOP; }
"->"  { return STRUCTOP; }
"."|","|";"|"("|")"|"["|"]"|"{"|"}"|"="|"|"|"&"|"-"|"+"|"%"|"/"|"*"|"!" { return yytext[0]; }

{white-space}
<*>\n		   ++lineno;
<INITIAL,COMMENT>"/*"	{
		   ++comment_depth;
		   BEGIN (COMMENT);
		}
<COMMENT>"*/"	   if(--comment_depth == 0) BEGIN (INITIAL);
<COMMENT>.		 {}


%%

void print_error(const char* fmt, ...)
{
   /* prepare format */
   char extended_fmt[1024];
	 sprintf(extended_fmt, "%s:%d: error:%s\n", filename, lineno, fmt);
    /* prepare varags */
	 va_list args;
   va_start(args, fmt);
   vprintf(extended_fmt, args);
   va_end(args);
}

int main(int argc, char* argv[])
{
   /* populate keyword hash with keyword token type */
   struct keyword {
       int type;
       const char* text;
   };
   static const struct keyword keywords[] = {
       {TYPE, "int"}, {TYPE, "char"}, {VOID, "void"}, {STRUCT, "struct"}, 
       {RETURN, "return"}, {IF, "if"}, {ELSE, "else"}, {WHILE, "while"},
       {FOR, "for"}, {BREAK, "break"}, {CONTINUE, "continue"},
       {0, NULL}
   };
   for(int i = 0; keywords[i].text; i++) {
       enter(keywords[i].type, keywords[i].text);
   }
   init_type();
   if(argc >= 2) {
		yyin = fopen(argv[1], "r");
		filename = argv[1];
	}
   else {
		yyin = stdin;
		filename = "stdin";
	}
   if(!yyin) {
      printf("Can't open input stream!\n");
      exit(1);
   }
   
   yyparse();
   fclose(yyin);
   return 0;
}
