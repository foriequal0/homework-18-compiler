%{
/*
 * File Name   : subc.l
 * Description : a skeleton flex input
 */

#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include "subc.h"
#include "subc.tab.h"

static int comment_depth = 0;
static int lineno = 1;
static const char *filename = NULL;
static FILE* fout;

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
   if (yytext[0] == '\\') {
      switch(yytext[1]) {
      case 'n': yylval.ch = '\n'; break;
      case 't': yylval.ch = '\t'; break;
      default: assert(0);
      }
   } else {
      yylval.ch = yytext[0];
   }
   return CHAR_CONST;
}
"||" { return LOGICAL_OR; }
"&&" { return LOGICAL_AND; }
"<"|"<="|">"|">=" {
    yylval.string = enter(RELOP, yytext)->name;
    return RELOP;
}
"=="|"!=" {
    yylval.string = enter(EQUOP, yytext)->name;
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

void print_command(const char* fmt, ...)
{
   /* prepare format */
   char extended_fmt[1024];
   sprintf(extended_fmt, "        %s\n", fmt);
   /* prepare varags */
   va_list args;
   va_start(args, fmt);
   vfprintf(fout, extended_fmt, args);
   va_end(args);
}

void print_jump_label(struct label label)
{
   fprintf(fout, "%s:\n", formatLabel(label));
}

void print_data_label(struct label label, const char* fmt, ...)
{
   /* prepare format */
   char extended_fmt[1024];
	sprintf(extended_fmt, "%s. %s\n", formatLabel(label), fmt);
   /* prepare varags */
	va_list args;
   va_start(args, fmt);
   vfprintf(fout, extended_fmt, args);
   va_end(args);
}

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
   exit(-1);
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
   if (argc == 1) {
		yyin = stdin;
      fout = stdout;
		filename = "stdin";
   } else if(argc == 2) {
		yyin = fopen(argv[1], "r");
      fout = stdout;
		filename = argv[1];
	} else if (argc >= 3) {
      yyin = fopen(argv[1], "r");
      fout = fopen(argv[2], "w");
      filename = argv[1];
   }

   if(!yyin) {
      printf("Can't open input stream!\n");
      exit(1);
   }
   if(!fout) {
      printf("Can't open output stream!\n");
      exit(1);
   }
   
   yyparse();
   fclose(yyin);
   fclose(fout);
   return 0;
}
