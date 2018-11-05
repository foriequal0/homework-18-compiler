%{
/*
 * File Name   : subc.l
 * Description : a skeleton flex input
 */

#include "subc.h"
#include "subc.tab.h"
static int comment_depth = 0;
static int lineno = 1;

%}

letter         [A-Za-z_]
digit          [0-9]
id             {letter}({letter}|{digit})*
integer_const  ([1-9]{digit}*)|"0"
white-space    [ \t]+
escape         \\[nt]

%x COMMENT
%%
{id} {
    const struct id * existing = lookup(yytext);
    if (existing) {
        yylval.stringVal = existing->name;
        return existing->lextype;
    }
    const struct id *entered  = enter(ID, yytext);
    yylval.stringVal = entered->name;
    return entered->lextype;
}
{integer_const} {
    yylval.intVal = atoi(yytext);
    return INTEGER_CONST;
}
\"({escape}|[^\\\"\n])*\" {
    /* String Variable */
    yylval.stringVal = yytext;
    return STRING;
}
\'({escape}|[^\\\'\n])\' {
    /* Character Variable */
    yylval.stringVal = yytext;
    return CHAR_CONST;
}
"+="|"-="|"*="|"/="|"%=" {
    yylval.stringVal = yytext;
    return ASSIGNOP;
}
"||" { return LOGICAL_OR; }
"&&" { return LOGICAL_AND; }
"<"|"<="|">"|">=" {
    yylval.stringVal = yytext;
    return RELOP;
}
"=="|"!=" {
    yylval.stringVal = yytext;
    return EQUOP;
}
"++" { return PLUS_PLUS; }
"--" { return MINUS_MINUS; }
"."  { return STRUCTOP; }
","|";"|"("|")"|"["|"]"|"{"|"}"|"="|"|"|"&"|"-"|"+"|"%"|"/"|"*"|"!" { return yytext[0]; }
{white-space}
<*>\n { ++lineno; }

<INITIAL,COMMENT>"/*" {
    ++comment_depth;
    BEGIN (COMMENT);
}
<COMMENT>"*/" {
    if(--comment_depth == 0) BEGIN (INITIAL);
}
<COMMENT>.


%%

int read_line()
{
   return lineno;
}

int main(int argc, char* argv[])
{
    /* populate keyword hash with keyword token type */
   struct keyword {
       int type;
       const char* text;
   };
   static const struct keyword keywords[] = {
       {TYPE, "int"}, {TYPE, "char"}, {STRUCT, "struct"},
       {RETURN, "return"}, {IF, "if"}, {ELSE, "else"}, {WHILE, "while"},
       {FOR, "for"}, {BREAK, "break"}, {CONTINUE, "continue"},
       {0, NULL}
   };
   for(int i = 0; keywords[i].text; i++)
       enter(keywords[i].type, keywords[i].text);

   if(argc >= 2) yyin = fopen(argv[1], "r");
   else yyin = stdin;
   if(!yyin) {
      printf("Can't open input stream!\n");
      exit(1);
   }
   //yydebug=1;
   yyparse();
   fclose(yyin);
   return 0;
}
