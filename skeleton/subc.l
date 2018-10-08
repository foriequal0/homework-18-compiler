%{
#include "subc.h"

/* A depth of the nested comment */
int commentdepth=0;

/* Identifier or keyword was found.
 * Whether it is identifier or keyword is determined by
 * pre-populated items in hash table */
void found_id(const char *s) {
    id *data = enter(IDENTIFIER, s, strlen(s));
    data->count++;
    if (data->tokenType == KEYWORD)
        printf("KEY\t%s\t%d\n", s, data->count);
    else if (data->tokenType == IDENTIFIER)
        printf("ID\t%s\t%d\n", s, data->count);
    else exit(-1);
}

/* Operators, operators, integers are found */
void found(int tokenType, char *s) {
    static const char* tokenTypeTable[] = {
        [OPERATOR] = "OP",
        [FLOAT] = "F",
        [INTEGER] = "INT"
    };
    const char* tt = tokenTypeTable[tokenType];
    printf("%s\t%s\n", tt, s);
}
%}

letter      [A-Za-z_]
alnum       [A-Za-z0-9_]
digit       [0-9]
whitespace  [ \t\n]
integer     0|([1-9]{digit}*)
float       {digit}+\.{digit}*([eE][+-]?{digit}+)?

identifier  {letter}{alnum}*
comment_open "/*"
comment_close "*/"
operator    "("|")"|"["|"]"|"{"|"}"|"->"|"."|".."|","|"!"|"~"|"++"|"--"|"*"|"/"|"%"|"+"|"-"|"<<"|">>"|"<"|"<="|">"|">="|"=="|"!="|"&"|"^"|"|"|"&&"|"||"|"?"|":"|";"|"="|"+="|"-="|"*="|"/="|"%="|"&="|"^="|"|="|"<<="|">>="

%start COMMENT

%%
<INITIAL>{
{comment_open}  { commentdepth++; BEGIN COMMENT; }
{comment_close} { /* comment close underflow! */ exit(-1); }
{identifier}    { found_id(yytext); }
{operator}      { found(OPERATOR, yytext); }
{integer}       { found(INTEGER, yytext); }
{integer}/".."  { found(INTEGER, yytext); /* lookahead ".." will be handled by {operator} rule */ }
{float}         { found(FLOAT, yytext); }
{whitespace}|.
}
<COMMENT>{
{comment_open}  { commentdepth++; }
{comment_close} { commentdepth--; if (commentdepth == 0) { BEGIN INITIAL; } }
{whitespace}|.
}

%%
/* pre-populate hashtable with keywords */
void initHash()
{
    static const char* keywords[]={
        "auto", "break", "case", "char", "continue", "default", "do", "double",
        "else", "extern", "float", "for", "goto", "if", "int", "long",
        "register", "return", "short", "sizeof", "static", "struct", "switch",
        "typedef", "union", "unsigned", "void", "while", NULL
    };

    for (int i = 0; keywords[i]; i++) {
        enter(KEYWORD,keywords[i], strlen(keywords[i]));
    }
}

int main(int argc, char* argv[])
{
    if (argc > 1) {
        yyin = fopen(argv[1],"r");
        if (!yyin) {
            fprintf(stderr, "Cannot open file %s\n", argv[1]);
        }
    }

    initHash();

    yylex ();

    return 0;
}
