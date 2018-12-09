%{
/*
 * File Name   : subc.y
 * Description : a skeleton bison input
 */
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include "subc.h"
#include "subc.tab.h"

int    yylex ();
void   yyerror (char* s);

struct ste;

enum declclass {
    D_INVALID=0, D_VAR=1, D_CONST, D_FUNC, D_TYPE, D_NULL
};

enum typeclass {
    T_INVALID=0, T_VOID, T_INT, T_CHAR, T_ARRAY, T_PTR, T_STRUCT,
};

struct decl {
    /* DECL Class: VAR, FUNC, TYPE, CONST */
    enum declclass declclass;

    /* CONST, VAR pointer to its type decl */
    struct decl *type;

    /* CONST: value of integer const */
    int intValue;
    char charValue;

    /* FUNC: ptr to formals list */
    struct ste *formals;
    /* FUNC: ptr to return TYPE decl */
    struct decl *returntype;

    /* TYPE: type class: int, array, ptr */
    enum typeclass typeclass;
    /* TYPE (array): ptr to element VAR decl */
    struct decl *elementvar;
    /* TYPE (array): number of elements */
    int num_index;
    /* TYPE (struct): ptr to field list */
    struct ste *fieldlist;
    /* TYPE (pointer): type of the pointer */
    struct decl *ptrto;

    /* ALL: size in bytes */
    int size;

    /* VAR: scope when VAR declared */
    struct ste **scope;

    /* For list_of_variables declarations */
    struct decl *next;
};

/* decl smart constructor & destructors */
struct decl* mkTypeDecl(enum typeclass typeclass) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_TYPE;
	decl->typeclass = typeclass;
	return decl;
}

/* does it has typeclass? */
bool isType(struct decl* decl, enum typeclass typeclass) {
	if ((decl->declclass != D_TYPE) || (decl->typeclass != typeclass)) {
		return false;
	} else {
		return true;
	}
}

struct decl* mkConst(struct decl* x) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_CONST;
	decl->type = x;
	return decl;
}

struct decl* mkVar(struct decl* x) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_VAR;
	decl->type = x;
	return decl;
}

bool is_var(struct decl* x) {
	return x->declclass == D_VAR;
}

bool is_var_or_const(struct decl* x) {
	return x->declclass == D_VAR || x->declclass == D_CONST;
}

/* destructure const or var */
struct decl* unConstVar(struct decl* x) {
	if (!x) return NULL;
	if (!is_var_or_const(x)) {
		print_error("not const or variable");
		return NULL;
	}
	return x->type;
}

/* destructure var */
struct decl* unVar(struct decl* x) {
	if (!x) return NULL;
	if (!is_var(x)) {
		print_error("not variable");
		return NULL;
	}
	return x->type;
}

struct decl* mkStructDecl(struct ste* fields) {
	struct decl* decl = mkTypeDecl(T_STRUCT);
	decl->fieldlist = fields;
	return decl;
}

/* destructure struct and get fieldlist */
struct ste* unStruct(struct decl* x) {
	if (!x) return NULL;
	if (!isType(x, T_STRUCT)) {
		print_error("variable is not struct");
		return NULL;
	}
	return x->fieldlist;
}

struct decl* mkPointer(struct decl* ptrto) {
	struct decl* decl = mkTypeDecl(T_PTR);
	decl->ptrto = ptrto;
	return decl;
}

/* get underlying type of pointer */
struct decl* unPointer(struct decl* x) {
	if (!x) return NULL;
	if (!isType(x, T_PTR)) {
		print_error("not a pointer");
		return NULL;
	}
	return x->ptrto;
}

struct decl* mkArray(struct decl* elementvar, int num_index) {
	struct decl* decl = mkTypeDecl(T_ARRAY);
	decl->elementvar = elementvar;
	decl->num_index = num_index;
	return decl;
}

/* get underlying type of array */
struct decl* unArray(struct decl* x) {
	if (!x) return NULL;
	if (!isType(x, T_ARRAY)) {
		print_error("not array type");
		return NULL;
	}
	return x->elementvar;
}

struct decl* mkFunc(struct decl* returntype, struct ste* formals) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_FUNC;
	decl->returntype = returntype;
	decl->formals =formals;
	return decl;
}

struct decl* mkNull() {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_NULL;
	return decl;
}

bool isNull(struct decl* x) {
	return x && (x->declclass == D_NULL);
}

struct ste {
    struct id* id;
    struct decl* decl;
    struct ste* prev;
};

struct scope_entry {
    struct ste* ste_top;
    struct scope_entry* prev;
};

struct scope_entry* scope_stack_top;
struct ste* flat_scope;

void scope_push() {
    struct scope_entry* new_entry = calloc(1, sizeof(struct scope_entry));
    new_entry->prev = scope_stack_top;
    if (scope_stack_top != NULL) {
        new_entry->ste_top = scope_stack_top->ste_top;
    }
    scope_stack_top = new_entry;
}

struct ste* scope_pop() {
    struct scope_entry* tmp = scope_stack_top;
    assert(tmp);
    scope_stack_top = tmp->prev;
	if (scope_stack_top == tmp) {
		return NULL;
	}

	for(struct ste* ste = tmp->ste_top; ste; ste = ste->prev) {
		if (ste->prev == scope_stack_top->ste_top) {
			ste->prev = NULL;
		}
	}
	return tmp->ste_top;
}

/* Lookup varaiable environment */
struct decl* decl_lookup(const char* name) {
    if (scope_stack_top != NULL) {
        struct ste* ste = scope_stack_top->ste_top;
        for(;ste; ste=ste->prev) {
            if (strcmp(name, ste->id->name) == 0){
                return ste->decl;
            }
        }
    }
    return NULL;
}

/* Lookup varaiable only in current scope */
struct decl* decl_lookup_scope(const char* name) {
	struct ste** scope = &scope_stack_top->ste_top;

    if (scope_stack_top != NULL) {
        struct ste* ste = scope_stack_top->ste_top;
        for(;ste; ste=ste->prev) {
			if (ste->decl->scope != scope) return NULL;
            if (strcmp(name, ste->id->name) == 0){
                return ste->decl;
            }
        }
    }
    return NULL;
}

/* lookup type */
struct decl* decl_type_lookup(const char* name) {
    struct ste* ste = flat_scope;
    for(;ste; ste=ste->prev) {
        if (strcmp(name, ste->id->name) == 0){
            return ste->decl;
        }
    }
    return NULL;
}

/* does a variable be declared in current scope? */
bool check_decl_scope(const char* name) {
	struct decl* existing_global = decl_lookup(name);
	if (existing_global != NULL && isNull(existing_global)) {
		print_error("unqualified id");
		return NULL;
	}
	struct decl* existing_scope = decl_lookup_scope(name);
	if (existing_scope) {
		print_error("redeclaration");
		return false;
	}
	return true;
}

struct decl* decl_scope(const char* name, struct decl* decl) {
	struct ste** scope = &scope_stack_top->ste_top;
	if (!check_decl_scope(name)) {
		return NULL;
	}

	struct id* id = lookup(name);

    assert(id->lextype == ID);
    struct ste* ste = calloc(1, sizeof(struct ste));
    ste->id = id;
    ste->decl = decl;
	
    ste->prev = scope_stack_top->ste_top;
    scope_stack_top->ste_top = ste;
	
	decl->scope = scope;
	assert(decl_lookup(name) == decl);
	return decl;
}

/* does a type be declared? */
bool check_decl_type(const char* name) {
	if (strcmp(name, "NULL") == 0) {
		print_error("unqualified id");
		return NULL;
	}
	struct decl* existing = decl_type_lookup(name);
	if (existing) {
		print_error("redeclaration");
		return false;
	}
	return true;
}

struct decl* decl_type(const char* name, struct decl* decl) {
	if (!check_decl_type(name)) {
		return NULL;
	}

	struct id* id = lookup(name);
	assert(((id->lextype == TYPE || id->lextype == VOID) && decl->declclass == D_TYPE) ||
		(id->lextype == ID && decl->declclass == D_TYPE));

    struct ste* ste = calloc(1, sizeof(struct ste));
    ste->id = id;
    ste->decl = decl;

    ste->prev = flat_scope;
    flat_scope = ste;

	assert(decl_type_lookup(name) == decl);
	return decl;
}

/* initialize some tokens */
void init_type() {
    if (scope_stack_top == NULL) scope_push();

    decl_type("int", mkTypeDecl(T_INT));
    decl_type("char", mkTypeDecl(T_CHAR));
    decl_type("void", mkTypeDecl(T_VOID));
	enter(ID, "NULL");
	decl_scope("NULL", mkNull());
	enter(ID, "*return");
}

/**
 * printing function for debugging
 */
void print_id(struct id* id) {
	switch(id->lextype) {
	case ID: printf("%s", id->name); return;
	case TYPE: printf("%s_T", id->name); return;
	case VOID: printf("%s_V", id->name); return;
	default: assert(0);
	}
}

void print_decl(struct decl* decl, bool newline);
void print_ste(struct ste* ste, bool newline) {
	for(; ste; ste = ste->prev) {
		print_id(ste->id);
		printf(": ");
		print_decl(ste->decl, false);
		printf(", ");
	}
	if (newline) printf("\n");
}

void print_decl(struct decl* decl, bool newline) {
	if (decl == NULL) return;
	switch(decl->declclass) {
	case D_CONST: printf("const of "); print_decl(decl->type, false); break;
	case D_FUNC: 
		printf("func of "); 
		printf("("); print_ste(decl->formals, false); printf(") -> "); 
		print_decl(decl->returntype, false); 
		break;
	case D_TYPE:
		switch (decl->typeclass) {
		case T_INT: printf("int"); break;
		case T_CHAR: printf("char"); break;
		case T_VOID: printf("void"); break;
		case T_ARRAY: printf("array[%d] of ", decl->num_index); print_decl(decl->elementvar, false);  break;
		case T_PTR: printf("ptr of "); print_decl(decl->ptrto, false);  break;
		case T_STRUCT: printf("struct of "); printf("{"); print_ste(decl->fieldlist, false); printf("}");  break;
		case T_INVALID: printf("invalid"); break;
		}
	break;
	case D_VAR: printf("var of "); print_decl(decl->type, false); break;
	case D_NULL: printf("null"); break;
	default: assert(0);
	}
	if (decl->next != NULL) {
		printf(", "); print_decl(decl->next, false);
	} else {
		if (newline) printf("\n");
	}
}

void print_scope() {
	if (scope_stack_top == NULL) { return; }
	struct ste* ste = scope_stack_top->ste_top;
	struct ste* prev = NULL;
	for(; ste; ste = ste->prev) {
		if (prev != NULL && prev->decl->scope != ste->decl->scope) {
			printf("--\n");
		}
		print_decl(ste->decl, true);
		prev = ste;
	}
}

void print_types() {
	if (flat_scope == NULL) { return; }
	struct ste* ste = flat_scope;
	for(; ste; ste = ste->prev) {
		print_decl(ste->decl, true);
	}
}

void print_env() {
	print_types();
	printf("=====");
	print_scope();
}

/* does it have same types? 
require: lhs, rhs must stripped its 'const', 'var' wrapper 
*/
bool has_same_type(struct decl* lhs, struct decl* rhs) {
	for(int i=0; lhs && rhs; i++) {
		assert((i % 2 == 0) || (is_var(lhs) && is_var(rhs)));
		if (i%2 == 1) {
			lhs = lhs->type;
			rhs = rhs->type;
			continue;
		}
		if (lhs == rhs) {
			return true;
		}
		assert(lhs->declclass == D_TYPE && rhs->declclass == D_TYPE);
		if (lhs->typeclass == T_ARRAY) {
			lhs = lhs->elementvar;
			rhs = rhs->elementvar;
		} else if (lhs->typeclass == T_PTR) {
			lhs = lhs->ptrto;
			rhs = rhs->ptrto;
		} else {
			return false;
		}
	}
	assert(lhs != rhs);
	return false;
}

/* rhs assignable to lhs?
require: lhs must stripped its 'const', 'var' wrapper 
*/
bool assignable(struct decl* lhs, struct decl* rhs) {
	assert(lhs->declclass == D_TYPE);
	struct decl* void_t = decl_type_lookup("void");
	if (lhs == void_t || isType(lhs, T_ARRAY)) { 
		return false; 
	}
	
	if (isType(lhs, T_PTR) && isNull(rhs)) {
		return true;
	}

	if (!is_var_or_const(rhs)) {
		return false;
	} else {
		rhs = rhs->type;
	}

	/* pointer to array is impossible */
	/* pointer to const is impossible */
	/* syntax allows only (array of)? (pointer of)? (int|char|struct) */
	/* array relop doesn't matter in grading */
	/* pointer of (var of struct, var of int, var of char, var of void, var of pointer) */
	// if (isType(lhs, T_PTR) && isType(rhs, T_ARRAY)) {
	// 	// array to pointer decay
	// 	lhs = lhs->ptrto->type;
	// 	rhs = rhs->elementvar->type;
	// }

	return has_same_type(lhs, rhs);
}

/* integer binary operator checker */
struct decl* binary_int(struct decl* lhs, struct decl* rhs) {
	if (lhs == NULL || rhs == NULL) {
		return NULL;
	}
	struct decl* int_t = decl_type_lookup("int");
	if (is_var_or_const(lhs) && is_var_or_const(rhs) 
		&& has_same_type(int_t, unConstVar(lhs)) 
		&& has_same_type(int_t, unConstVar(rhs)))
	{
		return mkConst(int_t);
	}
	print_error("not int type");
	return NULL;
}

/* binary relation operator checker */
struct decl* binary_rel(struct decl* lhs, struct decl* rhs) {
	if (lhs == NULL || rhs == NULL) {
		return NULL;
	}
	struct decl* int_t = decl_type_lookup("int");
	struct decl* char_t = decl_type_lookup("char");
	
	lhs = unConstVar(lhs);
	if (!lhs) { return NULL; }
	rhs = unConstVar(rhs);
	if (!rhs) { return NULL; }
	
	if ((has_same_type(int_t, lhs) || has_same_type(char_t, lhs))
		&& (has_same_type(int_t, rhs) || has_same_type(char_t, rhs))) 
	{
		if (has_same_type(lhs, rhs)){
			return mkConst(int_t);
		} else {
			print_error("not comparable");
			return NULL;
		}
	} else {
		print_error("not int or char type");
		return NULL;
	}
}

/* binary equality operator checker */
struct decl* binary_equal(struct decl* lhs, struct decl* rhs) {
	if (lhs == NULL || rhs == NULL) {
		return NULL;
	}
	struct decl* int_t = decl_type_lookup("int");
	struct decl* char_t = decl_type_lookup("char");
	
	/* allow null comparision */
	if (isNull(lhs) || isNull(rhs)) {
		if ((is_var_or_const(lhs) && isType(unConstVar(lhs), T_PTR))
		|| (is_var_or_const(rhs) && isType(unConstVar(rhs), T_PTR))) {
			return mkConst(int_t);
		}
	}

	lhs = unConstVar(lhs);
	if (!lhs) { return NULL; }
	rhs = unConstVar(rhs);
	if (!rhs) { return NULL; }

	if ((has_same_type(int_t, lhs) 
			|| has_same_type(char_t, lhs) 
			|| isType(lhs, T_PTR))
		&& (has_same_type(int_t, rhs) 
			|| has_same_type(char_t, rhs)
			|| isType(rhs, T_PTR))) 
	{
		if (has_same_type(lhs, rhs)){
			return mkConst(int_t);
		} else {
			print_error("not comparable");
			return NULL;
		}
	} else {
		print_error("not int or char type");
		return NULL;
	}
}

/* unary increment or decrement operator */
struct decl* unary_indec(struct decl* x) {
	struct decl* int_t = decl_type_lookup("int");
	struct decl* char_t = decl_type_lookup("char");
	x = unVar(x);
	if (!x) {
		return NULL;
	} 
	if (!has_same_type(int_t, x) && !has_same_type(char_t, x)) {
		print_error("not int or char type");
		return NULL;
	} 
	return mkConst(int_t);
}

/* check it is integer */
struct decl* unary_int(struct decl* x) {
	struct decl* int_t = decl_type_lookup("int");
	x = unConstVar(x);
	if (!x) {
		return NULL;
	} 
	if (!has_same_type(int_t, x)) {
		print_error("not int type");
		return NULL;
	}	
	return mkConst(int_t);
}

/* get member or raise error */
struct decl* struct_member(struct decl* x, const char* name) {
	struct ste* ste = unStruct(unConstVar(x));
	if (!ste) return NULL;
	for(; ste; ste = ste->prev) {
		if (strcmp(ste->id->name, name) == 0) {
			return ste->decl;
		}
	}
	print_error("struct not have same name field");
	return NULL;
}

%}

/* yylval types */
%union {
    int		integer;
	char    ch;
    char*   string;
    struct id* id;
    struct decl* decl;
    struct ste* ste;
}

/* Precedences and Associativities */
%right THEN ELSE
%left	','
%right	'='
%left	LOGICAL_OR
%left	LOGICAL_AND
%left	'&'
%left	EQUOP
%left	RELOP
%left	'+' '-'
%left	'*'
%right	'!' INCOP DECOP
%left	'[' ']' '(' ')' '.' STRUCTOP

/* Token and Types */
%token <string> TYPE VOID
%token STRUCT STRUCTOP
%token RETURN IF ELSE WHILE FOR BREAK CONTINUE 
%token LOGICAL_OR LOGICAL_AND INCOP DECOP RELOP EQUOP
%token <integer> INTEGER_CONST
%token <string> STRING
%token <ch> CHAR_CONST
%nonassoc <string>  ID
%type <decl> type_specifier struct_specifier func_decl pointers param_list param_decl ext_def def stmt expr expr_e or_expr or_list and_list and_expr binary unary args

%%
program
	: ext_def_list
	;

ext_def_list
	: ext_def_list ext_def
	| /* empty */
	;

ext_def
	: type_specifier pointers ID ';' {  
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkVar(target)); 
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']' ';' { 
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), 0))); // TODO const_expr
		} else {
			$$ = NULL;
		}
	}
	| func_decl ';'
	| type_specifier ';'
	| func_decl '{' { 
			scope_push();
			decl_scope("*return", $1->returntype);
			struct ste* ste = $1->formals;
			for(; ste; ste = ste->prev) {
				decl_scope(ste->id->name, ste->decl);
			}
		} 
		local_defs stmt_list '}' { scope_pop(); }
	;

type_specifier
	: TYPE { $$ = decl_type_lookup($1); }
	| VOID { $$ = decl_type_lookup($1); }
	| struct_specifier
	;

struct_specifier
	: STRUCT ID { 
		check_decl_type($2);
		scope_push(); 
	} '{' def_list '}' { 
		struct ste* fields = scope_pop(); 
		if (fields == NULL) {
			print_error("incomplete type error");
			$$ = NULL;
		} else {
			if (!decl_type_lookup($2)) {
				$$ = decl_type($2, $$=mkStructDecl(fields));
			} else {
				$$ = NULL;
			}
		}
	}
	| STRUCT ID { 
		struct decl* decl = decl_type_lookup($2); 
		if (decl && decl->typeclass == T_STRUCT) {
			$$ = decl;
		} else {
			print_error("incomplete type error");
			$$ = NULL;
		}
	}
	;

func_decl
	: type_specifier pointers ID '(' ')' {
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID);
		struct decl* func = mkFunc(returntype, NULL);
		decl_scope($3, func);
		$$ = func;
	}
	| type_specifier pointers ID '(' _push_scope VOID ')' {
		scope_pop();
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID);
		struct decl* func = mkFunc(returntype, NULL);
		decl_scope($3, func);
		$$ = func;
	}
	| type_specifier pointers ID '(' _push_scope param_list ')' {
		struct ste* fields = scope_pop();
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID);
		struct decl* func = mkFunc(returntype, fields);
		decl_scope($3, func);
		$$ = func;
	}
	;
_push_scope: { scope_push(); }
pointers
	: '*' { 
		struct decl* target = $<decl>0; 
		if (target) {
			$$ = mkPointer(mkVar(target));
		} else {
			$$ = NULL;
		}
	}
	| /* empty */ { $$ = $<decl>0; }
	;

param_list  /* list of formal parameter declaration */
	: param_decl
	| param_list ',' param_decl
	;

param_decl  /* formal parameter declaration */
	: type_specifier pointers ID {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkVar(target));
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']'  {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), 0))); // TODO const_expr
		} else {
			$$ = NULL;
		}
	}
	;

def_list    /* list of definitions, definition can be type(struct), variable, function */
	: def_list def
	| /* empty */
	;

def
	: type_specifier pointers ID ';' {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkVar(target));
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']' ';' {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), 0))); // TODO const_expr
		} else {
			$$ = NULL;
		}
	}
	| type_specifier ';'
	| func_decl ';'
	;

compound_stmt
	: '{' { scope_push(); } local_defs stmt_list '}' { scope_pop(); }
	;

local_defs  /* local definitions, of which scope is only inside of compound statement */
	:	def_list
	;

stmt_list
	: stmt_list stmt 
	| /* empty */
	;

stmt
	: expr ';' {$$ = NULL;}
	| compound_stmt {$$ = NULL;}
	| RETURN ';' { 
		struct decl* returntype = decl_lookup("*return");
		if (!isType(returntype, T_VOID)) {
			print_error("return value is not return type");
		}
		$$ = NULL;
	}
	| RETURN expr ';' {
		if ($2 != NULL) {
			struct decl* returntype = decl_lookup("*return");
			if (!assignable(returntype, $2)) {
				print_error("return value is not return type");
			}
		}
		$$ = NULL;
	}
	| ';' { $$ = NULL; }
	| IF '(' expr ')' stmt %prec THEN {
		$$ = unary_int($3);
	}
	| IF '(' expr ')' stmt ELSE stmt {
		$$ = unary_int($3);
	}
	| WHILE '(' expr ')' stmt {
		$$ = unary_int($3);
	}
	| FOR '(' expr_e ';' expr_e ';' expr_e ')' stmt {
		$$ = unary_int($5);
	}
	| BREAK ';' { $$ = NULL; }
	| CONTINUE ';' { $$ = NULL; }
	;

expr_e
	: expr
	| /* empty */ { $$ = NULL; }
	;

const_expr
	: expr
	;

expr
	: unary '=' expr {
		if ($1 == NULL || $3 == NULL) {  // error on subexpression.
			$$ = NULL;
		} else if (!is_var($1)) {
			print_error("LHS is not a variable");
			$$ = NULL;
		} else if (assignable($1->type, $3)) {
			$$ = $1;
		} else if (!is_var_or_const($3)) {
			print_error("RHS is not a const or variable");
			$$ = NULL;
		} else {
			print_error("LHS and RHS are not same type");
			$$ = NULL;
		}
	}
	| or_expr
	;

or_expr
	: or_list
	;

or_list
	: or_list LOGICAL_OR and_expr { $$ = binary_int($1, $3); }
	| and_expr
	;

and_expr
	: and_list
	;

and_list
	: and_list LOGICAL_AND binary { $$ = binary_int($1, $3); }
	| binary
	;

binary
	: binary RELOP binary { $$ = binary_rel($1, $3); }
	| binary EQUOP binary { $$ = binary_equal($1, $3); }
	| binary '+' binary { $$ = binary_int($1, $3); }
	| binary '-' binary { $$ = binary_int($1, $3); }
	| unary %prec '='
	;

unary
	: '(' expr ')' { $$ = $2; }
	| '(' unary ')' { $$ = $2; }
	| INTEGER_CONST { ($$ = mkConst(decl_type_lookup("int")))->intValue = $1; }
	| CHAR_CONST { ($$ = mkConst(decl_type_lookup("char")))->charValue = $1; }
	| STRING { 
		$$ = mkConst(mkPointer(mkVar(decl_type_lookup("char")))); // TODO: assign stringValue
	}
	| ID {
		struct decl* decl = decl_lookup($1);
		if (decl == NULL) {
			print_error("not declared");
		}
		$$ = decl;
	}
	| '-' unary %prec '!' {
		if ($2 == NULL) {
			$$ = NULL;
		} else if (!assignable(decl_type_lookup("int"), $2)) {
			print_error("not int type");
			$$ = NULL;
		} else {
			$$ = $2;
		}
	}
	| '!' unary { $$ = unary_int($2); }
	| unary INCOP { $$ = unary_indec($1); }
	| unary DECOP { $$ = unary_indec($1); }
	| INCOP unary { $$ = unary_indec($2); }
	| DECOP unary { $$ = unary_indec($2); }
	| '&' unary %prec '!' {
		if ($2 == NULL) {
			$$ = NULL;
		} else if (!is_var($2)) {
			print_error("not variable");
			$$ = NULL;
		} else {
			$$ = mkConst(mkPointer($2));
		}
	}
	| '*' unary	 %prec '!' {
		$$ = unPointer(unConstVar($2));
	}
	| unary '[' expr ']' {
		struct decl* elem = unArray(unConstVar($1));
		if (elem && unary_int($3)) {
			$$ = elem;
		} else {
			$$ = NULL;
		}
	}
	| unary '.' ID {
		$$ = struct_member($1, $3);
	}
	| unary STRUCTOP ID {
		$$ = struct_member(unPointer(unConstVar($1)), $3);
	}
	| unary '(' args ')' {
		if ($1 == NULL) {
			$$ = NULL;
		} else {
			if ($1->declclass != D_FUNC) {
				print_error("not a function");
				$$ = NULL;
			} else {
				enum result_t { SO_FAR_SO_GOOD, INVALID, SILENT } result = SO_FAR_SO_GOOD; 
				struct ste* formal = $1->formals;
				struct decl* arg_list = $3;
				while(formal && arg_list) {
					assert(is_var_or_const(formal->decl));
					assert(arg_list->declclass == D_CONST); /* assert dummy wrapper const */
					struct decl* arg = unConstVar(arg_list); /* peel dummy wrapper const */
					if (arg == NULL) {
						result = SILENT;
						break;
					}
					if (!assignable(formal->decl->type, arg)) {
						result = INVALID;
						break;
					}
					formal = formal->prev;
					arg_list = arg_list->next;
				}
				if (result == SO_FAR_SO_GOOD && !(formal == NULL && arg_list == NULL)) {
					result = INVALID;
				}
				switch(result) {
				case SO_FAR_SO_GOOD: 
					$$ = mkConst($1->returntype);
					break;
				case INVALID:
					print_error("actual args are not equal to formal args");
					$$ = NULL;
					break;
				case SILENT:
					$$ = NULL;
					break;
				}
			}
		}
	}
	| unary '(' ')' {
		if ($1 == NULL) {
			$$ = NULL;
		} else {
			if ($1->declclass != D_FUNC) {
				print_error("not a function");
				$$ = NULL;
			} else if ($1->formals != NULL) {
				print_error("actual args are not equal to formal args");
				$$ = NULL;
			} else {
				$$ = mkConst($1->returntype);
			}
		}
	}
	;

args    /* actual parameters(function arguments) transferred to function */
	: expr { $$ = mkConst($1); } // dummy const wrapper
	| args ',' expr { $$ = mkConst($3); $$->next = $1; }
    ;
%%

/*  Additional C Codes here */

void yyerror (char* s)
{
	print_error(s);
}
