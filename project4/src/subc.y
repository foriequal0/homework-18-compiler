%{
/*
 * File Name   : subc.y
 * Description : a skeleton bison input
 */
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include "subc.h"
#include "gen.h"
#include "subc.tab.h"

int    yylex ();
void   yyerror (char* s);

struct ste;

enum declclass {
    D_INVALID=0, D_VAR=1, D_CONST, D_FUNC, D_TYPE, D_NULL
};

enum funcclass {
	F_REGULAR, F_READ_INT, F_READ_CHAR, F_WRITE_INT, F_WRITE_CHAR, F_WRITE_STRING
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
    struct constant constant;

	enum funcclass funcclass;
    /* FUNC: ptr to formals list */
    struct ste *formals;
    /* FUNC: ptr to return TYPE decl */
    struct decl *returntype;
	const char *funcname;

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
	struct alloc alloc;

    /* VAR: scope when VAR declared */
    struct ste **scope;

    /* For list_of_variables declarations */
    struct decl *next;
};

struct ste {
    struct id* id;
    struct decl* decl;
    struct ste* prev;
};

struct scope_entry {
    struct ste* ste_top;
    struct scope_entry* prev;
	enum location location;
	int size;
};

static struct scope_entry* scope_stack_top;
static struct ste* flat_scope;
static struct decl* func_scope;

/* decl smart constructor & destructors */
struct decl* mkTypeDecl(enum typeclass typeclass, int size) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_TYPE;
	decl->typeclass = typeclass;
	decl->size = size;
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
	decl->size = x->size;
	return decl;
}

struct decl* mkVar(struct decl* x) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_VAR;
	decl->type = x;
	decl->size = x->size;
	return decl;
}

bool is_var(struct decl* x) {
	return x->declclass == D_VAR;
}

bool is_const(struct decl* x) {
	return x->declclass == D_CONST;
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
	int total_size = 0;
	struct ste* it = fields;
	for(; it; it = it->prev) {
		total_size += it->decl->size;
	}
	struct decl* decl = mkTypeDecl(T_STRUCT, total_size);
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
	struct decl* decl = mkTypeDecl(T_PTR, 1);
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
	if (!elementvar) NULL;
	struct decl* decl = mkTypeDecl(T_ARRAY, elementvar->size*num_index);
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

struct decl* mkFunc(struct decl* returntype, struct ste* formals, const char* funcname) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_FUNC;
	decl->funcclass = F_REGULAR;
	decl->returntype = returntype;
	decl->funcname = funcname;
	decl->formals =formals;
	return decl;
}

struct decl* mkPrim(enum funcclass funcclass, struct decl* returntype, struct ste* formals) {
	struct decl* decl = calloc(1, sizeof(struct decl));
	decl->declclass = D_FUNC;
	decl->funcclass = funcclass;
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

void scope_push(enum location location) {
    struct scope_entry* new_entry = calloc(1, sizeof(struct scope_entry));
    new_entry->prev = scope_stack_top;
	new_entry->location = location;
	new_entry->size = 0;
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

struct decl* decl_scope(const char* name, struct decl* decl, bool stack_alloc) {
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
	if (stack_alloc && is_var_or_const(decl)) {
		struct alloc alloc = {
			.location = scope_stack_top->location,
			.offset = scope_stack_top->size,
		};
		decl->alloc = alloc;
		scope_stack_top->size += decl->size;
	}

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
    if (scope_stack_top == NULL) scope_push(LOC_GLOBAL);

    decl_type("int", mkTypeDecl(T_INT, 1));
    decl_type("char", mkTypeDecl(T_CHAR, 1));
    decl_type("void", mkTypeDecl(T_VOID, 1));
	enter(ID, "*realvoid");
    decl_type("*realvoid", mkTypeDecl(T_VOID, 0));
	enter(ID, "NULL");
	decl_scope("NULL", mkNull(), false);
	enter(ID, "*return");

	enter(ID, "read_int");
	decl_scope("read_int", mkPrim(F_READ_INT, decl_type_lookup("int"), NULL), false);
	enter(ID, "read_char");
	decl_scope("read_char", mkPrim(F_READ_CHAR, decl_type_lookup("char"), NULL), false);
	
	enter(ID, "x");

	scope_push(LOC_LOCAL);
	decl_scope("x", mkVar(decl_type_lookup("int")), false);
	enter(ID, "write_int");
	decl_scope("write_int", mkPrim(F_WRITE_INT, decl_type_lookup("*realvoid"), scope_pop()), false);

	scope_push(LOC_LOCAL);
	decl_scope("x", mkVar(decl_type_lookup("char")), false);
	enter(ID, "write_char");
	decl_scope("write_char", mkPrim(F_WRITE_CHAR, decl_type_lookup("*realvoid"), scope_pop()), false);

	scope_push(LOC_LOCAL);
	decl_scope("x", mkConst(mkPointer(mkVar(decl_type_lookup("char")))), false);
	enter(ID, "write_string");
	decl_scope("write_string", mkPrim(F_WRITE_STRING, decl_type_lookup("*realvoid"), scope_pop()), false);
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
	} else {
		// assume it as pointer + int operation
		return lhs;
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
struct decl* unary_indec_pre(struct decl* x) {
	struct decl* int_t = decl_type_lookup("int");
	struct decl* char_t = decl_type_lookup("char");
	x = unVar(x);
	if (!x) {
		return NULL;
	} 
	if (has_same_type(int_t, x) || has_same_type(char_t, x) || isType(x, T_PTR)) {
		return mkVar(x);
	}
	return NULL;
}

struct decl* unary_indec_post(struct decl* x) {
	struct decl* int_t = decl_type_lookup("int");
	struct decl* char_t = decl_type_lookup("char");
	x = unVar(x);
	if (!x) {
		return NULL;
	}
	if (has_same_type(int_t, x)) {
		return mkConst(int_t);
	} else if (has_same_type(char_t, x)) {
		return mkConst(char_t);
	} else if (isType(x, T_PTR)) {
		return mkConst(x);
	}
	print_error("not int or char type");
	return NULL;
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
	struct ste* reverse[100] = {NULL};
	int i=0;
	for(; ste; ste = ste->prev) {
		reverse[i++] = ste;
	}

	int offset = 0;
	for(i--; i>=0; i--) {
		if (strcmp(reverse[i]->id->name, name) == 0) {
			if (offset > 0) {
				print_command("push_const %d", offset);
				print_command("add");
			}
			return reverse[i]->decl;
		}
		offset += reverse[i]->decl->size;
	}
	print_error("struct not have same name field");
	return NULL;
}

void autofetch(struct decl* x) {
	if (is_var(x)) {
		if (x->size == 1) {
			print_command("fetch");
		} else {
			print_command("shift_sp %d", x->size-1);
			print_command("push_reg sp"); 
			print_command("push_const %d", x->size-1); 
			print_command("sub"); 
			print_command("fetch");
			for(int i=0; i<x->size; i++) {
				print_command("push_reg sp");
				print_command("push_const %d", x->size-i); 
				print_command("sub");
				print_command("push_reg sp");
				print_command("push_const -1"); 
				print_command("add"); 
				print_command("fetch");
				print_command("fetch");
				print_command("assign");
				print_command("push_const 1");
				print_command("add");
			}
			print_command("shift_sp -1");
		}
	}
}

void autoassign(struct decl* x) {
	autofetch(x);
	int size = x->size;
	if (size == 1) {
		print_command("assign");
	} else {
		for(int i=0; i<size; i++) {
			print_command("push_reg sp"); print_command("push_const -%d", size); print_command("add"); print_command("fetch");
			if (i > 0) {
				print_command("push_const %d", i);
				print_command("add");
			}
			print_command("push_reg sp"); print_command("push_const -%d", size-i); print_command("add"); print_command("fetch");
			print_command("assign");
		}
		print_command("shift_sp -2");
	}
}

struct loop_scope {
	struct label reent, cond, body, end;
	struct loop_scope* prev;
};
struct loop_scope * loop_scope_top = NULL;

struct loop_scope* push_loop_scope() {
	struct loop_scope* new_scope = calloc(1, sizeof(struct loop_scope));
	new_scope->prev = loop_scope_top;
	loop_scope_top = new_scope;
	return new_scope;
}

struct loop_scope* pop_loop_scope() {
	struct loop_scope* old_scope = loop_scope_top;
	loop_scope_top = loop_scope_top->prev;
	return old_scope;
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
	void*   bag;
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
%token LOGICAL_OR LOGICAL_AND INCOP DECOP 
%token <integer> INTEGER_CONST
%token <string> STRING RELOP EQUOP
%token <ch> CHAR_CONST
%nonassoc <string>  ID
%type <integer> const_expr const_or_expr const_or_list const_and_expr const_and_list const_binary const_unary
%type <decl> type_specifier struct_specifier func_decl pointers param_list param_decl ext_def def stmt expr expr_e or_expr or_list and_list and_expr binary unary args

%%
program
	: {
		struct label exit_label = mkLabel2("EXIT", NULL);
		print_command("push_const %s", formatLabel(exit_label));
        print_command("push_reg fp");
        print_command("push_reg sp");
        print_command("pop_reg fp");
        print_command("jump main");
		print_jump_label(exit_label);
        print_command("exit");
	} ext_def_list {
		struct label lglob = mkLabel2("Lglob", NULL);
		assert(scope_stack_top->location == LOC_GLOBAL);
		print_data_label(lglob, " data %d", scope_stack_top->size);
	}
	;

ext_def_list
	: ext_def_list ext_def
	| /* empty */
	;

ext_def
	: type_specifier pointers ID ';' {  
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkVar(target), true); 
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']' ';' { 
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), $5)), true);
		} else {
			$$ = NULL;
		}
	}
	| func_decl ';'
	| type_specifier ';'
	| func_decl '{' {
			print_jump_label(mkLabel2($1->funcname, NULL));
			scope_push(LOC_LOCAL);
			decl_scope("*return", $1->returntype, false);
			func_scope = $1;
			struct ste* ste = $1->formals;
			struct ste* reverse[100] = {NULL};
			int i=0;
			for(; ste; ste = ste->prev) {
				reverse[i++] = ste;
			}
			for(i--; i>=0; i--) {
				decl_scope(reverse[i]->id->name, reverse[i]->decl, true);
			}
		} 
		local_defs {
			int frame_size = scope_stack_top->size;
			int params_size = 0;
			struct ste* ste = $1->formals;
			for(; ste; ste = ste->prev) {
				params_size += ste->decl->size;
			}
			int locals_size = frame_size - params_size;
			if (locals_size > 0) {
				print_command("shift_sp %d", locals_size);
			}
			print_jump_label(mkLabel2($1->funcname, "start"));
		}
		stmt_list {
			print_jump_label(mkLabel2($1->funcname, "final"));
			print_command("push_reg fp");
			print_command("pop_reg sp");
			print_command("pop_reg fp");
			print_command("pop_reg pc");
		}
		'}' {
			scope_pop(); 
			print_jump_label(mkLabel2($1->funcname, "end"));
		}
	;

type_specifier
	: TYPE { $$ = decl_type_lookup($1); }
	| VOID { $$ = decl_type_lookup($1); }
	| struct_specifier
	;

struct_specifier
	: STRUCT ID { 
		check_decl_type($2);
		scope_push(LOC_RELATIVE); 
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
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID, 0);
		struct decl* func = mkFunc(returntype, NULL, $3);
		decl_scope($3, func, false);
		$$ = func;
	}
	| type_specifier pointers ID '(' _push_scope VOID ')' {
		scope_pop();
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID, 0);
		struct decl* func = mkFunc(returntype, NULL, $3);
		decl_scope($3, func, false);
		$$ = func;
	}
	| type_specifier pointers ID '(' _push_scope param_list ')' {
		struct ste* fields = scope_pop();
		struct decl* returntype = ($2 != NULL) ? $2 : mkTypeDecl(T_INVALID, 0);
		struct decl* func = mkFunc(returntype, fields, $3);
		decl_scope($3, func, false);
		$$ = func;
	}
	;
_push_scope: { scope_push(LOC_LOCAL); }
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
			$$ = decl_scope($3, mkVar(target), false);
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']'  {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), $5)), false);
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
			$$ = decl_scope($3, mkVar(target), true);
		} else {
			$$ = NULL;
		}
	}
	| type_specifier pointers ID '[' const_expr ']' ';' {
		struct decl* target = $2;
		if (target) {
			$$ = decl_scope($3, mkConst(mkArray(mkVar(target), $5)), true);
		} else {
			$$ = NULL;
		}
	}
	| type_specifier ';'
	| func_decl ';'
	;

compound_stmt
	: '{' stmt_list '}'
	;

local_defs  /* local definitions, of which scope is only inside of compound statement */
	:	def_list
	;

stmt_list
	: stmt_list stmt 
	| /* empty */
	;

stmt
	: expr ';' { if ($1 && $1->size > 0) { print_command("shift_sp -%d", $1->size); } $$ = NULL; }
	| compound_stmt { $$ = NULL; }
	| RETURN ';' { 
		struct decl* returntype = decl_lookup("*return");
		if (!isType(returntype, T_VOID)) {
			print_error("return value is not return type");
		}
		$$ = NULL;
	}
	| RETURN {
		print_command("push_reg fp");
		print_command("push_const -%d", func_scope->returntype->size + 1);
		print_command("add");
	 } expr ';' {
		if ($3 != NULL) {
			struct decl* returntype = decl_lookup("*return");
			if (!assignable(returntype, $3)) {
				print_error("return value is not return type");
			}
			autoassign($3);
			print_command("jump %s", formatLabel(mkLabel2(func_scope->funcname, "final")));
		}
		$$ = NULL;
	}
	| ';' { $$ = NULL; }
	| IF if_header_label '(' expr ')' if_jump stmt %prec THEN {
		struct label *labels = $<bag>6;
		$$ = unary_int($4);
		print_jump_label(labels[0]);
		print_jump_label(labels[1]);
	}
	| IF if_header_label '(' expr ')' if_jump stmt ELSE {
		struct label *labels = $<bag>6;
		print_command("jump %s", formatLabel(labels[1]));
		print_jump_label(labels[0]);
	} stmt {
		struct label *labels = $<bag>6;
		$$ = unary_int($4);
		print_jump_label(labels[1]);
	}
	| WHILE { 
		print_jump_label(mkLabel("label"));
		struct loop_scope* scope = push_loop_scope();
		scope->reent = mkLabel("label");
		scope->end = mkLabel("label");
		print_jump_label(scope->reent);
	} '(' expr ')' {
		autofetch($4);
		struct loop_scope* scope = loop_scope_top;
		print_command("branch_false %s", formatLabel(scope->end));
	} stmt {
		$$ = unary_int($4);
		struct loop_scope* scope = pop_loop_scope();
		print_command("jump %s", formatLabel(scope->reent));
		print_jump_label(scope->end);
	}
	| FOR { 
		print_jump_label(mkLabel("label"));
		struct loop_scope* scope = push_loop_scope();
		scope->reent = mkLabel("label");
		scope->cond = mkLabel("label");
		scope->body = mkLabel("label");
		scope->end = mkLabel("label");
	} '(' expr_e ';' {
		struct loop_scope* scope = loop_scope_top;
		print_jump_label(scope->cond);
	} expr_e ';' {
		struct loop_scope* scope = loop_scope_top;
		if ($7) {
			print_command("branch_false %s", formatLabel(scope->end));
		}
		print_command("jump %s", formatLabel(scope->body));
		print_jump_label(scope->reent);
	} expr_e ')' {
		struct loop_scope* scope = loop_scope_top;
		print_command("jump %s", formatLabel(scope->cond));
		print_jump_label(scope->body);
	} stmt {
		$$ = unary_int($7);
		struct loop_scope* scope = pop_loop_scope();
		print_command("jump %s", formatLabel(scope->reent));
		print_jump_label(scope->end);
	}
	| BREAK ';' { 
		$$ = NULL;
		struct loop_scope* scope = loop_scope_top;
		print_command("jump %s", formatLabel(scope->end));
	 }
	| CONTINUE ';' { 
		$$ = NULL;
		struct loop_scope* scope = loop_scope_top;
		print_command("jump %s", formatLabel(scope->reent));
	}
	;

if_header_label: { print_jump_label(mkLabel("label")); };
if_jump: {
	struct label *labels = calloc(3, sizeof(struct label));
	$<bag>$ = labels;
	labels[0] = mkLabel("label");
	labels[1] = mkLabel("label");
	autofetch($<decl>-1);
	print_command("branch_false %s", formatLabel(labels[0]));
};


expr_e
	: expr
	| /* empty */ { $$ = NULL; }
	;

const_expr : const_or_expr ;
const_or_expr: const_or_list;
const_or_list
	: const_or_list LOGICAL_OR const_and_expr { $$ = $1 || $3; }
 	| const_and_expr
	;
const_and_expr: const_and_list;
const_and_list
	: const_and_list LOGICAL_AND const_binary { $$ = $1 && $3; }
	| const_binary
	;

const_binary
	: const_binary RELOP const_binary { 
		if (strcmp($2, "<") == 0) { $$ = $1 < $3; }
		else if (strcmp($2, "<=") == 0) { $$ = $1 <= $3; }
		else if (strcmp($2, ">") == 0) { $$ = $1 > $3; } 
		else if (strcmp($2, ">=") == 0) { $$ = $1 >= $3; }
	 }
	| const_binary EQUOP const_binary { 
		if (strcmp($2, "==") == 0) { $$ = $1 == $3; }
		else if (strcmp($2, "!=") == 0) { $$ = $1 != $3; }
	 }
	| const_binary '+' const_binary { $$ = $1 + $3; }
	| const_binary '-' const_binary { $$ = $1 - $3; }
	| const_unary %prec '=' { $$ = $1; }
	;

const_unary
	: '(' const_expr ')' { $$ = $2; }
	| '(' const_unary ')' { $$ = $2; }
	| INTEGER_CONST
	| '-' const_unary %prec '!' { $$ = -$2; }
	| '!' const_unary { $$ = !$2; }
	;
	
expr
	: unary { print_command("push_reg sp"); print_command("fetch"); } '=' expr {
		if ($1 == NULL || $4 == NULL) {  // error on subexpression.
			$$ = NULL;
		} else if (!is_var($1)) {
			print_error("LHS is not a variable");
			$$ = NULL;
		} else if (assignable($1->type, $4)) {
			$$ = $1;
			autoassign($4);
			autofetch($1);
			// TODO: handle struct assign
		} else if (!is_var_or_const($4)) {
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
	: or_list { autofetch($1); } LOGICAL_OR and_expr { autofetch($4);
		$$ = binary_int($1, $4); 
		print_command("or"); 
	}
	| and_expr
	;

and_expr
	: and_list
	;

and_list
	: and_list { autofetch($1); } LOGICAL_AND binary { autofetch($4); 
		$$ = binary_int($1, $4); 
		print_command("and"); 
	}
	| binary
	;

binary
	: binary { autofetch($1); } RELOP binary { autofetch($4);
		$$ = binary_rel($1, $4);
		if (strcmp($3, "<") == 0) { print_command("less"); }
		else if (strcmp($3, "<=") == 0) { print_command("less_equal"); }
		else if (strcmp($3, ">") == 0) { print_command("greater"); } 
		else if (strcmp($3, ">=") == 0) { print_command("greater_equal"); }
	}
	| binary { autofetch($1); } EQUOP binary { autofetch($4);
		$$ = binary_equal($1, $4);
		if (strcmp($3, "==") == 0) { print_command("equal"); }
		else if (strcmp($3, "!=") == 0) { print_command("not_equal"); }
	}
	| binary { autofetch($1); } '+' binary { autofetch($4); 
		$$ = binary_int($1, $4);
		if (isType(unConstVar($1), T_PTR)) {
			int size = unPointer(unConstVar($1))->size;
			if (size > 1) {
				print_command("push_const %d", size);
				print_command("mul");
			}
		}
		print_command("add");
	}
	| binary { autofetch($1); } '-' binary { autofetch($4); 
		$$ = binary_int($1, $4);
		if (isType(unConstVar($1), T_PTR)) {
			int size = unPointer(unConstVar($1))->size;
			if (size > 1) {
				print_command("push_const %d", size);
				print_command("mul");
			}
		}
		print_command("sub");
	}
	| unary %prec '='
	;

unary
	: '(' expr ')' { $$ = $2; }
	| '(' unary ')' { $$ = $2; }
	| INTEGER_CONST { 
		$$ = mkConst(decl_type_lookup("int"));
		print_command("push_const %d", $1); 
	}
	| CHAR_CONST { 
		$$ = mkConst(decl_type_lookup("char"));
		print_command("push_const %d", $1);
	}
	| STRING { 
		struct label l = mkLabel("str");
		print_data_label(l, "string %s", $1);
		print_command("push_const %s", formatLabel(l));
		$$ = mkConst(mkPointer(mkVar(decl_type_lookup("char"))));
		$$->constant = mkConstantLabel(l, 0);
	}
	| ID {
		struct decl* decl = decl_lookup($1);
		$$ = decl;
		if (decl == NULL) {
			print_error("not declared");
		} else if (decl->declclass == D_FUNC) {
			// do notihng
		} else if (is_var_or_const(decl)) {
			if (decl->alloc.location == LOC_LOCAL) {
				print_command("push_reg fp");
				print_command("push_const %d", decl->alloc.offset + 1);
				print_command("add");
			} else if (decl->alloc.location == LOC_GLOBAL) {
				print_command("push_const Lglob+%d", decl->alloc.offset);
			} else {
				assert(0);
			}
		} else {
			assert(0);
		}
	}
	| '-' unary %prec '!' {
		if ($2 == NULL) {
			$$ = NULL;
		} else if (!assignable(decl_type_lookup("int"), $2)) {
			print_error("not int type");
			$$ = NULL;
		} else {
			$$ = $2;
			autofetch($$);
			print_command("negate");
		}
	}
	| '!' unary { $$ = unary_int($2); autofetch($$); print_command("not"); }
	| unary INCOP { $$ = unary_indec_post($1);
		int size = 1;
		if (isType(unVar($1), T_PTR)) {
			size = unPointer($1)->size;
		}
		// let ref a = $1, stack: a
		print_command("push_reg sp"); print_command("fetch"); // lvalue ref copy
		// stack: a a
		print_command("push_reg sp"); print_command("fetch"); print_command("fetch");
		// stack: a a *a
		print_command("push_const %d", size);
		// stack: a a *a sz
		print_command("add");
		// stack: a a (*a+sz)
		print_command("assign");
		// stack: a
		print_command("fetch");
		// stack: *a
		print_command("push_const %d", size);
		// stack: *a sz
		print_command("sub");
		// stack: *a
	}
	| unary DECOP { $$ = unary_indec_post($1); 
		int size = 1;
		if (isType(unVar($1), T_PTR)) {
			size = unPointer($1)->size;
		}
		// let ref a = $1, stack: a
		print_command("push_reg sp"); print_command("fetch"); // lvalue ref copy
		// stack: a a
		print_command("push_reg sp"); print_command("fetch"); print_command("fetch");
		// stack: a a *a
		print_command("push_const %d", size);
		// stack: a a *a sz
		print_command("sub");
		// stack: a a (*a+sz)
		print_command("assign");
		// stack: a
		print_command("fetch");
		// stack: *a
		print_command("push_const %d", size);
		// stack: *a sz
		print_command("add");
		// stack: *a
	}
	| INCOP unary { $$ = unary_indec_pre($2); 
		// let ref a = $1, stack: a
		print_command("push_reg sp"); print_command("fetch");
		// stack a a
		print_command("push_reg sp"); print_command("fetch"); print_command("fetch");
		// stack a a *a
		if (isType(unVar($2), T_PTR)) {
			print_command("push_const %d", unPointer($2)->size);
		} else {
			print_command("push_const 1");
		}
		// stack: a a *a sz
		print_command("add");
		// stack: a a (*a+sz)
		print_command("assign");
		// stack: a
	}
	| DECOP unary { $$ = unary_indec_pre($2); 
		// let ref a = $1, stack: a
		print_command("push_reg sp"); print_command("fetch");
		// stack a a
		print_command("push_reg sp"); print_command("fetch"); print_command("fetch");
		// stack a a *a
		if (isType(unVar($2), T_PTR)) {
			print_command("push_const %d", unPointer($2)->size);
		} else {
			print_command("push_const 1");
		}
		// stack: a a *a sz
		print_command("sub");
		// stack: a a (*a+sz)
		print_command("assign");
		// stack: a
	}
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
		autofetch($$);
	}
	| unary '[' expr ']' {
		struct decl* elem = unArray(unConstVar($1));
		if (elem && unary_int($3)) {
			$$ = elem;
			autofetch($3);
			if (elem->size > 1) {
				print_command("push_const %d", elem->size);
				print_command("mul");
			}
			print_command("add");
		} else {
			$$ = NULL;
		}
	}
	| unary '.' ID {
		$$ = struct_member($1, $3);
	}
	| unary STRUCTOP ID {
		struct decl* deref = unPointer(unConstVar($1));
		$$ = struct_member(deref, $3);
		/* NOT NECESSARY */
	}
	| unary fcall_prepare '(' args ')' {
		if ($1 == NULL) {
			$$ = NULL;
		} else {
			if ($1->declclass != D_FUNC) {
				print_error("not a function");
				$$ = NULL;
			} else {
				enum result_t { SO_FAR_SO_GOOD, INVALID, SILENT } result = SO_FAR_SO_GOOD; 
				struct ste* formal = $1->formals;
				struct decl* arg_list = $4;
				int argsize = 0;
				while(formal && arg_list) {
					assert(is_var_or_const(formal->decl));
					assert(arg_list->declclass == D_CONST); /* assert dummy wrapper const */
					struct decl* arg = unConstVar(arg_list); /* peel dummy wrapper const */
					if (arg == NULL) {
						result = SILENT;
						break;
					}
					if (!assignable(unConstVar(formal->decl), arg)) {
						result = INVALID;
						break;
					}
					argsize += formal->decl->size;
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
				switch ($1->funcclass) {
				case F_READ_CHAR: print_command("read_char"); break;
				case F_READ_INT: print_command("read_int"); break;
				case F_WRITE_CHAR: print_command("write_char"); break;
				case F_WRITE_INT: print_command("write_int"); break;
				case F_WRITE_STRING: print_command("write_string"); break;
				case F_REGULAR: {
					print_command("push_reg sp");
					print_command("push_const -%d", argsize);
					print_command("add");
					print_command("pop_reg fp");
					struct label cont = getLastLabel("label");
					print_command("jump %s", $1->funcname);
					print_jump_label(cont);
					break;
				}
				}
			}
		}
	}
	| unary fcall_prepare '(' ')' {
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
			switch ($1->funcclass) {
			case F_READ_CHAR: print_command("read_char"); break;
			case F_READ_INT: print_command("read_int"); break;
			case F_WRITE_CHAR: print_command("write_char"); break;
			case F_WRITE_INT: print_command("write_int"); break;
			case F_WRITE_STRING: print_command("write_string"); break;
			case F_REGULAR: {
				print_command("push_reg sp");
				print_command("pop_reg fp");
				struct label cont = getLastLabel("label");
				print_command("jump %s", $1->funcname);
				print_jump_label(cont);
				break;
			}
			}
		}
	}
	;

fcall_prepare: {
	if ($<decl>0->funcclass == F_REGULAR) {
		struct label cont = mkLabel("label");
		print_command("shift_sp %d", $<decl>0->returntype->size);
		print_command("push_const %s", formatLabel(cont));
		print_command("push_reg fp");
	}
};

args    /* actual parameters(function arguments) transferred to function */
	: expr { $$ = mkConst($1); autofetch($1); } // dummy const wrapper
	| args ',' expr { $$ = mkConst($3); $$->next = $1; autofetch($3); }
    ;
%%

/*  Additional C Codes here */

void yyerror (char* s)
{
	print_error(s);
}
