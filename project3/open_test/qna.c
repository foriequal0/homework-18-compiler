int main() {
    {
        int x;
        struct y {} y;
        x+y; /* not int type */
    }
}

void bar(void); /* first declaration */

int main2(){
        bar();
        return 0;
}

void bar(void) { /* redeclaration */
        return;
}
void bar(void); /* redeclaration */
void bar(void); /* redeclaration */

void NULL(); /* unqualified id */

struct foo{};
int foo; /* OK */
int main3(){
   int foo(); /* OK */
}

int a;
void a(int x){ /* redecl*/
    int y;
    char y; /* redecl*/
}

int main33() {
    int a;
    int *b; int c[3];
    a[3]; /* not an array */
    b < c; /* error */
}

int aa(){ return 1; }
struct ss{} bb;
int ss; /* legal */
void main4(){
    int aa; /* legal */
    int bb; /* legal */
    int x[c+1]; /* error */
    int y[aa+1];
    bb = aa(); /* not a function */
}

int func() { return 1; }
void main5() {
    int a;
    a=0;
    func = a[1]; /* not array type */
}

void main6() {
    int a[10];
    &a; /* not variable */
}

void main7() {
    struct fooboo bar; /* incomplete type error */
}

void main8() {
    int *a; int *b;
    int c;
    struct blahblah {} fuck;
    a < b; /* not int or char */
    c < fuck; /* not int or char */
    c == fuck; /* not int or char */
}

void goo1(){ }
void goo2(){ return ; }

void main9() {
    struct aaa{} b;
    struct aaa;
    int a; 
    struct a b; /* incomplete */
    int *c;
    c->x; /* not struct */
}

void main10() {
    int a;
    char a; /* redeclartion error */
    a = 1;
}

void main11() {
    &"string"; /* not variable */
}

void main12() {
    int NULL; /* unqualified id */
    int NULL(int foo); /* unqualified id */
    struct baz {} NULL; /* unqualified id */
    struct NULL qux; /* syantaxerror? */
    int bar(int boo);
    bar(NULL); /* arg */
    NULL(3, 6); /* not a function */
}

int aaa;
void main13() {
   if ( aaa==1) return 1;

   else return 0;
}

void main14() {
    int *ip; char *cp;
    ip = NULL; 
    cp = NULL;  /* legal */
}

void main15() {
    char *c;
    c = "abcd"; /* legal */
}

int func2() {
    return 0;
}

void func2() { /* redecl */
    return 'a'; /* return type */
}