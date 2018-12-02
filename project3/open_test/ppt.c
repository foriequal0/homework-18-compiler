int main() {
    {
        /* int a; */
        int boo;
        struct bar {} baf;
        a = 0; /* error */

        /* void foo(); */
        foo(); /* error */
        bar(); /* not a function */
        baf(); /* not a function */
    }
    {
        int a;
        int a;   /* error */
        char a;  /* error */
    }
    {
        int a;		
        {
            int a;
        }
    }
    {
        int* b; int* c;
        int d[5]; int e[5];
        b = NULL;
        c = b;
        b = 0;  /* error */
        0 != NULL; /* error */
        b == c;
        d == e; /* error */
        b == d; /* error */
    }
    {
        int *a[2];
        int *b;
        int c;
        char *d;
        char e;

        a = 0; /* error (int** != int) */
        b = 0; /* error (int* != int) */
        b = NULL; /* legal */
        c = 0; /* legal */
        d = 0; /* error */
        e = 0; /* error */
    }
    {
        int *a[5];
        int *b;
        int c[10];
        struct temp1 { int a; } *s1;
        struct temp1 s2;
        struct temp2 { int b; } *s3;

        a = b; /* error (int**) != (int*) */
        b = c; /* error (int*) != (int array) */
        s1 = s3; /* error (*temp1 != *temp2) */
        s1 = s2; /* error */
        s1 = &s2; /* legal */
    }
    {
        int a[10];
        int *b;
        a = 0;			/* error */
        a[0] = 0;		/* legal */
        b = a;			/* error */
        b = &a;		/* error */
        b = &a[10];		/* legal */
        b = &b;		/* error */
        b = &*(a+5);		/* error */
        b = &(b++);		/* error */
        b = &*(b++);		/* error */
    }
    {
        int a;
        char b;

        a = 10; 
        b = 'a';
        a = -a;  /* legal */
        b = -b;  /* error */
    }
    {
        int a;
        char b;
        int* c; 	
        char d[10];
        struct temp { int a;} e; 

        a++;
        --a;
        b++;
        c++;     /* error */
        --d;     /* error */
        ++e;     /* error */
        e.a++;
        e.a = NULL; /* error */
        e.b; /* ERROR */
        e->a++; /* ERROR */
        e->a = NULL; /* error */
        e->b;  /* ERROR */
        {
            struct temp *f;
            f = &e;
            e = *f;
            f->a++;
            f = NULL;
            f->a = NULL; /* ERROR */
            f->b; /* ERROR */
            f.a++; /* ERROR */
            f.a = NULL; /* error */
            f.b; /* ERROR */
        }
    }
}

struct a {
    struct b x;		/* incomplete type error */
    struct b* p;		/* incomplete type error */
    struct b { } y;
};

struct b {			/* error */
};

int func() {
    struct b { } x;		/* error */
}