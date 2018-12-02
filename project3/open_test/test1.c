int main() {
    int a;
    char a; // redeclaration error
    int b;
    int b; // redeclaration error
    int c;
    char d;

    return 0;
}
/*
test1.c:3: error: declaration error
test1.c:5: error: declaration error
*/