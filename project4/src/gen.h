#ifndef __GEN_H__
#define __GEN_H__
struct label {
    const char* prefix;
    int id;
};

enum const_type {
    INTEGRAL, LABEL,
};

struct constant {
    enum const_type type;
    int integral;
    struct label label;
    int offset;
};

struct label mkLabel(const char* prefix);
struct label getLastLabel(const char* prefix);
struct label mkLabel2(const char* prefix, const char* postfix);
const char* formatLabel(struct label l);

struct constant mkConstantIntegral(int i);
struct constant mkConstantLabel(struct label label, int offset);

enum location {
    LOC_GLOBAL, LOC_LOCAL, LOC_RELATIVE
};

struct alloc {
    enum location location;
    int offset;
};

#endif