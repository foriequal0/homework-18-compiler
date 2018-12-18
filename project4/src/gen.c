#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "gen.h"

struct prefix_counter { const char* prefix; int count; struct prefix_counter* next; };
static struct prefix_counter *prefix_counters_head = NULL;

struct label mkLabel(const char* prefix) {
    struct prefix_counter* it = prefix_counters_head;
    for(; it; it = it->next) {
        if (strcmp(it->prefix, prefix) == 0) {
            it->count++;
            struct label l = {
                .prefix = it->prefix,
                .id = it->count,
            };
            return l;
        }
    }
    it = calloc(1, sizeof(struct prefix_counter));
    it->prefix = strdup(prefix);
    it->count = 0;
    it->next = prefix_counters_head;
    prefix_counters_head = it;

    struct label l = {
        .prefix = it->prefix,
        .id = it->count,
    };
    return l;
}

struct label getLastLabel(const char* prefix) {
    struct prefix_counter* it = prefix_counters_head;
    for(; it; it = it->next) {
        if (strcmp(it->prefix, prefix) == 0) {
            struct label l = {
                .prefix = it->prefix,
                .id = it->count,
            };
            return l;
        }
    }
    assert(0);
}

struct label mkLabel2(const char* prefix, const char* postfix) {
    static char buf[128];
    if (postfix) {
        sprintf(buf, "%s_%s", prefix, postfix);
    } else {
        sprintf(buf, "%s", prefix);
    }
    struct label l = {
        .prefix = strdup(buf),
        .id = -1,
    };
    return l;
}

const char* formatLabel(struct label l) {
    static char buf[128];
    if (l.id == -1) {
        sprintf(buf, "%s", l.prefix);
    } else {
        sprintf(buf, "%s_%d", l.prefix, l.id);
    }
    return &buf[0];
}

struct constant mkConstantIntegral(int i) {
    struct constant c = {
        .type = INTEGRAL,
        .integral = i,
    };
    return c;
}
struct constant mkConstantLabel(struct label label, int offset) {
    struct constant c = {
        .type = LABEL,
        .label = label,
        .offset = offset,
    };
    return c;
}