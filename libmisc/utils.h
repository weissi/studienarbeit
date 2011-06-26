#ifndef __UTILS_H
#define __UTILS_H 1

#include <sys/types.h>

#include "generic.pb-c.h"

#define BUG_ON(x) assert(!(x))
#define DIE_NE0(x) BUG_ON((x) != 0)
#define DIE_NE0_ERR(x,desc) \
    { \
        if ((x) != 0) { \
            fprintf(stderr, "UNRECOVERABLE ERROR in %s:%d\n", \
                    __FILE__, __LINE__); \
            perror((desc)); \
            exit(-1); \
        } \
    }
#define DIE_EQNULL_ERR(x,desc) \
    { \
        if ((x) == NULL) { \
            fprintf(stderr, "UNRECOVERABLE ERROR in %s:%d\n", \
                    __FILE__, __LINE__); \
            perror((desc)); \
            exit(-1); \
        } \
    }

size_t strlcpy(char *d, char const *s, size_t n);
size_t strlcat(char *d, char const *s, size_t n);

void timestamp_from_timespec(Timestamp *dest, struct timespec *src);
void timespec_from_timestamp(struct timespec *dest, Timestamp *src);

#endif
