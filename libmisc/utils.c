#include "utils.h"

#include "generic.pb-c.h"

#include <stdio.h>

size_t strlcpy(char *d, char const *s, size_t n) {
        return snprintf(d, n, "%s", s);
}

size_t strlcat(char *d, char const *s, size_t n) {
        return snprintf(d, n, "%s%s", d, s);
}

void timestamp_from_timespec(Timestamp *dest, struct timespec *src) {
    timestamp__init(dest);
    if (NULL != src) {
        dest->sec = src->tv_sec;
        dest->nsec = src->tv_nsec;
    }
}

void timespec_from_timestamp(struct timespec *dest, Timestamp *src) {
    if (NULL != src) {
        dest->tv_sec = src->sec;
        dest->tv_nsec = src->nsec;
    }
}
