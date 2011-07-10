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

double timediff(struct timespec start, struct timespec end) {
    double d;
    struct timespec temp;
    if ((end.tv_nsec-start.tv_nsec)<0) {
        temp.tv_sec = end.tv_sec-start.tv_sec-1;
        temp.tv_nsec = 1000000000+end.tv_nsec-start.tv_nsec;
    } else {
        temp.tv_sec = end.tv_sec-start.tv_sec;
        temp.tv_nsec = end.tv_nsec-start.tv_nsec;
    }
    d = temp.tv_sec + (temp.tv_nsec / 1000000000.0);
    /*
    printf("%ld.%ld - %ld.%ld = %ld.%ld = %f = %e\n", end.tv_sec, end.tv_nsec,
           start.tv_sec, start.tv_nsec, temp.tv_sec, temp.tv_nsec, d, d);
           */
    return d;
}
