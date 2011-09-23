/*
 *  Misc utility functions.
 *  Copyright (C)2011, Johannes Weiß <weiss@tux4u.de>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef __UTILS_H
#define __UTILS_H 1

#include <sys/types.h>
#include <stdint.h>

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
#define assert_err(op, p) \
    { \
        if (true != (p)) { \
            fprintf(stderr, "UNRECOVERABLE ERROR in %s:%d\n", \
                    __FILE__, __LINE__); \
            perror((op)); \
            exit(-1); \
        } \
    }

size_t strlcpy(char *d, char const *s, size_t n);
size_t strlcat(char *d, char const *s, size_t n);

void timestamp_from_timespec(Timestamp *dest, struct timespec *src);
void timespec_from_timestamp(struct timespec *dest, Timestamp *src);

double timediff(struct timespec start, struct timespec end);

int open_parport(const char *dev);
void parport_write_data(const int fd, unsigned char data);
void close_parport(const int fd);

uint64_t rdtsc();

#endif
