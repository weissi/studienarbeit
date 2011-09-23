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
#include "utils.h"

#include "generic.pb-c.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>
#include <linux/ppdev.h>
#include <linux/parport.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

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

int open_parport(const char *dev) {
    int fd;
    int mode = IEEE1284_MODE_BYTE;
    int dir = 0x00; //direction = output

    fd = open(dev, O_WRONLY);
    assert_err("open", fd > 0);
    assert_err("PPEXCL", 0 == ioctl (fd, PPEXCL));
    assert_err("PPCLAIM", 0 == ioctl (fd, PPCLAIM));
    assert_err("IEEE1284_MODE_BYTE", 0 == ioctl (fd, PPSETMODE, &mode));
    assert_err("PPDATADIR", 0 == ioctl(fd, PPDATADIR, &dir));

    fprintf(stderr, "PARPORT: OPENED SUCCESSFULLY\n");

    return fd;
}

void parport_write_data(const int fd, unsigned char data) {
    assert_err("PPWDATA", 0 == ioctl(fd, PPWDATA, &data));
    fprintf(stderr, "PARPORT: written 0x%x\n", data);
}

void close_parport(const int fd) {
    assert_err("PPRELEASE", 0 == ioctl(fd, PPRELEASE));
    fprintf(stderr, "PARPORT: CLOSED SUCCESSFULLY\n");
}

/* borrowed from
 * http://en.wikipedia.org/wiki/Time_Stamp_Counter
 */
__inline__ uint64_t rdtsc(void) {
  uint32_t lo, hi;
  __asm__ __volatile__ (      // serialize
  "xorl %%eax,%%eax \n        cpuid"
  ::: "%rax", "%rbx", "%rcx", "%rdx");
  /* We cannot use "=A", since this would use %rax on x86_64 and return only
   * the lower 32bits of the TSC */
  __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
  return (uint64_t)hi << 32 | lo;
}
