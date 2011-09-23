/*
 * This is a DUMMY VERSION of dumpcounters
 * Copyright (c) 2011, Johannes Weiß <weiss@tux4u.de>
 *
 * dumpcounters is based on libpfm-4.1.0's perf_examples/syst_count.c
 * Copyright (c) 2011, Johannes Weiß <weiss@tux4u.de>
 *
 * --- SNIP (syst_count.c header) ---
 * syst.c - example of a simple system wide monitoring program
 *
 * Copyright (c) 2010 Google, Inc Contributed by Stephane Eranian
 * <eranian@google.com>
 * --- SNAP ---
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <sys/types.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <err.h>
#include <locale.h>
#include <sys/ioctl.h>
#include <time.h>
#include <assert.h>
#include <stdbool.h>
#include <signal.h>
#include <sys/stat.h>

#include <utils.h>
#include "perf-counters.pb-c.h"

#ifndef STR
# define _STR(x) #x
# define STR(x) _STR(x)
#endif

typedef struct {
    char *shot_id;
    const char *dump_fname;
    int parport_fd;
    bool no_parport;
    char *run_cmd;
} options_t;

static volatile options_t options;

static void sig_hnd( void ) {
    if (-1 != options.parport_fd) {
        parport_write_data(options.parport_fd, 0x00);
        close_parport(options.parport_fd);
        options.parport_fd = -1;
        exit(1);
    }
}

static void
write_dump_data(struct timespec *start, struct timespec *stop,
                unsigned int ncpus)
{
    CounterData msg_cd = COUNTER_DATA__INIT;
    Timestamp msg_ts_start;
    Timestamp msg_ts_stop;
    size_t len;
    void *buf = 0;
    int fd;

    if (0 == strcmp("-", options.dump_fname)) {
        fd = 1;
    } else {
        fd = open(options.dump_fname, O_WRONLY | O_CREAT | O_TRUNC,
                  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH );
    }
    assert(fd > 0);

    msg_cd.shot_id = options.shot_id;
    msg_cd.start_time = &msg_ts_start;
    timestamp_from_timespec(msg_cd.start_time, start);
    msg_cd.stop_time = &msg_ts_stop;
    timestamp_from_timespec(msg_cd.stop_time, stop);
    msg_cd.cpu_count = ncpus;
    msg_cd.benchmark_cmd = options.run_cmd;

    msg_cd.n_counters = 0;
    msg_cd.counters = NULL;
    len = counter_data__get_packed_size(&msg_cd);
    buf = malloc(len);
    assert(NULL != buf);
    counter_data__pack(&msg_cd, buf);

    assert(len == write(fd, buf, len));
    if (1 != fd) {
        assert(0 == fsync(fd));
        assert(0 == close(fd));
    }

    free(buf);
}

void
measure(void)
{
    int cmax, ncpus;
    struct timespec start_t, stop_t;

    cmax = (int)sysconf(_SC_NPROCESSORS_ONLN);
    ncpus = cmax;

    fprintf(stderr, "<press CTRL-C to quit>\n");

    /* START CPUs */
    if (!options.no_parport) {
        parport_write_data(options.parport_fd, 0xFF);
    }
    assert(0 == clock_gettime(CLOCK_REALTIME, &start_t));

    /* WAIT OR EXECUTE CMD */
    assert (NULL != options.run_cmd);
    assert (0 <= system(options.run_cmd));

    assert(0 == clock_gettime(CLOCK_REALTIME, &stop_t));
    if (!options.no_parport) {
        parport_write_data(options.parport_fd, 0x00);
    }

    write_dump_data(&start_t, &stop_t, ncpus);
}

static void
usage(void)
{
    fprintf(stderr, "usage: dummy_dumpcounters [-c cpu] [-x] [-h] [-p] [-d delay] [-P] [-G cgroup name] [-e event1,event2,...] -s shot-id -o output-file\n");
}

int
main(int argc, char **argv)
{
    int c;

    setlocale(LC_ALL, "");

    options.parport_fd = -1;
    options.no_parport = false;
    options.run_cmd = NULL;

    signal( SIGINT, (void (*)(int))sig_hnd );

    while ((c=getopt(argc, argv,"hc:e:d:xPG:s:o:r:n")) != -1) {
        switch(c) {
            case 'r':
                options.run_cmd = optarg;
                break;
            case 's':
                options.shot_id = optarg;
                break;
            case 'o':
                options.dump_fname = optarg;
                break;
            case 'x':
                break;
            case 'e':
                break;
            case 'c':
                break;
            case 'd':
                break;
            case 'P':
                break;
            case 'h':
                usage();
                exit(0);
            case 'G':
                break;
            case 'n':
                options.no_parport = true;
                break;
            default:
                errx(1, "unknown error");
        }
    }

    if(!options.shot_id) {
        errx(1, "please pass a 'shot id' with -s SHOT-ID");
    }

    if(!options.dump_fname) {
        options.dump_fname = "-";
    }

    if (!options.no_parport) {
        options.parport_fd = open_parport("/dev/parport0");
        parport_write_data(options.parport_fd, 0x00);
    }
    measure();
    if (!options.no_parport) {
        close_parport(options.parport_fd);
        options.parport_fd = -1;
    }

    return 0;
}
