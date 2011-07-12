/*
 * based on libpfm-4.1.0's perf_examples/syst_count.c
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

#include <utils.h>

#include "perf_util.h"
#include "perf-counters.pb-c.h"

#define MAX_GROUPS    16
#define MAX_PATH    1024

#define MAX_CPUS 16
#define MAX_EVENTS 64

#ifndef STR
# define _STR(x) #x
# define STR(x) _STR(x)
#endif

typedef struct {
    char *shot_id;
    const char *dump_fname;
    char *events[MAX_GROUPS];
    int nevents[MAX_GROUPS]; /* #events per group */
    int num_groups;
    int delay;
    int excl;
    int pin;
    int cpu;
    int parport_fd;
    bool no_parport;
    char *cgroup_name;
    char *run_cmd;
} options_t;

static volatile options_t options;
static perf_event_desc_t **all_fds;

static void sig_hnd( void ) {
    if (-1 != options.parport_fd) {
        parport_write_data(options.parport_fd, 0x00);
        close_parport(options.parport_fd);
        options.parport_fd = -1;
        exit(1);
    }
}

static int
cgroupfs_find_mountpoint(char *buf, size_t maxlen)
{
    FILE *fp;
    char mountpoint[MAX_PATH+1], tokens[MAX_PATH+1], type[MAX_PATH+1];
    char *token, *saved_ptr = NULL;
    int found = 0;

    fp = fopen("/proc/mounts", "r");
    if (!fp)
        return -1;

    /*
     * in order to handle split hierarchy, we need to scan /proc/mounts
     * and inspect every cgroupfs mount point to find one that has
     * perf_event subsystem
     */
    while (fscanf(fp, "%*s %"STR(MAX_PATH)"s %"STR(MAX_PATH)"s %"
                STR(MAX_PATH)"s %*d %*d\n",
                mountpoint, type, tokens) == 3) {

        if (!strcmp(type, "cgroup")) {

            token = strtok_r(tokens, ",", &saved_ptr);

            while (token != NULL) {
                if (!strcmp(token, "perf_event")) {
                    found = 1;
                    break;
                }
                token = strtok_r(NULL, ",", &saved_ptr);
            }
        }
        if (found)
            break;
    }
    fclose(fp);
    if (!found)
        return -1;

    if (strlen(mountpoint) < maxlen) {
        strcpy(buf, mountpoint);
        return 0;
    }
    return -1;
}

int
open_cgroup(char *name)
{
        char path[MAX_PATH+1];
        char mnt[MAX_PATH+1];
        int cfd;

        if (cgroupfs_find_mountpoint(mnt, MAX_PATH+1))
                errx(1, "cannot find cgroup fs mount point");

        snprintf(path, MAX_PATH, "%s/%s", mnt, name);

        cfd = open(path, O_RDONLY);
        if (cfd == -1)
                warn("no access to cgroup %s\n", name);

        return cfd;
}

void
setup_cpu(int cpu, int cfd)
{
    perf_event_desc_t *fds = NULL;
    int old_total, total = 0, num;
    int i, j, n, ret, is_lead, group_fd;
    unsigned long flags;
    pid_t pid;

    for(i=0, j=0; i < options.num_groups; i++) {
        old_total = total;
        ret = perf_setup_list_events(options.events[i], &fds, &total);
        if (ret)
            errx(1, "cannot setup events\n");

        all_fds[cpu] = fds;

        num = total - old_total;

        options.nevents[i] = num;

        for(n=0; n < num; n++, j++) {

            is_lead = perf_is_group_leader(fds, j);
            if (is_lead) {
                fds[j].hw.disabled = 1;
                group_fd = -1;
            } else {
                fds[j].hw.disabled = 0;
                group_fd = fds[fds[j].group_leader].fd;
            }
            fds[j].hw.size = sizeof(struct perf_event_attr);

            if (options.cgroup_name) {
                flags = PERF_FLAG_PID_CGROUP;
                pid = cfd;
                //fds[j].hw.cgroup = 1;
                //fds[j].hw.cgroup_fd = cfd;
            } else {
                flags = 0;
                pid = -1;
            }

            if (options.pin && is_lead)
                fds[j].hw.pinned = 1;

            if (options.excl && is_lead)
                fds[j].hw.exclusive = 1;

            /* request timing information necessary for scaling counts */
            fds[j].hw.read_format = PERF_FORMAT_SCALE;
            fds[j].fd = perf_event_open(&fds[j].hw, pid, cpu, group_fd, flags);
            if (fds[j].fd == -1) {
                if (errno == EACCES)
                    err(1, "you need to be root to run system-wide on this machine");

                errx(2, "cannot attach event %s to CPU%ds, skipping it", fds[j].name, cpu);
                goto error;
            }
        }
    }
    return;
error:
    for (i=0; i < j; i++) {
        close(fds[i].fd);
        fds[i].fd = -1;
    }
}

void start_cpu(int c)
{
    perf_event_desc_t *fds = NULL;
    int j, ret, n = 0;

    fds = all_fds[c];

    if (fds[0].fd == -1)
        return;

    for(j=0; j < options.num_groups; j++) {
        /* group leader always first in each group */
        ret = ioctl(fds[n].fd, PERF_EVENT_IOC_ENABLE, 0);
        if (ret)
            err(1, "cannot enable event %s\n", fds[j].name);
        n += options.nevents[j];
    }
}

void stop_cpu(int c)
{
    perf_event_desc_t *fds = NULL;
    int j, ret, n = 0;

    fds = all_fds[c];

    if (fds[0].fd == -1)
        return;

    for(j=0; j < options.num_groups; j++) {
        /* group leader always first in each group */
        ret = ioctl(fds[n].fd, PERF_EVENT_IOC_DISABLE, 0);
        if (ret)
            err(1, "cannot disable event %s\n", fds[j].name);
        n += options.nevents[j];
    }
}

void read_cpu(int c, uint64_t event_values[MAX_EVENTS][MAX_CPUS])
{
    perf_event_desc_t *fds;
    uint64_t values[3];
    double ratio;
    int i, j, n, ret;

    fds = all_fds[c];

    if (fds[0].fd == -1) {
        fprintf(stderr, "CPU%d not monitored\n", c);
        return;
    }

    assert(options.num_groups == 1);
    for(i=0, j = 0; i < options.num_groups; i++) {
        for(n = 0; n < options.nevents[i]; n++, j++) {
            memset(values, 0, sizeof(values));
            ret = read(fds[j].fd, values, sizeof(values));
            if (ret != sizeof(values)) {
                if (ret == -1)
                    err(1, "cannot read event %s : %d", fds[j].name, ret);
                else {
                    warnx("CPU%d G%-2d could not read event %s, read=%d", c, i, fds[j].name, ret);
                    continue;
                }
            }
            /*
             * scaling because we may be sharing the PMU and
             * thus may be multiplexed
             */
            fds[j].value = perf_scale(values);
            ratio = perf_scale_ratio(values);

            fprintf(stderr, "CPU%-3d G%-2d %-20"PRIu64" %s (scaling %.2f%%, ena=%"PRIu64", run=%"PRIu64") %s\n",
                c,
                i,
                fds[j].value,
                fds[j].name,
                (1.0-ratio)*100,
                values[1],
                values[2],
                options.cgroup_name ? options.cgroup_name : "");
            event_values[j][c] = fds[j].value;
    if (values[2] > values[1])
        errx(1, "WARNING: time_running > time_enabled %"PRIu64"\n", values[2] - values[1]);
        }
    }
}

void close_cpu(int c)
{
    perf_event_desc_t *fds = NULL;
    int i, j;

    fds = all_fds[c];

    if (fds[0].fd == -1)
        return;

    for(i=0; i < options.num_groups; i++) {
        for(j=0; j < options.nevents[i]; j++)
            close(fds[j].fd);
    }

    free(fds);
}

static void
write_dump_data(struct timespec *start, struct timespec *stop,
                unsigned int ncpus, unsigned int ncounters,
                uint64_t event_values[MAX_EVENTS][MAX_CPUS])
{
    CounterData msg_cd = COUNTER_DATA__INIT;
    CounterValue **msg_cvs;
    Timestamp msg_ts_start;
    Timestamp msg_ts_stop;
    size_t len;
    void *buf = 0;
    int fd;

    assert(0 != (msg_cvs = alloca(sizeof(CounterValue *) * ncounters)));

    if (0 == strcmp("-", options.dump_fname)) {
        fd = 1;
    } else {
        fd = open(options.dump_fname, O_WRONLY | O_CREAT,
                  S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH );
    }
    assert(fd > 0);

    msg_cd.shot_id = options.shot_id;
    msg_cd.start_time = &msg_ts_start;
    timestamp_from_timespec(msg_cd.start_time, start);
    msg_cd.stop_time = &msg_ts_stop;
    timestamp_from_timespec(msg_cd.stop_time, stop);
    msg_cd.cpu_count = ncpus;

    for (int i = 0; i < ncounters; i++) {
        uint64_t global_counter_val = 0;

        msg_cvs[i] = alloca(sizeof(CounterValue));
        counter_value__init(msg_cvs[i]);
        msg_cvs[i]->n_counter_value_per_cpu = ncpus;
        msg_cvs[i]->counter_value_per_cpu = event_values[i];
        for (int j = 0; j < ncpus; j++) {
            global_counter_val += event_values[i][j];
        }
        msg_cvs[i]->has_global_counter_value = true;
        msg_cvs[i]->global_counter_value = global_counter_val;
        msg_cvs[i]->counter_name = (char*)all_fds[0][i].name; //options.events[i];
    }

    msg_cd.n_counters = ncounters;
    msg_cd.counters = msg_cvs;
    len = counter_data__get_packed_size(&msg_cd);
    buf = malloc(len);
    assert(NULL != buf);
    counter_data__pack(&msg_cd, buf);

    assert(len == write(fd, buf, len));

    free(buf);
}

void
measure(void)
{
    int c, cmin, cmax, ncpus;
    int cfd = -1;
    uint64_t event_values[MAX_EVENTS][MAX_CPUS] = {{ 0 }};
    struct timespec start_t, stop_t;

    cmin = 0;
    cmax = (int)sysconf(_SC_NPROCESSORS_ONLN);
    ncpus = cmax;

    assert(ncpus <= MAX_CPUS);
    assert(options.nevents[0] <= MAX_EVENTS);

    if (options.cpu != -1) {
        cmin = options.cpu;
        cmax = cmin + 1;
    }

    all_fds = malloc(ncpus * sizeof(perf_event_desc_t *));
    if (!all_fds)
        err(1, "cannot allocate memory for all_fds");

    if (options.cgroup_name) {
        cfd = open_cgroup(options.cgroup_name);
        if (cfd == -1)
            exit(1);
    }

    if (!options.no_parport) {
        parport_write_data(options.parport_fd, 0xFF);
    }
    for(c=cmin ; c < cmax; c++)
        setup_cpu(c, cfd);

    fprintf(stderr, "<press CTRL-C to quit before %ds time limit>\n", options.delay);
    /*
     * FIX this for hotplug CPU
     */

    /* START CPUs */
    assert(0 == clock_gettime(CLOCK_REALTIME, &start_t));
    for(c=cmin ; c < cmax; c++) {
        start_cpu(c);
    }

    /* WAIT OR EXECUTE CMD */
    if (NULL != options.run_cmd) {
        assert(0 <= system(options.run_cmd));
    } else {
        sleep(options.delay);
    }

    /* STOP CPUs */
    for(c=cmin ; c < cmax; c++) {
        stop_cpu(c);
    }
    assert(0 == clock_gettime(CLOCK_REALTIME, &stop_t));

    for(c = cmin; c < cmax; c++) {
        fprintf(stderr, "# -----\n");
        read_cpu(c, event_values);
    }

    if (!options.no_parport) {
        parport_write_data(options.parport_fd, 0x00);
    }
    write_dump_data(&start_t, &stop_t, ncpus, options.nevents[0], event_values);

    for (int i = 0; i < options.nevents[0]; i++) {
        fprintf(stderr, "EVENT %s: ", all_fds[0][i].name);
        for (int j = 0; j < ncpus; j++) {
            fprintf(stderr, "%"PRIu64"\t", event_values[i][j]);
        }
        fprintf(stderr, "\n");
    }

    for(c = cmin; c < cmax; c++)
        close_cpu(c);

    free(all_fds);
}

static void
usage(void)
{
    fprintf(stderr, "usage: dumpcounters [-c cpu] [-x] [-h] [-p] [-d delay] [-P] [-G cgroup name] [-e event1,event2,...] -s shot-id -o output-file\n");
}

int
main(int argc, char **argv)
{
    int c, ret;

    setlocale(LC_ALL, "");

    options.cpu = -1;
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
                options.excl = 1;
                break;
            case 'e':
                if (options.num_groups < MAX_GROUPS) {
                    options.events[options.num_groups++] = optarg;
                } else {
                    errx(1, "you cannot specify more than %d groups.\n",
                        MAX_GROUPS);
                }
                break;
            case 'c':
                options.cpu = atoi(optarg);
                break;
            case 'd':
                options.delay = atoi(optarg);
                break;
            case 'P':
                options.pin = 1;
                break;
            case 'h':
                usage();
                exit(0);
            case 'G':
                options.cgroup_name = optarg;
                break;
            case 'n':
                options.no_parport = true;
                break;
            default:
                errx(1, "unknown error");
        }
    }
    if (!options.delay)
        options.delay = 20;

    if (!options.events[0]) {
        options.events[0] = "PERF_COUNT_HW_CPU_CYCLES,PERF_COUNT_HW_INSTRUCTIONS";
        options.num_groups = 1;
    }

    if(!options.shot_id) {
        errx(1, "please pass a 'shot id' with -s SHOT-ID");
    }

    if(!options.dump_fname) {
        options.dump_fname = "-";
    }

    ret = pfm_initialize();
    if (ret != PFM_SUCCESS) {
        errx(1, "libpfm initialization failed: %s\n", pfm_strerror(ret));
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

    /* free libpfm resources cleanly */
    pfm_terminate();

    return 0;
}
