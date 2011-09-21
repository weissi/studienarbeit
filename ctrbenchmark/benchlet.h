/*
 *  Helps programming benchlets.
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
#ifndef BENCHLET_H
#define BENCHLET_H 1

#include <time.h>

#define REGISTER_BENCHLET(desc, f, ctr) \
    run(NULL);
    //printf("registered benchmark for counter '%s': %s", (ctr), (desc));
    //

typedef struct {
    time_t desired_running_time;
} benchlet_config_t;

typedef struct {
    const char *name;
    const char *description;
    const char **penetrated_counters;
} benchlet_info_t;

typedef void (*benchlet_init_call_t)(benchlet_info_t *cfg);
typedef int (*benchlet_call_t)(benchlet_config_t *cfg);

#define BENCHLET_INIT(fun) \
    static benchlet_init_call_t __bl_##fun##_init_call  __attribute__ ((used)) \
           __attribute__ ((section("__benchlet_init_calls"))) = (fun);

#define BENCHLET_CALL(fun) \
    static benchlet_call_t __bl_##fun##_call  __attribute__ ((used)) \
           __attribute__ ((section("__benchlet_calls"))) = (fun);

#define MICRO_BENCH_LOOP(cfg) \
        register time_t __t_end = time(NULL) + (cfg)->desired_running_time; \
        while (time(NULL) <= __t_end)
#endif
