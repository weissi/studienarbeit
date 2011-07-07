#include <sys/types.h>
#include <time.h>

#include <benchlet.h>

#define ROUNDS_TO_UNDERFLOW 10000

static const char *COUNTERS[] = { "FP_ASSIST", NULL };

static void init_fp_assist(benchlet_info_t *info) {
    info->name = "FP_ASSIST";
    info->description = "double underflow --> FP_ASSIST (~1M/s)";
    info->penetrated_counters = COUNTERS;
}

static int penetrate_fp_assist(benchlet_config_t *cfg) {
    register double d = 1.0;
    register unsigned long int i;
    int tmp;

    MICRO_BENCH_LOOP(cfg) {
        for(i = 0; i < ROUNDS_TO_UNDERFLOW; i++) {
            d /= 2.0;
        }
        tmp += (int)d;
        d = 1.0;
    }
    return tmp;
}

BENCHLET_INIT(init_fp_assist)
BENCHLET_CALL(penetrate_fp_assist)
