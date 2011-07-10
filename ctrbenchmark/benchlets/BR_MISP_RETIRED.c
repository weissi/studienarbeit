#include <sys/types.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#include <benchlet.h>

#define LOOPS_IN_ROW 15000000
#define BHT_SIZE 32

static const char *COUNTERS[] = { "BR_MISP_RETIRED", "BR_MISP_EXEC", NULL };

static void init_mispredict_branches(benchlet_info_t *info) {
    info->name = "mispredict branches";
    info->description = "small loop but bigger than BHT (branch history table)"
                        " --> branch mispreds: ~13.5M/s";
    info->penetrated_counters = COUNTERS;
}

static int generate_mispredicted_branches(benchlet_config_t *cfg) {
    bool take_branch[] = { false, true };
    register int tmp = 0;

    MICRO_BENCH_LOOP(cfg) {
        for (int j = 0; j < LOOPS_IN_ROW; j++) {
            for (int k = 0; k <= BHT_SIZE; k++) {
                tmp++;
            }
        }
    }
    return tmp+take_branch[0];
}

BENCHLET_INIT(init_mispredict_branches)
BENCHLET_CALL(generate_mispredicted_branches)
