#include <sys/types.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>

#include <benchlet.h>

#define LOOPS_IN_ROW 550000000

static const char *COUNTERS[] = { "BR_MISP_RETIRED", NULL };

static void init_mispredict_branches(benchlet_info_t *info) {
    info->name = "mispredict branches";
    info->description = "always alternate taken/not taken --> many mispred";
    info->penetrated_counters = COUNTERS;
}

static int generate_mispredicted_branches(benchlet_config_t *cfg) {
    bool take_branch[] = { false, true };
    register int i = 0, j;
    register int tmp = 0;
    int a, b, c;

    MICRO_BENCH_LOOP(cfg) {
        for (j = 0; j < LOOPS_IN_ROW; j++) {
            if ((j%2) == 0) a=1;
            else a=0;
            if ((j%5) == 0) b=1;
            else b=0;
            if (j<0) a=1; //dummy branch
            //...
            if (j<0) a=1; //dummy branch
            if ( (a*b) == 1) c=1;
        }
    }
    return tmp+a+b+c+i+take_branch[0];
}

BENCHLET_INIT(init_mispredict_branches)
BENCHLET_CALL(generate_mispredicted_branches)
