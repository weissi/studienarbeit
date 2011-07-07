#include <stdio.h>
#include <assert.h>
#include <err.h>
#include <stdlib.h>
#include <string.h>

#include <benchlet.h>

extern benchlet_call_t __start___benchlet_calls[];
extern benchlet_call_t __stop___benchlet_calls[];

extern benchlet_init_call_t __start___benchlet_init_calls[];
extern benchlet_init_call_t __stop___benchlet_init_calls[];
#define SECTION_SIZE(sect) ((size_t)((__stop_##sect - __start_##sect)))

int main(int argc, char **argv) {
    int num_benchs = 0;
    time_t rtime = 0, start_t;
    char *benchlet_prefixes[1024] = { "", NULL };

    if (argc < 2) {
        errx(1, "Usage: %s RUNNING-TIME [BENCHLETS...]", argv[0]);
    }

    rtime = atoi(argv[1]);
    if (argc > 2) {
        for (int i = 2; i < argc; i++, num_benchs++) {
            benchlet_prefixes[num_benchs] = argv[i];
            benchlet_prefixes[num_benchs+1] = NULL;
        }
    } else {
        num_benchs = 1;
    }
    printf("numbenchs: %d\n", num_benchs);

    assert(SECTION_SIZE(__benchlet_calls) ==
           SECTION_SIZE(__benchlet_init_calls));

    for (int i = 0; i < SECTION_SIZE(__benchlet_calls); i++) {
        const char **counters;
        const char *counter;
        benchlet_config_t cfg;
        benchlet_info_t info;
        benchlet_init_call_t init_fun = __start___benchlet_init_calls[i];
        benchlet_call_t run_fun = __start___benchlet_calls[i];

        init_fun(&info);
        printf("- benchlet '%s':\n\t* desc: '%s'\n\t* counters: ",
               info.name, info.description);
        counters = info.penetrated_counters;
        while(NULL != (counter = *counters++)) {
            printf("%s, ", counter);
        }
        printf("\b\b  \n");

        cfg.desired_running_time = (int)rtime;
        for (int j = 0; j < num_benchs; j++) {
            if (info.name == strstr(info.name, benchlet_prefixes[j])) {
                printf("- Running '%s' for %ds...\n",
                       info.name, (int)cfg.desired_running_time);
                start_t = time(NULL);
                run_fun(&cfg);
                printf("- OK (%ds)\n", (int)(time(NULL)-start_t));
            } else {
                printf("- not running '%s'", info.name);
            }
        }
    }
    return 0;
}
