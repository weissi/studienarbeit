#include <datapoints.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#include <utils.h>

int main(int argc, char **argv) {
    DP_HANDLE h;
    dp_error err;
    DP_DATA_POINT **data;
    unsigned int spc;
    struct timespec ref_tp = { 0 };
    struct timespec last_tp = { .tv_sec = 0, .tv_nsec = 0 };
    struct timespec tp;
    double t;
    double t0tb;
    double tbte;

    if (2 != argc) {
        fprintf(stderr, "Usage: %s DATAPOINTS-FILE.dpts\n", argv[0]);
        exit(1);
    }

    h = open_datapoints_file_input(argv[1]);

    printf("t\t");
    for (int j = 0; j < num_of_channels(h); j++) {
        printf("\t%s", channel_name(h, j));
    }
    printf("\n");

    while(DP_OK == (err = read_dataset(h, &tp, &spc, &data))) {
        if (0 != last_tp.tv_sec) {
            t0tb = timediff(ref_tp, last_tp);
            tbte = timediff(last_tp, tp);
            for (int i = 0; i < spc; i++) {
                t = t0tb + ((tbte / spc) * i);
                printf("%f", t);
                for (int j = 0; j < num_of_channels(h); j++) {
                    printf("\t%f", data[j][i]);
                }
                printf("\n");
            }
        } else {
            ref_tp = tp;
        }
        free_dataset(h, data);
        last_tp = tp;
    }
    close_datapoints_file(h);
    return 0;
}
