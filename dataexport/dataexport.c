#include <datapoints.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

double diff(struct timespec start, struct timespec end) {
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

int main(int argc, char **argv) {
    DP_HANDLE h;
    dp_error err;
    DP_DATA_POINT **data;
    unsigned int spc;
    unsigned long int cnt = 0;
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

    while(DP_OK == (err = read_dataset(h, &tp, &spc, &data))) {
        if (0 != last_tp.tv_sec) {
            t0tb = diff(ref_tp, last_tp);
            tbte = diff(last_tp, tp);
            for (int i = 0; i < spc; i++) {
                t = t0tb + ((tbte / spc) * i);
                printf("%ld\t%f", cnt, t);
                for (int j = 0; j < num_of_channels(h); j++) {
                    printf("\t%f", data[j][i]);
                }
                printf("\n");
                cnt++;
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
