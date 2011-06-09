#include "datapoints.h"

/* stdlib/POSIX includes */
#include <time.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/* SITE includes */
#include <utils.h>

/* LOCAL includes */
#include "measured-data.pb-c.h"

/* PRIVATE stuff */
typedef struct {
    int fd;
    bool write;
    unsigned int channels;
} dp_handle;

static void encode_datapoints(unsigned int samples_pc, DP_DATA_POINT *data,
                              unsigned int channelNo, DataPoints *msg_dps) {
    DataPoints tmp_dps = DATA_POINTS__INIT;
    *msg_dps = tmp_dps;

    msg_dps->n_datapoints = samples_pc;
    msg_dps->datapoints = data;
    msg_dps->has_channel = true;
    msg_dps->channel = channelNo;

    return;
}

void free_dp_handle(dp_handle *h) {
    free(h);
}

void write_header(dp_handle *h, unsigned int channels,
                  DP_SAMPLING_RATE sample_rate) {
    MeasuredData msg_md = MEASURED_DATA__INIT;
    const char magic[] = { 0x03, 0x01, 0x56, 0x01 };
    const char version = 0x1;
    uint32_t len;
    void *buf;
    assert(sizeof(magic) == write(h->fd, magic, sizeof(magic)));
    assert(1 == write(h->fd, &version, sizeof(char)));

    msg_md.samplingrate = sample_rate;
    msg_md.channels = channels;
    msg_md.hasexternaldata = true;
    msg_md.n_inlinedata = 0;

    len = measured_data__get_packed_size(&msg_md);
    assert(4 == write(h->fd, &len, sizeof(uint32_t)));
    buf = malloc(len);

    assert(len == measured_data__pack(&msg_md, buf));
    assert(len == write(h->fd, buf, len));

    free(buf);
    return;
}

/* PUBLIC stuff */
DP_HANDLE open_datapoints_file_output(const char *filename,
                                      unsigned int channels,
                                      DP_SAMPLING_RATE sample_rate
                                     ) {
    int fd = open(filename, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP |
                            S_IROTH );
    dp_handle *h = (dp_handle *)malloc(sizeof(dp_handle));
    h->channels = channels;
    h->write = true;
    h->fd = fd;

    if (fd < 0) {
        free_dp_handle(h);
        return NULL;
    }

    write_header(h, channels, sample_rate);

    return (DP_HANDLE)h;
}

int write_dataset(DP_HANDLE opaque_handle,
                  unsigned int samples_per_channel,
                  DP_DATA_POINT **data
                 ) {
    const char magic[] = { 0x03, 0x01, 0x56, 0x02 };
    uint32_t len;
    void *buf;
    unsigned int i;
    dp_handle *h = (dp_handle *)opaque_handle;
    struct timespec cur_time;
    DataSet msg_ds = DATA_SET__INIT;
    DataPoints **msg_dps = alloca(sizeof(DataPoints *) * h->channels);

    for(i = 0; i < h->channels; i++) {
        msg_dps[i] = alloca(sizeof(DataPoints));
    }

    DIE_NE0_ERR(clock_gettime(CLOCK_REALTIME, &cur_time), "clock_gettime");
    msg_ds.timesecs = cur_time.tv_sec;
    msg_ds.timenanosecs = cur_time.tv_nsec;
    msg_ds.n_channeldata = h->channels;

    for(i = 0; i < h->channels; i++) {
        encode_datapoints(samples_per_channel, data[i], i, msg_dps[i]);
    }

    msg_ds.n_channeldata = h->channels;
    msg_ds.channeldata = msg_dps;

    len = data_set__get_packed_size(&msg_ds);
    buf = malloc(len);

    assert(sizeof(magic) == write(h->fd, magic, sizeof(magic)));
    assert(4 == write(h->fd, &len, sizeof(uint32_t)));
    data_set__pack(&msg_ds, buf);
    assert(len == write(h->fd, buf, len));

    free(buf);

    return 0;
}

int close_datapoints_file(DP_HANDLE opaque_handle) {
    dp_handle *h = (dp_handle *)opaque_handle;

    int ret = close(h->fd);
    free_dp_handle(h);

    return ret;
}

int main(int argc, char **argv) {
    double d[3][10] = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 },
                        { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 },
                        { 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 }
                      };
    double **p_d = malloc(sizeof(double *) * 3);
    unsigned i, j;
    for(i = 0; i < 3; i++) {
        p_d[i] = d[i];
    }
    DP_HANDLE h = open_datapoints_file_output("out", 3, 10);
    DIE_EQNULL_ERR(h, "open_datapoints_file_output");

    DIE_NE0_ERR(write_dataset(h, 10, p_d), "write_dataset");
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 10; j++) {
            d[i][j] *= 2;
        }
    }
    DIE_NE0_ERR(write_dataset(h, 10, p_d), "write_dataset");
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 10; j++) {
            d[i][j] *= 2;
        }
    }
    DIE_NE0_ERR(write_dataset(h, 10, p_d), "write_dataset");

    DIE_NE0_ERR(close_datapoints_file(h), "close_datapoints_file");

    return 0;
}
