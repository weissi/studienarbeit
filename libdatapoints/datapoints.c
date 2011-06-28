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
#include <string.h>

/* SITE includes */
#include <utils.h>

/* LOCAL includes */
#include "measured-data.pb-c.h"

/* PRIVATE stuff */
typedef struct {
    char *shot_id;
    int fd;
    bool write;
    unsigned int channels;
    DP_SAMPLING_RATE sample_rate;
} dp_handle;

static const unsigned char HEADER_MAGIC[] = { 0x03, 0x01, 0x86, 0x01 };
static const unsigned char DATASET_MAGIC[] = { 0x03, 0x01, 0x86, 0x02 };
static const unsigned char VERSION = 0x1;
#define PRE_HEADER_SIZE (sizeof(HEADER_MAGIC) + sizeof(VERSION) +\
                         sizeof(header_length))
#define PRE_DATASET_SIZE (sizeof(DATASET_MAGIC) + sizeof(uint32_t))

#define MAX_CHAN_NAME_LEN 16
#define MAX_SHOT_ID_LEN 64

static void encode_datapoints(unsigned int samples_pc, DP_DATA_POINT *data,
                              unsigned int channelNo, DataPoints *msg_dps) {
    data_points__init(msg_dps);

    msg_dps->n_data_points = samples_pc;
    msg_dps->data_points = data;
    msg_dps->has_channel_no = true;
    msg_dps->channel_no = channelNo;

    return;
}

void free_dp_handle(dp_handle *h) {
    free(h);
}

void write_header(dp_handle *h, const char *channel_names[]) {
    MeasuredData msg_md = MEASURED_DATA__INIT;
    char **chanNames_copy;
    uint32_t len;
    void *buf;
    unsigned int i;
    assert(sizeof(HEADER_MAGIC) ==
           write(h->fd, HEADER_MAGIC, sizeof(HEADER_MAGIC)));
    assert(1 == write(h->fd, &VERSION, sizeof(char)));
    assert(NULL != (chanNames_copy = alloca(sizeof(char *) * h->channels)));

    if (channel_names != NULL) {
        for (i = 0; i < h->channels; i++) {
            assert(NULL != (chanNames_copy[i] = alloca(MAX_CHAN_NAME_LEN)));
            strlcpy(chanNames_copy[i], channel_names[i], MAX_CHAN_NAME_LEN);
        }
    } else {
        chanNames_copy = NULL;
    }

    msg_md.shot_id = h->shot_id;
    msg_md.sampling_rate = h->sample_rate;
    msg_md.channel_count = h->channels;
    msg_md.n_channel_names = chanNames_copy != NULL ? h->channels : 0;
    msg_md.channel_names = chanNames_copy;
    msg_md.has_external_data = true;
    msg_md.n_inline_data = 0;

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
                                      const char *shot_id,
                                      unsigned int channels,
                                      const char *channel_names[],
                                      DP_SAMPLING_RATE sample_rate
                                     ) {
    int fd = open(filename, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP |
                            S_IROTH );
    assert(fd > 0);
    dp_handle *h = (dp_handle *)malloc(sizeof(dp_handle));
    assert(NULL != h);
    assert(NULL != (h->shot_id = malloc(MAX_SHOT_ID_LEN)));
    strlcpy(h->shot_id, shot_id, MAX_SHOT_ID_LEN);
    h->channels = channels;
    h->write = true;
    h->fd = fd;
    h->sample_rate = sample_rate;

    if (fd < 0) {
        free_dp_handle(h);
        return NULL;
    }

    write_header(h, channel_names);

    return (DP_HANDLE)h;
}

DP_HANDLE open_datapoints_file_input(const char *filename) {
    MeasuredData *msg_md;
    unsigned char *buf;
    dp_handle *h;
    int fd;
    size_t offset = 0;
    uint32_t header_length;

    fd = open(filename, O_RDONLY);
    assert(fd > 0);

    assert(NULL != (buf = malloc(PRE_HEADER_SIZE)));
    assert(PRE_HEADER_SIZE == read(fd, buf, PRE_HEADER_SIZE));

    offset = 0;
    assert(0 == memcmp(buf+offset, HEADER_MAGIC, sizeof(HEADER_MAGIC)));

    offset += sizeof(HEADER_MAGIC);
    assert(0 == memcmp(buf+offset, &VERSION, sizeof(VERSION)));

    offset += sizeof(VERSION);
    memcpy(&header_length, buf+offset, sizeof(uint32_t));

    free(buf);
    assert(NULL != (buf = malloc(header_length)));
    assert(header_length == read(fd, buf, header_length));

    assert(NULL != (msg_md = measured_data__unpack(NULL, header_length, buf)));
    free(buf);

    assert(true == msg_md->has_external_data);
    assert(0 == msg_md->n_inline_data);

    h = (dp_handle *)malloc(sizeof(dp_handle));
    h->write = false;
    h->fd = fd;
    h->sample_rate = msg_md->sampling_rate;
    h->channels = msg_md->channel_count;

    measured_data__free_unpacked(msg_md, NULL);

    return (DP_HANDLE)h;
}

int write_dataset(DP_HANDLE opaque_handle,
                  unsigned int samples_per_channel,
                  DP_DATA_POINT **data
                 ) {
    uint32_t len;
    void *buf;
    unsigned int i;
    dp_handle *h = (dp_handle *)opaque_handle;
    struct timespec cur_time;
    Timestamp msg_time;
    DataSet msg_ds = DATA_SET__INIT;
    DataPoints **msg_dps = alloca(sizeof(DataPoints *) * h->channels);

    for(i = 0; i < h->channels; i++) {
        msg_dps[i] = alloca(sizeof(DataPoints));
    }

    DIE_NE0_ERR(clock_gettime(CLOCK_REALTIME, &cur_time), "clock_gettime");
    msg_ds.time = &msg_time;
    timestamp_from_timespec(msg_ds.time, &cur_time);
    msg_ds.n_channel_data = h->channels;

    for(i = 0; i < h->channels; i++) {
        if (data[i] != NULL) {
            encode_datapoints(samples_per_channel, data[i], i, msg_dps[i]);
        }
    }

    msg_ds.n_channel_data = h->channels;
    msg_ds.channel_data = msg_dps;

    len = data_set__get_packed_size(&msg_ds);
    buf = malloc(len);

    assert(sizeof(DATASET_MAGIC) ==
           write(h->fd, DATASET_MAGIC, sizeof(DATASET_MAGIC)));
    assert(4 == write(h->fd, &len, sizeof(uint32_t)));
    data_set__pack(&msg_ds, buf);
    assert(len == write(h->fd, buf, len));

    free(buf);

    return 0;
}

unsigned int num_of_channels(DP_HANDLE handle) {
    dp_handle *h = (dp_handle *)handle;
    return h->channels;
}

dp_error read_dataset(DP_HANDLE handle,
                      struct timespec *tp,
                      unsigned int *samples_per_channel,
                      DP_DATA_POINT **out_data[]
                     ) {
    dp_handle *h = (dp_handle *)handle;
    uint32_t ds_len;
    unsigned char *buf;
    DataSet *msg_ds;
    size_t offset;
    DP_DATA_POINT **data;
    ssize_t bytes_read;

    assert(NULL != (buf = malloc(PRE_DATASET_SIZE)));

    bytes_read = read(h->fd, buf, PRE_DATASET_SIZE);
    if (bytes_read == 0) {
        /* EOF */
        free(buf);
        return DP_EOF;
    }
    assert(PRE_DATASET_SIZE == bytes_read);

    offset = 0;
    assert(0 == memcmp(DATASET_MAGIC, buf+offset, sizeof(DATASET_MAGIC)));
    offset += sizeof(DATASET_MAGIC);

    memcpy(&ds_len, buf+offset, sizeof(uint32_t));
    free(buf);

    assert(NULL != (buf = malloc(ds_len)));
    assert(ds_len == read(h->fd, buf, ds_len));
    assert(NULL != (msg_ds = data_set__unpack(NULL, ds_len, buf)));
    free(buf);

    if (NULL != tp) {
        timespec_from_timestamp(tp, msg_ds->time);
    }

    assert(h->channels == msg_ds->n_channel_data);
    assert(NULL != (data = malloc(h->channels * sizeof(DP_DATA_POINT *))));
    for (int i = 0; i < h->channels; i++) {
        DataPoints *msg_dps = msg_ds->channel_data[i];
        unsigned int spc = msg_dps->n_data_points;
        if (NULL != samples_per_channel) {
            *samples_per_channel = spc;
        }
        assert(NULL != (data[i] = malloc(spc * sizeof(DP_DATA_POINT))));
        memcpy(data[i], msg_dps->data_points, spc * sizeof(DP_DATA_POINT));
    }

    data_set__free_unpacked(msg_ds, NULL);
    *out_data = data;
    return DP_OK;
}

void free_dataset(DP_HANDLE handle, DP_DATA_POINT *data[]) {
    dp_handle *h = (dp_handle *)handle;

    for (int i = 0; i < h->channels; i++) {
        free(data[i]);
    }
    free(data);
}

int close_datapoints_file(DP_HANDLE opaque_handle) {
    dp_handle *h = (dp_handle *)opaque_handle;

    int ret = close(h->fd);
    free_dp_handle(h);

    return ret;
}
