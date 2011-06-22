#ifndef __DATAPOINTS_H
#define __DATAPOINTS_H 1

#include <time.h>

typedef void *DP_HANDLE;
typedef double DP_SAMPLING_RATE;
typedef double DP_DATA_POINT;
typedef enum {
    DP_OK,
    DP_EOF,

    /* leave at the end! */
    DP_ERROR_LENGTH
} dp_error;

DP_HANDLE open_datapoints_file_output(const char *filename,
                                      unsigned int channels,
                                      const char *channelNames[],
                                      DP_SAMPLING_RATE sample_rate
                                     );
DP_HANDLE open_datapoints_file_input(const char *filename);
unsigned int num_of_channels(DP_HANDLE handle);
int write_dataset(DP_HANDLE handle,
                  unsigned int samples_per_channel,
                  DP_DATA_POINT *data[]
                 );
dp_error read_dataset(DP_HANDLE handle,
                      struct timespec *tp,
                      unsigned int *samples_per_channel,
                      DP_DATA_POINT **data[]
                     );
void free_dataset(DP_HANDLE handle, DP_DATA_POINT *data[]);


int close_datapoints_file(DP_HANDLE);

#endif
