#ifndef __DATAPOINTS_H
#define __DATAPOINTS_H 1

typedef void *DP_HANDLE;
typedef double DP_SAMPLING_RATE;
typedef double DP_DATA_POINT;

DP_HANDLE open_datapoints_file_output(const char *filename,
                                      unsigned int channels,
                                      const char *channelNames[],
                                      DP_SAMPLING_RATE sample_rate
                                     );
int write_dataset(DP_HANDLE handle,
                  unsigned int samples_per_channel,
                  DP_DATA_POINT **data
                 );

int close_datapoints_file(DP_HANDLE);

#endif
