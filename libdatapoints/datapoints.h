/*
 *  Saving/Loading datapoints files
 *  Copyright (C)2011, Johannes Weiß <weiss@tux4u.de>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
                                      const char *shot_id,
                                      unsigned int channels,
                                      const char *channel_names[],
                                      DP_SAMPLING_RATE sample_rate
                                     );
DP_HANDLE open_datapoints_file_input(const char *filename);
unsigned int num_of_channels(DP_HANDLE handle);
const char *channel_name(DP_HANDLE handle, unsigned int channel_no);
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
