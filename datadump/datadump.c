/*
 *  Records data in datapoints format from a NI USB-6218
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
#include <NIDAQmxBase.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <sys/param.h>
#include <signal.h>
#include <stdbool.h>

#include <datapoints.h>

#define \
    CHK(functionCall) { \
        printf("NI call...\n"); fflush(stdout); \
        if( DAQmxFailed(error=(functionCall)) ) { \
            goto TearDown; \
        } \
    }

#define TIMEOUT 10.0
#define ONE_SAMPLE 1
#define D_OUT "Dev1/port1"
#define CLK_SRC "OnboardClock"

#define MAX_PRINT_SMPLS 10

/**
 * WARNING: Don't make any errors with channel names!
 * Dev/ai0 instead of Dev1/ai0 causes a free() on an
 * invalid address in nidaxmxbase!
 */
static const char *AI_CHANNELS = "Dev1/ai0, Dev1/ai1, Dev1/ai2";
static const char *CHAN_NAMES[] = { "CPU", "BOARD", "TRIGGER" };
#define NO_CHANNELS 3
#define U_MIN -0.2
#define U_MAX 0.2
/*
#define U_MIN -5
#define U_MAX 5
*/

/*
static const char *AI_CHANNELS = "Dev1/ai0, Dev1/ai1";
static const char *CHAN_NAMES[] = { "CPU", "BOARD" };
#define NO_CHANNELS 2
#define U_MIN -0.2
#define U_MAX 0.2
*/

#define SMPL_RATE 50000 /* samples per second */
#define DATA_SIZE 8192

/**
 * WARNING: When it crashes dereferencing 0x4 '0x0x4'
 * replug the NI device
 *
 * original message:
 * Reason: address not mapped to object
 * Attempt to reference address: 0x0x4
 */

static volatile bool running = true;
static void sig_hnd() {
    running = false;
}

int main(int argc, char **argv) {
    int ret = 1;
    TaskHandle h = 0;
    int32 error = 0;
    float64 data[DATA_SIZE];
    char errBuff[2048] = { 0 };
    DP_HANDLE dp_h;
    char *fname, *shot;
    time_t t, t_end;
    int mtime;
    DP_DATA_POINT **dp_data = malloc(sizeof(DP_DATA_POINT *) * NO_CHANNELS);

    if (argc != 3 && argc != 4) {
        fprintf(stderr,
                "datadump, Copyright (C)2011, "
                "Johannes Weiß <weiss@tux4u.de>\n");
        fprintf(stderr,
                "This program comes with ABSOLUTELY NO WARRANTY; "
                "for details type `show w'.\n"
                "This is free software, and you are welcome to redistribute it"
                "\nunder certain conditions; type `show c' for details.\n\n");
        fprintf(stderr, "Usage: %s FILENAME SHOT-ID [MEASURING-TIME]\n",
                argv[0]);
        exit(1);
    }
    fname = argv[1];
    shot = argv[2];
    if (argc == 4) {
        mtime = atoi(argv[3]);
    } else {
        mtime = -1;
    }

    dp_h = open_datapoints_file_output(fname, shot, NO_CHANNELS, CHAN_NAMES,
                                       SMPL_RATE);
    assert(NULL != dp_h);

    /* measure diffs */
    CHK(DAQmxBaseCreateTask("analog-inputs", &h));
    printf("chan setup '%s'(%p)\n", AI_CHANNELS, AI_CHANNELS);
    fflush(stdout);
    CHK(DAQmxBaseCreateAIVoltageChan(h, AI_CHANNELS, NULL, DAQmx_Val_Diff,
                                     U_MIN, U_MAX, DAQmx_Val_Volts, NULL));
    printf("done\n");
    fflush(stdout);
    CHK(DAQmxBaseCfgSampClkTiming(h, CLK_SRC, SMPL_RATE,
                      DAQmx_Val_Rising, DAQmx_Val_ContSamps,
                      0));
    CHK(DAQmxBaseStartTask(h));

    signal( SIGINT, (void (*)(int))sig_hnd );

    printf("GOOOOOO!\n");
    if (0 < mtime) {
        t_end = mtime + time(NULL);
    } else {
        t_end = 0;
    }

    do {
        int32 pointsPerChan;
        t = time(NULL);

        CHK(DAQmxBaseReadAnalogF64(h, SMPL_RATE, TIMEOUT,
                       DAQmx_Val_GroupByChannel,
                       data, DATA_SIZE, &pointsPerChan, NULL));
        if (0 != t_end) {
            printf("[ETA: %ds] Acquired %d samples per channel:\n",
                   (int)(t_end-t), (int)pointsPerChan);
        } else {
            printf("[Ctrl-C to stop] Acquired %d samples per channel:\n",
                   (int)pointsPerChan);
        }

        for (int i=0; i<MIN(pointsPerChan, MAX_PRINT_SMPLS); i++) {
            printf("data[%d] = \t", i);
            for (int j = 0; j < NO_CHANNELS; j++) {
                printf("%F\t", (double)(data[i + (j * pointsPerChan)]));
            }
            printf("\n");
        }
        for (int i=0; i<NO_CHANNELS; i++) {
            dp_data[i] = data + (i * pointsPerChan);
        }
        write_dataset(dp_h, pointsPerChan, dp_data);
    } while(running && (0 == t_end || t <= t_end));

    ret = 0;
TearDown:
    close_datapoints_file(dp_h);
    if( DAQmxFailed(error) )
        DAQmxBaseGetExtendedErrorInfo(errBuff,2048);
    if(h != 0) {
        DAQmxBaseStopTask (h);
        DAQmxBaseClearTask (h);
    }
    if( DAQmxFailed(error) ) {
        printf ("DAQmxBase Error %d %s\n", (int)error, errBuff);
    }

    printf("FINISHED!\n");

    return ret;
}
