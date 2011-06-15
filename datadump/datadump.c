#include <NIDAQmxBase.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>
#include <sys/param.h>

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

#define U_MIN -5
#define U_MAX 5

#define SMPL_RATE 1000 /* samples per second */
#define DATA_SIZE 8192

int main(int argc, char **argv) {
    TaskHandle h = 0;
    int32 error = 0;
    float64 data[DATA_SIZE];
    char errBuff[2048] = { 0 };
    DP_HANDLE dp_h;
    DP_DATA_POINT **dp_data = malloc(sizeof(DP_DATA_POINT *) * NO_CHANNELS);
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
                      3));
    CHK(DAQmxBaseStartTask(h));

    dp_h = open_datapoints_file_output("out", NO_CHANNELS, CHAN_NAMES,
                                       SMPL_RATE);
    printf("GOOOOOO!\n");

    for (int rounds=0; rounds<3; rounds++) {
        int32 pointsPerChan;

        CHK(DAQmxBaseReadAnalogF64(h, SMPL_RATE, TIMEOUT,
                       DAQmx_Val_GroupByChannel,
                       data, DATA_SIZE, &pointsPerChan, NULL));
        printf("Acquired %d samples per channel:\n", (int)pointsPerChan);

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
    }

TearDown:
    close_datapoints_file(dp_h);
    if( DAQmxFailed(error) )
        DAQmxBaseGetExtendedErrorInfo(errBuff,2048);
    if(h != 0) {
        DAQmxBaseStopTask (h);
        DAQmxBaseClearTask (h);
    }
    if( DAQmxFailed(error) )
                printf ("DAQmxBase Error %d %s\n", (int)error, errBuff);
    return 0;
}
