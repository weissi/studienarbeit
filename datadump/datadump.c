#include <NIDAQmxBase.h>
#include <stdio.h>
#include <assert.h>

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
#define AI_CPU "Dev1/ai0"
#define AI_BOARD "Dev1/ai1"
#define D_START_TRIG "/Dev1/PFI3"
#define D_STOP_TRIG "/Dev1/PFI8"
#define CLK_SRC "OnboardClock"

#define U_MIN -0.2
#define U_MAX 0.2

#define SMPL_RATE 10 /* samples per second */
#define DATA_SIZE 1024

int main(int argc, char **argv) {
    TaskHandle h = 0;
    bool32 done = 0;
    int32 written = 0;
    int32 error = 0;
    uInt8 wr_data = 0xAA;
    int32 pointsRead = 0;
    float64 data[DATA_SIZE];
    char errBuff[2048] = { 0 };
    int i;

    /* task 1, 0xAA -> port1 */
    CHK(DAQmxBaseCreateTask("digital-output", &h));
    CHK(DAQmxBaseCreateDOChan(h, D_OUT, NULL, DAQmx_Val_ChanForAllLines));
    CHK(DAQmxBaseWriteDigitalU8(h, ONE_SAMPLE, TRUE,
                    TIMEOUT, DAQmx_Val_GroupByChannel,
                    &wr_data, &written, NULL));
    assert(written == ONE_SAMPLE);
    CHK(DAQmxBaseStartTask(h));
    CHK(DAQmxBaseStopTask(h));
    CHK(DAQmxBaseClearTask(h));

    /* task 2: measure diff ai0 */
    CHK(DAQmxBaseCreateTask("analog-input", &h));
    CHK(DAQmxBaseCreateAIVoltageChan(h, AI_CPU, NULL, DAQmx_Val_Diff,
                         U_MIN, U_MAX, DAQmx_Val_Volts, NULL));
    CHK(DAQmxBaseCfgSampClkTiming(h, CLK_SRC, SMPL_RATE,
                      DAQmx_Val_Rising, DAQmx_Val_ContSamps,
                      0));
    CHK(DAQmxBaseStartTask(h));
    printf("GOOOOOO!\n");

    while (1) {
        CHK(DAQmxBaseReadAnalogF64(h, SMPL_RATE, TIMEOUT,
                       DAQmx_Val_GroupByScanNumber,
                       data, DATA_SIZE, &pointsRead, NULL));
        printf("Acquired %d samples:\n", pointsRead);
        for (i=0; i<pointsRead; i++) {
            printf("data[%d] = %f\n", i, data[i]);
        }
    }

TearDown:
    if( DAQmxFailed(error) )
        DAQmxBaseGetExtendedErrorInfo(errBuff,2048);
    if(h != 0) {
        DAQmxBaseStopTask (h);
        DAQmxBaseClearTask (h);
    }
    if( DAQmxFailed(error) )
                printf ("DAQmxBase Error %d: %s\n", error, errBuff);
    return 0;
}
