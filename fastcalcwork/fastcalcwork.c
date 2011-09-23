/*
 *  Quickly calculates work in datapoints files.
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
#include <datapoints.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <utils.h>

#define TRIGGER_THRESHOLD 0.03

int main(int argc, char **argv) {
    DP_HANDLE h;
    dp_error err;
    DP_DATA_POINT **data;
    unsigned int spc;
    struct timespec last_tp = { .tv_sec = 0, .tv_nsec = 0 };
    struct timespec tp;
    double tbte;
    double work = 0.0;
    double resistor_val;
    int cpu_channel = -1;
    int trigger_channel = -1;
    int trigger_changes = 1;
    bool last_trigger_state = 0;

    if (5 != argc) {
        fprintf(stderr,
                "fastcalcwork, Copyright (C)2011, "
                "Johannes Weiß <weiss@tux4u.de>\n");
        fprintf(stderr,
                "This program comes with ABSOLUTELY NO WARRANTY; "
                "for details type `show w'.\n"
                "This is free software, and you are welcome to redistribute it"
                "\nunder certain conditions; type `show c' for details.\n\n");
        fprintf(stderr, "Usage: %s INPUT.dpts TAB-COL RESIST-VAL TRIGGER-COL\n",
                argv[0]);
        exit(1);
    }

    h = open_datapoints_file_input(argv[1]);

    for (int j = 0; j < num_of_channels(h); j++) {
        if (0 == strcmp(channel_name(h, j), argv[2])) {
            cpu_channel = j;
        }
        if (0 == strcmp(channel_name(h, j), argv[4])) {
            trigger_channel = j;
        }
    }

    assert(0 <= cpu_channel);
    assert(0 <= trigger_channel);
    resistor_val = atof(argv[3]);

    while(DP_OK == (err = read_dataset(h, &tp, &spc, &data))) {
        if (0 != last_tp.tv_sec) {
            tbte = timediff(last_tp, tp);
            for (int i = 0; i < spc; i++) {
                if (data[trigger_channel][i] > TRIGGER_THRESHOLD) {
                    if (false == last_trigger_state) {
                        last_trigger_state = true;
                        trigger_changes++;
                    }
                    work += (tbte / spc) *
                            ((12.0 * data[cpu_channel][i]) /
                             resistor_val);
                } else {
                    if (true == last_trigger_state) {
                        last_trigger_state = false;
                        trigger_changes++;
                    }
                }
            }
        }

        free_dataset(h, data);
        last_tp = tp;
    }
    close_datapoints_file(h);
    printf("%f\n", work);
    printf("#\n#work: %f J\n", work);
    if (3 != trigger_changes || false != last_trigger_state) {
        printf("# #############\n");
        printf("# #  WARNING  #\n");
        printf("# #############\n");
        printf("# Trigger threshold: %f\n", TRIGGER_THRESHOLD);
        printf("# Trigger seems weird:\n");
        printf("# last trigger state: %d\n", last_trigger_state);
        printf("# trigger changes: %d\n", trigger_changes);
    } else {
        printf("# trigger looks good, threshold: %f\n", TRIGGER_THRESHOLD);
    }
    return 0;
}
