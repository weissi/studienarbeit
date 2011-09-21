/*
 *  simple libdatapoints demo
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
#include <stdlib.h>
#include <stdio.h>

#include <utils.h>

#include "datapoints.h"

int main(int argc, char **argv) {
    double d[3][10] = { { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 },
                        { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20 },
                        { 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 }
                      };
    double **p_d = malloc(sizeof(double *) * 3);
    unsigned i, j, k;
    for(i = 0; i < 3; i++) {
        p_d[i] = d[i];
    }
    DP_HANDLE h = open_datapoints_file_output("demo.out", "demo-shot-N",
                                              3, NULL, 10);
    DIE_EQNULL_ERR(h, "open_datapoints_file_output");

    DIE_NE0_ERR(write_dataset(h, 10, p_d), "write_dataset");

    for (k = 0; k < 10000; k++) {
        for (i = 0; i < 3; i++) {
            for (j = 0; j < 10; j++) {
                d[i][j] *= 2;
            }
        }
        DIE_NE0_ERR(write_dataset(h, 10, p_d), "write_dataset");
    }

    DIE_NE0_ERR(close_datapoints_file(h), "close_datapoints_file");
    free(p_d);

    return 0;
}
