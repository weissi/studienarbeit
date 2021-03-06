#!/usr/bin/Rscript

args <- commandArgs(TRUE)

if (length(args) != 3 && length(args) != 4) {
    cat("Usage: Rscript test.r INPUT-FILE TABLE-COLUMN RESISTOR-VAL [TRIGGER-COLUMN]\n")
    cat("Sample: Rscript test.r input.rtab CPU 0.01 TRIGGER\n")
    quit("no", 1)
}

tab_fname <- args[1]
data_col <- args[2]
resistor_val <- as.double(args[3])
trigger_col <- args[4]

table <- read.table(tab_fname, header=TRUE)

attach(table)
time <- t
data <- (12 * get(data_col)) / resistor_val
data_raw <- data
if (!is.na(trigger_col)) {
    trigger_data <- get(trigger_col)
    trigger_threshold <- (max(trigger_data) + min(trigger_data)) / 2
    trigger <- as.integer(trigger_data > trigger_threshold)
    trigger_rle <- rle(trigger)
    data <- data * trigger
}
detach(table)

sample_rate <- nrow(table)/table$t[nrow(table)]
work_in_joule <- sum(data[-nrow(table)]*diff(time))
work_in_joule_raw <- sum(data_raw[-nrow(table)]*diff(time))

start <- min(time)
stop <- max(time)
run_time <- stop - start

#starti <- as.integer(start)
#stopi <- as.integer(stop)+1
#
#data_fun <- approxfun(time, data, method="linear", 0, 0)
##data_fun <- approxfun(time, data, method="constant", 0, 0)
##data_fun <- splinefun(time, data)
#work_in_joule <- integrate(data_fun, starti, stopi,
#                           subdivisions=as.integer(run_time)*sample_rate)$value

cat(work_in_joule, "\n")
cat("#\n")
cat("# work:", work_in_joule, "J\n")
cat("# work raw (no trigger):", work_in_joule_raw, "J\n")
cat("# data capturing running time:", run_time , "s\n")
cat("# sample rate:", sample_rate, "Hz\n")
cat("# number of subdivisions:", max(time)*sample_rate, "\n")
if (!is.na(trigger_col)) {
    if (length(trigger_rle$values) != 3 ||
        sum(trigger_rle$values == c(0,1,0)) != 3) {
        cat("# #############\n")
        cat("# #  WARNING  #\n")
        cat("# #############\n")
        cat("# Trigger threshold:", trigger_threshold, "\n")
        cat("# Trigger seems weird, its rle:\n")
        cat("# values :", trigger_rle$values, "\n")
        cat("# lengths:", trigger_rle$lengths, "\n")
    } else {
        cat("# trigger looks good, threshold:", trigger_threshold, " :-) \n")
    }
}
