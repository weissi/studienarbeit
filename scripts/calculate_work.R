#!/usr/bin/Rscript

args <- commandArgs(TRUE)

if (length(args) != 2) {
    cat("Usage: Rscript test.r INPUT-FILE TABLE-COLUMN\n")
    quit("no", 1)
}

tab_fname <- args[1]
data_col <- args[2]

table <- read.table(tab_fname, header=TRUE)

attach(table)
time <- t
data <- (12 * get(data_col)) / 0.01
detach(table)

sample_rate <- nrow(table)/table$t[nrow(table)]
work_in_joule <- sum(data[-nrow(table)]*diff(time))

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
cat("# data capturing running time:", run_time , "s\n")
cat("# sample rate:", sample_rate, "Hz\n")
cat("# number of subdivisions:", max(time)*sample_rate, "\n")
