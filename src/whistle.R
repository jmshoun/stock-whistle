library(readr)
library(lubridate)
library(dplyr)
library(sound)

source("src/utils.R")

args <- commandArgs(trailingOnly=TRUE)

# Parse command-line args
periods <- as.integer(args[1])
units <- substring(tolower(args[2]), 1, 1)
time <- as.numeric(args[3])
max.points <- as.integer(args[4])
outfile <- args[5]

# Compute first and last dates of the window
stop.date <- Sys.Date()
start.date <- if (units == "m") {
    stop.date %m+% months(-periods)
} else {
    stop.date - years(periods)
}

# Load the SPY value data
SPY.value.df <- read_csv("data/spy.csv") %>% 
    filter(date >= start.date)

# Compute the indices of the series to use as control points
idx <- if (max.points < nrow(SPY.value.df)) {
    round(seq(1, nrow(SPY.value.df), length.out=max.points))
} else {
    1:nrow(SPY.value.df)
}

# Render and save the audio
audio <- play_value(SPY.value.df$value[idx], base.frequency=600, octave.ratio=0.15,
                    time=time, sample.rate=44100)
saveSample(audio, outfile, overwrite=TRUE)
