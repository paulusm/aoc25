library(readr)
library(data.table)
library(purrr)
library(dtplyr)
library(dplyr)
library(stringr)
options(scipen = 999)

data <- read_lines(file = "data/5.txt")

dtFresh <- data.table(from = numeric(), to = numeric())
dtSamples <- data.table(sample = numeric())

data |>
  map(\(x) {
    range <- str_split_1(x, "-")
    if (length(range) == 2) {
      dtFresh <<- rbind(dtFresh, list(as.double(range[1]), as.double(range[2])))
    } else {
      if (nchar(x) > 0) {
        dtSamples <<- rbind(dtSamples, list(as.double(x)))
      }
    }
  })

dtSamples$sample |>
  map(\(x) {
    ifelse(nrow(dtFresh |> filter(x >= from & x <= to)) > 0, 1, 0)
  }) |>
  reduce(sum) |>
  print()

# Part 2 not yet working
dtNoOverlaps <- data.table(f = numeric(), t = numeric())
dtFresh |>
  arrange(from) |>
  pmap(\(from, to) {
    overlaps <- dtNoOverlaps[
      (from >= f & to >= t & from < t) |
        (from <= f & to <= t & to < f),
    ]

    if (nrow(overlaps) == 0) {
      dtNoOverlaps <<- dtNoOverlaps |> rbind(list(from, to))
    } else {
      overlaps <- overlaps |> rbind(list(from, to))
      #print(paste("overlaps", nrow(overlaps)))
      frommin <- from
      tomax <- to
      overlaps |>
        pmap(\(f, t) {
          frommin <<- min(f, frommin)
          tomax <<- max(to, tomax)
        })
      dtNoOverlaps <<- dtNoOverlaps |> anti_join(overlaps, by = join_by(f, t))
      dtNoOverlaps <<- dtNoOverlaps |> rbind(list(frommin, tomax))
    }
  })

dtNoOverlaps |>
  pmap(\(f, t) {
    t - f + 1
  }) |>
  reduce(sum) |>
  print()
