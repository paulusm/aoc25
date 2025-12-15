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
dtNoOverlaps <- data.table(newfrom = numeric(), newto = numeric())
dtFresh |>
  pmap(\(from, to) {
    overlaps <- dtNoOverlaps[
      (from > newfrom & to > newto & from < newto) |
        (from < newfrom & to < newto & to < newfrom),
    ]
    print(paste("overlaps", nrow(overlaps)))
    if (nrow(overlaps) == 0) {
      dtNoOverlaps <<- dtNoOverlaps |> rbind(list(from, to))
    } else {
      print(paste("overlaps:", nrow(overlaps)))
    }
  })
