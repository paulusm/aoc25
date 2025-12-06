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

# dtSamples$sample |>
#   map(\(x) {
#     ifelse(nrow(dtFresh |> filter(x >= from & x <= to)) > 0, 1, 0)
#   }) |>
#   reduce(sum) |>
#   print()

dtFreshClean <- data.table(from = numeric(), to = numeric())
dtFreshNonOverlap <- dtFresh

dtFresh$from |>
  map2(dtFresh$to, \(a, b) {
    dtFresh$from |>
      map2(dtFresh$to, \(x, y) {
        if (a != x & b != y) {
          # Overlapping head
          if (a <= x & b > x & b < y) {
            dtFreshClean <<- dtFreshClean |> rbind(list(a, y))
            dtFreshNonOverlap <<- dtFreshNonOverlap[
              -((from == a & to == b) | (from == x & to == y)),
            ]
          }
          # Overlapping tail
          if (a > x & a < y & b >= y) {
            dtFreshClean <<- dtFreshClean |> rbind(list(x, b))
            dtFreshNonOverlap <<- dtFreshNonOverlap[
              -((from == a & to == b) | (from == x & to == y)),
            ]
          }
          # Enclosed
          if (a > x & b < y) {
            dtFreshNonOverlap <<- dtFreshNonOverlap[
              -(from == a & to == b),
            ]
          }
        }
      })
  })

dupes <- duplicated(dtFreshClean)
dtFreshClean <- dtFreshClean[-dupes, ]
dupes2 <- duplicated(dtFreshNonOverlap)
dtFreshNonOverlap <- dtFreshNonOverlap[-dupes2, ]

# 214452980313054 too low
# 455191274181386, 554540882371704 too high
dtFreshClean |>
  bind_rows(dtFreshNonOverlap) |>
  mutate(diff = to - from) |>
  summarise(sum(diff)) |>
  print()
