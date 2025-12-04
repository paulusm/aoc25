library(readr)
library(stringr)
library(dplyr)
library(purrr)

rollmap <- read_lines(file = "data/4.txt")
gridsize <- nchar(rollmap[1])
rollnum <- rollmap |>
  map(\(x) {
    x |> str_split_1("") == "@"
  }) |>
  flatten()

rollgrid <- matrix(rollnum, nrow = gridsize, ncol = gridsize)


seq(1:gridsize) |>
  map(\(x) {
    seq(1:gridsize) |>
      map(\(y) {
        if (rollgrid[x, y] == T) {
          coordx <- list(-1, 0, 1, -1, 1, -1, 0, 1)
          coordy <- list(1, 1, 1, 0, 0, -1, -1, -1)
          coordx |>
            map2(coordy, \(a, b) {
              tryCatch(
                {
                  ifelse(rollgrid[x + a, y + b] == T, 1, 0)
                },
                # off the map, no rolls
                error = function(cond) {
                  0
                }
              )
            }) |>
            reduce(sum)
        } else {
          99
        }
      })
  }) |>
  flatten() |>
  keep(\(x) {
    x < 4
  }) |>
  length() |>
  print()
