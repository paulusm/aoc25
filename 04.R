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
batchToRemove <- tibble(x = c(), y = c())

removeRolls <- function() {
  return(
    seq(1:gridsize) |>
      map(\(x) {
        seq(1:gridsize) |>
          map(\(y) {
            if (rollgrid[x, y] == T) {
              coordx <- list(-1, 0, 1, -1, 1, -1, 0, 1)
              coordy <- list(1, 1, 1, 0, 0, -1, -1, -1)
              neighbours <- coordx |>
                map2(coordy, \(a, b) {
                  tryCatch(
                    {
                      if (rollgrid[x + a, y + b] == T) {
                        1
                      } else {
                        0
                      }
                    },
                    # off the map, no rolls
                    error = function(cond) {
                      0
                    }
                  )
                }) |>
                reduce(sum)
              if (neighbours < 4) {
                batchToRemove <<- batchToRemove |> bind_rows(list(x = x, y = y))
              }
              neighbours
            } else {
              99
            }
          })
      }) |>
      flatten() |>
      keep(\(x) {
        x < 4
      }) |>
      length()
  )
}

tot <- 0
foo <- removeRolls()
while (foo > 0) {
  foo <- removeRolls()
  # print(foo)
  tot <- tot + foo
  batchToRemove |>
    pmap(\(x, y) {
      rollgrid[x, y] <<- F
    })
}
print(paste("total", tot))
