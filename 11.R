library(readr)
library(purrr)
library(stringr)
library(dplyr)
library(igraph)
library(tidyr)

input <- read_lines(file = "data/11.txt")

netdf <- tibble()
net <- input |>
  map(\(x) {
    parts <- x |> str_split_1(":")
    parts[2] |>
      map(\(y) {
        outputs <- y |>
          str_trim() |>
          str_split_1(" ") |>
          map(\(z) {
            list(parts[1], z)
          })
      })
  }) |>
  flatten() |>
  flatten() |>
  map(\(x) {
    netdf <<- rbind(netdf, x)
  })
names(netdf) <- c("from", "to")

netobj <- graph_from_data_frame(netdf)
paths <- netobj |> all_shortest_paths("you", "out")
print(paths$epaths |> length())


paths <- k_shortest_paths(
  netobj,
  k = 5000,
  "svr",
  "out",
  mode = "out"
)$vpaths
print(paste("part 2 paths =", length(paths)))
paths |>
  map(
    \(x) {
      if (
        sum(str_count(names(x), "dac") > 0) &
          sum(str_count(names(x), "fft") > 0)
      ) {
        1
      } else {
        0
      }
    },
    .progress = T
  ) |>
  reduce(sum)
