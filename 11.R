library(readr)
library(purrr)
library(stringr)
library(dplyr)
library(igraph)

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
paths <- netobj |> k_shortest_paths("you", "out", k = 1000)
print(paths$epaths |> length())
