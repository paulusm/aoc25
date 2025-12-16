library(readr)
library(purrr)
library(stringr)
library(dplyr)
library(igraph)
library(tidyr)
library(data.table)
library(dtplyr)
library(mirai)

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

paths <- netobj|> all_shortest_paths(from="fft", to="dac") # 160944 too low