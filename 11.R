library(readr)
library(purrr)
library(stringr)

input <- read_lines(file = "data/11-test.txt")

input |>
  map(\(x) {
    parts <- x |> str_split_1(":")
    parts[2] |>
      map(\(y) {
        outputs <- y |> str_trim() |> str_split(" ")
        c(parts[1], outputs)
      })
  })
