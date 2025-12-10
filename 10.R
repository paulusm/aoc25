library(readr)
library(dplyr)
library(purrr)
library(stringr)

input <- read_lines(file = "data/10-test.txt")

input |>
  map(\(x) {
    parse <- str_match(
      x,
      "\\[([\\.|\\#)]*)\\] ([\\([0-9|,]+\\) ]+) \\{([[0-9]*|,]*)\\}"
    )
    list(
      str_split_1(parse[2], "") == "#",
      str_split_1(parse[3], " "),
      str_split_1(parse[4], ",")
    )
  })
