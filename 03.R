library(readr)
library(purrr)
library(stringr)
library(dplyr)

banks <- read_lines(file = "data/3.txt")

banks |>
    map(\(bank) {
        lengbank <- nchar(bank)
        first <- bank |>
            str_split_1("") |>
            discard_at(lengbank) |>
            as.integer() |>
            reduce(max) |>
            as.character()
        remainder <- bank |> str_split(as.character(first), n = 2) # only split on first occurrance
        second <- remainder |>
            pluck(1) |>
            pluck(2) |>
            str_split_1("") |>
            as.integer() |>
            reduce(max) |>
            as.character()
        as.integer(paste0(first, second))
    }) |>
    reduce(sum)
