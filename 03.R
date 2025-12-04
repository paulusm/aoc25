library(readr)
library(purrr)
library(stringr)
library(dplyr)
options(scipen = 999)


banks <- read_lines(file = "data/3-test.txt")

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


removeMin <- function(b) {
    minJolt <- b |> str_split_1("") |> as.integer() |> reduce(min)
    b |> str_remove(as.character(minJolt))
}


banks |>
    map(\(bank) {
        while (nchar(bank) > 12) {
            bank <- removeMin(bank)
        }
        as.double(bank)
    }) |>
    reduce(sum)
