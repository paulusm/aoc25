library(readr)
library(purrr)
library(stringr)
library(dplyr)
options(scipen = 999)


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

getMax <- function(bank, result, n, target_n, indices) {
    if (n == 0) {
        return(result)
    }
    top <- 0
    lastIndex <- 0
    bank |>
        str_split_1("") |>
        as.integer() |>
        imap(\(x, i) {
            if (
                (!(i %in% indices)) &
                    (i < nchar(bank) - n + 2) &
                    (i > max(indices))
            ) {
                if (x > top) {
                    #print(i)
                    #print(indices)
                    top <<- x
                    lastIndex <<- i
                }
            }
        })
    return(getMax(
        bank,
        result + (top * 10^(n - 1)),
        n - 1,
        n,
        c(indices, lastIndex)
    ))
}

banks |>
    map(\(bank) {
        getMax(bank, 0, 12, 12, c())
    }) |>
    reduce(sum)
