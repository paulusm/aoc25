library(readr)
library(purrr)
result <- 0

input <- read_lines(file = "data/2.txt") |> str_split(",")
input[[1]] |>
  as.list() |>
  map(\(x) {
    lims <- str_split(x, "-")
    seq(as.numeric(lims[[1]][1]), as.numeric(lims[[1]][2])) |>
      map(\(y) {
        codeaschar <- as.character(y)
        thelen <- nchar(codeaschar)
        if (thelen %% 2 == 0) {
          if (
            substring(codeaschar, 1, thelen / 2) ==
              substring(codeaschar, thelen / 2 + 1, thelen)
          ) {
            result <<- result + y
          }
        }
      })
  })


print(paste("Part1 ", result))
