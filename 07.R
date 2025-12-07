library(readr)
library(purrr)
library(stringr)

data <- read_lines(file = "data/7-test.txt")
splits <- 0
beampos <- list(str_locate(data[[1]], "S")[1, 1])
hist <- list()

data <- data |>
  discard_at(1) |>
  map(\(line) {
    newline <- line
    beampos <<- beampos |>
      imap(\(y, i) {
        if (substring(line, y, y) == ".") {
          substring(newline, y) <<- "|"
          y
        } else {
          if (substring(line, y, y) == "^") {
            {
              substring(newline, y + 1) <<- "|"
              substring(newline, y - 1) <<- "|"
              splits <<- splits + 1
              hist[[length(hist) + 1]] <<- list(i, y - 1, y + 1)
              list(y - 1, y + 1)
            }
          }
        }
      }) |>
      flatten() |>
      unique()
    #print(beampos)
    print(newline)
  })

print(splits)
