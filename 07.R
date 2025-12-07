library(readr)
library(purrr)
library(stringr)

data <- read_lines(file = "data/7-test.txt")
splits <- 0
beampos <- list(str_locate(data[[1]], "S")[1, 1])


## Part One

data <- data |>
  discard_at(1) |>
  imap(\(line, i) {
    newline <- line
    beampos <<- beampos |>
      map(\(y) {
        if (substring(line, y, y) == ".") {
          substring(newline, y) <<- "|"
          y
        } else {
          if (substring(line, y, y) == "^") {
            {
              substring(newline, y + 1) <<- "|"
              substring(newline, y - 1) <<- "|"
              splits <<- splits + 1
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

## Part Two
forks <- data <- data |>
  discard_at(1) |>
  imap(\(line, i) {
    forks <- (line |> str_locate_all("\\^"))[[1]][, 1]
    print(forks)
  })
