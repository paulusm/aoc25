library(readr)
library(purrr)
library(stringr)
library(tidyr)

data <- read_lines(file = "data/7.txt")
splits <- 0
startpos <- list(str_locate(data[[1]], "S")[1, 1])
beampos <- startpos

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

print(paste("Part1: ", splits))

## Part Two

forks <- data |>
  discard_at(1) |>
  imap(\(line, i) {
    (line |> str_locate_all("\\^"))[[1]][, 1]
  }) |>
  keep(\(x) {
    all(length(x) > 0)
  })


findQuantumPaths <- function(junctions, level, pos, pathsCount) {
  if (level == length(junctions)) {
    return(
      pathsCount
    )
  }
  currentForks <- junctions[[level]]
  return(
    currentForks |>
      map(\(x) {
        nextpos <- as.numeric(x)
        diff <- nextpos - pos
        if (abs(diff) == 1) {
          pathsCount <<- pathsCount + 1
          findQuantumPaths(forks, level + 1, nextpos, pathsCount)
        } else {
          {
            0
          }
        }
      }) |>
      reduce(sum)
  )
}
print(paste(
  "Part 2:",
  findQuantumPaths(forks, 2, startpos[[1]], 0) +
    length(forks[[length(forks)]]) -
    length(forks[[length(forks) - 1]])
))
