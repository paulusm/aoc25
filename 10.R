library(readr)
library(dplyr)
library(purrr)
library(stringr)

input <- read_lines(file = "data/10.txt")

data <- input |>
  map(\(x) {
    parse <- str_match(
      x,
      "\\[([\\.|\\#)]*)\\] ([\\([0-9|,]+\\) ]+) \\{([[0-9]*|,]*)\\}"
    )
    list(
      str_split_1(parse[2], "") == "#",
      str_split_1(parse[4], " "),
      str_split(parse[3], " ") |>
        pluck(1) |>
        map(\(y) {
          match <- str_match(y, "\\(([0-9|,]+)\\)")
          match[2] |>
            unlist() |>
            str_split_1(",") |>
            as.integer() |>
            map_vec(~ .x + 1)
        })
    )
  })

pressButtons <- function(states, target, buttons, its) {
  #print(paste(paste(target, collapse=",") , "iteration", its))
  match <- F
  states <- states |>
    map(\(state) {
      candidate <- state
      #print(candidate)
      buttons |>
        map(\(button) {
          candidate[button] <- !candidate[button]
          if (identical(candidate, target)) {
            match <<- T
          }
          candidate
        })
    }) |>
    flatten()
  if (match) {
    return(its)
  } else {
    #print(paste("states length", length(states)))
    return(pressButtons(states, target, buttons, its + 1))
  }
}

data |>
  map(\(x) {
    its <- 1
    pattern <- x |> pluck(1)
    base <- rep(F, length(pattern))
    pressButtons(list(base), pattern, x |> pluck(3), 1)
  }) |>
  reduce(sum) |>
  print()
