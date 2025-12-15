library(readr)
library(dplyr)
library(purrr)
library(stringr)

input <- read_lines(file = "data/10-test.txt")

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
          match[2] |> unlist() |> str_split_1(",") |> as.integer() |> map_vec(~ .x + 1)
        })
    )
  })

pressButtons <- function(state, target, buttons){
  candidate <- state
  buttons |> map(\(button){
    candidate[button] <- !candidate[button]
    print(candidate)
    if(identical(candidate,target)){
      print("match")
      return(target)
    }
  })
  return(target)
}


data |> map(\(x){
  its <- 1
  pattern <- x |> pluck(1)
  base <- rep(F,length(pattern))
  while(!identical(pattern, base)){
    base <- pressButtons(base, pattern, x |> pluck(3))
    its <<- its + 1
  }
  its
})

