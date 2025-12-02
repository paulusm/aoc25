library(tidyverse)

data <- read_lines(file="data/1-test.txt")
pos <- 50

data |> map(\(x) {
  instruction <- x |> str_match("([L|R])(\\d*)")
  amount <- as.integer(instruction[1,3])
  pos <<- ifelse(instruction[1,2] == "R", (pos + amount) %% 100, (pos - amount) %% 100)
  pos
}) |> reduce(\(x,y) y <- ifelse(y == 0, x + 1, x))
