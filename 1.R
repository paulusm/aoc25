library(purrr)
library(readr)
library(stringr)

data <- read_lines(file="data/1-test.txt")
pos <- 50
zeropasses <- 0

pt1 <- data |> map(\(x) {
  instruction <- x |> str_match("([L|R])(\\d*)")
  amount <- as.integer(instruction[1,3])
  pos <<- ifelse(instruction[1,2] == "R", (pos + amount) %% 100, (pos - amount) %% 100)
  zeropasses <<- zeropasses +  ifelse(instruction[1,2] == "R", as.integer((pos + amount) / 100), pos > 0 & pos - amount < 0)

  pos
}) |> reduce(\(x,y)  ifelse(y == 0, x + 1, x),.init = 0)

print(pt1)
print(pt1 + zeropasses)
