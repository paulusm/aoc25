library(purrr)
library(readr)
library(stringr)

data <- read_lines(file="data/1.txt")
pos <- 50
zeropasses <- 0

pt1 <- data |> map(\(x) {
  instruction <- x |> str_match("([L|R])(\\d*)")
  amount <- as.integer(instruction[1,3])
  cycles <- as.integer(amount / 100)
  remainder <- amount - cycles * 100
  zeropasses <<- zeropasses + cycles + ifelse(instruction[1,2] == "R", (pos + remainder) > 100, (pos!=0) & (pos - remainder < 0) ) 
  pos <<- ifelse(instruction[1,2] == "R", (pos + amount) %% 100, (pos - amount) %% 100)
 
  pos
  }) |> reduce(\(x,y)  ifelse(y == 0, x + 1, x),.init = 0)


print(paste("Part One:", pt1))
print(paste("Part Two:", pt1 + zeropasses))
