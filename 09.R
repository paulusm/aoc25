library(readr)
library(dplyr)

coords <- read_csv(file = "data/9-test.txt", col_names = c("x", "y"))

combis <- coords |> cross_join(coords)
combis <- combis |>
  mutate(area = (abs(x.x - x.y) + 1) * (abs(y.x - y.y) +1))

# Part 1
combis |>
  slice_max(order_by = area, n = 1) |>
  slice(1)|>
  print()

# Part 2

isGreen <- function(a,b){
  lims <- coords |> filter(x <= a ) |> slice(1) |>
    rbind(coords |> filter(x >= a )|> slice(1)) |>
    rbind(coords |> filter(y <= b )|> slice(1)) |>
    rbind(coords |> filter(y >= b )|> slice(1)) |>
    nrow()
    print(lims)
  return(lims > 3)kk
}

combis |> rowwise() |>
  mutate(isGreen =  isGreen(x.x, y.y) & isGreen(x.y,y.x))|> 
  filter(isGreen == T) |>
  ungroup() |>
  slice_max(order_by = area, n = 1) |>
  print()

