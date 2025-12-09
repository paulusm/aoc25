library(readr)
library(dplyr)

coords <- read_csv(file = "data/9.txt", col_names = c("x", "y"))

combis <- coords |> cross_join(coords)
combis <- combis |>
  mutate(area = (abs(x.x - x.y) + 1) * (abs(y.x - y.y) +1))

combis |>
  slice_max(order_by = area, n = 1) |>
  slice(1)|>
  print()
