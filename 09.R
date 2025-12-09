library(readr)
library(dplyr)

coords <- read_csv(file = "data/9.txt", col_names = c("x", "y"))

combis <- coords |> cross_join(coords)
combis <- combis |>
  mutate(x_diff = x.x^2 - x.y^2, y_diff = y.x^2 - y.y^2) |>
  mutate(
    best_diff = (y_diff^2 - x_diff^2)
  )

combis |>
  slice_max(order_by = best_diff, n = 1) |>
  mutate(
    area = (max(c(x.x, x.y)) -
      min(c(x.x, x.y)) +
      1) *
      (max(c(y.x, y.y)) -
        min(c(y.x, y.y)) +
        1)
  ) |>
  slice(1) |>
  print()

# 4666698220
