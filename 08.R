library(readr)
library(purrr)
library(dplyr)
library(igraph)

data <- read_csv(file = "data/8.txt", col_names = F)
data$id <- as.integer(rownames(data))

distances <- data |> cross_join(data)

distances <- distances |>
  mutate(
    distance = sqrt((X1.x - X1.y)^2 + (X2.x - X2.y)^2 + (X3.x - X3.y)^2)
  ) |>
  filter(distance > 0)

wired <- distances |>
  slice_min(n = 2000, order_by = distance) |>
  rename(from = id.x, to = id.y) |>
  select(from, to)

circuits <- wired |>
  graph_from_data_frame(directed = F)

comps <- (components(circuits)$csize)
comps |>
  tibble() |>
  arrange(desc(comps)) |>
  slice_head(n = 3) |>
  summarise(prod(comps))
