library(readr)
library(purrr)
library(dplyr)
library(igraph)

data <- read_csv(file = "data/8-test.txt", col_names = F, show_col_types = F)
circuit_limit <- 10
data$id <- as.integer(rownames(data))

distances <- data |> cross_join(data)

distances <- distances |>
  mutate(
    distance = sqrt((X1.x - X1.y)^2 + (X2.x - X2.y)^2 + (X3.x - X3.y)^2)
  ) |>
  inner_join(data, by = join_by(X1.x == X1, X2.x == X2, X3.x == X3)) |>
  filter(distance > 0)

wired <- distances |>
  filter(X1.x < X1.y) |>
  slice_min(n = circuit_limit, order_by = distance) |>
  mutate(from = id.x, to = (id.y)) |>
  select(from, to)

circuits <- wired |>
  graph_from_data_frame(directed = F)

comps <- (components(circuits)$csize)
pt1 <- comps |>
  tibble() |>
  arrange(desc(comps)) |>
  slice_head(n = 3) |>
  summarise(prod(comps))

print(paste("Part 1:", pt1))

# Part 2
circuits <- graph(directed = F, edges = c())

newGraphItems <- distances |>
  arrange(distance) |>
  filter(X1.x < X1.y) |>
  #slice((circuit_limit + 1):nrow(distances)) |>
  mutate(from = id.x, to = id.y) |>
  filter(distance > 0)

getVertexId <- function(vname) {
  tryCatch(
    {
      return(as.numeric(V(circuits)[[vname]]))
    },
    error = function(msg) {
      return(0)
    }
  )
}

for (i in 1:nrow(newGraphItems)) {
  newFrom <- as.character(newGraphItems[i, "from"])
  newTo <- as.character(newGraphItems[i, "to"])
  if (
    components(circuits)$no != 1 |
      length(V(circuits)) == 0 |
      length(V(circuits)) < 5
  ) {
    if (getVertexId(newFrom) == 0) {
      circuits <- circuits + vertex(newFrom)
    }
    if (getVertexId(newTo) == 0) {
      circuits <- circuits + vertex(newTo)
    }
    circuits <- circuits + edge(newFrom, newTo)
    #print(paste(i, "no of Components", components(circuits)$no))
  } else {
    print(paste("Part 2:", newGraphItems[i, "X1.x"] * newGraphItems[i, "X1.y"]))
    break
  }
}
