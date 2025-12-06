library(readr)
library(purrr)
library(stringr)

data <- read_lines(file = "data/6.txt") |>
  map(\(x) x |> str_split_1(" ")) |>
  map(\(x) x |> discard(\(z) z == "")) |>
  as_tibble(, .name_repair = "unique") |>
  mutate(across(!last_col(), as.integer)) |>
  rename("operation" = last_col()) |>
  rowwise() |>
  mutate(
    tot = case_when(
      operation == "+" ~ sum(c_across(!last_col())),
      operation == "*" ~ prod(c_across(!last_col()))
    )
  ) |>
  ungroup() |>
  summarise(grandtot = sum(tot))

print(data$grandtot)
