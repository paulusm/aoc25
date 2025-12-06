library(readr)
library(purrr)
library(stringr)

# Part One
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


#Part 2, not proud of this :()
data <- read_lines(file = "data/6.txt")
symbolpos <- data[[5]] |> str_locate_all("[\\*|\\+]")
symbolpos <- symbolpos[[1]][, 1]

rejigged <- data |>
  map(\(y) {
    symbolpos |>
      map(\(x) {
        if (x > 1) {
          y |> str_sub(start = x - 1, end = x - 1) <<- "|"
        }
      })
    y |> str_split_1("\\|")
  }) |>
  as_tibble(.name_repair = "unique")

stringops <- function(theRow, theOp) {
  runtot <- ""
  for (j in 1:nchar(theRow[1])) {
    theNo <- ""
    for (i in 1:ncol(theRow)) {
      theNo <- paste0(theNo, str_trim((str_sub(theRow[i], j, j))))
    }
    runtot <- paste0(runtot, theNo, str_trim(theOp))
  }
  runtot <- str_sub(runtot, 1, nchar(runtot) - 1)
  return(eval(parse(text = runtot)))
}


gttib <- rejigged |>
  rename("operation" = last_col()) |>
  rowwise() |>
  mutate(
    tot = stringops(across(!last_col()), operation)
  ) |>
  ungroup() |>
  summarise(grandtot = sum(tot))

print(gttib$grandtot)
