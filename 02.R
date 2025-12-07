library(readr)
library(purrr)
result <- 0

input <- read_lines(file = "data/2.txt") |> str_split(",")

# input[[1]] |>
#   as.list() |>
#   map(\(x) {
#     lims <- str_split(x, "-")
#     seq(as.numeric(lims[[1]][1]), as.numeric(lims[[1]][2])) |>
#       map(\(y) {
#         codeaschar <- as.character(y)
#         thelen <- nchar(codeaschar)
#         if (thelen %% 2 == 0) {
#           if (
#             substring(codeaschar, 1, thelen / 2) ==
#               substring(codeaschar, thelen / 2 + 1, thelen)
#           ) {
#             result <<- result + y
#           }
#         }
#       })
#   })

# print(paste("Part1 ", result))

result <- input[[1]] |>
  as.list() |>
  map(\(x) {
    lims <- str_split(x, "-")
    seq(as.numeric(lims[[1]][1]), as.numeric(lims[[1]][2])) |>
      map(\(y) {
        if (!is.na(str_match(y, "(^\\d{1,5})\\1+$") |> pluck(1))) {
          y
        }
      })
  }) |>
  flatten() |>
  reduce(sum)


# 36855614794 too low
# 36862281460 too high

#test 4174379265
print(paste("Part2 ", result))
