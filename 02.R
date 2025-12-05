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

result <- 0

input[[1]] |>
  as.list() |>
  map(\(x) {
    lims <- str_split(x, "-")
    seq(as.numeric(lims[[1]][1]), as.numeric(lims[[1]][2])) |>
      map(\(y) {
        codeaschar <- as.character(y)
        thelen <- nchar(codeaschar)
        # 2, 3, 4 and 5 length repeats
        match <- F
        seq(1:(thelen / 2)) |>
          map(\(a) {
            if (!match) {
              seq(2:(thelen - a + 2)) |>
                map(\(b) {
                  candidate <- rep(substring(codeaschar, 1, a), b) |>
                    str_flatten()
                  if (candidate == codeaschar) {
                    #print(paste("gotcha", codeaschar, candidate))
                    result <<- result + y
                    match <<- T
                  }
                })
            }
          })
      })
  })

# 36855614794 too low
# 36862281460 too high

#test 4174379265
print(paste("Part2 ", result))
