library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
txt <- readLines("input/day3.txt")
# txt <- "467..114..
# ...*......
# ..35..633.
# ......#...
# 617*......
# .....+.58.
# ..592.....
# ......755.
# ...$.*....
# .664.598.." |>
#   str_split_1("\\n")


numbers <- tibble(num_info = map(txt, function(x) {
  tibble(
    num = str_extract_all(x, "\\d+") |> unlist()
  ) |>
    bind_cols(str_locate_all(x, "\\d+"))
})) |>
  mutate(row = row_number()) |>
  unnest(num_info) |>
  mutate(x_range = map2(start, end, seq),
         part = FALSE)

symbols <- tibble(num_info = map(txt, function(x) {
  tibble(
    sym = str_extract_all(x, "[^A-Za-z0-9.]") |> unlist()
  ) |>
    bind_cols(str_locate_all(x, "[^A-Za-z0-9.]"))
})) |>
  mutate(row = row_number()) |>
  unnest(num_info) |>
  select(-end) |>
  mutate(
    n = map2(start, row, \(x, y) c(x, y - 1)),
    ne = map2(start, row, \(x, y) c(x + 1, y - 1)),
    e = map2(start, row, \(x, y) c(x + 1, y)),
    se = map2(start, row, \(x, y) c(x + 1, y + 1)),
    s = map2(start, row, \(x, y) c(x, y + 1)),
    sw = map2(start, row, \(x, y) c(x - 1, y + 1)),
    w = map2(start, row, \(x, y) c(x - 1, y)),
    nw = map2(start, row, \(x, y) c(x - 1, y - 1))
  )

dirct_check <- function(df, vec){
  df |>
    mutate(
      part =
             if_else(row == vec[2]& unlist(map(x_range, \(x) vec[1] %in% x)),
                     TRUE,
                     part))
}

for (i in seq(1, nrow(symbols))) {
  check <- symbols[i, ]
  numbers <- dirct_check(numbers, check$n[[1]]) |>
    dirct_check(check$ne[[1]]) |>
    dirct_check(check$e[[1]]) |>
    dirct_check(check$se[[1]]) |>
    dirct_check(check$s[[1]]) |>
    dirct_check(check$sw[[1]]) |>
    dirct_check(check$w[[1]]) |>
    dirct_check(check$nw[[1]])
}
numbers |>
  filter(part) |>
  summarise(sum(as.numeric(num)))


# Part 2 ------------------------------------------------------------------

gears <- symbols |>
  filter(sym == "*")


tot <- 0
for(i in seq(1, nrow(gears))){
  check <- gears[i, ]
  adj_num <- dirct_check(numbers, check$n[[1]]) |>
    dirct_check(check$ne[[1]]) |>
    dirct_check(check$e[[1]]) |>
    dirct_check(check$se[[1]]) |>
    dirct_check(check$s[[1]]) |>
    dirct_check(check$sw[[1]]) |>
    dirct_check(check$w[[1]]) |>
    dirct_check(check$nw[[1]]) |>
    filter(part == TRUE)
  if(nrow(adj_num) == 2){
    tot = tot + prod(as.numeric(adj_num$num))
  }
}
tot
