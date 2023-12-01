library(dplyr)
library(stringi)
library(stringr)
library(rlang)


# Part 1 ------------------------------------------------------------------

tibble(text = readLines("input/day1.txt")) |>
  mutate(first_num = str_extract(text, "\\d"),
         last_num = str_extract(stri_reverse(text), "\\d"),
         cal_val = as.numeric(paste0(first_num, last_num))) |>
  summarise(tot = sum(cal_val))


# Part 2 ------------------------------------------------------------------

nums <- c("one", "two", "three",
          "four", "five", "six",
          "seven", "eight", "nine")
replace_num <- c("on1e", "tw2o", "thr3ee",
                         "fo4ur", "fi5ve", "si6x",
                         "se7ven", "ei8ght", "ni9ne")

test <- tibble(text = str_split_1("two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen", "\\n"))

data <- tibble(text = readLines("input/day1.txt"))

for(i in c(1:9)){
  data <- data |>
    mutate(text := str_replace_all(text, nums[[i]],
                                           replace_num[[i]]
                                           ))
}

data|>
  mutate(first_num = str_extract(text, "\\d"),
         last_num = str_extract(stri_reverse(text), "\\d"),
         cal_val = as.numeric(paste0(first_num, last_num))) |>
  summarise(tot = sum(cal_val))

