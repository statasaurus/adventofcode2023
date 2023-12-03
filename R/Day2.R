library(dplyr)
library(stringr)
library(purrr)
txt = readLines("input/day2.txt")
# txt = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |>
#   str_split_1("\\n")


# Part 1 ------------------------------------------------------------------

tibble(game = str_extract(txt, "(?<=^Game )\\d+") |> as.numeric(),
      red = str_extract_all(txt, "\\d+(?= red)") |>
        map(\(x) as.numeric(x)|> max()) |> unlist(),
      green = str_extract_all(txt, "\\d+(?= green)") |>
        map(\(x) as.numeric(x)|> max()) |> unlist(),
      blue = str_extract_all(txt, "\\d+(?= blue)") |>
        map(\(x) as.numeric(x)|> max()) |> unlist(),
      test = red <= 12 & green <= 13 & blue <= 14
      ) |>
  filter(test) |>
  summarise(sum = sum(game))


# Part 2 ------------------------------------------------------------------

tibble(game = str_extract(txt, "(?<=^Game )\\d+") |> as.numeric(),
       red = str_extract_all(txt, "\\d+(?= red)") |>
         map(\(x) as.numeric(x)|> max()) |> unlist(),
       green = str_extract_all(txt, "\\d+(?= green)") |>
         map(\(x) as.numeric(x)|> max()) |> unlist(),
       blue = str_extract_all(txt, "\\d+(?= blue)") |>
         map(\(x) as.numeric(x)|> max()) |> unlist(),
       power = red * green * blue
) |>
  summarise(sum = sum(power))
