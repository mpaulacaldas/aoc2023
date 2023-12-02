library(tidyverse)

input <- read_file("input/day02.txt")

parse_games <- function(input) {
  input |>
    str_remove_all("Game ") |>
    read_delim(
      delim = ": ",
      col_names = c("game", "record"),
      col_types = c("d", "c")
    ) |>
    separate_rows(record, sep = "; ") |>
    group_by(game) |>
    mutate(set = row_number()) |>
    ungroup() |>
    separate_rows(record, sep = ", ") |>
    separate(record, c("value", "colour"), convert = TRUE)
}

sum_id_valid_games <- function(input) {

  config <- tibble(red = 12, green = 13, blue = 14) |>
    pivot_longer(everything(), names_to = "colour", values_to = "max_value")

  input |>
    parse_games() |>
    left_join(config, by = "colour") |>
    mutate(is_valid = value <= max_value) |>
    group_by(game) |>
    summarise(is_valid = all(is_valid)) |>
    summarise(sum(game * is_valid)) |>
    pull()
}

sum_power_games <- function(input) {
  input |>
    parse_games() |>
    group_by(game, colour) |>
    summarise(value = max(value), .groups = "drop_last") |>
    summarise(power = reduce(value, `*`), .groups = "drop_last") |>
    summarise(sum(power)) |>
    pull()
}

sum_id_valid_games(input)
sum_power_games(input)
