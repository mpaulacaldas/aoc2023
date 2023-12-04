add_points_scratchcards <- function(input) {
  input |>
    str_remove_all("Card\\s+\\d+\\: ") |>
    read_delim(
      col_names = c("winning", "mine"),
      col_types = cols(.default = col_character()),
      delim = " | ") |>
    mutate(card = row_number()) |>
    mutate(across(
      c(winning, mine),
      \(x) str_extract_all(x, "\\d+")
    )) |>
    rowwise() |>
    mutate(n = sum(mine %in% winning)) |>
    ungroup() |>
    mutate(points = ifelse(n > 0, 2 ^ (n - 1), 0)) |>
    summarise(sum(points)) |>
    pull()
}
