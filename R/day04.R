parse_scratchcards <- function(input) {
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
    ungroup()
}

add_points_scratchcards <- function(input) {
  input |>
    parse_scratchcards() |>
    mutate(points = ifelse(n > 0, 2 ^ (n - 1), 0)) |>
    summarise(sum(points)) |>
    pull()
}

count_copies_scratchcards <- function(input) {
  df <- input |>
    parse_scratchcards() |>
    mutate(copy = 1)
  for (i in 1:(nrow(df) - 1)) {
    m <- (i + 1)
    n <- pmin(i + df$n[i], nrow(df))
    df$copy[m:n] <- df$copy[m:n] + ((df$n[i] > 0) * df$copy[i])
  }
  sum(df$copy)
}
