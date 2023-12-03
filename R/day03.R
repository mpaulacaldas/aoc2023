find_adj_numbers <- function(input) {

  loc_lines <- tibble(line = input) |>
    mutate(row = row_number())

  loc_special <- input |>
    str_locate_all("[^A-Za-z0-9\\.]") |>
    map_dfr(as_tibble, .id = "row") |>
    mutate(row = as.integer(row)) |>
    left_join(loc_lines, by = "row") |>
    mutate(start = start - 1, end = end + 1)

  loc_numbers <- input |>
    str_locate_all("[0-9]+") |>
    map_dfr(as_tibble, .id = "row") |>
    mutate(
      row = as.integer(row),
      row_before = row - 1L,
      row_after = row + 1L,
    ) |>
    left_join(loc_lines, by = "row") |>
    rowwise() |>
    mutate(number = as.numeric(str_sub(line, start, end))) |>
    ungroup()

  loc_filtered <- loc_special |>
    left_join(
      loc_numbers,
      by = join_by(
        # keep the numbers that appear in adjacent rows
        between(x$row, y$row_before, y$row_after),
        # and those whose column locations overlap
        overlaps(x$start, x$end, y$start, y$end)
      ),
      suffix = c("_sp", "_nb")
    )

  answer <- loc_filtered |>
    pull(number) |>
    sum()
}