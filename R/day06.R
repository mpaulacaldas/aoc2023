ways_to_win <- function(input, fix_kerning = FALSE) {

  info <- read_lines(input) |>
    str_extract_all("\\d+") |>
    map(\(x) if (fix_kerning) paste0(x, collapse = "") else x) |>
    map(as.numeric)

  time <- info[[1]]
  dist <- info[[2]]

  time |>
    map(\(x) (1:(x - 1)) * ((x-1):1)) |>
    map2(dist, \(x, y) sum(x > y)) |>
    reduce(`*`)
}
