ways_to_win <- function(input) {

  info <- read_lines(input) |>
    str_extract_all("\\d+") |>
    map(as.numeric)

  time <- info[[1]]
  dist <- info[[2]]

  hold  <- map(time, \(x) 1:(x - 1))
  left  <- map(time, \(x) (x-1):1)
  map2(hold, left, \(x, y) x * y) |>
    map2(dist, \(x, y) sum(x > y)) |>
    reduce(`*`)
}
