parse_seeds <- function(input, expand = FALSE) {
  seeds <- input |>
    readr::read_lines(n_max = 1) |>
    str_extract_all("\\d+") |>
    unlist() |>
    as.numeric()
  if (expand) {
    start <- seeds[seq_along(seeds) %% 2 == 1]
    range <- seeds[seq_along(seeds) %% 2 == 0]
    seeds <- map2(start, range, \(x, y) seq(x, x + y - 1)) |>
      unlist() |>
      unique()
  }
  seeds
}

parse_almanac <- function(input) {
  almanac <- tibble(line = readr::read_lines(input, skip = 1)) |>
    filter(line != "") |>
    mutate(
      map_specs = str_extract(line, "\\d.*"),
      step = cumsum(ifelse(is.na(map_specs), 1, 0))
    ) |>
    select(-line) |>
    filter(!is.na(map_specs)) |>
    separate(
      map_specs,
      c("dest", "source", "range"),
      sep = " ",
      convert = TRUE
    )
  split(almanac, almanac$step)
}

map_seeds <- function(seeds = NULL, dest, source, range) {
  map_seed <- function(seed, dest, source, range) {
    if (seed < source || seed > (source + range - 1)) return(NA)
    dest + (seed - source)
  }
  map_dbl(seeds, \(x) map_seed(x, dest = dest, source = source, range = range))
}

map_seeds_cumulatively <- function(seeds, params) {
  # message(unique(params$step))
  # message("seed start: ", paste0(seeds, collapse = ", "))
  seeds <- params |>
    rowwise() |>
    mutate(seeds_temp = list(map_seeds(seeds, dest, source, range))) |>
    ungroup() |>
    pull(seeds_temp) |>
    append(list(seeds)) |>
    reduce(coalesce)
  # message("seed end:", paste0(seeds, collapse = ", "), "\n")
  return(seeds)
}

get_lowest_location <- function(input, expand = FALSE) {
  steps <- parse_almanac(input)
  seeds <- parse_seeds(input, expand = expand)
  locations <- reduce(
    steps,
    map_seeds_cumulatively,
    .init = seeds
  )
  min(locations)
}
