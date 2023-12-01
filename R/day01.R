library(tidyverse)

input <- read_lines("input/day01.txt")

# Part 1 ------------------------------------------------------------------

add_digits <- function(input) {
  input |>
    str_extract_all("\\d") |>
    map_chr(\(x) paste0(x[1], x[length(x)])) |>
    as.numeric() |>
    sum()
}

add_digits(input)


# Part 2 ------------------------------------------------------------------

digits  <- xfun::numbers_to_words(1:9)

str_extract_all2 <- function(x) {
  str_extract_all_ <- function(x) {
    stopifnot(length(x) == 1L)
    patterns <- c("\\d", digits)
    matches  <- map(patterns, \(p) str_extract_all(x, p))
    order    <- map(patterns, \(p) str_locate_all(x, p)[[1]][, "start"])
    m <- unlist(matches)
    o <- unlist(order) |> setNames(m)
    names(sort(o))
  }
  purrr::map(x, \(.x) str_extract_all_(.x))
}

words_to_number <- function(x) {
  words_to_number_ <- function(x) {
    stopifnot(length(x) == 1L)
    num <- which(digits == x)
    if (length(num) > 0L) as.character(num) else x
  }
  map_chr(x, words_to_number_)
}

add_digits2 <- function(input) {
  input |>
    str_extract_all2() |>
    map(words_to_number) |>
    map_chr(\(x) paste0(x[1], x[length(x)])) |>
    as.numeric() |>
    sum()
}

add_digits2(input)
