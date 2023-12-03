# have the tidyverse available interactively
library(tidyverse)

day <- function() {
  d <- format(Sys.Date(), "%d")
  sprintf("day%s", d)
}

aoc_code  <- function() use_r(day())
aoc_test  <- function() use_test(day())
aoc_input <- function(browse = FALSE) {
  p <- fs::path("input", day(), ext = "txt")
  if (browse) file.edit(p) else p
}
