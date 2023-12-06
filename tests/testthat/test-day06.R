test_that("multiplication works", {
  example <- glue::glue(
    "Time:      7  15   30
     Distance:  9  40  200"
    )
  input <- read_file("input/day06.txt")

  expect_equal(ways_to_win(example), 288)
  expect_equal(ways_to_win(input), 281600)

  expect_equal(ways_to_win(example, fix_kerning = TRUE), 71503)
  expect_equal(ways_to_win(input, fix_kerning = TRUE), 33875953)
})
