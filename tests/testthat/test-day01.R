test_that("first example works", {

  example1 <- glue::glue(
    "1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet"
  ) |> readr::read_lines()

  expect_equal(add_digits(example1), 142)
})

test_that("second example works", {
  example2 <- glue::glue(
    "two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen"
    ) |>
    read_lines()

  expect_equal(add_digits2(example2), 281)
})
