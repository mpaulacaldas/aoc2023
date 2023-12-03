test_that("Day 03", {

  example <- glue::glue(
    "467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598.."
    ) |>
    read_lines()

  expect_equal(find_adj_numbers(example), 4361)
  expect_equal(find_gear_ratio(example), 467835)


  input <- read_lines("input/day03.txt")

  expect_equal(find_adj_numbers(input), 522726)
  expect_equal(find_gear_ratio(input), 81721933)
})
