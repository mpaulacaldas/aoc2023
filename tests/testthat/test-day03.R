test_that("first example works", {

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

})
