test_that("Day 02", {

  example <- glue::glue(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

  expect_equal(sum_id_valid_games(example), 8)
  expect_equal(sum_power_games(example), 2286)

  input <- read_file("input/day02.txt")

  expect_equal(sum_id_valid_games(input), 2176)
  expect_equal(sum_power_games(input), 63700)
})

