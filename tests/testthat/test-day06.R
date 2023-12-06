test_that("multiplication works", {
  example <- glue::glue(
    "Time:      7  15   30
     Distance:  9  40  200"
    )

  expect_equal(ways_to_win(example), 288)

})
