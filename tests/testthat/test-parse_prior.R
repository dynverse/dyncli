context("Test parse_prior()")

test_that("start_n works", {
  expect_equal(parse_prior("start_n", "10"), 10L)
  expect_equal(parse_prior("start_n", "789"), 789L)
})

test_that("end_n works", {
  expect_equal(parse_prior("end_n", "10"), 10L)
  expect_equal(parse_prior("end_n", "789"), 789L)
})

test_that("groups_n works", {
  expect_equal(parse_prior("groups_n", "10"), 10L)
  expect_equal(parse_prior("groups_n", "789"), 789L)
})

test_that("start_id works", {
  expect_equal(parse_prior("start_id", "C1"), "C1")
  expect_equal(parse_prior("start_id", "A,B,C"), c("A", "B", "C"))

  fi <- tempfile()
  write_lines(c("A", "B", "C"), fi)
  parse_prior("start_id", fi)
})
