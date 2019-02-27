context("Test parse_prior()")

test_that("start_n works", {
  expect_equal(parse_prior("10", "start_n"), 10L)
  expect_equal(parse_prior("789", "start_n"), 789L)
})

test_that("end_n works", {
  expect_equal(parse_prior("10", "end_n"), 10L)
  expect_equal(parse_prior("789", "end_n"), 789L)
})

test_that("groups_n works", {
  expect_equal(parse_prior("10", "groups_n"), 10L)
  expect_equal(parse_prior("789", "groups_n"), 789L)
})

# test_that("start_id works", {
#   expect_equal(parse_prior("start_id", "C1"), "C1")
#   expect_equal(parse_prior("start_id", "A,B,C"), c("A", "B", "C"))
#
#   fi <- tempfile()
#   write_lines(c("A", "B", "C"), fi)
#   parse_prior("start_id", fi)
# })
