test_that("failing test", {
  stop("A failure in a test", call. = FALSE)
})

test_that("failing expectation", {
  expect_true(FALSE)
})
