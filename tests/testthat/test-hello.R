test_that("hello returns greeting with default name", {
  expect_equal(hello(), "Hello, Eclipse!")
})

test_that("hello returns greeting with provided name", {
  expect_equal(hello("World"), "Hello, World!")
})
