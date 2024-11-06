## Tests for `make_bread()`

test_that("error returned if inputs are not numeric", {
  expect_error(make_bread(grains = "a",
                          yeast = 2,
                          water = 3,
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = "b",
                          water = 3,
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = 2,
                          water = "c",
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = 2,
                          water = 3,
                          salt = "d"))
  expect_error(make_bread(grains = TRUE,
                          yeast = 2,
                          water = 3,
                          salt = 4))
})

test_that("error returned if inputs are not scalar", {
  expect_error(make_bread(grains = "1",
                          yeast = 2,
                          water = 3,
                          salt = 4))
  expect_error(make_bread(grains = c(1, 2),
                          yeast = 2,
                          water = 3,
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = c(2, 3),
                          water = 3,
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = 2,
                          water = c(3, 4),
                          salt = 4))
  expect_error(make_bread(grains = 1,
                          yeast = 2,
                          water = 3,
                          salt = c(4, 5)))
})

test_that("Output is numeric", {
  expect_type(make_bread(grains = 1,
                         yeast = 2,
                         water = 3,
                         salt = 4), "double")
})


## Tests for `make_focaccia()`

test_that("error returned if inputs are not numeric", {
  expect_error(make_focaccia(grains = "a",
                             yeast = 2,
                             water = 3,
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = "b",
                             water = 3,
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = 2,
                             water = "c",
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = 2,
                             water = 3,
                             salt = "d"))
  expect_error(make_focaccia(grains = TRUE,
                             yeast = 2,
                             water = 3,
                             salt = 4))
})

test_that("error returned if inputs are not scalar", {
  expect_error(make_focaccia(grains = "1",
                             yeast = 2,
                             water = 3,
                             salt = 4))
  expect_error(make_focaccia(grains = c(1, 2),
                             yeast = 2,
                             water = 3,
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = c(2, 3),
                             water = 3,
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = 2,
                             water = c(3, 4),
                             salt = 4))
  expect_error(make_focaccia(grains = 1,
                             yeast = 2,
                             water = 3,
                             salt = c(4, 5)))
})

test_that("Output is numeric", {
  expect_type(make_focaccia(grains = 1,
                            yeast = 2,
                            water = 3,
                            salt = 4), "double")
})
