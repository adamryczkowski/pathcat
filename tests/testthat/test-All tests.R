context("All tests")

## TODO: Rename context
## TODO: Add more tests

test_that("Simple path concatenation", {
  expect_equal(path.cat("/home","adam"),"/home/adam")
})

test_that("Simple for .. in path", {
  expect_equal(path.cat("/home/adam/.local",".."),"/home/adam")
})

test_that("Simple test for making relative paths", {
  expect_equal(make.path.relative("/home","/home/adam"),"adam")
})

test_that("Test for making relative paths that require '..'", {
  expect_equal(make.path.relative("/home/zosia","/home/adam"),"../adam")
})

test_that("Test for empty paths", {
  expect_equal(make.path.relative("","/home/adam"),"/home/adam")
})

test_that("Corner case for 2 identical paths in make.path.relative", {
  expect_equal(make.path.relative("/home/adam","/home/adam"),"")
})

test_that("Relative paths", {
  expect_equal(path.cat("","home/adam"),"home/adam")
})

test_that("Relative paths", {
  expect_equal(path.cat("..","home/adam"),"../home/adam")
})


test_that("Relative paths", {
  expect_equal(path.cat("root","..", "home/adam"),"home/adam")
})

test_that("Relative paths", {
  expect_equal(path.cat("","..", "home/adam"),"../home/adam")
})

test_that("Relative paths", {
  expect_equal(path.cat("..","..", "home/adam"),"../../home/adam")
})

test_that("Relative paths", {
  expect_equal(path.cat("","../adam"),"../../home/adam")
})
