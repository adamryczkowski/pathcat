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
  expect_equal(path.cat("","../adam"),"../adam")
})

test_that("Multiple paths", {
  testthat::expect_equivalent(
    make.path.relative('/home/adam/Videos/tmp', c('/home/adam/Videos/tmp/Jednorazowe', '/home/adam/Videos/tmp/Tutoriale/SciShow', '/home/adam/Videos/Zosia')),
    c('Jednorazowe', 'Tutoriale/SciShow', '../Zosia'))

  testthat::expect_equivalent(
    path.cat('bla',c('adam','mi', 'w'), fsep='/'),
    c('bla/adam', 'bla/mi', 'bla/w')
  )

})

test_that("Strange paths", {
  testthat::expect_equal(path.cat("","adam", '/', 'a'), '/a')
})

test_that("Test for skipping coma", {
  testthat::expect_equal(path.cat("","adam", '.', 'a'), 'adam/a')
})
