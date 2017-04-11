
context("load_or_install")
if(options()$repos["CRAN"] == "@CRAN@")
  options(repos = c(CRAN = "https://cran.rstudio.com"))

test_that("installs not installed package", {

  if("checkpoint" %in% installed.packages()[,1])
    remove.packages("checkpoint")

  expect_true(load_or_install(checkpoint))
  expect_true("checkpoint" %in% loadedNamespaces())

  detach("package:checkpoint")
})

test_that("loads installed packages", {
  expect_false(load_or_install(checkpoint))

  detach("package:checkpoint")
})

test_that("error if package doesn't exist", {
  expect_error(load_or_install(packagedoesntexist))
})

