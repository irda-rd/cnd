context("S4Class-CndAugmentedData")
#--------------------------------------------------------------------
#Adequate objects section
##Defining data
n <- 20
yield1 <- rnorm(n)
X1 <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label1 <- LETTERS[1:n]

other <- data.frame(value = rnorm(n))
other_long <- data.frame(value = rnorm(n+1))
other_vector <- rnorm(n)
suppl <- list(message = "test")

test_that("adequate objects can be constructed", {
  expect_silent(CndDataAugmented(yield = yield1, X = X1, label = label1, other = other, suppl = suppl))
  expect_silent(CndDataAugmented(yield = yield1, X = X1, label = label1, other = other))
  expect_silent(CndDataAugmented(yield = yield1, X = X1, label = label1, suppl = suppl))
  expect_silent(CndDataAugmented(yield = yield1, X = X1, label = label1))
  expect_silent(CndDataAugmented())
})

test_that("unadequate objects cannot be constructed (class)", {
  expect_error(CndDataAugmented(yield = yield_long, X = X1, label = label1, other = other_vector))
})
test_that("unadequate objects cannot be constructed (dimension)", {
  expect_error(CndDataAugmented(yield = yield_long, X = X1, label = label1, other = other_long))
})
