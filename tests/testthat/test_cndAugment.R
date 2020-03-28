context("CndAugment")
library(dplyr)
#--------------------------------------------------------------------
#General data
n <- 20
yield1 <- rnorm(n)
X1 <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label1 <- LETTERS[1:n]
other1 <- data.frame(value = rnorm(n))
suppl1 <- list(message = "test")
cndData <- CndData(yield = yield1, X = X1, label = label1)

test_that("the function return the same as direct assignation (CndData)", {
  #Direct assignation
  answer <- CndDataAugmented(yield = yield1, X = X1, label = label1, other = other1, suppl = suppl1)

  #Test for equality between assignation and augmentation
  expect_identical(cndAugment(cndData, other = other1, suppl = suppl1), answer)
})
#--------------------------------------------------------------------
#Supplementary data
other2 <- data.frame(value2 = rnorm(n))
suppl2 <- list(message = "test")
cndDataAugmented <- CndDataAugmented(yield = yield1, X = X1, label = label1, other = other1, suppl = suppl1)

test_that("the function return the same as direct assignation (CndDataAugmented)", {
  #Establish further entries
  answer <- CndDataAugmented(yield = yield1, X = X1, label = label1, other = bind_cols(other1, other2), suppl = c(suppl1, suppl2))
  #Test for equality between assignation and augmentation
  expect_identical(cndAugment(cndDataAugmented, other = other2, suppl = suppl2), answer)
})
#--------------------------------------------------------------------
test_that("the function does not throw an error if the parameter other and suppl have default values (cndDataAugmented)", {
  cndData <- CndData(yield = yield1, X = X1, label = label1)
  expect_silent(cndAugment(cndData))
})
#--------------------------------------------------------------------
#Suplementary data
other_long <- data.frame(value = rnorm(n+1))

test_that("the function throw an error if other is not of the proper dimension (cndData)", {
  expect_error(cndAugment(cndData, other = other_long, suppl = suppl1))
  expect_error(cndAugment(cndDataAugmented, other = other_long, suppl = suppl1))
})
#--------------------------------------------------------------------
#Suplementary data
other_vector <- rnorm(n)

test_that("the function throw an error if other is not a data.frame (cndData)", {
  expect_error(cndAugment(cndData, other = other_vector, suppl = suppl1))
  expect_error(cndAugment(cndDataAugmented, other = other_vector, suppl = suppl1))
})
#--------------------------------------------------------------------
#Suplementary data
cndDataAugmented_dupl <- cndAugment(cndDataAugmented, other = other1, suppl = suppl1)
test_that("the function rename the added columns if columns names already exist (cndDataAugmented, other)", {
  other_duplicated <- any(duplicated(colnames(cndDataAugmented_dupl@other)))
  expect_true(!other_duplicated)
})
#--------------------------------------------------------------------
#Problem not covered
#test_that("the function rename the added columns if columns names already exist (cndDataAugmented, suppl)", {
#  suppl_duplicated <- any(duplicated(names(cndDataAugmented_dupl@suppl)))
#  expect_true(!suppl_duplicated)
#})
#--------------------------------------------------------------------


