context("CndSubsetData")
#--------------------------------------------------------------------
#General data
n <- 20
yield <- 100 + rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)%%4+1]
data <- CndData(yield = yield, X = X, label = label)
subset = sample(c(TRUE,FALSE), n, replace = TRUE)

test_that("subset reduce the dimension of all slots (cndData)", {
  cndSubset <- cndSubsetData(data, subset)
  answer <- length(which(subset))
  expect_equal(nrow(cndSubset@yield), answer)
  expect_equal(nrow(cndSubset@X), answer)
  expect_equal(nrow(cndSubset@label), answer)
})
#--------------------------------------------------------------------
#Supplementary data
dataAugmented <- cndAugment(data, other = X, suppl = list(comment = "comment"))
test_that("subset reduce the dimension of all slots (cndDataAugmented)", {
  cndSubset <-  cndSubsetData(dataAugmented, subset)
  answer <- length(which(subset))
  expect_equal(nrow(cndSubset@yield), answer)
  expect_equal(nrow(cndSubset@X), answer)
  expect_equal(nrow(cndSubset@label), answer)
})
#--------------------------------------------------------------------
#Supplementary data
data_empty_label <- CndData(yield = yield, X = X)
test_that("subset work with empty label slot (cndData)", {
  cndSubset <-  cndSubsetData(data_empty_label, subset)
  answer <- length(which(subset))
  expect_equal(nrow(cndSubset@yield),answer)
  expect_equal(nrow(cndSubset@X),answer)
  expect_equal(nrow(cndSubset@label),answer)
})
#--------------------------------------------------------------------
#Supplementary data
dataAugmented_empty <- cndAugment(data)

test_that("subset work with empty other slot (cndDataAugmented)", {
  cndSubset <-  cndSubsetData(dataAugmented_empty, subset)
  answer <- length(which(subset))
  expect_equal(nrow(cndSubset@yield),answer)
  expect_equal(nrow(cndSubset@X),answer)
  expect_equal(nrow(cndSubset@label),answer)
})
#--------------------------------------------------------------------
#Supplementary data
subset_na <- sample(c(TRUE,FALSE,NA), n, replace = TRUE)

test_that("NA as subset return an error", {
  expect_error(cndSubsetData(data, subset_na))
  expect_error(cndSubsetData(dataAugmented, subset_na))
})
#--------------------------------------------------------------------
