context("cndBind")
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#General data
n <- 20L
yield <- 100 + rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)%%4+1]
data <- CndData(yield = yield, X = X, label = label)

test_that("dimension are respected for CndData", {
  df <- cndBind(data)
  answerCol <- ncol(data@yield) + ncol(data@X) + ncol(data@label)
  answer <- c(n, answerCol)
  expect_equal(dim(df), answer)
})
#--------------------------------------------------------------------
#Supplementary data
other <- data.frame(value = rnorm(n))
suppl <- list(message = "test")
dataAugmented <- cndAugment(data, other = other, suppl = suppl)

test_that("dimension are respected for CndDataAugmented", {
  df <- cndBind(dataAugmented)
  answerCol <- ncol(dataAugmented@yield) + ncol(dataAugmented@X) + ncol(dataAugmented@label) + ncol(dataAugmented@other)
  answer <- c(n, answerCol)
  expect_equal(dim(df), answer)
})
#--------------------------------------------------------------------
#Supplementary data
dataAugmentedEmpty <- cndAugment(data)

test_that("empty other does not affect dimension", {
  df <- cndBind(dataAugmentedEmpty)
  answerCol <- ncol(dataAugmentedEmpty@yield) + ncol(dataAugmentedEmpty@X) + ncol(dataAugmentedEmpty@label)
  answer <- c(n,answerCol)
  expect_equal(dim(df), answer)

})
#--------------------------------------------------------------------
#Supplementary data
dataEmptyLabel <- CndData(yield = yield, X = X)

test_that("empty label does not affect dimension", {
  df <- cndBind(dataEmptyLabel)
  answerCol <- ncol(dataEmptyLabel@yield) + ncol(dataEmptyLabel@X)
  answer <- c(n,answerCol)
  expect_equal(dim(df), answer)

})
#--------------------------------------------------------------------
#Supplementary data
dataAugmentedEmptyAll <- CndDataAugmented()

test_that("empty object return empty data.frame (except for yield)", {
  df <- cndBind(dataAugmentedEmptyAll )
  answerCol <- ncol(dataEmptyLabel@yield)
  answer <- c(0,answerCol)
  expect_equal(dim(df), answer)
})
#--------------------------------------------------------------------
