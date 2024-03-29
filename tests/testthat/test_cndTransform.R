context("cndTransform")
#--------------------------------------------------------------------
#General data
##CndData
n <- 50L
yield <- 100 + rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)%%4+1]
cndData <- CndData(yield = yield, X = X, label = label)

##CndMethod
cndMethod <- cndMethodRobustIrda(dropNutrient = "x3")

#--------------------------------------------------------------------
test_that("the function return an object with the appropriate class", {
  transfData <- cndTransform(cndData, cndMethod)
  expect_true(is(transfData, "CndData"))
})
#--------------------------------------------------------------------
