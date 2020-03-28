context("cndMcd")
#--------------------------------------------------------------------
#General data
n <- 50L
yield <- 100+rnorm(n)
X <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))
label <- LETTERS[(seq_len(n)-1)%%4+1]
cndData <- CndData(yield = yield, X = X, label = label)
transfData <- cndIlr(cndData)
#--------------------------------------------------------------------
test_that("the function return an object with the appropriate class", {
  #Compute the MCD norm
  cndNorm <- cndMcd(transfData, nSamp = 1000)
  expect_true(is(cndNorm, "McdNorm"))
})
#--------------------------------------------------------------------

