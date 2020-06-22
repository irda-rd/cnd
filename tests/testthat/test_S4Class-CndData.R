context("S4Class-CndData")
#--------------------------------------------------------------------
#Adequate objects section
##Defining data
n <- 20
yield1 <- rnorm(n)
yield2 <- data.frame(yield = yield1)
X1 <- data.frame(x1 = runif(n), x2 = runif(n), x3 = runif(n))

label1 <- LETTERS[1:n]
label2 <- character(0)
label3 <- data.frame()
label4 <- data.frame(label1 = label1, label2 = 1:n)

test_that("adequate objects can be constructed (label)", {
  expect_silent(CndData(yield = yield1, X = X1, label = label1))
  expect_silent(CndData(yield = yield1, X = X1, label = label2))
  expect_silent(CndData(yield = yield1, X = X1, label = label3))
  expect_silent(CndData(yield = yield1, X = X1, label = label4))
  expect_silent(CndData(yield = yield1, X = X1))
})

test_that("adequate objects can be constructed (yield)", {
  expect_silent(CndData(yield = yield2, X = X1, label = label1))
})
#--------------------------------------------------------------------
#Undequate objects section
##Additional data
yield0 <- data.frame()
X0 <- data.frame()
yield_long <- c(yield1,1)
label_long <- LETTERS[seq(1, n+1)]
yield_doubled <- data.frame(yield1 = yield1, yield2 = yield1)
yield_character <- as.character(yield1)
X_character <- X1; X_character[[1]]<-as.character(X_character[[1]])
X_na <- X1; X_na[1,1] <- NA
yield_na <- yield1; yield_na[2] <- NA

test_that("inadequate objects cannot be constructed (number of lines)", {
  expect_error(CndData(yield = yield_long, X = X1))
  expect_error(CndData(yield = yield1, X = X1, label = label_long))
  expect_error(CndData(yield = yield_doubled, X = X1, label = label))
})

test_that("inadequate objects cannot be constructed (number of columns)", {
  expect_error(CndData(yield = yield_doubled, X = X1, label = label))
})

test_that("inadequate objects cannot be constructed (numeric classes)", {
  expect_error(CndData(yield = yield_character, X = X1))
  expect_error(CndData(yield = yield1, X = X_character))
})

test_that("inadequate objects cannot be constructed (NA in yield or X)", {
  expect_error(CndData(yield = yield_na, X = X1))
  expect_error(CndData(yield = yield1, X = X_na))
})
#--------------------------------------------------------------------
#Section on conformity option
##Additional data
X_coercion <- X_character
X_coercion[1,1] <- "A"
yield_coercion <- yield_character
yield_coercion[1] <- "A"

test_that("conformity option properly convert as numeric", {
  expect_silent(CndData(yield = yield_character, X = X_character, conformClass = TRUE))
  
})
test_that("conformity option properly remove NA", {
  expect_silent(CndData(yield = yield_na, X = X_na, removeNA = TRUE))
  
  cndData <- CndData(yield = yield_na, X = X_na, removeNA = TRUE)
  expect_true(nrow(cndData@yield == n-2) & nrow(cndData@X == n-2))
})
test_that("conformity option return warning if coercion as NA", {
  expect_warning(CndData(yield = yield_coercion, X = X1, conformClass = TRUE,removeNA = TRUE))
  expect_warning(CndData(yield = yield1, X = X_coercion, conformClass = TRUE, removeNA = TRUE))
})
#--------------------------------------------------------------------