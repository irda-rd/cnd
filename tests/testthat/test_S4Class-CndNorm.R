context("S4Class-CndNorm")
#--------------------------------------------------------------------
#Defining data
##Adequate cases
location <- 1:3
scatter <- matrix(1:9, 3, 3)
location_int <- 1:3
scatter_int <- matrix(1L:9L, 3, 3)
location_null <-numeric(0)
scatter_null <- matrix(numeric(0), 0, 0)

##Problematic cases
location_long <- c(location, 1)
location_character <- as.character(location)
scatter_character <- matrix(LETTERS[1L:9L], 3, 3)
scatter_df <- as.data.frame(scatter)
location_df <- as.data.frame(location)
scatter_vector <- location
location_matrix <- scatter
#--------------------------------------------------------------------
test_that("adequate objects can be constructed", {
  expect_silent(CndNorm(location = location, scatter = scatter))
  expect_silent(CndNorm(location = location_int, scatter = scatter_int))
  expect_silent(CndNorm(location = location_null, scatter = scatter_null))
})
#--------------------------------------------------------------------
test_that("objects with inadequate dimension cannot be constructed", {
  expect_error(CndNorm(location = location_long, scatter = scatter))
})
#--------------------------------------------------------------------

test_that("objects with inadequate class cannot be constructed", {
  expect_error(CndNorm(location = location_character, scatter = scatter))
  expect_error(CndNorm(location = location, scatter = scatter_character))
  expect_error(CndNorm(location = location_df, scatter = scatter))
  expect_error(CndNorm(location = location, scatter = scatter_df))
  expect_error(CndNorm(location = location, scatter = scatter_vector))
  expect_error(CndNorm(location = location_matrix, scatter))
})
#--------------------------------------------------------------------
