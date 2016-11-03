context('02 utils functions') 
 
test_that('df_to_matrix', {
  # data
  df <- data.frame(i = c(1, 1, 3), j = c(1, 3, 3), v = c(10, 20, 30))
  df2 <- data.frame(a = c(1, 1), b = c(1, 2), c = c(5, 25))
  df3 <- data.frame(i = c(1, NA, 3), j = c(1, 3, 3), v = c(10, 20, 30))
  df4 <- data.frame(i = c(1, 1, 3), j = c(NA, 3, 3), v = c(10, 20, 30))
  df5 <- data.frame(i = c(1, 1, 3), j = c(1, 3, 3), v = c(10, 20, NA))
  # correct results
  mtx <- matrix(c(10, 0, 20, 0, 0, 0, 0, 0, 30), ncol=3, nrow=3, byrow=TRUE)
  mtx2 <- matrix(c(10, 0, 20, 0, 0, 0, 20, 0, 30), ncol=3, nrow=3, byrow=TRUE)
  mtx5 <- matrix(c(10, 0, 20, 0, 0, 0, 0, 0, NA), ncol=3, nrow=3, byrow=TRUE)
  # tests
  expect_equal(as.matrix(df_to_matrix(df)), mtx)
  expect_equal(as.matrix(df_to_matrix(df, sparse = FALSE)), mtx)
  expect_equal(as.matrix(df_to_matrix(df, sparse = FALSE, add_lower = TRUE)), mtx2)
  expect_error(df_to_matrix(df[,1]))
  expect_error(df_to_matrix(df[,-1:-3]))
  expect_error(df_to_matrix(df3))
  expect_error(df_to_matrix(df4))
  expect_equal(as.matrix(df_to_matrix(df5)),mtx5)
})

test_that('is_integer', {
  expect_true(is_integer(1:5))
  expect_true(is_integer(c(1, 2, 3, 4, 5)))
  expect_false(is_integer(c(0.3, 0.8, 5, 1.666)))
  expect_false(is_integer(c(3, 1e-10)))
  expect_false(is_integer(letters))
})
