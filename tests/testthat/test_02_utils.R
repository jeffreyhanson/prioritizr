context('utils functions') 
 
test_that('df_to_matrix', {
  
















})

test_that('is_integer', {
  expect_true(is_integer(1:5))
  expect_true(is_integer(1, 2, 3, 4, 5))
  expect_false(is_integer(c(0.3, 0.8, 5, 1.666)))
  expect_false(is_integer(c(3, 1e-10)))
})

