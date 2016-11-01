context('prioritize') 

run_maxcover_tests <- function(solver='best') {
  # data
  budget <- 4.23
  cost <- raster::raster(matrix(c(1,2,2,NA), ncol=2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2,1,1,0), ncol=2)), raster::raster(matrix(c(10,10,10,10), ncol=2)))
  # generate object
  prb <- maxcover_model(x=cost, features=features, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=c(2,10))
  # generate result
  sol <- prioritize(prb, solver=solver)
  # tests
  expect_equal(sol$objval, 22) # correct amount held
  expect_equal(sol$x, c(0,1,1,NA)) # correct solution
}

run_minsetcover_tests <- function(solver='best') {
  # data
  cost <- raster::raster(matrix(c(1,2,2,NA), ncol=2))
  locked_in <- 2
  locked_out <- 1
  features <- raster::stack(raster::raster(matrix(c(2,1,1,0), ncol=2)), raster::raster(matrix(c(10,10,10,10), ncol=2)))
  # generate object
  prb <- minsetcover_model(x=cost, features=features, locked_in=locked_in, locked_out=locked_out, targets=c(2,10), target_type='absolute')  
  # generate result
  sol <- prioritize(prb, solver=solver)
  # tests
  expect_equal(sol$objval, 4) # correct expenditure
  expect_equal(sol$x, c(0,1,1,NA)) # correct solution
}


test_that('prioritize (best solver)', {
  skip_on_cran()
  run_minsetcover_tests(solver='best')
  run_maxcover_tests(solver='best')
})

test_that('prioritize_gurobi.maxcover_model', {
  skip_if_not_installed('gurobi')
  run_maxcover_tests(solver='gurobi')
})

test_that('prioritize_gurobi.minsetcover_model', {
  skip_if_not_installed('gurobi')
  run_minsetcover_tests(solver='gurobi')
})

test_that('prioritize_symphony.maxcover_model', {
  skip_if_not(requireNamespace("Rsymphony", quietly = TRUE) | requireNamespace("lpsymphony", quietly = TRUE))
  run_maxcover_tests(solver='symphony')

})

test_that('prioritize_symphony.minsetcover_model', {
  skip_if_not(requireNamespace("Rsymphony", quietly = TRUE) | requireNamespace("lpsymphony", quietly = TRUE))
  run_minsetcover_tests(solver='symphony')
})

test_that('prioritize_glpk.maxcover_model', {
  skip_if_not_installed('glpkAPI')
  run_maxcover_tests(solver='glpk')

})

test_that('prioritize_glpk.minsetcover_model', {
  skip_if_not_installed('glpkAPI')
  run_minsetcover_tests(solver='glpk')
})

