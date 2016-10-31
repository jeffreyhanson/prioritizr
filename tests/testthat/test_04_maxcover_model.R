context('maxcover_model') 
 
test_that('numeric input', {
  # data
  budget <- 1.23
  cost <- 1:4
  locked.in <- 2
  locked.out <- 4
  rij <- data.frame(feature=c(1L,1L,2L, 2L,2L), pu=c(1:2, 2:4), amount=c(1,1,10,10,10))
  # generate object
  mc1 <- maxcover_model(x=cost, rij=rij, locked.in=locked.in, locked.out=locked.out, budget=budget)
  mc2 <- maxcover_model(x=cost, rij=rij, locked.in=locked.in, budget=budget)
  mc3 <- maxcover_model(x=cost, rij=rij, locked.out=locked.out, budget=budget) 
  # tests
  expect_equal(mc1$cost, cost)
  expect_equal(mc1$locked.in, locked.in)
  expect_equal(mc1$locked.out, locked.out)
  expect_equal(data.frame(feature=mc1$rij$i,pu=mc1$rij$j,amount=mc1$rij$v), rij)
  expect_equal(mc1$included, TRUE)
  expect_equal(mc1$budget, budget)

  expect_equal(mc2$cost, cost)
  expect_equal(mc2$locked.in, locked.in)
  expect_equal(mc2$locked.out, integer())
  expect_equal(data.frame(feature=mc2$rij$i,pu=mc2$rij$j,amount=mc2$rij$v), rij)
  expect_equal(mc2$included, TRUE)
  expect_equal(mc2$budget, budget)

  expect_equal(mc3$cost, cost)
  expect_equal(mc3$locked.in, integer())
  expect_equal(mc3$locked.out, locked.out)
  expect_equal(data.frame(feature=mc3$rij$i,pu=mc3$rij$j,amount=mc3$rij$v), rij)
  expect_equal(mc3$included, TRUE)
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, rij=rij, budget=0.5)) # budget is too low to reach any target
  expect_error(maxcover_model(x=c(NA, NA, NA, NA), rij=rij, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, rij=rij[0,], budget=budget)) # there are no features in the problem
})

test_that('RasterLayer input', {
  # data
  budget <- 1.23
  cost <- raster::raster(matrix(c(1,2,3,NA), ncol=2))
  locked.in <- 2
  locked.out <- 4
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
  # generate object
  mc1 <- maxcover_model(x=cost, features=features1, locked.in=locked.in, locked.out=locked.out, budget=budget)
  mc2 <- maxcover_model(x=cost, features=features1, locked.in=locked.in, budget=budget)
  mc3 <- maxcover_model(x=cost, features=features1, locked.out=locked.out, budget=budget) 
  # tests
  # generate object
  mc1 <- maxcover_model(x=cost, rij=rij, locked.in=locked.in, locked.out=locked.out, budget=budget)
  mc2 <- maxcover_model(x=cost, rij=rij, locked.in=locked.in, budget=budget)
  mc3 <- maxcover_model(x=cost, rij=rij, locked.out=locked.out, budget=budget) 
  # tests
  expect_equal(mc1$cost, cost)
  expect_equal(mc1$locked.in, locked.in)
  expect_equal(mc1$locked.out, locked.out)
  expect_equal(data.frame(feature=mc1$rij$i,pu=mc1$rij$j,amount=mc1$rij$v), rij)
  expect_equal(mc1$included, TRUE)
  expect_equal(mc1$budget, budget)
  
  expect_equal(mc1, maxcover_model(x=cost, features=features2, locked.in=locked.in, locked.out=locked.out, budget=budget)) # test with NA data instead of zeros

  expect_equal(mc2$cost, cost)
  expect_equal(mc2$locked.in, locked.in)
  expect_equal(mc2$locked.out, integer())
  expect_equal(data.frame(feature=mc2$rij$i,pu=mc2$rij$j,amount=mc2$rij$v), rij)
  expect_equal(mc2$included, TRUE)
  expect_equal(mc2$budget, budget)

  expect_equal(mc3$cost, cost)
  expect_equal(mc3$locked.in, integer())
  expect_equal(mc3$locked.out, locked.out)
  expect_equal(data.frame(feature=mc3$rij$i,pu=mc3$rij$j,amount=mc3$rij$v), rij)
  expect_equal(mc3$included, TRUE)
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, features=features, budget=100)) # budget is too low to reach any target
  expect_error(maxcover_model(x=raster::setValues(cost, NA), features=features, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, features=raster::setValues(features, NA), budget=budget)) # there are no features in the problem
})

test_that('SpatialPolygons input', {
  # data
  cost <- raster::rasterToPolygons(raster::raster(matrix(1:4, ncol=2)))
  locked.in <- 2
  locked.out <- 4
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
  # generate object
  mc1 <- maxcover_model(x=cost, features=features1, locked.in=locked.in, locked.out=locked.out, budget=budget)
  mc2 <- maxcover_model(x=cost, features=features1, locked.in=locked.in, budget=budget)
  mc3 <- maxcover_model(x=cost, features=features1, locked.out=locked.out, budget=budget) 
  # tests
  expect_equal(mc1$cost, cost)
  expect_equal(mc1$locked.in, locked.in)
  expect_equal(mc1$locked.out, locked.out)
  expect_equal(data.frame(feature=mc1$rij$i,pu=mc1$rij$j,amount=mc1$rij$v), rij)
  expect_equal(mc1$included, TRUE)
  expect_equal(mc1$budget, budget)

  expect_equal(mc1, maxcover_model(x=cost, features=features2, locked.in=locked.in, locked.out=locked.out, budget=budget)) # test with NA data instead of zeros
  
  expect_equal(mc2$cost, cost)
  expect_equal(mc2$locked.in, locked.in)
  expect_equal(mc2$locked.out, integer())
  expect_equal(data.frame(feature=mc2$rij$i,pu=mc2$rij$j,amount=mc2$rij$v), rij)
  expect_equal(mc2$included, TRUE)
  expect_equal(mc2$budget, budget)

  expect_equal(mc3$cost, cost)
  expect_equal(mc3$locked.in, integer())
  expect_equal(mc3$locked.out, locked.out)
  expect_equal(data.frame(feature=mc3$rij$i,pu=mc3$rij$j,amount=mc3$rij$v), rij)
  expect_equal(mc3$included, TRUE)
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, features=features, budget=100)) # budget is too low to reach any target
  expect_error(maxcover_model(x=raster::setValues(cost, NA), features=features, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, features=raster::setValues(features, NA), budget=budget)) # there are no features in the problem
})

