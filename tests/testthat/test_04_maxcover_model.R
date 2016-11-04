context('04 maxcover_model')

test_that('numeric input', {
  # data
  budget <- 3.23
  targets <- c(2, 10)
  cost <- 1:4
  locked_in <- 2
  locked_out <- 4
  rij <- data.frame(feature=c(1L,1L,2L, 2L,2L), pu=c(1:2, 2:4), amount=c(1,1,10,10,10))
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  # generate object
  mc1 <- maxcover_model(x=cost, rij=rij, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  mc2 <- maxcover_model(x=cost, rij=rij, locked_in=locked_in, budget=budget, targets=targets, target_type="absolute")
  mc3 <- maxcover_model(x=cost, rij=rij, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  # tests
  expect_equal(mc1$cost, cost)
  expect_equal(mc1$locked_in, locked_in)
  expect_equal(mc1$locked_out, locked_out)
  expect_equal(as.matrix(mc1$rij), rij_mat)
  expect_equal(mc1$included, TRUE)
  expect_equal(mc1$budget, budget)

  expect_equal(mc2$cost, cost)
  expect_equal(mc2$locked_in, locked_in)
  expect_equal(mc2$locked_out, integer())
  expect_equal(as.matrix(mc2$rij), rij_mat)
  expect_equal(mc2$included, TRUE)
  expect_equal(mc2$budget, budget)

  expect_equal(mc3$cost, cost)
  expect_equal(mc3$locked_in, integer())
  expect_equal(mc3$locked_out, locked_out)
  expect_equal(as.matrix(mc3$rij), rij_mat)
  expect_equal(mc3$included, TRUE)
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, rij=rij, budget=0.5)) # budget is too low to reach any target
  expect_error(maxcover_model(x=c(NA, NA, NA, NA), rij=rij, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, rij=rij[0,], budget=budget)) # there are no features in the problem
})

test_that('RasterLayer input', {
  # data
  budget <- 3.23
  targets <- c(2, 10)
  cost <- raster::raster(matrix(c(1,2,3,NA), ncol=2))
  locked_in <- 2
  locked_out <- 3
  locked_in_rst <- raster::raster(matrix(c(FALSE,FALSE,TRUE,FALSE), ncol=2))
  locked_out_rst <- raster::raster(matrix(c(FALSE,TRUE,FALSE,FALSE), ncol=2))  
  rij <- data.frame(feature=c(1L,2L,1L, 2L), pu=c(1L,2L,3L,3L), amount=c(1,10,1,10))
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
  # generate object
  mc1 <- maxcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  mc2 <- maxcover_model(x=cost, features=features1, locked_in=locked_in, budget=budget, targets=targets, target_type="absolute")
  mc3 <- maxcover_model(x=cost, features=features1, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  # tests
  expect_equal(mc1$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(mc1$locked_in, locked_in)
  expect_equal(mc1$locked_out, locked_out)
  expect_equal(as.matrix(mc1$rij), rij_mat)
  expect_equal(mc1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(mc1$budget, budget)
  expect_equal(mc1, maxcover_model(x=cost, features=features2, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute"))
  expect_equal(mc1, maxcover_model(x=cost, features=features1, locked_in=locked_in_rst, locked_out=locked_out_rst, budget=budget, targets=targets, target_type="absolute"))
  
  expect_equal(mc2$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(mc2$locked_in, locked_in)
  expect_equal(mc2$locked_out, integer())
  expect_equal(as.matrix(mc2$rij), rij_mat)
  expect_equal(mc2$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(mc2$budget, budget)
  
  expect_equal(mc3$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(mc3$locked_in, integer())
  expect_equal(mc3$locked_out, locked_out)
  expect_equal(as.matrix(mc3$rij), rij_mat)
  expect_equal(mc3$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, features=features, budget=100)) # budget is too low to reach any target
  expect_error(maxcover_model(x=raster::setValues(cost, NA), features=features, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, features=raster::setValues(features, NA), budget=budget)) # there are no features in the problem
})

test_that('SpatialPolygons input', {
  # data
  budget <- 3.23
  targets <- c(2, 10)
  cost <- raster::rasterToPolygons(raster::raster(matrix(c(1:3, NA), ncol=2)))
  names(cost@data) <- 'cost'
  locked_in <- 2
  locked_out <- 3
  rij <- data.frame(feature=c(1L,1L,2L,2L), pu=c(1L,3L,2L,3L), amount=c(1,1,10,10))
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
  # generate object
  mc1 <- maxcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  mc2 <- maxcover_model(x=cost, features=features1, locked_in=locked_in, budget=budget, targets=targets, target_type="absolute")
  mc3 <- maxcover_model(x=cost, features=features1, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute")
  # tests
  expect_equal(mc1$cost, as.vector(na.omit(cost$cost)))
  expect_equal(mc1$locked_in, locked_in)
  expect_equal(mc1$locked_out, locked_out)
  expect_equal(as.matrix(mc1$rij), rij_mat)
  expect_equal(mc1$included, TRUE)
  expect_equal(mc1$budget, budget)
  expect_equal(mc1, maxcover_model(x=cost, features=features2, locked_in=locked_in, locked_out=locked_out, budget=budget, targets=targets, target_type="absolute"))

  expect_equal(mc2$cost, as.vector(na.omit(cost$cost)))
  expect_equal(mc2$locked_in, locked_in)
  expect_equal(mc2$locked_out, integer())
  expect_equal(as.matrix(mc2$rij), rij_mat)
  expect_equal(mc2$included, TRUE)
  expect_equal(mc2$budget, budget)

  expect_equal(mc3$cost, as.vector(na.omit(cost$cost)))
  expect_equal(mc3$locked_in, integer())
  expect_equal(mc3$locked_out, locked_out)
  expect_equal(as.matrix(mc3$rij), rij_mat)
  expect_equal(mc3$included, TRUE)
  expect_equal(mc3$budget, budget)

  expect_error(maxcover_model(x=cost, features=features, budget=100)) # budget is too low to reach any target
  expect_error(maxcover_model(x=raster::setValues(cost, NA), features=features, budget=NA)) # budget is not finite
  expect_error(maxcover_model(x=cost, features=raster::setValues(features, NA), budget=budget)) # there are no features in the problem
  expect_error({cost2$cost[4] <- NA; maxcover_model(x=cost2, features=features, budget=budget)}) # cost has NA value
})
