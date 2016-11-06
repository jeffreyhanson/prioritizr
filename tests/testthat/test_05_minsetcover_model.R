context('05 minsetcover_model')

test_that('numeric input', {
  # data
  targets <- c(1, 10)
  cost <- 1:4
  locked_in <- 2
  locked_out <- 4
  rij <- data.frame(feature=c(1L,1L,2L, 2L,2L), pu=c(1:2, 2:4), amount=c(1,1,10,10,10))
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  # generate object
  ms1 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_in=locked_in, locked_out=locked_out, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_in=locked_in, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_out=locked_out, target_type='absolute')
  # tests
  expect_equal(ms1$cost, cost)
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(as.matrix(ms1$rij), rij_mat)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms1$targets, targets)

  expect_equal(ms2$cost, cost)
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(as.matrix(ms2$rij), rij_mat)
  expect_equal(ms2$included, TRUE)
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, cost)
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(as.matrix(ms3$rij), rij_mat)
  expect_equal(ms3$included, TRUE)
  expect_equal(ms3$targets, targets)

  expect_error(minsetcover_model(x=cost, rij=rij, targets=c(100,100), target_type='absolute')) # targets is too high to reach any target
  expect_error(minsetcover_model(x=c(NA, NA, NA, NA), rij=rij, targets=targets, target_type='absolute')) # targets are not finite
  expect_error(minsetcover_model(x=cost, rij=rij, targets=c(NA, NA), target_type='absolute')) # targets are not finite
  expect_error(minsetcover_model(x=cost, rij=rij[0,], targets=targets, target_type='absolute')) # there are no features in the problem
})

test_that('RasterLayer input', {
  # data
  targets <- c(1, 10)
  cost <- raster::raster(matrix(c(1,2,3,NA), ncol=2))
  locked_in <- 2
  locked_out <- 1
  locked_in_rst <- raster::raster(matrix(c(FALSE,FALSE,TRUE,FALSE), ncol=2))
  locked_out_rst <- raster::raster(matrix(c(TRUE,FALSE,FALSE,FALSE), ncol=2))  
  rij <- data.frame(feature=c(1L,2L,1L,2L), pu=c(1L,2L,3L,3L), amount=c(1,10,1,10)) # planning unit for is excluded because it has NA cost
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
   
  # generate object
  ms1 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, targets=targets, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, features=features1, locked_out=locked_out, targets=targets, target_type='absolute')
  # tests
  expect_equal(ms1$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(as.matrix(ms1$rij), rij_mat)
  expect_equal(ms1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ms1$targets, targets)
  expect_equal(ms1, minsetcover_model(x=cost, features=features2, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute'))
  expect_equal(ms1, minsetcover_model(x=cost, features=features1, locked_in=locked_in_rst, locked_out=locked_out_rst, targets=targets, target_type='absolute'))
  
  expect_equal(ms2$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(as.matrix(ms2$rij), rij_mat)
  expect_equal(ms1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(as.matrix(ms3$rij), rij_mat)
  expect_equal(ms1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ms3$targets, targets)

  expect_error(minsetcover_model(x=cost, features=features, targets=100, target_type='absolute')) # targets is too low to reach any target
  expect_error(minsetcover_model(x=cost, features=features, targets=-1, target_type='absolute')) # targets is too low to reach any target
  expect_error(minsetcover_model(x=raster::setValues(cost, NA), features=features, targets=NA, target_type='absolute')) # targets is not finite
  expect_error(minsetcover_model(x=cost, features=raster::setValues(features, NA), targets=targets)) # there are no features in the problem
})

test_that('SpatialPolygons input', {
  # data
  cost <- raster::rasterToPolygons(raster::raster(matrix(1:4, ncol=2)))
  names(cost@data) <- 'cost'
  targets <- c(1, 10)
  locked_in <- 2
  locked_out <- 4
  rij <- data.frame(feature=c(1L,1L,2L, 2L,2L), pu=c(1,3,2,3,4), amount=c(1,1,10,10,10))
  rij_mat <- as.matrix(slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount))
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  features2 <- raster::stack(raster::raster(matrix(c(1,1,NA,NA), ncol=2)), raster::raster(matrix(c(NA,10,10,10), ncol=2)))
  # generate object
  ms1 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, targets=targets, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, features=features1, locked_out=locked_out, targets=targets, target_type='absolute')
  # tests
  expect_equal(ms1$cost, cost$cost)
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(as.matrix(ms1$rij), rij_mat)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms1$targets, targets)
  
  expect_equal(ms1, minsetcover_model(x=cost, features=features2, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute'))
  
  expect_equal(ms2$cost, cost$cost)
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(as.matrix(ms2$rij), rij_mat)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, cost$cost)
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(as.matrix(ms3$rij), rij_mat)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms3$targets, targets)

  expect_error(minsetcover_model(x=cost, features=features, targets=100, target_type='absolute')) # targets is too low to reach any target
  expect_error(minsetcover_model(x=raster::setValues(cost, NA), features=features, targets=NA, target_type='absolute')) # targets is not finite
  expect_error(minsetcover_model(x=cost, features=raster::setValues(features, NA), targets=targets, target_type='absolute')) # there are no features in the problem
  expect_error({cost2 <- cost; cost2$data[4] <- NA; minsetcover_model(x=cost, features=features, targets=targets, target_type='absolute')}) # cost has NA value

})

