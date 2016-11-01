context('minsetcover_model') 
 
test_that('numeric input', {
  # data
  targets <- c(1, 10)
  cost <- 1:4
  locked_in <- 2
  locked_out <- 4
  rij <- data.frame(feature=c(1L,1L,2L, 2L,2L), pu=c(1:2, 2:4), amount=c(1,1,10,10,10))
  # generate object
  ms1 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_in=locked_in, locked_out=locked_out, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_in=locked_in, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, targets=targets, rij=rij, locked_out=locked_out, target_type='absolute') 
  # tests
  expect_equal(ms1$cost, cost)
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(data.frame(feature=ms1$rij$i,pu=ms1$rij$j,amount=ms1$rij$v), rij)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms1$targets, targets)

  expect_equal(ms2$cost, cost)
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(data.frame(feature=ms2$rij$i,pu=ms2$rij$j,amount=ms2$rij$v), rij)
  expect_equal(ms2$included, TRUE)
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, cost)
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(data.frame(feature=ms3$rij$i,pu=ms3$rij$j,amount=ms3$rij$v), rij)
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
  rij <- data.frame(feature=c(1L,2L,1L,2L), pu=c(1L,2L,3L,3L), amount=c(1,10,1,10)) # planning unit for is excluded because it has NA cost
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  # generate object
  ms1 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, targets=targets, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, features=features1, locked_out=locked_out, targets=targets, target_type='absolute') 
  # tests
  expect_equal(ms1$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(data.frame(feature=ms1$rij$i,pu=ms1$rij$j,amount=ms1$rij$v), rij)
  expect_equal(ms1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ms1$targets, targets)
  
  expect_equal(ms2$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(data.frame(feature=ms2$rij$i,pu=ms2$rij$j,amount=ms2$rij$v), rij)
  expect_equal(ms1$included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, as.vector(na.omit(raster::getValues(cost))))
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(data.frame(feature=ms3$rij$i,pu=ms3$rij$j,amount=ms3$rij$v), rij)
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
  features1 <- raster::stack(raster::raster(matrix(c(1,1,0,0), ncol=2)), raster::raster(matrix(c(0,10,10,10), ncol=2)))
  # generate object
  ms1 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, locked_out=locked_out, targets=targets, target_type='absolute')
  ms2 <- minsetcover_model(x=cost, features=features1, locked_in=locked_in, targets=targets, target_type='absolute')
  ms3 <- minsetcover_model(x=cost, features=features1, locked_out=locked_out, targets=targets, target_type='absolute') 
  # tests
  expect_equal(ms1$cost, cost$cost)
  expect_equal(ms1$locked_in, locked_in)
  expect_equal(ms1$locked_out, locked_out)
  expect_equal(data.frame(feature=ms1$rij$i,pu=ms1$rij$j,amount=ms1$rij$v), rij)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms1$targets, targets)

  expect_equal(ms2$cost, cost$cost)
  expect_equal(ms2$locked_in, locked_in)
  expect_equal(ms2$locked_out, integer())
  expect_equal(data.frame(feature=ms2$rij$i,pu=ms2$rij$j,amount=ms2$rij$v), rij)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms2$targets, targets)

  expect_equal(ms3$cost, cost$cost)
  expect_equal(ms3$locked_in, integer())
  expect_equal(ms3$locked_out, locked_out)
  expect_equal(data.frame(feature=ms3$rij$i,pu=ms3$rij$j,amount=ms3$rij$v), rij)
  expect_equal(ms1$included, TRUE)
  expect_equal(ms3$targets, targets)

  expect_error(minsetcover_model(x=cost, features=features, targets=100, target_type='absolute')) # targets is too low to reach any target
  expect_error(minsetcover_model(x=raster::setValues(cost, NA), features=features, targets=NA, target_type='absolute')) # targets is not finite
  expect_error(minsetcover_model(x=cost, features=raster::setValues(features, NA), targets=targets, target_type='absolute')) # there are no features in the problem
  expect_error({cost2 <- cost; cost2$data[4] <- NA; minsetcover_model(x=cost, features=features, targets=targets, target_type='absolute')}) # cost has NA value 
  
})

