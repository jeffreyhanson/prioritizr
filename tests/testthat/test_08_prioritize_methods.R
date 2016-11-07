context('08 prioritize methods')

test_that('set_targets', {
  # data
  e <- raster::extent(0, 100, 0, 100)
  r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
  r_rare <- gaussian_field(r, range = 20, n = 1, prop = 0.1)
  r_common <- gaussian_field(r, range = 20, n = 1, prop = 0.5)
  r_features <- raster::stack(r_rare, r_common)
  names(r_features) <- letters[1:2]
  # tests
  expect_equal(set_targets(r_features, 0.2),
               `names<-`(raster::cellStats(r_features, 'sum')*0.2,
                         c('a', 'b')))
  expect_error(set_targets(r_features, c(0.1,NA)),
               info='non-finite proportion')
  expect_error(set_targets(r_features, rep(0.1,3)),
               info='wrong number of targets')
  expect_error(set_targets(r_features, c(0.1,-0.1)),
               info='negative target')
  expect_error(set_targets(r_features, c(0.5,2)),
               info='proportion greater than 100%')
})

test_that('plot_selection', {
  # data
  e <- raster::extent(0, 100, 0, 100)
  pu_raster <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
  x_selected <- as.vector(gaussian_field(pu_raster, 20, prop = 0.25)[])
  # test if plot is succefully made
  plot_selection(pu_raster, x_selected)
})

test_that('summarize_features', {
  # data
  r_features <- raster::stack(raster::raster(matrix(1:4, ncol=2)),
                              raster::raster(matrix(c(0,0,1,1), ncol=2)))
  names(r_features) <- letters[1:2]
  sq_grid <- as(r_features[[1]], 'SpatialPolygons')
  solution <- data.frame(feature=c(rep(1,4), rep(2,2)), pu=c(1:4, c(2,4)),
                         amount=c(c(1,3,2,4), 1, 1))
  solution <- dplyr::arrange(solution, feature, pu, amount)
  # create objects
  rij_sparse <- summarize_features(sq_grid, r_features)
  rij_matrix <- summarize_features(sq_grid, r_features, sparse = FALSE)
  rij_df <- summarize_features(sq_grid, r_features, matrix = FALSE)
  rownames(rij_df) <- NULL
  # tests
  expect_equal(
    dplyr::arrange(
      data.frame(feature=rij_sparse$i, pu=rij_sparse$j,amount=rij_sparse$v),
      feature, pu, amount
    ),
    solution)
  expect_equal(as.matrix(rij_sparse), rij_matrix)
  expect_equal(dplyr::arrange(rij_df, feature, pu, amount), solution)
  expect_equal(summarize_features(sq_grid,
                                  raster::setValues(r_features, NA),
                                  sparse=FALSE),
               matrix(0, ncol=4, nrow=2),
               info='no features occur in the study area')
  expect_error(summarize_features(sq_grid[0,], r_features),
               info='no polygons in spatial polygons object')
})
