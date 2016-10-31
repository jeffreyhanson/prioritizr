context('prioritize methods')

test_that('set_targets', {
  # data
  e <- raster::extent(0, 100, 0, 100)
  r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
  r_rare <- gaussian_field(r, range = 20, n = 1, prop = 0.1)
  r_common <- gaussian_field(r, range = 20, n = 1, prop = 0.5)
  r_features <- raster::stack(r_rare, r_common)
  names(r_features) <- letters[1:2]
  # tests
  expect_equal(set_targets(r_features, 0.2), `names<-`(cellStats(r_features, 'sum')*0.2, c('a', 'b')))
  expect_error(set_targets(r_features, c(0.1,NA))) # non-finite proportion
  expect_error(set_targets(r_features, rep(0.1,3))) # wrong number of targets
  expect_error(set_targets(r_features, c(0.1,-0.1))) # negative target
  expect_error(set_targets(r_features, c(0.5,2))) # proportion greater than 100%
})

test_that('plot_selection', {
  # data
  e <- raster::extent(0, 100, 0, 100)
  pu_raster <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
  x_selected <- as.vector(gaussian_field(pu_raster, 20, prop = 0.25)[])
  # test if plot is succefully made
  plot_selection(pu_raster, x_selected)
  dev.off()
})

test_that('summarize_features', {
  # data
  r_features <- raster::stack(raster::raster(matrix(1:4, ncol=2)),
                              raster::raster(matrix(c(0,0,1,1), ncol=2)))
  names(r_features) <- letters[1:2]
  sq_grid <- make_grid(raster::setValues(r_features[[1]], 1), cell_width = 0.5, type = "square")
  solution <- data.frame(feature=c('a', 'a', 'a', 'b', 'b'), pu=c(1:4, 3:4), amount=c(1:4, 1, 1))
  # create objects
  rij_sparse <- summarize_features(hex_grid, r_features)
  rij_matrix <- summarize_features(hex_grid, r_features, sparse = FALSE)
  rij_df <- summarize_features(hex_grid, r_features, matrix = FALSE)
  # tests
  expect_equal(data.frame(feature=rij_sparse$i, pu=rij_sparse$j, amount=rij_sparse$v), solution)
  expect_equal(data.frame(feature=rij_matrix[,1], pu=rij_matrix[,2], amount=rij_matrix[,3]), solution)
  expect_equal(rij_df, solution)
  expect_error(summarize_features(hex_grid[0,], r_features)) # no polygons in spatial polygons object
  expect_error(summarize_features(hex_grid, raster::setValues(r_features, 0))) # no features occur in the study area  
})

