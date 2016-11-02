context('misc functions') 

test_that('gaussian_field', {
  # data
  e <- raster::extent(0, 100, 0, 100)
  r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
  # create obejcts
  gf_gaussian <- gaussian_field(r, range = 20, n = 4) # gaussian raster
  gf_binary <- gaussian_field(r, range = 5, n = 1, prop = 0.5) # binary raster
  gf_linear <-gaussian_field(r, range = 20, coef = c(0.05, 0.05)) # linear trend  
  # tests
  expect_true(all(is.finite(c(raster::as.matrix(gf_gaussian)))))
  expect_equal(raster::nlayers((gf_gaussian)), 4L)
  expect_true(all(is.finite(c(raster::as.matrix(gf_binary)))))
  expect_equal(raster::nlayers((gf_binary)), 1L)
  expect_true(all(c(raster::as.matrix(gf_binary)) %in% 0:1))
  expect_true(all(is.finite(c(raster::as.matrix(gf_linear)))))
  expect_equal(raster::nlayers((gf_linear)), 1L)
})

test_that('make_grid', {
  # data
  r1 <- raster::raster(matrix(1:16, 4, 4)) # raster with finite values in all cells
  r2 <- raster::raster(matrix(c(1:15, NA), 4, 4)) # raster with one NA value
  r3 <- raster::raster(matrix(c(1:12, NA, NA, NA, NA), 4, 4)) # raster with one column of NA values
  r4 <- raster::raster(matrix(rep(NA, 16), 4, 4)) # raster with all values as NA
  # define function
  test_that_grid_is_valid_for_raster <- function(sp, rst, type=c('square', 'hexagonal'), label='') {
    type <- match.arg(type)
    label <- paste0(type, ' grid ', label)
    n_finite_cells <- sum(!is.na(raster::as.matrix(rst)))
    expect_equal(
      length(sp@polygons),
      n_finite_cells,
      info=paste0(label, ': grid has incorrect number of grid cells'))
    expect_equal(
      rgeos::gCentroid(sp, byid=TRUE)@coords,
      as.matrix(raster::rasterToPoints(rst, spatial=FALSE)[,1:2]),
      info=paste0(label, ': grid has incorrect location of grid cell centroids'))
    expect_equal(rgeos::gArea(sp, byid=TRUE), rep(prod(raster::res(rst)), n_finite_cells),
                 info=paste0(label, ': grid has cells with incorrect area'))
    expect_equal(
      c(sapply(
        sp@polygons,
        function(x) {
          n.coords <- nrow(x@Polygons[[1]]@coords)-1
          fields::rdist.vec(
            x@Polygons[[1]]@coords[seq_len(n.coords-1),],
            x@Polygons[[1]]@coords[2:n.coords,]
          )
        }
      )),
      ifelse(type=='square', raster::res(rst)[1], sqrt(2 * prod(raster::res(rs)) / sqrt(3))),
      info=paste0(label, ': grid has cells with incorrect widths'))
    expect_equal(
      c(sapply(
        sp@polygons,
        function(x) {return(length(x@Polygons))}
      )),
      rep(1, n_finite_cells),
      info=paste0(label, ': grid has polygons with more than one sub-polygon'))
    expect_equal(
      sapply(sp@polygons, function(x) {nrow(x@Polygons[[1]]@coords)}),
      rep(ifelse(type=='square', 4, 6), n_finite_cells),
      info=paste0(label, ': grid has cells with incorrect number of vertices'))
  }
  
  run_all_tests_on_raster <- function(rst) {
    ## init
    # compute areas and widths
    sq_cell_widths <- c(raster::res(rst)[1], raster::res(rst)[1]/2, raster::res(rst)[1]*2)
    sq_cell_areas <- sq_cell_widths^2
    hex_cell_areas <- sq_cell_areas
    hex_cell_widths <- sqrt(2 * sq_cell_areas / sqrt(3))
    ## make grids
    # square grids
    sq_grids1 <- lapply(sq_cell_widths, function(x) {do.call(make_grid, list(x=rst, type='square', cell_width=x))})
    sq_grids2 <- lapply(sq_cell_areas, function(x) {do.call(make_grid, list(x=rst, type='square', cell_area=x))})
    # hexagon grids
    hex_grids1 <- lapply(sq_cell_areas, function(x) {do.call(make_grid, list(x=rst, type='hexagon', cell_width=x))})
    hex_grids2 <- lapply(sq_cell_areas, function(x) {do.call(make_grid, list(x=rst, type='hexagon', cell_area=x))})
    ## run tests
    # check that geometries are correct
    Map(test_that_grid_is_valid_for_raster, sq_grids1, list(rst, raster::disaggregate(rst, fact=2), raster::aggregate(rst, fact=2)), list('square')[1:3], c('normal', 'missing one cell', 'missing one row', 'all missing'))
    Map(test_that_grid_is_valid_for_raster, hex_grids1, list(rst, raster::disaggregate(rst, fact=2), raster::aggregate(rst, fact=2)), list('hexagon')[1:3])
    # check that grids generated using corresponding cell_area and cell_width values are equal
    Map(expect_equal, sq_grids1, sq_grids2)
    Map(expect_equal, hex_grids1, hex_grids2)  
  }
  
  ## tests
  # check for correct outputs
  run_all_tests_on_raster(r1)
  run_all_tests_on_raster(r2)
  run_all_tests_on_raster(r3)
  run_all_tests_on_raster(r4)
  # check that invalid arguments return error
  expect_error(make_grid(r1, cell_width = 0.4, type = "rhombus"))
  expect_error(make_grid(r1, cell_width = 0.4, type = NA))
  expect_error(make_grid(r1, cell_width = Inf, type = 'hexagonal'))
  expect_error(make_grid(r1, cell_width = NA, type = 'hexagonal'))
  expect_error(make_grid(r1, cell_area = Inf, type = 'square'))
  expect_error(make_grid(r1, cell_area = NA, type = 'square'))
})

test_that('calculate_boundaries', {
  # data
  r1 <- raster::raster(matrix(1:4, 2, 2)) # raster with finite values in all cells
  r2 <- raster::raster(matrix(c(1:3, NA), 2, 2)) # raster with one NA value
  r3 <- raster::raster(matrix(c(1:2, NA, NA), 2, 2)) # raster with one column of NA values
  r4 <- raster::raster(matrix(rep(NA, 4), 2, 2)) # raster with all values as NA
  s1_1 <- data.frame(
    i = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L),
    j = c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L),
    v = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5))
  s1_2 <- data.frame(
    i = c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L),
    j = c(1L, 2L, 3L, 1L, 2L, 4L, 1L, 3L, 4L, 2L, 3L, 4L),
    v = c(0.25, 0.5, 0.5, 0.5, 0.25, 0.5, 0.5, 0.25, 0.5, 0.5, 0.5, 0.25))
  s2 <- data.frame(
    i = c(1L,   1L,   1L,  2L,   2L,   3L,  3L),
    j = c(1L,   2L,   3L,  1L,   2L,   1L,  3L),
    v = c( 1,  0.5,  0.5,  0.5,  1.5,  0.5,  1.5))
  s3 <- data.frame(
    i = c(1L,   1L,  2L,   2L),
    j = c(1L,   2L,  1L,   2L),
    v = c(1.5, 0.5, 0.5,  1.5))
  s4 <- data.frame(i = numeric(), j = numeric(), v = numeric())
  # create objects
  b1_1 <- calculate_boundaries(r1, edge_factor=0.5)
  b1_2 <- calculate_boundaries(r1, edge_factor=0.25)
  b2 <- calculate_boundaries(r2, edge_factor=1)
  b3 <- calculate_boundaries(r3, edge_factor=1)
  b4 <- calculate_boundaries(r4, edge_factor=1)
  # test
  expect_equal(data.frame(i=b1_1$i, j=b1_1$j, v=b1_1$v), s1_1)
  expect_equal(data.frame(i=b1_2$i, j=b1_2$j, v=b1_2$v), s1_2)
  expect_equal(data.frame(i=b2$i, j=b2$j, v=b2$v), s2)
  expect_equal(data.frame(i=b3$i, j=b3$j, v=b3$v), s3)
  expect_equal(data.frame(i=b4$i, j=b4$j, v=b4$v), s4)
  expect_error(calculate_boundaries(r1, edge_factor=Inf))
  expect_error(calculate_boundaries(r1, edge_factor=NA))
  expect_error(calculate_boundaries(r1, edge_factor=NULL))
  expect_error(calculate_boundaries(NA))
  expect_error(calculate_boundaries(NULL))
})

