context('03 misc functions')

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
  # raster with finite values in all cells
  r1 <- raster::raster(matrix(1:16, 4, 4))
  # raster with one NA value
  r2 <- raster::raster(matrix(c(1:15, NA), 4, 4))
  # raster with one column of NA values
  r3 <- raster::raster(matrix(c(1:12, NA, NA, NA, NA), 4, 4))
  # raster with all values as NA
  r4 <- raster::raster(matrix(rep(NA, 16), 4, 4))
  # define function
  test_that_grid_is_valid_for_raster <- function(
    sp, rst, type=c('square', 'hexagonal'), label='') {
    # init
    # NA values in raster should not affect grid generation
    rst <- raster::setValues(rst, 1)
    type <- match.arg(type)
    label <- paste0(type, ' grid ', label)
    n_cells <- raster::ncell(rst)

    if (type=='square') {
    expect_equal(
      length(sp@polygons),
      n_cells,
      info=paste0(label, ': grid has incorrect number of grid cells'))
    expect_equal(
      `dimnames<-`(rgeos::gCentroid(sp, byid=TRUE)@coords, NULL),
      `dimnames<-`(as.matrix(raster::rasterToPoints(rst, spatial=FALSE)[,1:2]),
                   NULL),
      info=paste0(label,
                  ': grid has incorrect location of grid cell centroids'))
    expect_equal(unname(rgeos::gArea(sp, byid=TRUE)),
                 rep(prod(raster::res(rst)), n_cells),
                 info=paste0(label, ': grid has cells with incorrect area'))
    }
    expect_true(
      raster::extent(sp)@xmin <= raster::extent(rst)@xmin &
      raster::extent(sp)@xmax >= raster::extent(rst)@xmax &
      raster::extent(sp)@ymin <= raster::extent(rst)@ymin &
      raster::extent(sp)@ymax >= raster::extent(rst)@ymax,
      info = paste0(label, 'extent of grid is smaller than raster')
    )
    expect_equal(
      unique(c(sapply(
        sp@polygons,
        function(x) {return(length(x@Polygons))}
      ))),
      1,
      info=paste0(label, ': grid has polygons with more than one sub-polygon'))
    expect_equal(
      unique(sapply(sp@polygons, function(x) {nrow(x@Polygons[[1]]@coords)-1})),
      ifelse(type=='square', 4, 6),
      info=paste0(label, ': grid has cells with incorrect number of vertices'))
  }

  run_all_tests_on_raster <- function(rst, label) {
    ## init
    # compute areas and widths
    sq_cell_widths <- c(raster::res(rst)[1], raster::res(rst)[1]/2,
                        raster::res(rst)[1]*2)
    sq_cell_areas <- sq_cell_widths^2
    hex_cell_areas <- sq_cell_areas
    hex_cell_widths <- sqrt(2 * hex_cell_areas / sqrt(3))
    ## make grids
    # square grids
    sq_grids1 <- lapply(sq_cell_widths,
                        function(x) {do.call(make_grid, list(x=rst,
                                                             type='square',
                                                             cell_width=x))})
    sq_grids2 <- lapply(sq_cell_areas,
                        function(x) {do.call(make_grid, list(x=rst,
                                                             type='square',
                                                             cell_area=x))})
    # hexagon grids
    hex_grids1 <- lapply(hex_cell_widths,
                         function(x) {do.call(make_grid, list(x=rst,
                                                              type='hexagon',
                                                              cell_width=x))})
    hex_grids2 <- lapply(hex_cell_areas,
                         function(x) {do.call(make_grid, list(x=rst,
                                                              type='hexagon',
                                                              cell_area=x))})
    ## run tests
    # check that geometries are correct
    Map(test_that_grid_is_valid_for_raster,
        sq_grids1,
        list(rst,
             raster::disaggregate(rst, fact=2),
             raster::aggregate(rst, fact=2)),
        list('square')[c(1,1,1)],
        as.list(paste(label[c(1,1,1)],
                      c('(original)', '(disaggregated)', '(aggregated)'))))
    Map(test_that_grid_is_valid_for_raster,
        hex_grids1,
        list(rst,
             raster::disaggregate(rst, fact=2),
             raster::aggregate(rst, fact=2)),
          list('hexagon')[c(1,1,1)],
          as.list(paste(label[c(1,1,1)],
                        c('(original)','(disaggregated)', '(aggregated)'))))
    # check that grids generated using corresponding
    # cell_area and cell_width values are equal
    Map(expect_equal, sq_grids1, sq_grids2)
    Map(expect_equal, hex_grids1, hex_grids2)
  }
  ## tests
  # check for correct outputs
  run_all_tests_on_raster(r1, 'normal')
  run_all_tests_on_raster(r2, 'missing one cell')
  run_all_tests_on_raster(r3, 'missing one row')
  run_all_tests_on_raster(r4, 'all missing')

  # check that invalid arguments return error
  expect_error(make_grid(r1, cell_width = 0.4, type = "rhombus"))
  expect_error(make_grid(r1, cell_width = 0.4, type = NA))
  expect_error(make_grid(r1, cell_width = Inf, type = 'hexagonal'))
  expect_error(make_grid(r1, cell_width = NA, type = 'hexagonal'))
  expect_error(make_grid(r1, cell_area = Inf, type = 'square'))
  expect_error(make_grid(r1, cell_area = NA, type = 'square'))
})
