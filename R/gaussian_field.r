#' Generate spatially autocorrelated random field
#'
#' Generate spatially autocorrelated random fields based on a Gaussian process
#' model. This function outputs a \code{RasterLayer} or \code{RasterStack}
#' object representing the random field. These fields can be used as
#' semi-realistic distributions of species or other spatial variables. Building
#' in spatial autocorrelation ensures that these random fields obey Tobler's
#' first law of geography: "everything is related to everything else, but near
#' things are more related than distant things".
#'
#' @param r Raster* object; the template for the random field.
#' @param range numeric; the range of the underlying variogram, the distance
#'   beyond which autocorrelation is negligible. This argument defines the
#'   spatial scale of the autocorrelation.
#' @param n integer; number of random fields to generate.
#' @param mean numeric; mean value of the random field.
#' @param variance numeric; the sill of the underlying variogram model, which
#'   can be thought of as the variance of the random field.
#' @param nugget numeric; the nugget component of the underlying variogram model
#' @param coef numeric vector; if a linear trend in the x and/or y direction is
#'   desired, this a vector with 2 components representing the slopes in the x
#'   and y directions, respectively.
#' @param prop numeric value between 0-1; if \code{pct} is supplied, the field
#'   will be reclassified into binary values, where \code{pct} represents the
#'   proportion of ones. Useful for generating presence/absence layers.
#'
#' @return RasterStack
#' @export
#' @examples
#' e <- raster::extent(0, 100, 0, 100)
#' r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
#' gf <- gaussian_field(r, range = 20, n = 4)
#' raster::plot(gf)
#'
#' # generate binary rasters with different ranges
#' gf_5 <- gaussian_field(r, range = 5, n = 1, prop = 0.5)
#' gf_20 <- gaussian_field(r, range = 20, n = 1, prop = 0.5)
#' s <- raster::stack(gf_5, gf_20)
#' raster::plot(s)
#'
#' # add a linear trend
#' gf_linear <-gaussian_field(r, range = 20, coef = c(0.05, 0.05))
#' raster::plot(gf_linear)
gaussian_field <- function(r, range, n = 1, mean = 0, variance = 1, nugget = 0,
                           coef = c(0, 0), prop = NULL) {
  # assertions
  assertthat::assert_that(inherits(r, "RasterLayer"),
                          assertthat::is.number(range), range > 0,
                          assertthat::is.count(n),
                          assertthat::is.number(mean),
                          assertthat::is.number(variance),
                          variance > 0, variance > nugget,
                          assertthat::is.number(nugget), nugget >= 0,
                          is.numeric(coef), length(coef) == 2,
                          is.null(prop) || (prop >= 0 & prop <= 1))

  beta <- c(mean, coef)
  psill <- variance - nugget
  # define spatial variogram model
  gsim <- gstat::gstat(formula = (z ~ x + y), dummy = TRUE, beta = beta,
                       nmax = 20, model = gstat::vgm(psill = psill,
                       range = range, nugget = nugget, model = 'Exp'))
  # prevent predict from printing useless output to screen
  utils::capture.output({
    vals <- raster::rasterToPoints(r, spatial = TRUE) %>%
      stats::predict(gsim, newdata = ., nsim = n)
  })
  vals <- vals@data
  # reclassify to binary, pct defines proportion of ones
  if (!is.null(prop)) {
    vals <- vapply(vals, function(x) as.integer(x <= stats::quantile(x, prop)),
                   integer(nrow(vals))) %>%
      data.frame
  }
  # assign to RasterStack object
  s <- list()
  for (i in 1:n) {
    r_tmp <- r
    r_tmp[] <- vals[, i]
    s[[i]] <- r_tmp
  }
  s <- raster::stack(s)
  names(s) <- paste0("gf", 1:n)
  return(s)
}
