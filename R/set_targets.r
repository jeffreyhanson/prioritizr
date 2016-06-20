#' Set percent-based representation targets for conservation features
#'
#' In a Marxan-like reserve design exercise, all conservation features need
#' representation targets. Often these targets are set as some percentage of the
#' total level of representation the features within the study area. This
#' function converts percent-based targets into absolute targets in preparation
#' for a reserve design exercise.
#'
#' @param features RasterStack of the distribution of features over the study
#'   area or a numeric vector giving the total representation level of the
#'   feature in the study area.
#' @param targets numeric vector of percent-based targets for each conservation
#'   feature. Values must be between 0 and 1, and the length of the vector must
#'   be either 1, to set the same proportional target for all features, or equal
#'   to the number of features.
#'
#' @return A numeric vector of targets. If \code{features} was named, then the
#'   returned vector of targets will also be named.
#' @export
#' @examples
#' e <- raster::extent(0, 100, 0, 100)
#' r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
#' # generate 4 feature distributions
#' r_rare <- gaussian_field(r, range = 20, n = 2, prop = 0.1)
#' r_common <- gaussian_field(r, range = 20, n = 2, prop = 0.5)
#' r_features <- raster::stack(r_rare, r_common)
#' names(r_features) <- letters[1:4]
#' # set 20% targets for all feature
#' set_targets(r_features, 0.2)
#' # species-specific targets
#' set_targets(r_features, c(0.5, 0.4, 0.2, 0.1))
set_targets <- function(features, targets) {
  # assertions
  assert_that(is.numeric(targets),
              all(targets >= 0),
              all(targets <= 1),
              inherits(features, "Raster") || is.numeric(features))

  feature_names <- names(features)
  # total representation level
  if (inherits(features, "Raster")) {
    assertthat::assert_that(
      length(targets) %in% c(1, raster::nlayers(features)))
    features <- unname(raster::cellStats(features, "sum"))
  }
  assertthat::assert_that(length(targets) %in% c(1, length(features)))
  stats::setNames(features * targets, feature_names)
}
