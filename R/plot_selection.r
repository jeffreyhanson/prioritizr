#' Plot selected planning units
#'
#' This function is a wrapper for \code{\link[rasterVis]{levelplot}} (for raster
#' planning units) and \code{\link[sp]{spplot}} (for vector planning units).
#'
#' @param pu RasterLayer of SpatialPolygons object of planning units.
#' @param x binary or logical vector; decision variables indicating whether
#'   planning units are selected or not selected. The order of the decision
#'   variables should match the order of planning units in \code{pu}.
#' @param title character; plot title.
#' @param colours character; vector of two colours to display unselected and
#'   selected planning units, respectively.
#' @param axes logical; whether or not to show axes.
#' @param ... additional arguments to pass to \code{\link[rasterVis]{levelplot}}
#'   or \code{\link[sp]{spplot}}.
#'
#' @return \code{plot_selection} returns a lattice plot of class \code{trellis},
#'   if you fail to see it explicity call \code{print(plot_selection(...))}.
#' @export
#' @examples
#' # raster planning units
#' e <- raster::extent(0, 100, 0, 100)
#' pu_raster <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
#' x_selected <- as.vector(gaussian_field(pu_raster, 20, prop = 0.25)[])
#' plot_selection(pu_raster, x_selected)
#'
#' # vector planning units
#' pu_spdf <- raster::rasterToPolygons(pu_raster)
#' plot_selection(pu_spdf, x_selected)
plot_selection <- function(pu, x, title, colours, axes, ...) {
  UseMethod("plot_selection")
}

#' @export
plot_selection.Raster <- function(pu, x,
                                  title = "Selected Planning Units",
                                  colours = c("grey40", "#4DAF4A"),
                                  axes = FALSE, ...) {
  # assertions
  assert_that(length(x) == raster::ncell(pu),
              is.logical(x) || all(x %in% c(0,1)),
              assertthat::is.flag(axes),
              is.character(colours), length(colours) == 2,
              assertthat::is.string(title))
  # need a RasterLayer not RasterStack
  if (inherits(pu, c("RasterStack", "RasterBrick"))) {
    pu <- pu[[1]]
  }
  # convert logical to binary
  if (is.logical(x)) {
    x <- as.integer(x)
  }
  pu[] <- x
  # make this a categorical raster
  pu <- raster::ratify(pu)
  rat <- raster::levels(pu)[[1]]
  rat$status <- c("Not Selected", "Selected")
  levels(pu) <- rat
  rasterVis::levelplot(pu, main = title, scales = list(draw = axes),
                       col.regions = colours,
                       colorkey = list(space = "bottom", height = 1), ...)
}

#' @export
plot_selection.SpatialPolygons <- function(pu, x,
                                           title = "Selected Planning Units",
                                           colours = c("grey40", "#4DAF4A"),
                                           axes = FALSE, ...) {
  # assertions
  assert_that(length(x) == length(pu),
              is.logical(x) || all(x %in% c(0,1)),
              assertthat::is.flag(axes),
              is.character(colours), length(colours) == 2,
              assertthat::is.string(title))
  if (is.logical(x)) {
    x <- as.integer(x)
  }
  # assign selection status
  pu <- sp::geometry(pu)
  # aggregate by common status
  pu <- rgeos::gUnaryUnion(pu, x)
  pu$selected <- as.integer(row.names(pu)) %>%
    factor(levels = 0:1, labels = c("Not Selected", "Selected"))
  sp::spplot(pu, "selected", main = title, col.regions = colours,
             colorkey = list(space = "bottom", height = 1))
}
