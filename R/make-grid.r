#' Create square or hexagonal grids
#'
#' Generate regular square or hexagonal spatial grids based on a Raster* or
#' Spatial* object defining a study area. Grids can be specified by width (i.e.
#' distance between cell centres) or area, and cells at the edge of the study
#' area can be optionally clipped to the study area boundary.
#'
#' @param x Raster* or Spatial* object; the region over which to define the
#'   study grid.
#' @param type "square" or "hexagonal"; type of grid.
#' @param cell_width numeric; distance between cell centers.
#' @param cell_area numeric; area of cell, only used if \code{cell_width} is
#'   missing.
#' @param clip logical; whether or not to clip the cells to the study area
#'   boundary.
#'
#' @return SpatialPolygons object
#' @export
#' @examples
#' r <- raster::raster(matrix(1:9, 3, 3))
#' hex_grid <- make_grid(r, cell_width = 0.4, type = "hexagonal")
#' raster::plot(r)
#' sp::plot(hex_grid, add = TRUE)
#' sq_grid <- make_grid(hex_grid[9, ], cell_width = 0.1, type = "square", clip = TRUE)
#' sp::plot(sq_grid, add = TRUE, border = "red", lwd = 2)
make_grid <- function(x, type = c("hexagonal", "square"), cell_width, cell_area,
                      clip = FALSE) {
  # assertions
  type <- match.arg(type)
  assert_that(!missing(cell_width) || !missing(cell_area),
              missing(cell_width) || assertthat::is.number(cell_width),
              missing(cell_area) || assertthat::is.number(cell_area),
              assertthat::is.flag(clip),
              inherits(x, c("Raster", "Spatial")))

  # if cell_width is missing calculate it based on cell area
  if (missing(cell_width)) {
    if (type == "square") {
      cell_width <- sqrt(cell_area)
    } else if (type == "hexagonal") {
      cell_width <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  # buffered extent of study area to define cells over
  ext <- methods::as(raster::extent(x) + cell_width, "SpatialPolygons")
  raster::projection(ext) <- raster::projection(x)
  # generate grid
  if (type == "square") {
    g <- raster::raster(ext, resolution = cell_width)
    g <- methods::as(g, "SpatialPolygons")
  } else if (type == "hexagonal") {
    # generate array of hexagon centers
    g <- sp::spsample(ext, type = "hexagonal", cellsize = cell_width,
                      offset = c(0, 0))
    # convert center points to hexagons
    g <- sp::HexPoints2SpatialPolygons(g, dx = cell_width)
  }
  # clip to boundary of study area
  # if x is a raster, convert boundary to polygon for clipping
  if (inherits(x, "Raster")) {
    x <- methods::as(raster::extent(x), "SpatialPolygons")
    raster::projection(x) <- raster::projection(ext)
  }
  # clip or subset
  if (clip) {
    g <- rgeos::gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}
