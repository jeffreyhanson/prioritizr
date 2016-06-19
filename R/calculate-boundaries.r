#' Calculate shared boundaries between planning units
#'
#' Prepare matrix of planning unit boundary lengths for a Marxan-like reseve
#' design exercise. Off diagonal components are shared boundaries between pairs
#' of planning units and diagonal components are external boundaries of planning
#' units, i.e. those not shared with another planning unit, such as boundaries
#' on the edge of the study area.
#'
#' @details
#' If planning units are provided as a SpatialPolygons object, it is recommended
#' that you install the \code{marxan} package (currently only available on
#' GitHub). This package has a boundary length function implemented in C++ that
#' will be called by \code{calculate_boundaries}. If \code{marxan} is not
#' installed, \code{calculate_boundaries} will use an alternate implementation,
#' which is several orders of magnitude slower.
#'
#' @param x Raster* or SpatialPolygons object; planning units for which
#'   boundaries lengths are calculated.
#' @param matrix logical; whether to return a matrix (\code{TRUE}) or data frame
#'   (\code{FALSE}).
#' @param sparse logical; whether to explicitly include shared boundaries of
#'   length zero. If \code{sparse} and \code{matrix} and both \code{TRUE} then a
#'   sparse matrix from the \code{slam} package is returned.
#' @param triangular logical; whether to return all matrix elements or just the
#'   upper triangle. Since this matrix will typically be symmetric, the lower
#'   triangle is redundant, and it may be desirable to drop it.
#' @param edge_factor numeric; factor to scale lengths of edge boundaries that
#'   aren't shared with another planning unit. Set this to 0 if you don't wish
#'   to include these boundaries as diagonal components of the matrix. This
#'   scale factor can also be used in cases where edge boundaries are much
#'   longer than other shared boundaries, such as when edges are composed of
#'   complicated coast lines. In such situations, the long boundary lengths can
#'   bias selection against these edge planning units, and it may be desirable
#'   to scale the length to smaller values.
#'
#' @return If \code{matrix} is \code{TRUE} (the default), a matrix where each
#'   element corresponds to the length of the boundary between a pair of
#'   planning units. If \code{matrix} is \code{FALSE}, a data frame with three
#'   columns corresponding to the IDs of a pair of planning and the length of
#'   the shared boundary.
#'
#'   Most pairs of planning units will have no shared boundary and by default
#'   these zero boundaries will not be stored explicitly. For matrices, this is
#'   accomplished using sparse matrix objects from the \code{slam} package,
#'   which use up much less memory than standard matrices. To explicitly include
#'   zero boundaries, use \code{sparse = FALSE}.
#'
#' @export
#' @examples
#' r <- raster::raster(matrix(1:9, 3, 3))
#' calculate_boundaries(r)
#' calculate_boundaries(r, sparse = FALSE, triangular = TRUE)
#' hex_grid <- make_grid(r, cell_width = 0.5, type = "hexagonal", clip = TRUE)
#' calculate_boundaries(hex_grid, matrix = FALSE)
calculate_boundaries <- function(x, matrix, sparse, triangular, edge_factor) {
  UseMethod("calculate_boundaries")
}

#' @export
calculate_boundaries.Raster <- function(x, matrix = TRUE, sparse = TRUE,
                                        triangular = FALSE, edge_factor = 1) {
  # assertions
  assert_that(assertthat::is.flag(matrix),
              assertthat::is.flag(sparse),
              assertthat::is.flag(triangular),
              assertthat::is.number(edge_factor))

  # shared boundaries
  ud <- matrix(c(NA, NA, NA, 1, 0, 1, NA, NA, NA), 3, 3)
  lf <- matrix(c(NA, 1, NA, NA, 0, NA, NA, 1, NA), 3, 3)
  shared_ud <- raster::adjacent(x, 1:raster::ncell(x), directions = ud) %>%
    data.frame(boundary = raster::res(x)[1])
  shared_lr <- raster::adjacent(x, 1:raster::ncell(x), directions = lf) %>%
    data.frame(boundary = raster::res(x)[2])
  b <- rbind(shared_ud, shared_lr)
  rm(shared_ud, shared_lr)
  b$id1 <- as.integer(b$from)
  b$id2 <- as.integer(b$to)
  b <- b[c("id1", "id2", "boundary")]

  # external boundaries
  if (!isTRUE(all.equal(edge_factor, 0))) {
    rrows <- raster::nrow(x)
    rcols <- raster::ncol(x)
    rres <- raster::res(x)
    # corners
    external <- cbind(c(1, rcols, rrows * rcols - rcols + 1, rrows * rcols),
                      sum(rres))
      # edges
    if (rcols > 2) {
      external <- rbind(
        external,
        cbind(2:(rcols - 1), rres[1]),
        cbind((rcols * (rrows - 1) + 2):(rrows * rcols - 1), rres[1]))

    }
    if (rrows > 2) {
      external <- rbind(
        external,
        cbind(rcols * (1:(rrows - 2)) + 1, rres[2]),
        cbind(rcols * (2:(rrows - 1)), rres[2]))
    }
    external <- data.frame(id1 = external[, 1],
                           id2 = external[, 1],
                           boundary = edge_factor * external[, 2])
    b <- rbind(b, external)
  }
  row.names(b) <- NULL

  # prepare desired return object
  prepare_boundaries(b, n = raster::ncell(x), matrix = matrix, sparse = sparse,
                     triangular = triangular)
}

#' @export
calculate_boundaries.SpatialPolygons <- function(x, matrix = TRUE, sparse = TRUE,
                                                 triangular = FALSE,
                                                 edge_factor = 1) {
  # assertions
  assertthat::assert_that(assertthat::is.flag(matrix),
                          assertthat::is.flag(sparse),
                          assertthat::is.flag(triangular),
                          assertthat::is.number(edge_factor))
  x <- sp::geometry(x)
  # set polygon ids to sequential integers
  row.names(x) <- as.character(seq_len(length(x)))
  # calculate shared boundaries
  if (requireNamespace("marxan", quietly = TRUE)) {
    # if marxan package available use C++ function
    b <- marxan::calcBoundaryData(x, tolerance = 1 / rgeos::getScale(),
                                  edgeFactor = edge_factor)
    if (isTRUE(all.equal(edge_factor, 0))) {
      b <-  b[b$id1 != b$id2, ]
    }
    # function only returns upper triangle, add lower triangle if desired
    if (!triangular) {
      b_lower <-  b[b$id1 != b$id2, ]
      tmp_id <- b$id1
      b$id1 <- b$id2
      b$id2 <- tmp_id
      b <- rbind(b, b_lower)
    }
  } else {
    # otherwise use own implementation, which is much much slower
    b <- methods::as(x, "SpatialLines") %>%
      rgeos::gIntersection(., ., byid = TRUE, drop_lower_td = TRUE) %>%
      rgeos::gLength(byid = TRUE) %>%
      data.frame(pid = names(.), boundary = ., stringsAsFactors = FALSE) %>%
      tidyr::separate_("pid", c("id1", "id2"), sep = " ")

    # external boundaries
    if (!isTRUE(all.equal(edge_factor, 0))) {
      external <- b[b$id1 == b$id2, ]
      b <- b[b$id1 != b$id2, ]
      external <- dplyr::group_by_(b, "id1") %>%
        dplyr::summarise_(internal = ~sum(boundary, na.rm = TRUE)) %>%
        dplyr::right_join(external, by = "id1") %>%
        dplyr::mutate_(boundary = ~(boundary - internal))
      external$boundary <- edge_factor * external$boundary
      external <- external[c("id1", "id2", "boundary")]
      b <- rbind(b, external)
    } else {
      # remove self boundaries
      b <- b[b$id1 != b$id2, ]
    }
  }
  b$id1 <- as.integer(b$id1)
  b$id2 <- as.integer(b$id2)
  row.names(b) <- NULL

  # prepare desired return object
  prepare_boundaries(b, n = length(x), matrix = matrix, sparse = sparse,
                     triangular = triangular)
}

prepare_boundaries <- function(b, n, matrix, sparse, triangular) {
  # convert to upper triangular form
  if (triangular) {
    b <- b[b$id1 <= b$id2, ]
  }
  b <- dplyr::arrange_(b, "id1", "id2")
  # construct desired return object
  if (matrix) {
    b <- slam::simple_triplet_matrix(b$id1, b$id2, b$boundary, nrow = n, ncol = n)
    if (!sparse) {
      b <- as.matrix(b)
    }
  } else {
    if (!sparse) {
      b <- tidyr::complete_(b, c("id1", "id2"), fill = list(boundary = 0))
    }
    b <- as.data.frame(b)
  }
  return(b)
}
