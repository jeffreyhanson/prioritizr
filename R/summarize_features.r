#' Calculate representation level of features in planning units
#'
#' Summarize a RasterStack of conservation features over a set of planning unit
#' polygons. If the \code{marxan} package is installed, this function is a
#' wrapper for \code{calcPuVsSpeciesData} from the \code{marxan} package
#' (available at \url{https://github.com/jeffreyhanson/marxan}), otherwise a much
#' slower implementation is used that doesn't require \code{marxan}.
#'
#' @param pu SpatialPolygons* object; planning unit polygons.
#' @param features RasterStack object; distribution of features over study area
#'   where each feature is a seperate layer in the RasterStack.
#' @param matrix logical; whether to return a matrix (\code{TRUE}) or data frame
#'   (\code{FALSE}).
#' @param sparse logical; whether to return a normal matrix or a sparse matrix
#'   as implemented by the \code{slam} package. Sparse matrices only explicitly
#'   store non-zero elements, which uses less memory.
#'
#' @return A matrix of feature representation levels within planning units where
#'   rows correspond to features and columns correspond to planning units. If
#'   \code{matrix = FALSE} a data frame is returned instead with columns
#'   corresponding to feature index, planning unit index, and amount within
#'   planning unit, repsectively.

#' @export
#' @examples
#' # generate 4 feature distributions
#' e <- raster::extent(0, 100, 0, 100)
#' r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
#' r_rare <- gaussian_field(r, range = 20, n = 2, prop = 0.1)
#' r_common <- gaussian_field(r, range = 20, n = 2, prop = 0.5)
#' r_features <- raster::stack(r_rare, r_common)
#' names(r_features) <- letters[1:4]
#'
#' # create hexagonal grid
#' hex_grid <- make_grid(r, cell_area = 1000, type = "hexagonal")
#'
#' # summarize
#' # spars matrix (default)
#' rij_sparse <- summarize_features(hex_grid, r_features)
#' rij_sparse
#' # normal matrix
#' rij_matrix <- summarize_features(hex_grid, r_features, sparse = FALSE)
#' dim(rij_matrix)
#' # data frame
#' rij_df <- summarize_features(hex_grid, r_features, matrix = FALSE)
#' head(rij_df)
summarize_features <- function(pu, features, matrix = TRUE, sparse = TRUE) {
  # assertions
  assert_that(inherits(pu, "SpatialPolygons"),
              inherits(features, "Raster"),
              assertthat::is.flag(matrix),
              assertthat::is.flag(sparse),
              identical(raster::projection(pu), raster::projection(features)))

  # sum raster cells over polygons
  if (requireNamespace("marxan", quietly = TRUE)) {
    # supress warning raised by marxan package when cell values are missing
    rij <- suppressWarnings(
      marxan::calcPuVsSpeciesData(pu, features)
    )
    rij$feature <- rij$species
  } else {
    rij <- raster::extract(features, pu, fun = sum, na.rm = TRUE,
                           sp = FALSE) %>%
      data.frame(pu = seq_len(length(pu)), .) %>%
      stats::setNames(c("pu", seq_len(raster::nlayers(features)))) %>%
      tidyr::gather_("feature", "amount", names(.)[-1])
    rij$feature <- as.integer(rij$feature)

  }
  rij <- rij[c("feature", "pu", "amount")]
  rij <- rij[!is.na(rij$amount) & rij$amount > 0, ]
  # construct desired return object
  if (matrix) {
    rij <- slam::simple_triplet_matrix(rij$feature, rij$pu, rij$amount,
                                       nrow = raster::nlayers(features),
                                       ncol = length(pu))
    if (!sparse) {
      rij <- as.matrix(rij)
    }
  }
  return(rij)
}
