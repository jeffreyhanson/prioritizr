#' Prepare a maximum coverage prioritization problem
#'
#' Specify a maximum coverage systematic conservation prioritization problem
#' from input data in a variety of formats. These are constructor functions for
#' \code{maxcover_model} objects which encapsulate prioritization problems in
#' a standardized format.
#'
#' @details In the context of systematic reserve design, the maximum coverage
#'   problem seeks to find the set of planning units that maximizes the overall
#'   level of representation across a suite of conservation features, while
#'   keeping cost within a fixed budget. The cost is often either the area of
#'   the planning units or the opportunity cost of foregone commericial
#'   activities (e.g. from logging or agriculture). Representation level is
#'   typically given by the occupancy within each planning unit, however, some
#'   measure of abundance or probability of occurence may also be used.
#'
#'   This problem is roughly the opposite of what the conservation planning
#'   software Marxan does.
#'
#' @param budget numeric; budget for reserve.
#' @inheritParams minsetcover_model
#'
#' @return A \code{maxcoverage_model} object describing the prioritization
#'   problem to be solved. This is an S3 object consisting of a list with the
#'   following components:
#'
#' \itemize{
#'   \item \code{cost}: numeric vector of planning unit costs
#'   \item \code{rij}: representation matrix
#'   \item \code{budget}: budget for reserve
#'   \item \code{locked_in}: indices of locked in planning units
#'   \item \code{locked_out}: indices of locked out planning units
#'   \item \code{included}: logical vector indicating which planning units are
#'     to be included in the analysis. If all units are to be included, this is
#'     single value (\code{TRUE}). Using a subset of planning units is only
#'     permitted if the \code{x} argument is provided as a RasterLayer object.
#' }
#'
#' @export
#' @seealso \code{\link{minsetcover_model}} for the minimum set cover problem.
#' @examples
#' # 5x5 raster template
#' e <- raster::extent(0, 1, 0, 1)
#' r <- raster::raster(e, nrows = 5, ncols = 5, vals = 1)
#'
#' # generate 4 random feature distributions
#' set.seed(419)
#' f <- raster::stack(r, r, r, r)
#' f[] <- sample(0:1, raster::nlayers(f) * raster::ncell(f), replace = TRUE)
#' f <- setNames(f, letters[1:raster::nlayers(f)])
#' # genrate cost layer
#' cost <- r
#' cost[] <- rnorm(raster::ncell(cost), mean = 100, sd = 10)
#' cost <- setNames(cost, "cost")
#'
#' # prepare prioritization model with budget at 25% of total cost
#' b_25 <- 0.25 * raster::cellStats(cost, "sum")
#' model <- maxcover_model(x = cost, features = f, budget = b_25)
#'
#' # the representation matrix (rij) can be supplied explicitly,
#' # in which case the features argument is no longer required
#' rep_mat <- unname(t(f[]))
#' model <- maxcover_model(x = cost, rij = rep_mat, budget = b_25)
#'
#' # cells can be locked in or out of the final solution
#' model <- maxcover_model(x = cost, features = f, budget = b_25,
#'                         locked_in = 6:10,
#'                         locked_out = 16:20)
#' # alternatively, binary rasters can be supplied indicating locked in/out
#' r_locked_in <- raster::raster(r)
#' r_locked_in[] <- 0
#' # lock in cells 6-10
#' r_locked_in[6:10] <- 1
#' model_raste_lock <- maxcover_model(x = cost, features = f, budget = b_25,
#'                                    locked_in = r_locked_in,
#'                                    locked_out = 16:20)
#'
#' # if some cells are to be exlcuded, e.g. those outside study area, set
#' # the cost to NA for these cells.
#' cost_na <- cost
#' cost_na[6:10] <- NA
#' model_na <- maxcover_model(x = cost_na, features = f, budget = b_25)
#' # the model object now contains an included component specifying which
#' # cells are to be included
#' model_na$included
#' which(!model_na$included)
#' # note that the representation matrix now has fewer columns because
#' # the decision variables corresponding to exlcuded cells have been removed
#' model$rij
#' model_na$rij
#'
#' # planning units can also be supplied as a SpatialPolygonsDataFrame object
#' # with cost stored as an attribute (x$cost). Typically the function takes
#' # longer to execute with polygons because summarizing features over planning
#' # units is less efficient.
#' cost_spdf <- raster::rasterToPolygons(cost)
#' model_spdf <- maxcover_model(x = cost_spdf, features = f, budget = b_25)
maxcover_model <- function(x, ...)  {
  UseMethod("maxcover_model")
}

#' @export
#' @describeIn maxcover_model Numeric vector of costs
maxcover_model.numeric <- function(
  x, budget, rij,
  locked_in = integer(),
  locked_out = integer(), ...) {
  # assertions on arguments
  assert_that(all(is.finite(x)),
              is_integer(locked_in),
              all(locked_in > 0),
              all(locked_in <= length(x)),
              is_integer(locked_out),
              all(locked_out > 0),
              all(locked_out <= length(x)),
              # can't be locked in and out
              length(intersect(locked_in, locked_out)) == 0,
              assertthat::is.number(budget), budget > 0,
              # budget isn't exceeded by locked in cells
              sum(x[locked_in], na.rm = TRUE) <= budget,
              # budget is greater than cost of cheapest cell
              min(x, na.rm=TRUE) <= budget,
              !missing(rij),
              inherits(rij, c("matrix", "simple_triplet_matrix",
                              "data.frame")))
  # representation matrix rij
  if (is.matrix(rij)) {
    rij <- slam::as.simple_triplet_matrix(unname(rij))
  } else if (is.data.frame(rij)) {
    rij <- df_to_matrix(rij,
                        ncol = raster::ncell(x),
                        vars = c("feature", "pu", "amount"))
  }
  # feature representations levels must be not be missing
  if (!all(is.finite(rij$v) & is.numeric(rij$v))) {
    stop("Representation matrix cannot have missing or non-numeric values.")
  }

  structure(
    list(
      cost = x,
      rij = rij,
      budget = budget,
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = TRUE
    ),
    class = c("maxcover_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn maxcover_model RasterLayer of planning units and corresponding
#'   costs
maxcover_model.Raster <- function(
  x, features, budget, rij,
  locked_in = integer(),
  locked_out = integer(), ...) {
  # assertions on arguments
  assert_that(raster::nlayers(x) == 1,
              is_integer(locked_in) | inherits(locked_in, "RasterLayer"),
              is_integer(locked_out) | inherits(locked_out, "RasterLayer"),
              assertthat::is.number(budget), budget > 0,
              # budget is greater than cost of cheapest cell
              raster::cellStats(x, 'min') <= budget)

  # convert 1-band RasterStack to RasterLayer
  x <- x[[1]]

  # check for NA cells indicating planning units to exclude
  pu_na <- is.na(x[])
  if (any(pu_na)) {
    # logical vector indicating included planning units
    included <- !pu_na
  } else {
    # all planning units included
    included <- TRUE
  }
  rm(pu_na)

  # prepare cost vector
  cost <- x[]
  # subset to included planning units
  cost <- cost[included]

  # identify locked in/out planning units from raster
  if (inherits(locked_in, "RasterLayer")) {
    assert_that(all(locked_in[] %in% c(0, 1)))
    locked_in <- which(locked_in[] == 1)
  }
  if (inherits(locked_out, "RasterLayer")) {
    assert_that(all(locked_out[] %in% c(0, 1)))
    locked_out <- which(locked_out[] == 1)
  }
  assert_that(all(locked_in > 0),
              all(locked_in <= length(x)),
              is_integer(locked_out),
              all(locked_out > 0),
              all(locked_out <= length(x)),
              # can't be locked in and out
              length(intersect(locked_in, locked_out)) == 0,
              # budget isn't exceeded by locked in cells
              sum(x[][locked_in], na.rm = TRUE) <= budget)

  # representation matrix rij
  if (missing(rij)) {
    # if not provided, calculate rij
    assert_that(inherits(features, "Raster"),
                raster::compareRaster(x, features))
    # subset to included planning units
    features_mat <- features[][included,]
    # assume missing values indicate absence
    features_mat[is.na(features_mat)] <- 0
    rij <- slam::as.simple_triplet_matrix(t(unname(features_mat)))
  } else {
    # ensure that rij is a matrix, sparse matrix, or data frame
    assert_that(inherits(rij, c("matrix", "simple_triplet_matrix",
                                "data.frame")))
    if (is.matrix(rij)) {
      rij <- slam::as.simple_triplet_matrix(unname(rij))
    } else if (is.data.frame(rij)) {
      rij <- df_to_matrix(rij,
                          ncol = raster::ncell(x),
                          vars = c("feature", "pu", "amount"))
    }
    # subset to included planning units
    if (!isTRUE(included)) {
      rij <- rij[, included]
    }
  }
  # feature representations levels must be not be missing
  if (!all(is.finite(rij$v) & is.numeric(rij$v))) {
    stop("Representation matrix cannot have missing or non-numeric values.")
  }

  # shift locked cells if some planning units are excluded
  if (!isTRUE(included)) {
    # locked in
    if (length(locked_out) > 0) {
      lock <- rep(FALSE, raster::ncell(x))
      lock[locked_in] <- TRUE
      lock <- lock[included]
      locked_in <- which(lock)
    }
    # locked out
    if (length(locked_out) > 0) {
      lock <- rep(FALSE, raster::ncell(x))
      lock[locked_out] <- TRUE
      lock <- lock[included]
      locked_out <- which(lock)
    }
  }

  structure(
    list(
      cost = cost,
      rij = rij,
      budget = budget,
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = included
    ),
    class = c("maxcover_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn maxcover_model SpatialPolygonsData frame of planning units with
#'   cost attribute
maxcover_model.SpatialPolygons <- function(
  x, features, budget, rij,
  locked_in = integer(),
  locked_out = integer(), ...) {
  # assertions on arguments
  assert_that("cost" %in% names(x),
              is.numeric(x$cost), all(is.finite(x$cost)),
              is_integer(locked_in),
              all(locked_in > 0),
              all(locked_in <= raster::ncell(x)),
              is_integer(locked_out),
              all(locked_out > 0),
              all(locked_out <= raster::ncell(x)),
              # can't be locked in and out
              length(intersect(locked_in, locked_out)) == 0,
              is.numeric(budget),
              # budget isn't exceeded by locked in cells
              sum(x$cost[locked_in], na.rm = TRUE) <= budget,
              # budget is greater than cost of cheapest cell
              min(x$cost) <= budget)

  # representation matrix rij
  if (missing(rij)) {
    # if not provided, calculate it
    assert_that(inherits(features, "Raster"))
    rij <- summarize_features(x, features)
  } else {
    # ensure that rij is a matrix, sparse matrix, or data frame
    assert_that(inherits(rij, c("matrix", "simple_triplet_matrix",
                                "data.frame")))
    if (is.matrix(rij)) {
      rij <- slam::as.simple_triplet_matrix(unname(rij))
    } else if (is.data.frame(rij)) {
      rij <- df_to_matrix(rij,
                          ncol = length(x),
                          vars = c("feature", "pu", "amount"))
    }
    # number of columns should be equal to number of planning units
    assert_that(rij$ncol == length(x))
  }
  # feature representations levels must be not be missing
  if (!all(is.finite(rij$v) & is.numeric(rij$v))) {
    stop("Representation matrix cannot have missing or non-numeric values.")
  }

  structure(
    list(
      cost = x$cost,
      rij = rij,
      budget = budget,
      locked_in = as.integer(locked_in),
      locked_out = as.integer(locked_out),
      included = TRUE
    ),
    class = c("maxcover_model", "prioritizr_model")
  )
}
