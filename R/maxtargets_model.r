#' Prepare a maximum target coverage prioritization problem
#'
#' Specify a maximum target coverage systematic conservation prioritization
#' problem from input data in a variety of formats. These are constructor
#' functions for \code{maxtargets_model} objects which encapsulate
#' prioritization problems in a standardized format.
#'
#' @details The maximum target coverage problem is a modified version of the
#'   the maximum coverage problem (see \code{\link{maxcover_model}}). Each
#'   conservation feature is assigned a representation target and the objective
#'   is to find the set of planning units that meets most targets
#'   while remaining within a fixed budget. The cost is often either the area of
#'   the planning units or the opportunity cost of foregone commericial
#'   activities (e.g. from logging or agriculture). Representation level is
#'   typically given by the occupancy within each planning unit, however, some
#'   measure of abundance or probability of occurence may also be used. The
#'   representation targets ensure that each species is adequately represented
#'   in the reserve network. If multiple solutions exists that all meet the
#'   targets within the budget, the least expensive option will be selected.
#'
#'   This problem meant to be a hybrid between the maximum coverage problem and
#'   a Marxan-like minimum set cover problem in that it allows for both a budget
#'   and targets to be set.
#'
#' @param budget numeric; budget for reserve.
#' @inheritParams minsetcover_model
#'
#' @return A \code{maxtargets_model} object describing the prioritization
#'   problem to be solved. This is an S3 object consisting of a list with the
#'   following components:
#'
#' \itemize{
#'   \item \code{cost}: numeric vector of planning unit costs
#'   \item \code{rij}: representation matrix
#'   \item \code{targets}: absolute feature targets
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
#'   \code{\link{maxcover_model}} for the traditional maximum coverage
#'   problem.
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
#' # and targets as 50% of the features' distributions
#' b_25 <- 0.25 * raster::cellStats(cost, "sum")
#' model <- maxtargets_model(x = cost, features = f, budget = b_25,
#'                           targets = 0.5)
#'
#' # the representation matrix (rij) can be supplied explicitly,
#' # in which case the features argument is no longer required
#' rep_mat <- unname(t(f[]))
#' model <- maxtargets_model(x = cost, rij = rep_mat, budget = b_25,
#'                           targets = 0.5)
#'
#' # cells can be locked in or out of the final solution
#' model <- maxtargets_model(x = cost, features = f, budget = b_25,
#'                           targets = 0.5, locked_in = 6:10,
#'                           locked_out = 16:20)
#' # alternatively, binary rasters can be supplied indicating locked in/out
#' r_locked_in <- raster::raster(r)
#' r_locked_in[] <- 0
#' # lock in cells 6-10
#' r_locked_in[6:10] <- 1
#' model_raste_lock <- maxtargets_model(x = cost, features = f, budget = b_25,
#'                                      locked_in = r_locked_in, targets=0.5,
#'                                      locked_out = 16:20)
#'
#' # if some cells are to be excluded, e.g. those outside study area, set
#' # the cost to NA for these cells.
#' cost_na <- cost
#' cost_na[6:10] <- NA
#' model_na <- maxtargets_model(x = cost_na, features = f, budget = b_25,
#'                              targets = 0.5)
#' # the model object now contains an included component specifying which
#' # cells are to be included
#' model_na$included
#' which(!model_na$included)
#' # note that the representation matrix now has fewer columns because
#' # the decision variables corresponding to excluded cells have been removed
#' dim(model$rij)
#' dim(model_na$rij)
#'
#' # planning units can also be supplied as a SpatialPolygonsDataFrame object
#' # with cost stored as an attribute (x$cost). Typically the function takes
#' # longer to execute with polygons because summarizing features over planning
#' # units is less efficient.
#' cost_spdf <- raster::rasterToPolygons(cost)
#' model_spdf <- maxtargets_model(x = cost_spdf, features = f, budget = b_25,
#'                                targets = 0.5)
maxtargets_model <- function(x, ...)  {
  UseMethod("maxtargets_model")
}

#' @export
#' @describeIn maxtargets_model Numeric vector of costs
maxtargets_model.numeric <- function(
  x, targets, budget, rij,
  locked_in = integer(),
  locked_out = integer(),
  target_type = c("relative", "absolute"), ...) {
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
              assertthat::is.number(budget),
              # budget isn't exceeded by locked in cells
              sum(x[locked_in], na.rm = TRUE) <= budget,
              # budget is greater than cost of cheapest cell
              min(x, na.rm = TRUE) <= budget,
              !missing(rij),
              is.numeric(targets),
              inherits(rij, c("matrix", "simple_triplet_matrix",
                              "data.frame")))
  # representation matrix rij
  if (nrow(rij) == 0) {
    stop("A prioritization problem must have at least one feature.")
  }
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
  # feature must be present somewhere
  if (!all(slam::row_sums(rij) > 0)) {
    stop("All features must be represented in at least one planning unit.")
  }

  # representation targets
  target_type <- match.arg(target_type)
  assert_that(length(targets) == rij$nrow || length(targets) == 1)
  if (length(targets) == 1) {
    targets <- rep(targets, rij$nrow)
  }
  # set proportional targets or check absolute targets
  if (target_type == "relative") {
    # convert relative targets to absolute targets
    targets <- set_targets(slam::row_sums(rij), targets)
  } else {
    # check that all targets are attainable
    assert_that(all(targets <= slam::row_sums(rij)))
  }

  structure(
    list(
      cost = x,
      rij = rij,
      targets = targets,
      budget = budget,
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = TRUE
    ),
    class = c("maxtargets_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn maxtargets_model RasterLayer of planning units and corresponding
#'   costs
maxtargets_model.Raster <- function(
  x, features, targets, budget, rij,
  locked_in = integer(),
  locked_out = integer(),
  target_type = c("relative", "absolute"), ...) {
  # assertions on arguments
  assert_that(raster::nlayers(x) == 1,
              is_integer(locked_in) | inherits(locked_in, "RasterLayer"),
              is_integer(locked_out) | inherits(locked_out, "RasterLayer"),
              assertthat::is.number(budget), budget > 0,
              is.numeric(targets),
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
    if (nrow(rij) == 0) {
      stop("A prioritization problem must have at least one feature.")
    }
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
  # feature must be present somewhere
  if (!all(slam::row_sums(rij) > 0)) {
    stop("All features must be represented in at least one planning unit.")
  }

  # representation targets
  target_type <- match.arg(target_type)
  assert_that(length(targets) == rij$nrow || length(targets) == 1)
  if (length(targets) == 1) {
    targets <- rep(targets, rij$nrow)
  }
  # set proportional targets or check absolute targets
  if (target_type == "relative") {
    # convert relative targets to absolute targets
    targets <- set_targets(slam::row_sums(rij), targets)
  } else {
    # check that all targets are attainable
    assert_that(all(targets <= slam::row_sums(rij)))
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
      targets = targets,
      budget = budget,
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = included
    ),
    class = c("maxtargets_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn maxtargets_model SpatialPolygonsData frame of planning units with
#'   cost attribute
maxtargets_model.SpatialPolygons <- function(
  x, features, targets, budget, rij,
  locked_in = integer(),
  locked_out = integer(),
  target_type = c("relative", "absolute"), ...) {
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
              assertthat::is.number(budget),
              is.numeric(targets),
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
    if (nrow(rij) == 0) {
      stop("A prioritization problem must have at least one feature.")
    }
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
  # feature must be present somewhere
  if (!all(slam::row_sums(rij) > 0)) {
    stop("All features must be represented in at least one planning unit.")
  }

  # representation targets
  target_type <- match.arg(target_type)
  assert_that(length(targets) == rij$nrow || length(targets) == 1)
  if (length(targets) == 1) {
    targets <- rep(targets, rij$nrow)
  }
  # set proportional targets or check absolute targets
  if (target_type == "relative") {
    # convert relative targets to absolute targets
    targets <- set_targets(slam::row_sums(rij), targets)
  } else {
    # check that all targets are attainable
    assert_that(all(targets <= slam::row_sums(rij)))
  }

  structure(
    list(
      cost = x$cost,
      rij = rij,
      targets = targets,
      budget = budget,
      locked_in = as.integer(locked_in),
      locked_out = as.integer(locked_out),
      included = TRUE
    ),
    class = c("maxtargets_model", "prioritizr_model")
  )
}
