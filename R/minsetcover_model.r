#' Prepare a minimum set cover prioritization problem
#'
#' Specify a minimum set cover systematic conservation prioritization problem
#' from input data in a variety of formats. These are constructor functions for
#' \code{minsetcover_model} objects which encapsulate prioritization problems in
#' a standardized format.
#'
#' @details In the context of systematic reserve design, the minimum set cover
#'   problem seeks to find the set of planning units that minimizes the overall
#'   cost of a reserve network, while meeting a set of representation targets
#'   for the conservation features. The cost is often either the area of the
#'   planning units or the opportunity cost of foregone commericial activities
#'   (e.g. logging or agriculture). The representation targets ensure that each
#'   species is adequately represented in the reserve network.
#'
#'   This problem is equivalent to a simplified Marxan reserve design problem,
#'   with the Bounday Length Modifier (BLM) set to zero.
#'
#' @param x object specifying the planning units to use in the reserve design
#'   exercise and their corresponding cost. The possible types of objects are:
#'
#'   \bold{\code{RasterLayer}}: planning units are the raster cells and cell
#'   values should contain the repsective cost. If \code{x} is a RasterLayer, it
#'   may be desirable to exlcude some planning units from the analysis, for
#'   example those outside the study area. To exclude planning units, set the
#'   cost for those raster cells to \code{NA}.
#'
#'  \bold{\code{SpatialPolygonsDataFrame}}: polygons correspond to the planning
#'  units and there must be an attribute field named \code{cost}.
#'
#'  \bold{\code{numeric vector}}: a vector of planning unit costs. In this case
#'  the spatial representation of the planning units is not provided. Therefore,
#'  the \code{feature} argument is ignored and the representation matrix
#'  \code{rij} is required.
#'
#'  \bold{\code{MarxanData}}: a \code{MarxanData} object, from the \code{marxan}
#'   package (available at \url{https://github.com/jeffreyhanson/marxan}),
#'  specifying a Marxan reserve design problem,
#'  which is equivalent to the minimum set cover problem. The \code{marxan}
#'  package can create \code{MarxanData} objects from a variety of data
#'  sources, including the standard Marxan input files. Thus this is valuable
#'  for those wanted to transition directly from Marxan. Note that all other
#'  arguments are ignored if x is a \code{MarxanData} object.
#' @param features RasterStack object; the distribution of conservation
#'   features. If \code{x} is a Raster object then \code{features} should be
#'   defined on the same raster template. If \code{x} is a
#'   SpatialPolygonsDataFrame \code{features} will be summarize over the
#'   polygons using \code{\link{summarize_features}}. Not required if
#'   \code{rij} is provided.
#' @param targets numeric; representation targets either as proportion (between
#'   0 and 1) of total representation level when \code{target_type = "relative"}
#'   (the default), or absolute targets when \code{target_type = "absolute"}.
#'   The order of the targets should match the ordering in the \code{features}
#'   argument.
#' @param rij numeric matrix (optional); matrix of representation levels of
#'   conservation features (rows) within planning units (columns). \code{rij}
#'   can be a sparse matrix from the \code{slam} package (i.e. a
#'   \code{\link[slam]{simple_triplet_matrix}}) or a normal base matrix object.
#'   Alternatively, a data frame representation of this matrix with three
#'   variables: feature index (\code{rij$feature}), planning unit index
#'   (\code{rij$pu}), and corresponding representation level
#'   (\code{rij$amount}). If this matrix is not provided it will be calculated
#'   based on the planning units and RasterStack of conservation feature
#'   distributions.
#' @param locked_in integer; indices of planning units to lock in to final
#'   solution. For example, it may be desirable to lock in planning units
#'   already within protected areas.
#' @param locked_out integer; indices of planning units to lock out of final
#'   solution. For example, it may be desirable to lock in planning units that
#'   are already heavily developed and therefore have little viable habitat.
#' @param target_type "relative" or "absolute"; specifies whether the
#'   \code{target} argument should be interpreted as relative to the total level
#'   of representation or as an absolute target
#' @param ... additional arguments passed on to methods
#'
#' @return A \code{minsetcover_model} object describing the prioritization
#'   problem to be solved. This is an S3 object consisting of a list with the
#'   following components:
#'
#' \itemize{
#'   \item \code{cost}: numeric vector of planning unit costs
#'   \item \code{rij}: representation matrix
#'   \item \code{targets}: absolute feature targets
#'   \item \code{locked_in}: indices of locked in planning units
#'   \item \code{locked_out}: indices of locked out planning units
#'   \item \code{included}: logical vector indicating which planning units are
#'     to be included in the analysis. If all units are to be included, this is
#'     single value (\code{TRUE}). Using a subset of planning units is only
#'     permitted if the \code{x} argument is provided as a RasterLayer object.
#' }
#'
#' @export
#' @seealso \code{\link{maxcover_model}} for the maximum cover problem.
#' @examples
#' # 5x5 raster template
#' e <- raster::extent(0, 1, 0, 1)
#' r <- raster::raster(e, nrows = 5, ncols = 5, vals = 1)
#'
#' # generate 4 random feature distributions
#' set.seed(1)
#' f <- raster::stack(r, r, r, r)
#' f[] <- sample(0:1, raster::nlayers(f) * raster::ncell(f), replace = TRUE)
#' f <- setNames(f, letters[1:raster::nlayers(f)])
#' # genrate cost layer
#' cost <- r
#' cost[] <- rnorm(raster::ncell(cost), mean = 100, sd = 10)
#' cost <- setNames(cost, "cost")
#'
#' # prepare prioritization model with 20% targets
#' model <- minsetcover_model(x = cost, features = f, targets = 0.2)
#'
#' # targets can also be species specific
#' ss_targets <- round(runif(raster::nlayers(f)), 2)
#' ss_targets
#' model <- minsetcover_model(x = cost, features = f, targets = ss_targets)
#'
#' # or targets can be absolute
#' abs_targets <- ss_targets * raster::cellStats(f, "sum")
#' abs_targets
#' model <- minsetcover_model(x = cost, features = f,
#'                           targets = abs_targets, target_type = "absolute")
#'
#' # the representation matrix (rij) can be supplied explicitly,
#' # in which case the features argument is no longer required
#' rep_mat <- unname(t(f[]))
#' model <- minsetcover_model(x = cost, rij = rep_mat, targets = 0.2)
#'
#' # cells can be locked in or out of the final solution
#' model <- minsetcover_model(x = cost, features = f, targets = 0.2,
#'                            locked_in = 6:10,
#'                            locked_out = 16:20)
#'
#' # if some cells are to be exlcuded, e.g. those outside study area, set
#' # the cost to NA for these cells.
#' cost_na <- cost
#' cost_na[6:10] <- NA
#' model_na <- minsetcover_model(x = cost_na, features = f, targets = 0.2)
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
#' model_spdf <- minsetcover_model(x = cost_spdf, features = f, targets = 0.2)
#'
#' # planning units don't necessarily need to be supplied explicitly
#' # instead, cost can be given as a numeric vector provided the representation
#' # matrix (rij) is also provided
#' cost_vec <- cost[]
#' rep_mat <- unname(t(f[]))
#' model_nopu <- minsetcover_model(x = cost_vec, rij = rep_mat, targets = 0.2)
minsetcover_model <- function(x, ...)  {
  UseMethod("minsetcover_model")
}

#' @export
#' @describeIn minsetcover_model Numeric vector of costs
minsetcover_model.numeric <- function(
  x, targets, rij,
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
              is.numeric(targets),
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
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = TRUE
    ),
    class = c("minsetcover_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn minsetcover_model RasterLayer of planning units and corresponding
#'   costs
minsetcover_model.Raster <- function(
  x, features, targets, rij,
  locked_in = integer(),
  locked_out = integer(),
  target_type = c("relative", "absolute"), ...) {
  # assertions on arguments
  assert_that(raster::nlayers(x) == 1,
              is_integer(locked_in),
              all(locked_in > 0),
              all(locked_in <= raster::ncell(x)),
              is_integer(locked_out),
              all(locked_out > 0),
              all(locked_out <= raster::ncell(x)),
              # can't be locked in and out
              length(intersect(locked_in, locked_out)) == 0,
              is.numeric(targets))

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

  # representation matrix rij
  if (missing(rij)) {
    # if not provided, calculate rij
    assert_that(inherits(features, "Raster"),
                raster::compareRaster(x, features))
    # subset to included planning units
    features <- features[][included,]
    rij <- slam::as.simple_triplet_matrix(t(unname(features)))
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
      locked_in = sort(as.integer(locked_in)),
      locked_out = sort(as.integer(locked_out)),
      included = included
    ),
    class = c("minsetcover_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn minsetcover_model SpatialPolygonsData frame of planning units
#'   withcost attribute
minsetcover_model.SpatialPolygons <- function(
  x, features, targets, rij,
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
              is.numeric(targets))

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
      locked_in = as.integer(locked_in),
      locked_out = as.integer(locked_out),
      included = TRUE
    ),
    class = c("minsetcover_model", "prioritizr_model")
  )
}

#' @export
#' @describeIn minsetcover_model Specify model based on Marxan inputs
minsetcover_model.MarxanData <- function(x, ...) {
  # data frame of planning unit cost and status
  pu <- dplyr::arrange_(x@pu, ~id)
  # need to be careful because id field not necessarily an index
  pu$index <- seq.int(nrow(pu))
  # cost of planning units
  cost <- pu$cost
  # locked planning units
  locked_in <- which(pu$status == 2)
  locked_out <- which(pu$status == 3)

  # data frame of species and targets
  features <- dplyr::arrange_(x@species, ~id)
  # need to be careful because id field not necessarily an index
  features$index <- seq.int(nrow(features))

  # convert id to index in representation matrix
  rij <- dplyr::arrange_(x@puvspecies, ~species, ~pu)
  rij <- dplyr::rename_(rij, feature_id = ~species, pu_id = ~pu)
  rij$feature <- features$index[match(rij$feature_id, features$id)]
  rij$pu <- pu$index[match(rij$pu_id, pu$id)]
  rij <- dplyr::select_(rij, ~feature, ~pu, ~amount)
  rij <- df_to_matrix(rij,
                      nrow = nrow(features),
                      ncol = length(cost),
                      vars = c("feature", "pu", "amount"))

  # targets may be relative, convert to absolute
  target <- features$target
  if (is.character(target)) {
    # subset to targets given as percent
    rows <- features$index[grep("%$", target)]
    # convert from character ("10%") to numeric (0.1)
    pct_targ <- as.numeric(gsub("%", "", target[rows], fixed = TRUE))
    pct_targ <- pct_targ / 100
    assert_that(all(pct_targ >= 0), all(pct_targ <= 1))
    # convert to absolute
    tot_amt <- slam::row_sums(rij)
    target[rows] <-  pct_targ * tot_amt[rows]
  }
  targets <- as.numeric(target)

  # call method for numeric cost vector
  minsetcover_model.numeric(x = cost, targets = targets, rij = rij,
                            locked_in = locked_in, locked_out = locked_out,
                            target_type = "absolute")
}
