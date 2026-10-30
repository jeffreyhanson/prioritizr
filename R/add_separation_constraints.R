#' @include internal.R Constraint-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add separation constraints
#'
#' Add constraints to a conservation planning problem to ensure
#' that selected planning units are separated from each other.
#' If the problem has multiple zones, then these constraints can
#' instead be used to ensure that planning units allocated to different
#' zones are separated from each other.
#'
#' @inheritParams add_manual_locked_constraints
#'
#' @param zones `matrix` or `Matrix` object describing the
#' separation scheme for different zones. Each row and column corresponds
#' to a different zone in `x`, and cell values must
#' contain binary `numeric` values (i.e., one or zero) that indicate
#' if neighboring planning units (per `data`) must not be
#' allocated to the corresponding combination of zones.
#' The cell values along the diagonal
#' of the matrix indicate if neighboring planning units must not be
#' allocated to the same zone. Defaults to an identity matrix
#' (i.e., a matrix with ones along the matrix diagonal and zeros elsewhere), so
#' that neighboring planning units cannot both be allocated to the same zone.
#'
#' @param data `NULL`, `matrix`, `Matrix`, `data.frame`, or
#' `array` object showing which planning units should be separated from
#' each other. For example, [adjacency_matrix()] can be used to separate
#' planning units from their immediate neighbors.
#' Alternatively, [proximity_matrix()] can be used to separate planning units
#' based on a distance threshold.
#' Defaults to `NULL` such that planning units are automatically separated
#' based on the [adjacency_matrix()] function.
#' See the Data format section for more information.
#'
#' @inheritSection add_neighbor_constraints Data format
#'
#' @inherit add_manual_locked_constraints return seealso
#'
#' @family constraints
#'
#' @examplesIf asNamespace("prioritizr")$do_run_example()
#' # load data
#' sim_pu_raster <- get_sim_pu_raster()
#' sim_features <- get_sim_features()
#'
#' # create minimal problem
#' p1 <-
#'   problem(sim_pu_raster, sim_features) %>%
#'   add_min_set_objective() %>%
#'   add_relative_targets(0.1) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # create problem with constraints so that none of the selected
#' # planning units are neighbors with each other
#' p2 <- p1 %>% add_separation_constraints()
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2))
#' names(s1) <- c("basic solution", "separation constraints")
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' # create problem so that each feature is represented in at least three
#' # spatially separate protected areas, using a zone for each replicate
#'
#' # prepare feature data for zones,
#' # here we define three zones called r1, r2, and r3, and each zone has
#' # a copy of the same feature data
#' z <- zones(
#'   "r1" = sim_features, "r2" = sim_features, "r3" = sim_features,
#'   feature_names = names(sim_features)
#' )
#'
#' # prepare zones,
#' # here we create a matrix that a value of 1 on non-diagonal elements and
#' # zones on diagonal elements of the matrix to specify that the separation
#' # constraints should be applied to planning units when they are allocated to
#' # different zones
#' zones_matrix <- matrix(1, nrow = 3, ncol = 3)
#' diag(zones_matrix) <- 0
#' print(zones_matrix)
#'
#' # create problem,
#' # here we specify that each zone has same cost values, targets so that each
#' # feature should be represented at least once in each and every zone, and
#' # separation constraints to prevent planning units that are allocated to
#' # different zones from being neighbors
#' p3 <-
#'   problem(c(sim_pu_raster, sim_pu_raster, sim_pu_raster), z) %>%
#'   add_min_set_objective() %>%
#'   add_absolute_targets(
#'     matrix(1, nrow = terra::nlyr(sim_features), ncol = 3)
#'   ) %>%
#'   add_separation_constraints(zones = zones_matrix) %>%
#'   add_binary_decisions() %>%
#'   add_default_solver(verbose = FALSE)
#'
#' # solve problem
#' s3 <- category_layer(solve(p3))
#'
#' # plot solution
#' plot(s3, main = "replicates", axes = FALSE)
#'
#' @name add_separation_constraints
#'
#' @exportMethod add_separation_constraints
#'
#' @aliases add_separation_constraints,ConservationProblem,ANY,array-method add_separation_constraints,ConservationProblem,ANY,matrix-method add_separation_constraints,ConservationProblem,ANY,data.frame-method add_separation_constraints,ConservationProblem,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_separation_constraints",
  signature = methods::signature("x", "zones", "data"),
  function(x, zones = diag(number_of_zones(x)), data = NULL) {
    assert_required(x)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      is_inherits(data, c("NULL", "matrix", "Matrix", "data.frame", "array"))
    )
    standardGeneric("add_separation_constraints")
  }
)

#' @name add_separation_constraints
#' @usage \S4method{add_separation_constraints}{ConservationProblem,ANY,ANY}(x, zones, data)
#' @rdname add_separation_constraints
methods::setMethod("add_separation_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY"),
  function(x, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      is_inherits(zones, c("matrix", "Matrix")),
      is_inherits(data, c("NULL", "Matrix"))
    )
    if (!is.null(data)) {
      # check argument to data if not NULL
      assert(
        ncol(data) == nrow(data),
        number_of_total_units(x) == ncol(data),
        all_binary(data)
      )
    } else {
      # check that planning unit data is spatially referenced
      assert(
        is_pu_spatially_explicit(x),
        msg = c(
          paste(
            "{.arg data} must be manually specified (e.g., as a {.cls Matrix})."
          ),
          "i" = paste(
            "This is because {.arg x} has planning unit data that are not",
            "spatially explicit",
            "(e.g., {.cls sf}, or {.cls SpatRaster} objects)."
          )
        )
      )
    }
    # add constraints
    internal_add_separation_constraints(x, zones, data)
  }
)

#' @name add_separation_constraints
#' @usage \S4method{add_separation_constraints}{ConservationProblem,ANY,data.frame}(x, zones, data)
#' @rdname add_separation_constraints
methods::setMethod("add_separation_constraints",
  methods::signature("ConservationProblem", "ANY", "data.frame"),
  function(x, zones, data) {
    # assert valid arguments
    assert(
      is.data.frame(data),
      assertthat::has_name(data, "id1"),
      assertthat::has_name(data, "id2"),
      assertthat::has_name(data, "boundary")
    )
    if (
      any(c("zone1", "zone2") %in% names(data))
    ) {
      assert(
        assertthat::has_name(data, "zone1"),
        assertthat::has_name(data, "zone2"),
        msg = paste(
          "{.arg data} must have {.col zone1} and {.col zone2} columns ",
          "when specifying constraints for multiple zones."
        )
      )
    }
    # add constraints
    add_separation_constraints(
      x, zones, marxan_connectivity_data_to_matrix(x, data, TRUE)
    )
  }
)

#' @name add_separation_constraints
#' @usage \S4method{add_separation_constraints}{ConservationProblem,ANY,matrix}(x, zones, data)
#' @rdname add_separation_constraints
methods::setMethod("add_separation_constraints",
  methods::signature("ConservationProblem", "ANY", "matrix"),
  function(x, zones, data) {
    # add constraints
    add_separation_constraints(x, zones, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name add_separation_constraints
#' @usage \S4method{add_separation_constraints}{ConservationProblem,ANY,array}(x, zones, data)
#' @rdname add_separation_constraints
methods::setMethod("add_separation_constraints",
  methods::signature("ConservationProblem", "ANY", "array"),
  function(x, zones, data) {
    # assert arguments are valid
    assert(
      is_conservation_problem(x),
      inherits(data, "array"),
      is.null(zones),
      length(dim(data)) == 4,
      dim(data)[1] == x$number_of_total_units(),
      dim(data)[2] == x$number_of_total_units(),
      dim(data)[3] == x$number_of_zones(),
      dim(data)[4] == x$number_of_zones(),
      all_finite(data),
      all_binary(data)
    )
    # add constraints
    internal_add_separation_constraints(x, zones, data)
  }
)

internal_add_separation_constraints <- function(x, zones, data) {
  # assert arguments valid
  assert(
    is_conservation_problem(x),
    is_inherits(data, c("NULL", "Matrix", "array")),
    .internal = TRUE
  )
  # convert zones to matrix
  if (!is.null(zones)) {
    zones <- as.matrix(zones)
    assert(
      is.numeric(zones),
      all_binary(zones),
      all_finite(zones),
      isSymmetric(zones),
      ncol(zones) == number_of_zones(x),
      call = fn_caller_env()
    )
    colnames(zones) <- x$zone_names()
    rownames(zones) <- colnames(zones)
  }
  # add the constraint
  x$add_constraint(
    R6::R6Class(
      "SeparationConstraint",
      inherit = Constraint,
      public = list(
        name = "separation constraints",
        data = list(zones = zones, data = data),
        calculate = function(x) {
          assert(is_conservation_problem(x))
          # if needed, generate adjacency matrix if null
          if (
            is.null(self$get_data("data")) &&
            is.Waiver(x$get_data("adjacency"))
          ) {
            x$set_data("adjacency", adjacency_matrix(x$data$cost))
          }
          # return success
          invisible(TRUE)
        },
        apply = function(x, y) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract data
          d <- self$get_data("data")
          if (is.null(d)) {
            d <- y$get_data("adjacency")
          }
          # prepare data
          m <- list()
          ind <- y$planning_unit_indices()
          if (inherits(d, "Matrix"))  {
            # if data is a matrix...
            d <- d[ind, ind]
            z <- self$get_data("zones")
            for (z1 in seq_len(ncol(z))) {
              m[[z1]] <- list()
              for (z2 in seq_len(nrow(z))) {
                m[[z1]][[z2]] <- as_Matrix(d * z[z1, z2], "dgCMatrix")
              }
            }
          } else if (inherits(d, "array")) {
            ## if data is an array...
            for (z1 in seq_len(dim(d)[3])) {
              m[[z1]] <- list()
              for (z2 in seq_len(dim(d)[4])) {
                m[[z1]][[z2]] <- as_Matrix(d[ind, ind, z1, z2], "dgCMatrix")
              }
            }
          } else {
            ## throw error if not recognized
            # nocov start
            cli::cli_abort(
              "Failed calculations for {.fn add_separation_constraints}.",
              .internal = TRUE,
            )
            # nocov end
          }
          # determine maximum number of neighbors for a given planning unit
          max_n <- m[[1]][[1]] * 0
          for (z1 in seq_len(ncol(z))) {
            for (z2 in seq_len(nrow(z))) {
              max_n <- max_n + m[[z1]][[z2]]
            }
          }
          Matrix::diag(max_n) <- 0
          max_n <- max(1, Matrix::rowSums(max_n))
          # apply constraints
          rcpp_apply_xor_constraints(x$ptr, m, max_n)
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
