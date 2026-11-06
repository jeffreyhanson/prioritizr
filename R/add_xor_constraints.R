#' @include internal.R Constraint-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add exclusive or constraints
#'
#' Add constraints to a conservation planning problem to specify that
#' particular pairs of planning units should not both be selected.
#' (in other words, an exclusive or logical operation) .
#' If the problem has multiple zones, then these constraints can
#' be used to ensure that a pair of planning units cannot both be allocated
#' to the same zone, or that one planning unit cannot be allocated to particular
#' zone if another planning unit is also allocated to a particular zone.
#'
#' @inheritParams add_separation_constraints
#'
#' @param zones `matrix` or `Matrix` object describing the
#' separation scheme for different zones. Each row and column corresponds
#' to a different zone in `x`, and cell values must
#' contain binary `numeric` values (i.e., one or zero) that indicate
#' if neighboring planning units (per `data`) must not be
#' allocated to the corresponding combination of zones.
#' The cell values along the diagonal
#' of the matrix indicate if pairs of planning units must not be
#' allocated to the same zone. Defaults to an identity matrix
#' (i.e., a matrix with ones along the matrix diagonal and zeros elsewhere), so
#' that pairs of planning units cannot both be allocated to the same zone.
#'
#' @param data `NULL`, `matrix`, `Matrix`, `data.frame`, or
#' `array` object indicating which pairs of planning units cannot
#' both be selected. For example, if `data` specifies that planning unit
#' A and B planning unit cannot both be selected, then this means that
#' a solution may select (i) only planning unit A, (ii) only planning unit B,
#' or (iii) neither of the two planning units.
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
#' # create problem with constraints so that the solution cannot
#' # select any pairs of neighboring planning units
#' p2 <- p1 %>% add_xor_constraints(data = adjacency_matrix(sim_pu_raster))
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2))
#' names(s1) <- c("basic solution", "xor constraints")
#'
#' # plot solutions
#' plot(s1, axes = FALSE)
#'
#' @name add_xor_constraints
#'
#' @exportMethod add_xor_constraints
#'
#' @aliases add_xor_constraints,ConservationProblem,ANY,ANY,array-method add_xor_constraints,ConservationProblem,ANY,ANY,matrix-method add_xor_constraints,ConservationProblem,ANY,ANY,data.frame-method add_xor_constraints,ConservationProblem,ANY,ANY,ANY-method
NULL

#' @export
methods::setGeneric("add_xor_constraints",
  signature = methods::signature("x", "method", "zones", "data"),
  function(x, method = "simple", zones = diag(number_of_zones(x)), data) {
    assert_required(x)
    assert_required(method)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      assertthat::is.string(method),
      assertthat::noNA(method),
      is_match_of(method, c("simple", "knapsack")),
      is_inherits(data, c("matrix", "Matrix", "data.frame", "array"))
    )
    standardGeneric("add_xor_constraints")
  }
)

#' @name add_xor_constraints
#' @usage \S4method{add_xor_constraints}{ConservationProblem,ANY,ANY,ANY}(x, method, zones, data)
#' @rdname add_xor_constraints
methods::setMethod("add_xor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "ANY"),
  function(x, method, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.string(method),
      assertthat::noNA(method),
      is_match_of(method, c("simple", "knapsack")),
      is_inherits(zones, c("matrix", "Matrix")),
      is_inherits(data, "Matrix")
    )
    # check argument to data
    assert(
      ncol(data) == nrow(data),
      number_of_total_units(x) == ncol(data),
      all_binary(data)
    )
    # add constraints
    internal_add_xor_constraints(x, method, zones, data)
  }
)

#' @name add_xor_constraints
#' @usage \S4method{add_xor_constraints}{ConservationProblem,ANY,ANY,data.frame}(x, method, zones, data)
#' @rdname add_xor_constraints
methods::setMethod("add_xor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "data.frame"),
  function(x, method, zones, data) {
    # assert valid arguments
    assert(
      is_conservation_problem(x),
      assertthat::is.string(method),
      assertthat::noNA(method),
      is_match_of(method, c("simple", "knapsack")),
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
    add_xor_constraints(
      x, method, zones, marxan_connectivity_data_to_matrix(x, data, TRUE)
    )
  }
)

#' @name add_xor_constraints
#' @usage \S4method{add_xor_constraints}{ConservationProblem,ANY,ANY,matrix}(x, method, zones, data)
#' @rdname add_xor_constraints
methods::setMethod("add_xor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "matrix"),
  function(x, method, zones, data) {
    # add constraints
    add_xor_constraints(x, method, zones, as_Matrix(data, "dgCMatrix"))
  }
)

#' @name add_xor_constraints
#' @usage \S4method{add_xor_constraints}{ConservationProblem,ANY,ANY,array}(x, method, zones, data)
#' @rdname add_xor_constraints
methods::setMethod("add_xor_constraints",
  methods::signature("ConservationProblem", "ANY", "ANY", "array"),
  function(x, method, zones, data) {
    # assert arguments are valid
    assert(
      is_conservation_problem(x),
      assertthat::is.string(method),
      assertthat::noNA(method),
      is_match_of(method, c("simple", "knapsack")),
      is.null(zones),
      inherits(data, "array"),
      length(dim(data)) == 4,
      dim(data)[1] == x$number_of_total_units(),
      dim(data)[2] == x$number_of_total_units(),
      dim(data)[3] == x$number_of_zones(),
      dim(data)[4] == x$number_of_zones(),
      all_finite(data),
      all_binary(data)
    )
    # add constraints
    internal_add_xor_constraints(x, method, zones, data)
  }
)

internal_add_xor_constraints <- function(x, method, zones, data) {
  # assert arguments valid
  assert(
    is_conservation_problem(x),
    is_inherits(data, c("Matrix", "array")),
    assertthat::is.string(method),
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
      "XorConstraint",
      inherit = Constraint,
      public = list(
        name = "xor constraints",
        data = list(method = method, zones = zones, data = data),
        apply = function(x, y) {
          # assert valid arguments
          assert(
            inherits(x, "OptimizationProblem"),
            inherits(y, "ConservationProblem"),
            .internal = TRUE
          )
          # extract data
          d <- self$get_data("data")
          method <- self$get_data("method")
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
            z <- 0
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
              "Failed calculations for {.fn add_xor_constraints}.",
              .internal = TRUE,
            )
            # nocov end
          }
          # determine maximum number of neighbors for a given planning unit
          max_n <- m[[1]][[1]] * 0
          for (z1 in seq_along(m)) {
            for (z2 in seq_along(m[[z1]])) {
              max_n <- max_n + m[[z1]][[z2]]
            }
          }
          Matrix::diag(max_n) <- 0
          max_n <- max(1, Matrix::rowSums(max_n))
          # apply constraints
          if ((sum(d) + sum(z)) > 1e-15) {
            if (identical(method, "knapsack")) {
              rcpp_apply_xor_constraints2(x$ptr, m, max_n)
            } else {
              rcpp_apply_xor_constraints(x$ptr, m)
            }
          }
          # return success
          invisible(TRUE)
        }
      )
    )$new()
  )
}
