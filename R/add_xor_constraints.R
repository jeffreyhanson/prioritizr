#' @include internal.R Constraint-class.R marxan_connectivity_data_to_matrix.R
NULL

#' Add exclusive disjunction (xor) constraints
#'
#' Add constraints to a conservation planning problem to specify that
#' particular pairs of planning units should not both be selected
#' (in other words, an exclusive or logical operation).
#' If the problem has multiple zones, then these constraints can
#' be used to ensure that a pair of planning units cannot both be allocated
#' to the same zone, or that one planning unit cannot be allocated to particular
#' zone if another planning unit is also allocated to a particular zone.
#'
#' @inheritParams add_manual_locked_constraints
#'
#' @param method `character` value denoting the name of the formulation
#' method. Available options include the cliques (`"cliques"`) and
#' pairwise (`"pairwise"`) methods. Although the cliques method generally has
#' better performance, the pairwise may have better performance for
#' especially large or complex problems. Note that the cliques method
#' requires installation of the \pkg{igraph} package. Defaults to `"cliques"`.
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
#'   add_default_solver(gap = 0, verbose = FALSE)
#'
#' # define matrix for xor constraints so that a solution cannot select
#' # (i) both of the 85th and 86th raster pixels, and
#' # (ii) both the 97th and 98th raster pixels
#' xor_matrix <- Matrix::sparseMatrix(
#'   i = c(85, 97), j = c(86, 98), x = 1,
#'   dims = rep(terra::ncell(sim_pu_raster), 2),
#'   symmetric = TRUE
#' )
#'
#' # create problem with constraints so that the solution cannot
#' # select these pairs of planning units
#' p2 <- p1 %>% add_xor_constraints(data = xor_matrix)
#'
#' # solve problems
#' s1 <- c(solve(p1), solve(p2))
#' names(s1) <- c("basic solution", "xor constraints")
#'
#' # prepare raster with labels to show pixel numbers
#' i <- sim_pu_raster * 0
#' i[] <- seq_len(terra::ncell(i))
#'
#' # plot solutions
#' plot(s1, axes = FALSE, fun = function() terra::text(i))
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
  function(x, method = "cliques", zones = diag(number_of_zones(x)), data) {
    assert_required(x)
    assert_required(method)
    assert_required(zones)
    assert_required(data)
    assert(
      is_conservation_problem(x),
      assertthat::is.string(method),
      assertthat::noNA(method),
      is_match_of(method, c("cliques", "pairwise")),
      is_inherits(data, c("matrix", "Matrix", "data.frame", "array"))
    )
    if (identical(method, "cliques")) {
      assert(
        is_installed("igraph"),
        msg = paste(
          "The {.pkg igraph} must be installed to use",
          "{.code method = \"cliques\"}."
        )
      )
    }
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
      is_inherits(zones, c("matrix", "Matrix")),
      is_inherits(data, "Matrix")
    )
    # check argument to data
    assert(
      ncol(data) == nrow(data),
      number_of_total_units(x) == ncol(data),
      all_binary(data),
      is_symmetric_matrix(data)
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
    assertthat::is.string(method),
    is_inherits(data, c("Matrix", "array")),
    .internal = TRUE
  )
  # convert zones to matrix
  if (!is.null(zones)) {
    zones <- as.matrix(zones)
    assert(
      is.numeric(zones),
      all_binary(zones),
      all_finite(zones),
      is_symmetric_matrix(zones),
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
          z <- self$get_data("zones")
          method <- self$get_data("method")
          indices <- y$planning_unit_indices()
          # process data
          m <- list()
          if (inherits(d, "Matrix")) {
            ## if data is a dgCMatrix...
            d <- d[indices, indices, drop = FALSE]
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
                m[[z1]][[z2]] <- as_Matrix(
                  d[indices, indices, z1, z2],
                  "dgCMatrix"
                )
              }
            }
          } else {
            ## throw error if not recognized
            # nocov start
            cli::cli_abort(
              "Failed calculations for {.fn add_xor_constraints}.",
              .internal = TRUE
            )
            # nocov end
          }
          # apply constraints
          if (identical(method, "cliques")) {
            internal_apply_xor_constraints(x, m)
          } else {
            rcpp_apply_xor_constraints2(x$ptr, m)
          }
        }
      )
    )$new()
  )
}

internal_apply_xor_constraints <- function(x, data) {
  # assert valid argument
  assert(
    inherits(x, "OptimizationProblem"),
    is.list(data),
    internal = TRUE
  )
  # create matrix with rows/columns for each planning unit status variable,
  # (i.e., combination of planning unit and zone)
  ## initialize matrix
  n_pu <- ncol(data[[1]][[1]])
  n_z <- length(data)
  m <- Matrix::sparseMatrix(
    i = 1, j = 1, x = 0, dims = rep(n_pu * n_z, 2)
  )
  ## add in connections based on data
  for (i in seq_along(data)) {
    for (j in seq_along(data[[i]])) {
      m2 <- as_Matrix(data[[i]][[j]], "dgTMatrix")
      m3 <- matrix(
        c(
          ((i - 1L) * n_pu) + (m2@i + 1L),
          ((j - 1L) * n_pu) + (m2@j + 1L)
        ),
        ncol = 2
      )
      m[m3[, c(2, 1)]] <- m2@x
    }
  }
  ## add in connections for the same planning unit allocated to different zones,
  ## this is done to tighten formulation
  if (n_z > 1L) {
    for (i in seq_along(data)) {
      for (j in seq_along(data[[i]])) {
        m3 <- matrix(
          c(
            ((i - 1L) * n_pu) + seq_len(n_pu),
            ((j - 1L) * n_pu) + seq_len(n_pu)
          ),
          ncol = 2
        )
        m[m3[, c(1, 2)]] <- 1
      }
    }
  }
  # find cliques based on the matrix
  cl <- igraph::max_cliques(
    igraph::graph_from_adjacency_matrix(m, mode = "undirected", diag = FALSE),
    min = 2
  )
  # free memory
  rm(m, m2, m3)
  # if at least one clique present, then add constraints based on cliques
  if (length(cl) > 0) {
    rcpp_apply_xor_constraints(x$ptr, cl)
  }
  # return success
  invisible(TRUE)
}
