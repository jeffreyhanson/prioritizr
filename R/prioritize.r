#' Solve a conservation prioritization problem using ILP
#'
#' Solve a systematic conservation planning problem using the the methods of
#' Integer Linear Programming (ILP). In particular, this function solves the
#' minimum set cover reserve design optimization problem. This is equivalent to
#' a simplified Marxan reserve design problem, with the Bounday Length Modifier
#' (BLM) set to zero. This function is an interface to a variety of optimization
#' packages and gives the user the ability to chose which to use based on the
#' availability on their machine. Choosing a solver is done via the
#' \code{solver} argument of the \code{prioritize()} function, or by calling
#' one of the sovler-specific functions (e.g. \code{prioritize_gurobi()}).
#'
#' @details
#'
#' In the context of systematic reserve design, the minimum set cover problem
#' seeks to find the set of planning units that minimizes the overall cost of a
#' reserve network, while meeting a set of representation targets for the
#' conservation features. The cost is often either the area of the planning
#' units or the opportunity cost of foregone commericial activities (e.g.
#' logging or agriculture). The representation targets ensure that each species
#' is adequately represented in the reserve network. This is a simplified
#' version of the Marxan objective function that doesn't account for the
#' boundary length of the resulting reserve.
#'
#' Marxan solves this optimization problem using simulated annealing, a
#' stochastic heuristic for approximating global optima of functions. However,
#' this problem can be formualted as a Integer Linar Program (ILP) for which
#' exact algorithms exist. This function uses the R interface to one of several
#' optimization packages to solve reserve design problems either exactly
#' or to within some specified gap to optimality.
#'
#' @section Solvers:
#'
#' The following optimization packages (and corresponding values for the
#' \code{solver} parameter) are supported:
#'
#' \itemize{
#'  \item \bold{\code{gurobi}}: \href{http://gurobi.com}{Gurobi} is a
#'  state-of-the-art commercial optimization software with an R package
#'  interface. It is by far the fastest of the solvers available in this
#'  package, however, it is also the only one that isn't free. That said, free
#'  academic licenses are available.
#'   \item \bold{\code{symphony}}:
#'   \href{https://projects.coin-or.org/SYMPHONY}{SYMPHONY} is an open-source
#'   integer programming solver that is part of the Computational Infrastructure
#'   for Operations Research (COIN-OR) project, an initiative to promote
#'   development of open-source tools for operations research (a field that
#'   includes linear programming). Two R packages exist to provide interfaces to
#'   SYMPHONY: \code{Rsymphony} (on CRAN) and \code{lpsymphony} (on
#'   Bioconductor). On Windows and Mac, \code{lpsymphony} may be easier to
#'   install. \code{prioritize()} will choose whichever package is available if
#'   \code{solver = "symphony"} is used.
#'   \item \bold{\code{glpk}}: The GNU Linear Programming Kit
#'     (\href{https://www.gnu.org/software/glpk/}{GLPK}) is an open-source
#'     package for solving linear and integer linear programs. The R package
#'     glpkAPI provides an interface to the low-level GLPK API.
#' }
#'
#' @param pm \code{prioritizr_model} object specifying the prioritization model
#'   to solve. This will typically be output from the function
#'   \code{\link{prioritizr_model}}.
#' @param solver character; specify the optimization package to use to solve the
#'   prioritization problem. Availability will depend on what packages are
#'   installed, however, the full list of possibilities is: gurobi, symphony,
#'   and glpk. Alternatively, specify "best" to automatically choose the best
#'   optimizer currently installed.
#' @param gap numeric; the relative gap to optimality. The optimizer will
#'   terminate when the difference between the upper and lower objective
#'   function bounds is less than the gap times the upper bound. For example, a
#'   value of 0.01 will result in the optimizer stopping when the difference
#'   between the bounds is 1 percent of the upper bound.
#' @param time_limit numeric; time limit in seconds to run the optimizer. The
#'   solver will return the current best solution when this time limit is
#'   exceeded.
#' @param first_feasible logical; whether to return the first feasible solution.
#'   If \code{first_feasible} is set to \code{TRUE}, the solver will return the
#'   first solution it encounters that meets all the constraints, regardless of
#'   solution quality. Note that the first feasible solution is not an arbitrary
#'   solution, rather it is derived from the relaxed solution, and is therefore
#'   often reasonably close to optimality.
#' @param bound numeric; lower bound on the objective function. To return the
#'   gap to optimality, the solver requires a lower bound on the objective
#'   function. Gurobi will estimate this lower bound, however, most other
#'   solvers won't, in which case, this argument can be supplied if the bound is
#'   known from some other source. This value is only used to calculate the
#'   optimality gap and will have no effect on the operation of the solver.
#'
#' @return A \code{prioritizr_results} object containing the solution to the
#'   prioritization problem. This is an S3 object consisting of a list with the
#'   following components:
#'
#' \itemize{
#'   \item \code{x}: vector of decision variables for the best solution. 1
#'     indicates that a planning unit is selected, 0 that it isn't.
#'   \item \code{objval}: objective function value for optimal solution.
#'   \item \code{objbound}: lower bound on objective function. This is useful
#'     only if the solver was stopped before finding the optimum. If the solver
#'     was run to completion, \code{objbound} and \code{objval} will be equal.
#'   \item \code{gap}: the relative gap to optimality:
#'     \code{objval} / \code{objbound} - 1. \code{gap} will be zero if the
#'     solver was run to comletion and the true optimum was found.
#'   \item \code{time}: the execution time (in seconds) of the solver.
#' }
#'
#' @export
#' @examples
#' # raster 100x100 template
#' e <- raster::extent(0, 100, 0, 100)
#' r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
#'
#' # generate 9 feature distributions with different scales and range sizes
#' f <- mapply(function(x, y, r) gaussian_field(r = r, range = x, prop = y),
#'             rep(c(5, 15, 25), each = 3),
#'             rep(c(0.1, 0.25, 0.5), times = 3),
#'             MoreArgs = list(r = r))
#' f <- raster::stack(f)
#' f <- setNames(f, letters[1:raster::nlayers(f)])
#' # genrate cost layer
#' cost <- gaussian_field(r, 20, mean = 1000, variance = 500)
#' cost <- setNames(cost, "cost")
#' # prepare prioritization model
#' model <- prioritizr_model(pu = cost, features = f,
#'                           # 20 percent targets
#'                           targets = 0.2,
#'                           # lock first 100 planning units in
#'                           locked_in = 1:100,
#'                           # lock last 100 planning units out
#'                           locked_out = 9901:10000)
#' # solve to within 1 percent of optimality
#' # pick solver automatically (uses Gurobi if installed)
#' results <- prioritize(model, gap = 0.001)
#' plot_selection(cost, results$x)
#' # specify SYMPHONY solver
#' results_symphony <- prioritize(model, solver = "symphony", gap = 0.001)
#' plot_selection(cost, results_symphony$x)
prioritize <- function(pm,
                       solver = c("best", "gurobi", "symphony", "glpk"),
                       gap = 1e-4,
                       time_limit = Inf,
                       first_feasible = FALSE,
                       bound = NA_real_) {
  # assertions on arguments
  solver <- match.arg(solver)
  assert_that(assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound))
  # choose best solver
  if (solver == "best") {
    if (requireNamespace("gurobi", quietly = TRUE)) {
      solver <- "gurobi"
    } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
      solver <- "symphony"
    } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
      solver <- "symphony"
    # } else if (requireNamespace("glpkAPI", quietly = TRUE)) {
    #   solver <- "glpk"
    } else {
      stop("No ILP solvers installed.")
    }
  }
  # pick solver
  if (solver == "gurobi") {
    pr <- prioritize_gurobi(pm = pm, gap = gap, time_limit = time_limit,
                            first_feasible = first_feasible, bound = bound)
  } else if (solver == "symphony") {
    pr <- prioritize_symphony(pm = pm, gap = gap, time_limit = time_limit,
                              first_feasible = first_feasible, bound = bound)
  } else {
    stop("Invalid solver.")
  }
  return(pr)
}

#' @export
#' @rdname prioritize
prioritize_gurobi <- function(pm,
                              gap = 1e-4,
                              time_limit = Inf,
                              first_feasible = FALSE,
                              bound = NA_real_) {
  # assertions on arguments
  assert_that(requireNamespace("gurobi", quietly = TRUE),
              inherits(pm, "prioritizr_model"),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound))

  # construct model
  model <- list()
  # goal is to minimize objective function
  model$modelsense <- "min"
  # binary decision variables
  model$vtype <- "B"
  # objective function
  model$obj <- pm$cost
  # constraints
  model$A <- pm$rij
  model$rhs <- pm$targets
  model$sense <- rep(">=", length(pm$targets))
  # locked planning units
  if (length(pm$locked_in) > 0) {
    # li_mat <- simple_triplet_matrix(
    #   i = seq.int(length(pm$locked_in)),
    #   j = pm$locked_in,
    #   v = rep(1, length(pm$locked_in)),
    #   ncol = ncol(model$A)
    # )
    # model$A <- rbind(model$A, li_mat)
    # model$rhs <- c(model$rhs, rep(1, nrow(li_mat)))
    # model$sense <- c(model$sense, rep("=", nrow(li_mat)))
    # rm(li_mat)
    model$lb <- rep(0, length(pm$cost))
    model$lb[pm$locked_in] <- 1
  }
  if (length(pm$locked_out) > 0) {
    # lo_mat <- simple_triplet_matrix(
    #   i = seq.int(length(pm$locked_out)),
    #   j = pm$locked_out,
    #   v = rep(1, length(pm$locked_out)),
    #   ncol = ncol(model$A)
    # )
    # model$A <- rbind(model$A, lo_mat)
    # model$rhs <- c(model$rhs, rep(0, nrow(lo_mat)))
    # model$sense <- c(model$sense, rep("=", nrow(lo_mat)))
    # rm(lo_mat)
    model$ub <- rep(1, length(pm$cost))
    model$ub[pm$locked_out] <- 0
  }

  # stopping conditions
  # gap to optimality
  params <- list(Presolve = -1, MIPGap = gap)
  # stop after specified number of seconds
  if (is.finite(time_limit)) {
    params <- c(params, TimeLimit = time_limit)
  }
  # first feasible solution
  if (first_feasible) {
    params <- c(params, SolutionLimit = 1)
  }

  # solve
  rm(pm)
  t <- system.time(
    results <- gurobi::gurobi(model, params)
  )
  # get rid of log file
  if (file.exists("gurobi.log")) {
    unlink("gurobi.log")
  }

  if (is.na(bound)) {
    bound <- results$objbound
  }
  # prepare return object
  structure(
    list(
      x = as.integer(round(results$x)),
      objval = results$objval,
      objbound = bound,
      gap = (results$objval / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
}

# find the relaxed solution using symphony
relaxed_symphony <- function(pm) {
  # assertions on arguments
  assert_that(inherits(pm, "prioritizr_model"))

  # bounded between 0 and 1
  n_pu <- length(pm$cost)
  bounds <- list(lower = list(ind = seq.int(n_pu), val = rep(0, n_pu)),
                 upper = list(ind = seq.int(n_pu), val = rep(1, n_pu)))

  # locked planning units
  if (length(pm$locked_in) > 0) {
    bounds$lower$val[pm$locked_in] <- 1
  }
  if (length(pm$locked_out) > 0) {
    bounds$upper$val[pm$locked_out] <- 0
  }

  # solve
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    results <- Rsymphony::Rsymphony_solve_LP(
      # objective function
      obj = pm$cost,
      # structural constraints
      mat = pm$rij,
      dir = rep(">=", length(pm$targets)),
      rhs = pm$targets,
      # decision variables between 0 and 1
      types = "C",
      bounds = bounds,
      # goal is to minimize objective function
      max = FALSE
    )
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    results <- lpsymphony::lpsymphony_solve_LP(
      # objective function
      obj = pm$cost,
      # structural constraints
      mat = pm$rij,
      dir = rep(">=", length(pm$targets)),
      rhs = pm$targets,
      # decision variables between 0 and 1
      types = "C",
      bounds = bounds,
      # goal is to minimize objective function
      max = FALSE
    )
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }
  list(x = results$solution, objval = results$objval)
}

#' @export
#' @rdname prioritize
prioritize_symphony <- function(pm,
                                gap = 1e-4,
                                time_limit = Inf,
                                first_feasible = FALSE,
                                bound = NA_real_) {

  # assertions on arguments
  assert_that(inherits(pm, "prioritizr_model"),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound))

  # symphony takes an absolute gap, but not a relative gap
  # estimate absolute gap from objective function for relaxed solution
  # convert relative to absolute gap
  if (gap == 0) {
    gap = -1
    t_gap <- 0
  } else {
    t_gap <- system.time({
      relaxed <- relaxed_symphony(pm)
    })
    t_gap <- summary(t_gap)[["user"]]
    gap <- gap * relaxed$objval
    rm(relaxed)
  }

  # locked planning units
  bounds <- NULL
  if (length(pm$locked_in) > 0 || length(pm$locked_out) > 0) {
    lb <- list(ind = c(pm$locked_in, pm$locked_out),
               val = c(rep(1, length(pm$locked_in)),
                       rep(0, length(pm$locked_out))
               )
    )
    ub <- list(ind = c(pm$locked_in, pm$locked_out),
               val = c(rep(1, length(pm$locked_in)),
                       rep(0, length(pm$locked_out))
               )
    )
    bounds <- list(lower = lb, upper = ub)
  }

  # first check for Rsymphony
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    t <- system.time({
      results <- Rsymphony::Rsymphony_solve_LP(
        # objective function
        obj = pm$cost,
        # structural constraints
        mat = pm$rij,
        dir = rep(">=", length(pm$targets)),
        rhs = pm$targets,
        # binary decision variables
        types = "B",
        # locked planning units
        bounds = bounds,
        # goal is to minimize objective function
        max = FALSE,
        # gap to optimality
        gap_limit = gap,
        # stop after specified number of seconds
        time_limit = ifelse(is.finite(time_limit), time_limit, -1),
        # first feasible solution
        first_feasible = first_feasible
      )
    })
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    t <- system.time({
      results <- lpsymphony::lpsymphony_solve_LP(
        # objective function
        obj = pm$cost,
        # structural constraints
        mat = pm$rij,
        dir = rep(">=", length(pm$targets)),
        rhs = pm$targets,
        # binary decision variables
        types = "B",
        # locked planning units
        bounds = bounds,
        # goal is to minimize objective function
        max = FALSE,
        # gap to optimality
        gap_limit = gap,
        # stop after specified number of seconds
        time_limit = ifelse(is.finite(time_limit), time_limit, -1),
        # first feasible solution
        first_feasible = first_feasible
      )
    })
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }

  # prepare return object
  structure(
    list(
      x = as.integer(round(results$solution)),
      objval = results$objval,
      objbound = bound,
      gap = (results$objval / bound - 1),
      time = summary(t)[["user"]] + t_gap
    ),
    class = "prioritizr_results"
  )
}
