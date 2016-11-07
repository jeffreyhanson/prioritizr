#' Solve a conservation prioritization problem using ILP
#'
#' Solve a systematic conservation planning problem using the the methods of
#' Integer Linear Programming (ILP). In particular, this function solves either
#' the the minimum set cover problem or the maximum coverage problem for
#' systematic reserve design. In addition, this function provides a unified
#' interface to a variety of optimization packages and gives the user the
#' ability to chose which to use based on the availability on their machine.
#' Choosing a solver is done via the \code{solver} argument of the
#' \code{prioritize()} function, or by invoking one of the \code{prioritize_*}
#' functions.
#'
#' @section Prioritization problems:
#'
#' The \code{prioritizr} package currently handles three types of conservation
#' prioritization problems. Each type of problem has a corresponding
#' \code{prioritizr_model} object that encapsulates the problem and is passed
#' as the first argument to \code{prioritize()}:
#'
#' \itemize{
#'   \item \bold{Minimum set cover}: find the set of planning units that
#'   minimizes the overall cost of a reserve network, while meeting a set of
#'   representation targets for the conservation features. The cost is often
#'   either the area of the planning units or the opportunity cost of foregone
#'   commericial activities (e.g. logging or agriculture). The representation
#'   targets ensure that each species is adequately represented in the reserve
#'   network.
#'
#'   This problem is equivalent to a simplified Marxan reserve design problem,
#'   with the Bounday Length Modifier (BLM) set to zero. Use
#'   \code{\link{minsetcover_model}} to construct a minimum set cover model.
#'
#'   \item \bold{Maximum coverage}: find the set of planning units that
#'   maximizes the overall level of representation across a suite of
#'   conservation features, while keeping cost within a fixed budget. The cost
#'   is often either the area of the planning units or the opportunity cost of
#'   foregone commericial activities (e.g. logging or agriculture).
#'   Representation level is typically given by the occupancy within each
#'   planning unit, however, some measure of abundance or probability of
#'   occurence may also be used.
#'
#'   This problem is roughly the opposite of what the conservation planning
#'   software Marxan does. Use \code{\link{maxcover_model}} to construct a
#'   maximum coverage model.
#'
#'   \item \bold{Maximum coverage of targets}: this is a modified version of
#'   the maximum coverage problem. Each conservation feature is assigned a
#'   representation target and the objective is to find the set of planning
#'   units that meets most targets while remaining within a fixed budget.
#'
#'   This problem is meant to be a hybrid between the maximum coverage problem
#'   and a Marxan-like minimum set cover problem in that it allows for both a
#'   budget and targets to be set. Use \code{\link{maxtargets_model}} to
#'   construct a maximum target coverage model.
#' }
#'
#' @section Solvers:
#'
#' The following optimization packages (and corresponding values for the
#' \code{solver} parameter) are supported. The packages must be installed
#' independently to function:
#'
#' \itemize{
#'   \item \bold{\code{gurobi}}: \href{http://gurobi.com}{Gurobi} is a
#'   state-of-the-art commercial optimization software with an R package
#'   interface. It is by far the fastest of the solvers available in this
#'   package, however, it is also the only one that isn't free. That said, free
#'   academic licenses are available.
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
#'   (\href{https://www.gnu.org/software/glpk/}{GLPK}) is an open-source
#'   package for solving linear and integer linear programs. The R package
#'   glpkAPI provides an interface to the low-level GLPK API.
#' }
#'
#' @param pm \code{prioritizr_model} object specifying the prioritization model
#'   to solve. This will typically be output from one of the corresponding
#'   functions: \code{\link{minsetcover_model}} or \code{\link{maxcover_model}}.
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
#' @param threads non-negative integer; the number of threads to use for the
#'   optimization algorithm. The default value of 0 will result in all cores in
#'   the machine being used. Currently only implemented for Gurobi.
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
#'
#' # prepare minimum set cover prioritization model
#' # use 20% targets
#' msc_model <- minsetcover_model(x = cost, features = f, targets = 0.2)
#' # solve to within 1 percent of optimality
#' # pick solver automatically (uses Gurobi if installed)
#' msc_results <- prioritize(msc_model, gap = 0.001)
#' plot_selection(cost, msc_results$x)
#' # specify SYMPHONY solver
#' msc_results_symphony <- prioritize(msc_model, solver = "symphony",
#'                                    gap = 0.001)
#' plot_selection(cost, msc_results_symphony$x)
#'
#' # prepare maximum coverage prioritization model
#' # set budget to 25% of total cost
#' b_25 <- 0.25 * raster::cellStats(cost, "sum")
#' mc_model <- maxcover_model(x = cost, features = f, budget = b_25)
#' # solve to within 1 percent of optimality
#' # pick solver automatically (uses Gurobi if installed)
#' mc_results <- prioritize(mc_model, gap = 0.001)
#' plot_selection(cost, mc_results$x)
#'
#' # prepare maximum target coverage prioritization model
#' # set budget to 25% of total cost
#' b_25 <- 0.25 * raster::cellStats(cost, "sum")
#' mtc_model <- maxtargets_model(x = cost, features = f, budget = b_25,
#'                             targets = 0.5)
#' # solve to within 1 percent of optimality
#' # pick solver automatically (uses Gurobi if installed)
#' mtc_results <- prioritize(mtc_model, gap = 0.001)
#' plot_selection(cost, mtc_results$x)
prioritize <- function(pm,
                       solver = c("best", "gurobi", "symphony", "glpk"),
                       gap = 1e-4,
                       time_limit = Inf,
                       first_feasible = FALSE,
                       bound = NA_real_,
                       threads = 0) {
  # assertions on arguments
  solver <- match.arg(solver)
  assert_that(inherits(pm, "prioritizr_model"),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound),
              is_integer(threads), length(threads) == 1)
  # choose best solver
  if (solver == "best") {
    if (requireNamespace("gurobi", quietly = TRUE)) {
      solver <- "gurobi"
    } else if (requireNamespace("Rsymphony", quietly = TRUE)) {
      solver <- "symphony"
    } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
      solver <- "symphony"
    } else if (requireNamespace("glpkAPI", quietly = TRUE)) {
       solver <- "glpk"
    } else {
      stop("No ILP solvers installed.")
    }
  }
  # pick solver
  if (solver == "gurobi") {
    pr <- prioritize_gurobi(pm = pm, gap = gap, time_limit = time_limit,
                            first_feasible = first_feasible, bound = bound,
                            threads = threads)
  } else if (solver == "symphony") {
    pr <- prioritize_symphony(pm = pm, gap = gap, time_limit = time_limit,
                              first_feasible = first_feasible, bound = bound)
  } else if (solver == "glpk") {
    if (first_feasible) {
      stop("first_feasible = FALSE not permitted for GLPK solver.")
    }
    pr <- prioritize_glpk(pm = pm, gap = gap, time_limit = time_limit,
                          bound = bound)
  } else {
    stop("Invalid solver.")
  }
  return(pr)
}
