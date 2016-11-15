#' @export
#' @rdname prioritize
prioritize_gurobi <- function(pm,
                              gap = 1e-4,
                              time_limit = Inf,
                              first_feasible = FALSE,
                              bound = NA_real_,
                              threads = 0) {
  UseMethod("prioritize_gurobi")
}

#' @export
prioritize_gurobi.minsetcover_model <- function(
  pm,
  gap = 1e-4,
  time_limit = Inf,
  first_feasible = FALSE,
  bound = NA_real_,
  threads = 0) {
  # assertions on arguments
  assert_that(requireNamespace("gurobi", quietly = TRUE),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound),
              is_integer(threads), length(threads) == 1)

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
    model$lb <- rep(0, length(pm$cost))
    model$lb[pm$locked_in] <- 1
  }
  if (length(pm$locked_out) > 0) {
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
  # number of threads
  if (threads > 0) {
    params <- c(params, Threads = threads)
  }

  # solve
  included <- pm$included
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

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(included)) {
    x <- rep(NA, length(included))
    x[included] <- results$x
  } else {
    x <- results$x
  }
  # prepare return object
  structure(
    list(
      x = as.integer(round(x)),
      objval = results$objval,
      objbound = bound,
      gap = abs(results$objval / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
}

#' @export
prioritize_gurobi.maxcover_model <- function(
  pm,
  gap = 1e-4,
  time_limit = Inf,
  first_feasible = FALSE,
  bound = NA_real_,
  threads = 0) {
  # assertions on arguments
  assert_that(requireNamespace("gurobi", quietly = TRUE),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound),
              is_integer(threads), length(threads) == 1)

  # construct model
  model <- list()
  # goal is to minimize objective function
  model$modelsense <- "max"
  # binary decision variables
  model$vtype <- "B"
  # objective function
  model$obj <- slam::col_sums(pm$rij)
  # constraints
  model$A <- matrix(unname(pm$cost), nrow = 1)
  model$rhs <- pm$budget
  model$sense <- "<="
  # locked planning units
  if (length(pm$locked_in) > 0) {
    model$lb <- rep(0, length(pm$cost))
    model$lb[pm$locked_in] <- 1
  }
  if (length(pm$locked_out) > 0) {
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
  # number of threads
  if (threads > 0) {
    params <- c(params, Threads = threads)
  }

  # solve
  included <- pm$included
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

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(included)) {
    x <- rep(NA, length(included))
    x[included] <- results$x
  } else {
    x <- results$x
  }
  # prepare return object
  structure(
    list(
      x = as.integer(round(x)),
      objval = results$objval,
      objbound = bound,
      gap = abs(results$objval / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
}

#' @export
prioritize_gurobi.maxtargets_model <- function(
  pm,
  gap = 1e-4,
  time_limit = Inf,
  first_feasible = FALSE,
  bound = NA_real_,
  threads = 0) {
  # assertions on arguments
  assert_that(requireNamespace("gurobi", quietly = TRUE),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound),
              is_integer(threads), length(threads) == 1)

  # construct model
  model <- list()
  # goal is to minimize objective function
  model$modelsense <- "max"
  # binary decision variables
  model$vtype <- "B"
  # objective function
  obj <- c(-0.01 * pm$cost / sum(pm$cost, na.rm = TRUE),
           rep(1, length(pm$targets)))
  #obj <- c(rep(0, length(pm$cost)), rep(1, length(pm$targets)))
  model$obj <- obj
  # constraints
  model$A <- rbind(
    cbind(pm$rij, slam::simple_triplet_diag_matrix(v = -pm$targets)),
    slam::simple_triplet_matrix(i = rep(1, length(pm$cost)),
                                j = seq_along(pm$cost),
                                v = pm$cost,
                                nrow = 1,
                                ncol = length(pm$cost) + length(pm$targets))
  )
  model$rhs <- c(rep(0, length(pm$targets)), pm$budget)
  model$sense <- c(rep('>=', length(pm$targets)), '<=')
  # locked planning units
  if (length(pm$locked_in) > 0) {
    model$lb <- rep(0, length(pm$cost) + length(pm$targets))
    model$lb[pm$locked_in] <- 1
  }
  if (length(pm$locked_out) > 0) {
    model$ub <- rep(1, length(pm$cost) + length(pm$targets))
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
  # number of threads
  if (threads > 0) {
    params <- c(params, Threads = threads)
  }

  # solve
  included <- pm$included
  n_pu <- length(pm$cost)
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

  # remove indicator variables
  results$x <- results$x[seq_len(n_pu)]

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(included)) {
    x <- rep(NA, length(included))
    x[included] <- results$x
  } else {
    x <- results$x
  }
  # prepare return object
  structure(
    list(
      x = as.integer(round(x)),
      objval = results$objval,
      objbound = bound,
      gap = abs(results$objval / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
}
