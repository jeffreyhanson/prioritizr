#' @export
#' @rdname prioritize
prioritize_symphony <- function(pm,
                                gap = 1e-4,
                                time_limit = Inf,
                                first_feasible = FALSE,
                                bound = NA_real_) {
  UseMethod("prioritize_symphony")
}

#' @export
#' @rdname prioritize
prioritize_symphony.minsetcover_model <- function(
  pm,
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

  # assign Rsymphony or lpsymphony depending on availability
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    symphony_solve_LP <- Rsymphony::Rsymphony_solve_LP
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    symphony_solve_LP <- lpsymphony::lpsymphony_solve_LP
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }
  # solve
  t <- system.time({
    results <- Rsymphony::Rsymphony_solve_LP(
      # objective function
      obj = pm$cost,
      # constraints
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

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(pm$included)) {
    x <- rep(NA, length(pm$included))
    x[pm$included] <- results$x
  } else {
    x <- results$x
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

#' @export
#' @rdname prioritize
prioritize_symphony.maxcover_model <- function(
  pm,
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

  # assign Rsymphony or lpsymphony depending on availability
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    symphony_solve_LP <- Rsymphony::Rsymphony_solve_LP
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    symphony_solve_LP <- lpsymphony::lpsymphony_solve_LP
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }

  # solve
  t <- system.time({
    results <- Rsymphony::Rsymphony_solve_LP(
      # objective function
      obj = slam::col_sums(pm$rij),
      # constraints
      mat = matrix(unname(pm$cost[]), nrow = 1),
      dir = "<=",
      rhs = pm$budget,
      # binary decision variables
      types = "B",
      # locked planning units
      bounds = bounds,
      # goal is to minimize objective function
      max = TRUE,
      # gap to optimality
      gap_limit = gap,
      # stop after specified number of seconds
      time_limit = ifelse(is.finite(time_limit), time_limit, -1),
      # first feasible solution
      first_feasible = first_feasible
    )
  })

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(pm$included)) {
    x <- rep(NA, length(pm$included))
    x[pm$included] <- results$x
  } else {
    x <- results$x
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

# find the relaxed solution using symphony
relaxed_symphony <- function(pm) {
  UseMethod("relaxed_symphony")
}

relaxed_symphony.minsetcover_model <- function(pm) {
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

  # assign Rsymphony or lpsymphony depending on availability
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    symphony_solve_LP <- Rsymphony::Rsymphony_solve_LP
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    symphony_solve_LP <- lpsymphony::lpsymphony_solve_LP
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }
  # solve relaxed
  results <- symphony_solve_LP(
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
  list(x = results$solution, objval = results$objval)
}

relaxed_symphony.maxcover_model <- function(pm) {
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

  # assign Rsymphony or lpsymphony depending on availability
  if (requireNamespace("Rsymphony", quietly = TRUE)) {
    symphony_solve_LP <- Rsymphony::Rsymphony_solve_LP
  } else if (requireNamespace("lpsymphony", quietly = TRUE)) {
    symphony_solve_LP <- lpsymphony::lpsymphony_solve_LP
  } else {
    stop("Neither Rsymphony nor lpsymphony are installed")
  }
  # solve relaxed
  results <- symphony_solve_LP(
    # objective function
    obj = slam::col_sums(pm$rij),
    # structural constraints
    mat = matrix(unname(pm$cost[]), nrow = 1),
    dir = "<=",
    rhs = pm$budget,
    # decision variables between 0 and 1
    types = "C",
    bounds = bounds,
    # goal is to minimize objective function
    max = TRUE
  )
  list(x = results$solution, objval = results$objval)
}
