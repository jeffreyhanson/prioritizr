#' @export
#' @rdname prioritize
prioritize_glpk <- function(pm,
                            gap = 1e-4,
                            time_limit = Inf,
                            first_feasible = FALSE,
                            bound = NA_real_) {
  UseMethod("prioritize_glpk")
}

#' @export
prioritize_glpk.minsetcover_model <- function(
  pm,
  gap = 1e-4,
  time_limit = Inf,
  first_feasible = FALSE,
  bound = NA_real_) {
  # assertions on arguments
  assert_that(requireNamespace("glpkAPI", quietly = TRUE),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound))

  # glpk has no means of returning the first feasible solution
  if (first_feasible) {
    stop("first_feasible = FALSE not permitted for GLPK solver.")
  }

  # construct model
  n_pu <- length(pm$cost)
  model <- glpkAPI::initProbGLPK()
  glpkAPI::setProbNameGLPK(model, "minsetcover")
  # goal is to minimize objective function
  glpkAPI::setObjDirGLPK(model, glpkAPI::GLP_MIN)
  # initialize decision variables
  glpkAPI::addColsGLPK(model, ncols = n_pu)
  # objective function
  # also specify no bounds on decision variables
  glpkAPI::setColsBndsObjCoefsGLPK(model, j = seq.int(n_pu),
                                   lb = NULL, ub = NULL,
                                   obj_coef = pm$cost,
                                   type = rep(glpkAPI::GLP_FR, n_pu))
  # binary decision variables
  glpkAPI::setColsKindGLPK(model, j = seq.int(n_pu),
                           kind = rep(glpkAPI::GLP_BV, n_pu))
  # constraints
  # initialize
  glpkAPI::addRowsGLPK(model, nrows = length(pm$targets))
  # set non-zero elements of constraint matrix
  glpkAPI::loadMatrixGLPK(model, ne = length(pm$rij$v),
                          ia = pm$rij$i, ja = pm$rij$j, ra = pm$rij$v)
  # rhs
  glpkAPI::setRowsBndsGLPK(model, i = seq_along(pm$targets),
                           lb = pm$targets, ub = NULL,
                           type = rep(glpkAPI::GLP_LO, length(pm$targets)))

  # locked planning units
  if (length(pm$locked_in) > 0) {
    lb <- rep(1, length(pm$locked_in))
    glpkAPI::setColsBndsGLPK(model, pm$locked_in, lb = lb, ub = lb)
  }
  if (length(pm$locked_out) > 0) {
    ub <- rep(0, length(pm$locked_in))
    glpkAPI::setColsBndsGLPK(model, pm$locked_out, lb = ub, ub = ub)
  }

  # stopping conditions
  # gap to optimality
  glpkAPI::setMIPParmGLPK(glpkAPI::MIP_GAP , gap)
  # stop after specified number of seconds, convert to milliseconds
  if (is.finite(time_limit)) {
    glpkAPI::setMIPParmGLPK(glpkAPI::TM_LIM, 1000 * time_limit)
  }

  # presolve and automatically calculate relaxed solution
  # otherwise glpkAPI::solveSimplexGLPK(model) must be called first
  glpkAPI::setMIPParmGLPK(glpkAPI::PRESOLVE, glpkAPI::GLP_ON)
  glpkAPI::setMIPParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_ON)

  # solve
  included <- pm$included
  rm(pm)
  t <- system.time(glpkAPI::solveMIPGLPK(model))

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(included)) {
    x <- rep(NA, length(included))
    x[included] <- glpkAPI::mipColsValGLPK(model)
  } else {
    x <- glpkAPI::mipColsValGLPK(model)
  }

  # prepare return object
  results <- structure(
    list(
      x = as.integer(round(x)),
      objval = glpkAPI::mipObjValGLPK(model),
      objbound = bound,
      gap = (glpkAPI::mipObjValGLPK(model) / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
  glpkAPI::delProbGLPK(model)
  return(results)
}

#' @export
prioritize_glpk.maxcover_model <- function(
  pm,
  gap = 1e-4,
  time_limit = Inf,
  first_feasible = FALSE,
  bound = NA_real_) {
  # assertions on arguments
  assert_that(requireNamespace("glpkAPI", quietly = TRUE),
              assertthat::is.number(gap),
              gap >= 0,
              assertthat::is.number(time_limit),
              time_limit > 0,
              assertthat::is.flag(first_feasible),
              assertthat::is.number(bound))

  # glpk has no means of returning the first feasible solution
  if (first_feasible) {
    stop("first_feasible = FALSE not permitted for GLPK solver.")
  }

  # construct model
  n_pu <- length(pm$cost)
  model <- glpkAPI::initProbGLPK()
  glpkAPI::setProbNameGLPK(model, "maxcover")
  # goal is to maximize objective function
  glpkAPI::setObjDirGLPK(model, glpkAPI::GLP_MAX)
  # initialize decision variables
  glpkAPI::addColsGLPK(model, ncols = n_pu)
  # objective function
  # also specify no bounds on decision variables
  glpkAPI::setColsBndsObjCoefsGLPK(model, j = seq.int(n_pu),
                                   lb = NULL, ub = NULL,
                                   obj_coef = slam::col_sums(pm$rij),
                                   type = rep(glpkAPI::GLP_FR, n_pu))
  # binary decision variables
  glpkAPI::setColsKindGLPK(model, j = seq.int(n_pu),
                           kind = rep(glpkAPI::GLP_BV, n_pu))
  # constraints
  # initialize
  glpkAPI::addRowsGLPK(model, nrows = 1)
  # set non-zero elements of constraint matrix
  glpkAPI::loadMatrixGLPK(model, ne = n_pu,
                          ia = rep(1, n_pu), ja = seq.int(n_pu),
                          ra = pm$cost)
  # rhs
  glpkAPI::setRowsBndsGLPK(model, i = 1, lb = NULL, ub = pm$budget,
                           type = glpkAPI::GLP_UP)

  # locked planning units
  if (length(pm$locked_in) > 0) {
    lb <- rep(1, length(pm$locked_in))
    glpkAPI::setColsBndsGLPK(model, pm$locked_in, lb = lb, ub = lb)
  }
  if (length(pm$locked_out) > 0) {
    ub <- rep(0, length(pm$locked_in))
    glpkAPI::setColsBndsGLPK(model, pm$locked_out, lb = ub, ub = ub)
  }

  # stopping conditions
  # gap to optimality
  glpkAPI::setMIPParmGLPK(glpkAPI::MIP_GAP , gap)
  # stop after specified number of seconds, convert to milliseconds
  if (is.finite(time_limit)) {
    glpkAPI::setMIPParmGLPK(glpkAPI::TM_LIM, 1000 * time_limit)
  }

  # presolve and automatically calculate relaxed solution
  # otherwise glpkAPI::solveSimplexGLPK(model) must be called first
  glpkAPI::setMIPParmGLPK(glpkAPI::PRESOLVE, glpkAPI::GLP_ON)
  glpkAPI::setMIPParmGLPK(glpkAPI::MSG_LEV, glpkAPI::GLP_MSG_ON)

  # solve
  included <- pm$included
  rm(pm)
  t <- system.time(glpkAPI::solveMIPGLPK(model))

  # if some planning units were excluded, convert back to full set
  if (!isTRUE(included)) {
    x <- rep(NA, length(included))
    x[included] <- glpkAPI::mipColsValGLPK(model)
  } else {
    x <- glpkAPI::mipColsValGLPK(model)
  }

  # prepare return object
  results <- structure(
    list(
      x = as.integer(round(x)),
      objval = glpkAPI::mipObjValGLPK(model),
      objbound = bound,
      gap = (glpkAPI::mipObjValGLPK(model) / bound - 1),
      time = summary(t)[["user"]]
    ),
    class = "prioritizr_results"
  )
  glpkAPI::delProbGLPK(model)
  return(results)
}
