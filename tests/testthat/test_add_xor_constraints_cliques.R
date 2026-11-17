test_that("compile (single zone)", {
  skip_if_not_installed("igraph")
  # create data
  pu <- terra::rast(matrix(c(1, 0.1, 1, 1)))
  ft <- terra::rast(matrix(c(1, 1, 0, 1)))
  m <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(2, 3, 3, 4),
    x = c(1, 1, 1, 1),
    dims = c(4, 4), symmetric = TRUE
  )
  # create problem
  p <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(2) %>%
    add_xor_constraints(method = "cliques", data = m) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # tests
  expect_equal(o$rhs()[-1], rep(1, 2))
  expect_equal(o$sense()[-1], rep("<=", 2))
  expect_equal(o$row_ids()[-1], rep("x", 2))
  oA <- o$A()[c(2, 3), , drop = FALSE]
  oA <- oA[order(Matrix::rowSums(oA)), , drop = FALSE]
  expect_equal(oA[1, ], c(0, 0, 1, 1))
  expect_equal(oA[2, ], c(1, 1, 1, 0))
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("igraph")
  # create data
  pu <- terra::rast(matrix(c(1, 0.1, 1, 1)))
  ft <- terra::rast(matrix(c(1, 1, 0, 1)))
  m <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(2, 3, 3, 4),
    x = c(1, 1, 1, 1),
    dims = c(4, 4), symmetric = TRUE
  )
  # create problem
  p <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(2) %>%
    add_xor_constraints(method = "cliques", data = m) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # tests
  expect_equal(c(terra::values(s)), c(0, 1, 0, 1))
})

test_that("infeasibility (single zone)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  skip_if_not_installed("igraph")
  # create data
  pu_data <- terra::rast(matrix(c(1, 1, 1, 1, 1, 1), nrow = 2))
  locked_in <- terra::rast(matrix(c(1, 1, 0, 0, 0, 0), nrow = 2))
  ft_data <- terra::rast(matrix(c(0, 1, 0, 0, 0, 1), nrow = 2))
  # create problem
  p <-
    problem(pu_data, ft_data) %>%
    add_min_set_objective() %>%
    add_absolute_targets(1) %>%
    add_locked_in_constraints(locked_in) %>%
    add_xor_constraints(
      method = "cliques",
      data = adjacency_matrix(pu_data)
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # tests
  expect_error(
    solve(p, force = TRUE, run_checks = FALSE),
    "impossible"
  )
})

test_that("invalid inputs (single zone)", {
  skip_if_not_installed("igraph")
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  adj_matrix <- adjacency_matrix(sim_pu_polygons)
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(
    add_xor_constraints(p, "asdf", data = adj_matrix),
    "method"
  )
  expect_tidy_error(
    add_xor_constraints(p, NA_character_, data = adj_matrix),
    "method"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", matrix(2), adj_matrix),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", matrix(NA_real_), adj_matrix),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", diag(2), adj_matrix),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, data = "a"),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, data = matrix(1, 3, 3)),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p),
    "data"
  )
})

test_that("compile (multiple zones)", {
  skip_if_not_installed("igraph")
  # create data
  pu <- c(
    terra::rast(matrix(c(1, 0.1, NA, 1))),
    terra::rast(matrix(c(1, 0.1, NA, 1)))
  )
  ft <- zones(
    terra::rast(matrix(c(0, 1, 0, 1))),
    terra::rast(matrix(c(1, 1, 0, 0)))
  )
  m <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(2, 3, 3, 4),
    x = c(1, 1, 1, 1),
    dims = c(4, 4), symmetric = TRUE
  )
  z <- matrix(1, ncol = 2, nrow = 2)
  diag(z) <- 0
  # create problem
  p <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, ncol = 2, nrow = 1)) %>%
    add_xor_constraints(method = "cliques", zones = z, data = m) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # calculations for tests
  rows <- seq(2 + length(p$planning_unit_indices()) + 1, o$nrow())
  # tests
  expect_length(rows, 2)
  expect_equal(o$rhs()[rows], rep(1, 2))
  expect_equal(o$sense()[rows], rep("<=", 2))
  expect_equal(o$row_ids()[rows], rep("x", 2))
  oA <- o$A()[rows, , drop = FALSE]
  oA <- oA[order(vapply(order(rows), function(i) oA[i, 1], numeric(1))), ]
  expect_equal(oA[1, ], c(0, 1, 0, 1, 0, 0))
  expect_equal(oA[2, ], c(1, 0, 0, 0, 1, 0))
})

test_that("compile (Matrix and array are equivalent)", {
  skip_if_not_installed("igraph")
  # create data
  pu <- c(
    terra::rast(matrix(c(1, 0.1, NA, 1))),
    terra::rast(matrix(c(1, 0.1, NA, 1)))
  )
  ft <- zones(
    terra::rast(matrix(c(0, 1, 0, 1))),
    terra::rast(matrix(c(1, 1, 0, 0)))
  )
  # create matrix with xor constraints
  m <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(2, 3, 3, 4),
    x = c(1, 1, 1, 1),
    dims = c(4, 4), symmetric = TRUE
  )
  z <- matrix(1, ncol = 2, nrow = 2)
  diag(z) <- 0
  # create array with xor constraints
  a <- array(0, dim = c(4, 4, 2, 2))
  a[, , 1, 2] <- as.matrix(m)
  a[, , 2, 1] <- as.matrix(m)
  # create problems
  p1 <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, ncol = 2, nrow = 1)) %>%
    add_xor_constraints(method = "cliques", zones = z, data = m) %>%
    add_binary_decisions()
  p2 <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, ncol = 2, nrow = 1)) %>%
    add_xor_constraints(method = "cliques", zones = NULL, data = a) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$vtype(), o2$vtype())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$col_ids(), o2$col_ids())
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$row_ids(), o2$row_ids())
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_no_solvers_installed()
  skip_if_not_installed("igraph")
  # create data
  pu <- c(
    terra::rast(matrix(c(1, 0.05, NA, 1))),
    terra::rast(matrix(c(1, 0.1, NA, 1)))
  )
  ft <- zones(
    terra::rast(matrix(c(0, 1, 0, 1))),
    terra::rast(matrix(c(1, 1, 0, 0)))
  )
  m <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(2, 3, 3, 4),
    x = c(1, 1, 1, 1),
    dims = c(4, 4), symmetric = TRUE
  )
  z <- matrix(1, ncol = 2, nrow = 2)
  diag(z) <- 0
  # create problem
  p <-
    problem(pu, ft) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, ncol = 2, nrow = 1)) %>%
    add_xor_constraints(method = "cliques", zones = z, data = m) %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0, verbose = FALSE)
  # solve problem
  s <- solve(p, run_checks = FALSE)
  # tests
  expect_equal(terra::values(s)[, 1], c(0, 0, NA, 1))
  expect_equal(terra::values(s)[, 2], c(0, 1, NA, 0))
})

test_that("invalid input (multiple zones)", {
  skip_if_not_installed("igraph")
  # import data
  sim_zones_pu_polygons <- get_sim_zones_pu_polygons()
  sim_zones_features <- get_sim_zones_features()
  # create data
  z <- diag(3)
  a <- array(1, dim = c(rep(nrow(sim_zones_pu_polygons), 2), 3, 3))
  m <- adjacency_matrix(sim_zones_pu_polygons)
  # create problem
  p <-
    problem(
      sim_zones_pu_polygons, sim_zones_features,
      c("cost_1", "cost_2", "cost_3")
    ) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, ncol = 3, nrow = 5))
  # tests
  ## method
  expect_tidy_error(
    add_xor_constraints(p, "asdf", z, m),
    "method"
  )
  expect_tidy_error(
    add_xor_constraints(p, NA_character_, z, m),
    "method"
  )
  ## zones
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z[, -1], m),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z[-1, ], m),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z - 1, m),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z + 1, m),
    "zones"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", `[<-`(z, 1, 1, NA), m),
    "zones"
  )
  ## data = Matrix
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, m[-1, ]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, m[, -1]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, m + 1),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, `[<-`(m, 1, 2, value = NA_real_)),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, `[<-`(m, 1, 3, value = 1)),
    "symmetric"
  )
  ## data = array
  expect_tidy_error(
    add_xor_constraints(p, "cliques", z, a),
    "NULL"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a[-1, , , ]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a[, -1, , ]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a[, , -1, ]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a[, , , -1]),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a + 1),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, a - 2),
    "data"
  )
  expect_tidy_error(
    add_xor_constraints(p, "cliques", NULL, `[<-`(a, 1, 1, 1, 1, NA)),
    "data"
  )
})
