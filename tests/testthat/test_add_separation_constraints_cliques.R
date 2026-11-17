test_that("compile (single zone)", {
  skip_if_not_installed("igraph")
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  adj_matrix <- adjacency_matrix(sim_pu_polygons)
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_separation_constraints(data = adj_matrix, method = "cliques")
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- nrow(sim_pu_polygons)
  n_f <- terra::nlyr(sim_features)
  cl <- igraph::max_cliques(
    igraph::graph_from_adjacency_matrix(
      adj_matrix, mode = "undirected", diag = FALSE
    ),
    min = 2
  )
  m <- Matrix::sparseMatrix(
    i = rep(seq_along(cl), lengths(cl)),
    j = as.integer(unlist(cl)),
    x = 1, dims = c(length(cl), n_pu)
  )
  sep_rows <- seq(n_f + 1, o$nrow())
  # tests
  expect_equal(o$rhs()[sep_rows], rep(1, length(sep_rows)))
  expect_equal(o$sense()[sep_rows], rep("<=", length(sep_rows)))
  expect_equal(o$row_ids()[sep_rows], rep("x", length(sep_rows)))
  expect_equal(as.matrix(o$A()[sep_rows, ]), as.matrix(m))
})

test_that("compile (data format, single zone)", {
  skip_if_not_installed("igraph")
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  # create matrix data
  a1 <- adjacency_matrix(sim_pu_polygons)
  a2 <- as.matrix(a1)
  a3 <- setNames(matrix_to_triplet_dataframe(a1), c("id1", "id2", "boundary"))
  a4 <- array(c(a2), dim = c(rep(nrow(sim_pu_polygons), 2), 1, 1))
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # compile problems
  o1 <- compile(add_separation_constraints(p), method = "cliques")
  o2 <- compile(add_separation_constraints(p, method = "cliques", data = a1))
  o3 <- compile(add_separation_constraints(p, method = "cliques", data = a2))
  o4 <- compile(add_separation_constraints(p, method = "cliques", data = a3))
  o5 <- compile(
    add_separation_constraints(p, method = "cliques", zones = NULL, data = a4)
  )
  # test that all problems are equivalent
  expect_equal(o1$A(), o2$A())
  expect_equal(o1$A(), o3$A())
  expect_equal(o1$A(), o4$A())
  expect_equal(o1$A(), o5$A())
  expect_equal(o1$rhs(), o5$rhs())
  expect_equal(o1$sense(), o5$sense())
})

test_that("solve (single zone)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("igraph")
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # define adjacency matrix
  adj_matrix <- adjacency_matrix(sim_pu_polygons)
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.1) %>%
    add_separation_constraints(data = adj_matrix, method = "cliques") %>%
    add_binary_decisions() %>%
    add_default_solver(gap = 0.1, verbose = FALSE)
  # solve problem
  s <- solve(p)
  # calculations for tests
  sel <- which(s$solution_1 > 0.5)
  # tests
  expect_gt(length(sel), 0)
  expect_equal(sum(adj_matrix[sel, sel]), 0)
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
    add_separation_constraints(method = "cliques") %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # tests
  expect_error(
    solve(p, force = TRUE, run_checks = FALSE),
    "impossible"
  )
})

test_that("invalid input (single zone)", {
  skip_if_not_installed("igraph")
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_binary_decisions()
  # tests
  expect_tidy_error(
    add_separation_constraints(p, "asdf"),
    "method"
  )
  expect_tidy_error(
    add_separation_constraints(p, NA_character_),
    "method"
  )
  expect_tidy_error(
    add_separation_constraints(p, zones = matrix(2)),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, zones = matrix(NA_real_)),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, zones = diag(2)),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, data = "a"),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, data = matrix(1, 3, 3)),
    "data"
  )
  expect_tidy_error({
    # create problem without spatial data
    pu <- data.frame(
      id = seq_len(10), cost = c(0.2, NA, runif(8)),
      spp1 = runif(10), spp2 = c(rpois(9, 4), NA)
    )
    problem(
      pu$cost, data.frame(id = seq_len(2), name = c("spp1", "spp2")),
      as.matrix(t(pu[, 3:4]))
    ) %>%
    add_separation_constraints()
  }, "spatially explicit")
})

test_that("compile (multiple zones)", {
  skip_if_not_installed("igraph")
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create data
  zones <- matrix(1, ncol = 3, nrow = 3)
  diag(zones) <- 0
  adj_matrix <- adjacency_matrix(sim_zones_pu_raster)
  # create problem
  p <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_separation_constraints(
      method = "cliques",
      zones = zones,
      data = adj_matrix
    ) %>%
    add_binary_decisions()
  # compile problem
  o <- compile(p)
  # calculations for tests
  n_pu <- p$number_of_planning_units()
  n_z <- p$number_of_zones()
  n_f <- p$number_of_features()
  pu_ind <- p$planning_unit_indices()
  m <- Matrix::sparseMatrix(
    i = 1, j = 1, x = 0, dims = rep(n_pu * n_z, 2)
  )
  for (i in seq_len(n_z)) {
    for (j in seq_len(n_z)) {
      m2 <- as_Matrix(adj_matrix[pu_ind, pu_ind] * zones[i, j], "dgTMatrix")
      m3 <- matrix(
        c(((i - 1) * n_pu) + (m2@i + 1), ((j - 1) * n_pu) + (m2@j + 1)),
        ncol = 2
      )
      m[m3[, c(1, 2)]] <- m2@x
    }
  }
  cl <- igraph::max_cliques(
    igraph::graph_from_adjacency_matrix(m, mode = "undirected", diag = FALSE),
    min = 2
  )
  m <- Matrix::sparseMatrix(
    i = rep(seq_along(cl), lengths(cl)),
    j = as.integer(unlist(cl)),
    x = 1, dims = c(length(cl), n_pu * n_z)
  )
  sep_rows <- seq(n_pu + (n_f * n_z) + 1, o$nrow())
  # tests
  expect_equal(o$rhs()[sep_rows], rep(1, length(sep_rows)))
  expect_equal(o$sense()[sep_rows], rep("<=", length(sep_rows)))
  expect_equal(o$A()[sep_rows, ], m)
})

test_that("solve (multiple zones)", {
  skip_on_cran()
  skip_if_no_fast_solvers_installed()
  skip_if_not_installed("igraph")
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()[[1:2]]
  # prepare data for problem
  n_r <- 3
  n_f <- terra::nlyr(sim_features)
  fts <- do.call(
    what = zones,
    append(
      stats::setNames(
        list(sim_features)[rep(1, n_r)],
        nm = paste0("r", seq_len(n_r))
      ),
      list(feature_names = names(sim_features))
    )
  )
  pu <- sim_pu_raster[[rep(1, n_r)]]
  names(pu) <- paste0("r", seq_len(n_r))
  z2 <- matrix(1, nrow = n_r, ncol = n_r)
  diag(z2) <- 0
  # create problem
  p <-
    problem(pu, fts) %>%
    add_min_set_objective() %>%
    add_absolute_targets(matrix(1, nrow = n_f, ncol = n_r)) %>%
    add_separation_constraints(
      method = "cliques",
      zones = z2,
      data = adjacency_matrix(pu[[1]], directions = 8)
    ) %>%
    add_binary_decisions() %>%
    add_default_solver(verbose = FALSE)
  # solve problem
  s <- solve(p)
  # calculations for tests
  ## assign each planning unit to replicate id
  rep_ids <- category_layer(s)
  rep_ids[rep_ids < 1] <- NA_real_
  ## assign each planning unit to a patch id, where neighboring planning
  ## units get the same patch id
  patch_ids <- terra::patches(terra::as.int(rep_ids), directions = 8)
  ## calculate zonal statistics for each patch id, so we can see
  ## if the a single patch contains multiple different replicate identifiers
  result_min <- terra::zonal(rep_ids, z = patch_ids, "min")
  result_max <- terra::zonal(rep_ids, z = patch_ids, "max")
  # tests
  expect_equal(result_min[[2]], result_max[[2]])
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
    add_separation_constraints(p, "asdf", z, m),
    "method"
  )
  expect_tidy_error(
    add_separation_constraints(p, NA_character_, z, m),
    "method"
  )
  ## zones
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z[, -1]),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z[-1, ]),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z - 1),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z + 1),
    "zones"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", `[<-`(z, 1, 1, NA)),
    "zones"
  )
  ## data = Matrix
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z, m[-1, ]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z, m[, -1]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z, m + 1),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(
      p, "cliques", z, `[<-`(m, 1, 2, value = NA_real_)
    ),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(
      p, "cliques", z, `[<-`(m, 1, 3, value = 1)
    ),
    "symmetric"
  )
  ## data = array
  expect_tidy_error(
    add_separation_constraints(p, "cliques", z, a),
    "NULL"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a[-1, , , ]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a[, -1, , ]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a[, , -1, ]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a[, , , -1]),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a + 1),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, a - 2),
    "data"
  )
  expect_tidy_error(
    add_separation_constraints(p, "cliques", NULL, `[<-`(a, 1, 1, 1, 1, NA)),
    "data"
  )
})
