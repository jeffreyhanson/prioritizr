test_that("compile (sf, single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()[c(1:2, 10:12, 20:22), ]
  sim_features <- get_sim_features()
  adj_m <- adjacency_matrix(sim_pu_polygons)
  # create problems
  p1 <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_separation_constraints(method = "pairwise")
  p2 <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_xor_constraints(method = "pairwise", data = adj_m)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$vtype(), o2$vtype())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$A(), o2$A())
})

test_that("invalid input (single zone)", {
  # import data
  sim_pu_polygons <- get_sim_pu_polygons()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_polygons, sim_features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2)
  # run tests
  expect_tidy_error(
    add_separation_constraints(p, method = "pairwise", zones = matrix(2))
  )
  expect_tidy_error(
    add_separation_constraints(p, method = "pairwise", zones = matrix(NA_real_))
  )
  expect_tidy_error(
    add_separation_constraints(p, method = "pairwise", zones = diag(2))
  )
  expect_tidy_error(
    add_separation_constraints(p, method = "pairwise", data = "a")
  )
  expect_tidy_error(
    add_separation_constraints(p, method = "pairwise", data = matrix(1, 3, 3))
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
    add_separation_constraints(method = "pairwise")
  })
})

test_that("compile (SpatRaster, multiple zones)", {
  # import data
  sim_zones_pu_raster <- get_sim_zones_pu_raster()
  sim_zones_features <- get_sim_zones_features()
  # create data
  zones <- matrix(1, ncol = 3, nrow = 3)
  diag(zones) <- 0
  adj_m <- adjacency_matrix(sim_zones_pu_raster)
  # create problems
  p1 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_separation_constraints(method = "pairwise", zones = zones, data = adj_m)
  p2 <-
    problem(sim_zones_pu_raster, sim_zones_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(matrix(0.2, nrow = 5, ncol = 3)) %>%
    add_xor_constraints(method = "pairwise", zones = zones, data = adj_m)
  # compile problems
  o1 <- compile(p1)
  o2 <- compile(p2)
  # run tests
  expect_equal(o1$obj(), o2$obj())
  expect_equal(o1$vtype(), o2$vtype())
  expect_equal(o1$ub(), o2$ub())
  expect_equal(o1$lb(), o2$lb())
  expect_equal(o1$rhs(), o2$rhs())
  expect_equal(o1$sense(), o2$sense())
  expect_equal(o1$A(), o2$A())
})
