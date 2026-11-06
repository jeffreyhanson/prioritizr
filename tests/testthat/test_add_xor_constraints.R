test_that("compile (sf, single zone)", {
  # import data
  sim_pu_raster <- get_sim_pu_raster()
  sim_features <- get_sim_features()
  # create problem
  p <-
    problem(sim_pu_raster, sim_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.2) %>%
    add_xor_constraints(data = adjacency_matrix(sim_pu_raster))
  # compile problem
  o <- compile(p)
})
