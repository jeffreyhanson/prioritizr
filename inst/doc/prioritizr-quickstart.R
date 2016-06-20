## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/quickstart_",
  fig.align = "center"
)

## ----packages, message=FALSE---------------------------------------------
library(prioritizr)
# for plotting
library(rasterVis)
library(viridis)

## ----generate-data-------------------------------------------------------
set.seed(1)
# raster 100x100 template
e <- raster::extent(0, 100, 0, 100)
r <- raster::raster(e, nrows = 100, ncols = 100, vals = 1)
# generate 9 feature distributions with different scales and range sizes
species <- mapply(function(x, y, r) gaussian_field(r = r, range = x, prop = y),
            rep(c(5, 25), each = 2),
            rep(c(0.1, 0.5), times = 2),
            MoreArgs = list(r = r))
species <- raster::stack(species)
species <- setNames(species, letters[1:raster::nlayers(species)])
levelplot(species, main = 'Species Distributions', layout = c(2, 2),
          scales = list(draw = FALSE),
          col.regions = c("grey20", "#fd9900"), colorkey = FALSE)
# genrate cost layer
cost <- gaussian_field(r, 20, mean = 1000, variance = 500)
cost <- setNames(cost, "cost")
levelplot(cost, main = "Cost", margin = FALSE, col.regions = viridis::viridis(100))

## ----msc_model-----------------------------------------------------------
msc_model <- minsetcover_model(pu = cost, features = species, targets = 0.2)
class(msc_model)

## ----mc_model------------------------------------------------------------
b_25 <- 0.25 * raster::cellStats(cost, "sum")
mc_model <- maxcover_model(pu = cost, features = species, budget = b_25)
class(mc_model)

## ----msc_solve, results="hide"-------------------------------------------
msc_results <- prioritize(msc_model, gap = 0.001)

## ----mc_solve, results="hide"--------------------------------------------
mc_results <- prioritize(mc_model, solver = "symphony", gap = 0.001)

## ----solutions-----------------------------------------------------------
plot_selection(cost, msc_results$x, title = "Minimum Set Cover Solution")
plot_selection(cost, mc_results$x, title = "Maximum Cover Solution")

## ----objectives----------------------------------------------------------
# minimum set cover objective
msc_results$objval
# maximum cover objective
mc_results$objval

