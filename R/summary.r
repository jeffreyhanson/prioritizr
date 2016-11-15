#' Summarize results of prioritization exercise
#'
#' @param model \code{prioritizr_model} object
#' @param results \code{prioritizr_results} object
#'
#' @return A list containing the number of planning units (\code{n_pu}), total
#'   cost (\code{cost}), representation level of each feature
#'   (\code{amount_held}), and whether or not each target was achieved, for the
#'   prioritization solution.
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
#'
#' # solve
#' msc_results <- prioritize(msc_model)
#' summary(msc_model, msc_results)
solution_summary <- function(model, results)  {
  assert_that(inherits(model, "prioritizr_model"),
              inherits(results, "prioritizr_results"))
  s <- list()
  s$n_pu <- sum(results$x)
  s$cost <- sum(results$x[model$included] * model$cost)
  s$amount_held <- (as.matrix(model$rij) %*% results$x[model$included])[, 1]
  if ("targets" %in% names(model)) {
    s$targets_met <- (s$amount_held >= model$targets)
  }
  return(s)
}
