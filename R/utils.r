#' Convert data frame to matrix
#'
#' Convert a data frame with row index, column index, and matrix components into
#' a corresponding matrix. By default returns a
#' \code{\link[slam]{simple_triplet_matrix}} sparse matrix from the slam
#' package.
#'
#' @param x data.frame; with variables \code{x$i} and \code{x$j} for the row and
#'   column indices, respectively, and \code{x$v} for the matrix components.
#'   Alternative names for these variables can be specified by \code{vars}.
#' @param nrow,ncol integer; the number of rows and columns, respectively.
#'   Defaults to the maximum row and column indices, respectively.
#' @param vars character vector; three element character vector with names of
#'   row, column, and matrix component variables repsectively.
#' @param sparse logical; whether to return a sparse matrix or a normal base
#'   matrix object.
#' @param add_lower logical; if the data frame represents a symmetric matrix
#'   where only the upper triangle is provided explicitly, set \code{add_lower}
#'   to \code{TRUE} to add in these values explicitly to the matrix.
#'
#' @return A \code{\link[slam]{simple_triplet_matrix}} sparse matrix from the
#'   slam package, or a normal base matrix object.
#'
#' @export
#' @examples
#' df <- data.frame(i = c(1, 1, 3), j = c(1, 3, 3), v = c(10, 20, 30))
#' df_to_matrix(df)
#' df_to_matrix(df, sparse = FALSE)
#' df_to_matrix(df, sparse = FALSE, add_lower = TRUE)
#'
#' df <- data.frame(a = c(1, 1), b = c(1, 2), c = c(5, 25))
#' df_to_matrix(df, sparse = FALSE, vars = c("a", "b", "c"))
df_to_matrix <- function(x, nrow, ncol, sparse = TRUE, add_lower = FALSE,
                         vars = c("i", "j", "v")) {

  # assertions
  assert_that(is.data.frame(x),
              missing(nrow) || assertthat::is.count(nrow),
              missing(ncol) || assertthat::is.count(ncol),
              length(vars) == 3,
              all(vars %in% names(x)),
              assertthat::is.flag(sparse),
              assertthat::is.flag(add_lower),
              is.character(vars))

  # custom variable names
  x <- x[vars]
  names(x) <- c("i", "j", "v")
  assert_that(is_integer(x$i), is_integer(x$j), is.numeric(x$v))

  # number of rows and columns
  if (missing(nrow)) {
    nrow <- max(x$i)
  }
  if (missing(ncol)) {
    ncol <- max(x$j)
  }

  # add lower triangle
  if (add_lower) {
    assert_that(all(x$i <= x$j))
    x_lower <-  x[x$i != x$j, ]
    tmp_id <- x$i
    x$i <- x$j
    x$j <- tmp_id
    x <- rbind(x, x_lower)
    # if triangular matrix must be symmetric
    n <- max(nrow, ncol)
    nrow <- n
    ncol <- n
  }
  m <- slam::simple_triplet_matrix(i = x$i, j = x$j, v = x$v,
                                   nrow = max(nrow), ncol = max(ncol))
  if (!sparse) {
    m <- as.matrix(m)
  }
  return(m)
}

is_integer <- function(x) {
  all(floor(x) == x, na.rm = TRUE)
}
