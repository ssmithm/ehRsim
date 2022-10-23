#' Generate a character variable
#'
#' `gen_var_num()` generates a character variable.
#'
#' @param v name of variable. vector of options to be sampled from.
#' @param nc positive integer indicating number of observations to generate; or,
#'  single data.frame from which to match observations
#' @param dist lowercase name of distribution (in quotes). Options are: normal
#' @param int logical indicating whether returned values should be rounded to
#' integers or not
#' @param ... Other options used to control the random number generation, e.g., mean,
#' var, lamba, min, max, n, etc.
#' @returns a data.frame with # observations equal to `nc` dataset.
#' @examples
#' x <- gen_var_num(v = systolic, nc = 1000, dist = "normal", int = TRUE, mean = 130, sd = 16)
#' @export
gen_var_num <- function(v, nc, dist, int = FALSE, ...) {

  if (is.numeric(nc) & length(nc) == 1) {

      s = nc

  } else if (is.data.frame(nc)) {

      s = nrow(nc)

  } else {

    stop("nc must be either a vector of length 1 or a data frame")

  }

  if (dist == "normal") {

    x <- data.frame(v = stats::rnorm(n = s, mean = mean, sd = sd))

  } else if (dist == "uniform") {

    x <- data.frame(v = stats::runif(n = s, min = min, max = as.integer(max)))

  } else if (dist == "sample.integer") {

    x <- data.frame(v = sample.int(n = n, size = s, replace = TRUE))

  } else {

    stop(paste0("Distribution ", dist, " not implemented yet."))

  }

  if (int == TRUE) {

    x <- x |> round(digits = 0)

  }

}
