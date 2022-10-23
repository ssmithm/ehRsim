#' Generate a character variable
#'
#' `gen_var_char()` generates a character variable.
#'
#' @param v name of variable.
#' @param nc name of cohort used to match number of observations.
#' @param opts vector of options to be sampled from.
#' @param p vector of probabilities for opts. Must be same length as opts.
#' @returns a data.frame with # observations equal to the cohort.
#' @examples
#' gen_var_char(v = sex, nc = 100, opts = c("F","M"), p = c(0.51, 0.49))
#' @export
gen_var_char <- function(v, nc, opts, p) {

  if (sum(p) != 1) {

    stop("Probabilities sum to > or < 1")

  } else if (length(p) != length(opts)) {

    stop("Number of options and number of probabilities do not match")

  } else {

    if (is.data.frame(nc)) {

      data.frame(sample(x = opts, size = nrow(nc), replace = TRUE, prob = p)) |>
        dplyr::rename({{ v }} := 1)

    } else if (is.vector(nc)) {

      if (length(nc) == 1) {

        data.frame(sample(x = opts, size = nc, replace = TRUE, prob = p)) |>
          dplyr::rename({{ v }} := 1)

      } else {

        stop("nc must be either a data frame or a vector of length 1")

      }

    }

  }

}
