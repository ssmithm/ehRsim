#' Generate random date(s)
#'
#' `rdate()` is an internal function that generates a random date, similarly
#' to other random number generators in base R.
#'
#' @param v name of variable.
#' @param nc number or name of data.table to match size of.
#' @param min minimum date of date range to sample from, formatted as
#' "YYYY-MM-DD"; defaults to Jan 1 of current year.
#' @param max maximum date of date range to sample from, formatted as
#' "YYYY-MM-DD"; defaults to execution date.
#' @returns A date.
#' @examples
#' rdate(nc = 10)
#' rdate(v = admit_date, nc = 10, "2010-12-01", "2015-11-30")
#' @importFrom rlang :=
#' @importFrom dplyr rename
#' @export
rdate <- function(v = rdate,
                  nc,
                  min = paste0(format(Sys.Date(), '%Y'), '-01-01'),
                  max = Sys.Date()) {

  if (is.numeric(nc) && length(nc) == 1) {

    size = nc

  } else if (is.data.frame(nc)) {

    size = nrow(nc)

  } else {

    stop("`nc` is requred, should be numeric, and should have a length of 1.")

  }

  dates <- data.frame(sample(seq(as.Date(min), as.Date(max), by = "day"),
                             size = size,
                             replace = TRUE)) |>
    dplyr::rename({{ v }} := 1)

}
