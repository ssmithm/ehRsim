#' Generate random date(s)
#'
#' `rdate()` is an internal function that generates a random date, similarly
#' to other random number generators in base R.
#'
#' @param v name of variable.
#' @param size number of name of data.table to match size of.
#' @param min minimum date of date range to sample from, formatted as
#' "YYYY-MM-DD"; defaults to Jan 1 of current year.
#' @param max maximum date of date range to sample from, formatted as
#' "YYYY-MM-DD"; defaults to execution date.
#' @returns A date.
#' @seealso [add_date_var()], which is the user function this function supports.
#' @examples
#' rdate(10)
#' rdate(10, "2010-12-01", "2015-11-30", sort = FALSE)
#' @export
rdate <- function(v = rdate,
                  size,
                  min = paste0(format(Sys.Date(), '%Y'), '-01-01'),
                  max = Sys.Date()) {

  if (length(size) > 1) stop("Size is requred and should have a length of 1")

  else {

    if (is.data.frame(size)) size = nrow(size)

    else if (is.vector(size)) size = size

  }

  dates <- data.frame(sample(seq(as.Date(min), as.Date(max), by = "day"),
                             size = nrow(size),
                             replace = TRUE)) |>
    dplyr::rename({{ v }} := 1)

}
