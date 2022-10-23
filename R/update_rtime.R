#' Generate random date(s)
#'
#' `update_rtime()` is an user-facing function that updates (fixes) a previously-
#' generated time which does not logically make sense. Primarily used to update
#' randomly-generated discharge times, based on admit times/dates (e.g., when
#' the randomly-generated discharge time occurs before the randomly generated
#' admit time, and admit and discharge date are equivalent).
#'
#' @param perm_time variable that represents the established time around which
#' `fix_time` will be corrected.
#' @param perm_date variable that represents the established date around which
#' `fix_time` will be corrected.
#' @param fix_time variable that represents the to-be-updated time which will be
#' corrected.
#' @param fix_date variable that represents the date of the to-be-updated time
#' which will be corrected.
#' @examples
#' encounters <- encounters |>
#'   dplyr::mutate(discharge_time = update_rtime(perm_time = admit_time,
#'                                               perm_date = admit_date,
#'                                               fix_time = discharge_time,
#'                                               fix_date = discharge_date))
#' #' @export
update_rtime <- function(perm_time, perm_date, fix_time, fix_date) {

  perm_dt <- lubridate::make_datetime(
    year = as.integer(lubridate::year(perm_date)),
    month = as.integer(lubridate::month(perm_date)),
    day = as.integer(lubridate::day(perm_date)),
    hour = as.integer(lubridate::hour(paste(perm_date, perm_time))),
    min = as.integer(lubridate::minute(paste(perm_date, perm_time))),
    sec = as.integer(lubridate::second(paste(perm_date, perm_time))),
    tz = "UTC"
  )

  fix_dt <- lubridate::make_datetime(
    year = as.integer(lubridate::year(fix_date)),
    month = as.integer(lubridate::month(fix_date)),
    day = as.integer(lubridate::day(fix_date)),
    hour = as.integer(lubridate::hour(paste(fix_date, fix_time))),
    min = as.integer(lubridate::minute(paste(fix_date, fix_time))),
    sec = as.integer(lubridate::second(paste(fix_date, fix_time))),
    tz = "UTC"
  )

  if (fix_dt <= perm_dt) {

    perm_date + sample(seq(1500:6300), size = 1)

  } else {

    fix_date

  }

}
