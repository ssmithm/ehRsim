#' Generate random date(s)
#'
#' `update_rtime()` is an user-facing function that updates (fixes) a previously-
#' generated time which does not logically make sense. Primarily used to update
#' randomly-generated discharge times, based on admit times/dates (e.g., when
#' the randomly-generated discharge time occurs before the randomly generated
#' admit time, and admit and discharge date are equivalent).
#'
#' @param data data.frame which needs updating.
#' @param perm_time existing time around which `fix_time` will be corrected.
#' @param perm_date the existing date around which `fix_time` will be corrected.
#' @param fix_time the to-be-corrected time.
#' @param fix_date the date of the to-be-corrected time.
#' @examples
#' df <- data.frame(admit_date = c("2022-02-01", "2022-02-01"),
#'                  discharge_date = c("2022-02-01", "2022-02-03"),
#'                  admit_time = c("08:00", "10:00"),
#'                  discharge_time = c("06:00", "06:00"))
#'
#' df <- update_rtime(data = df, perm_time = admit_time, perm_date = admit_date,
#'                    fix_time = discharge_time, fix_date = discharge_date)
#' @importFrom lubridate make_datetime year month day hour minute second
#' @importFrom dplyr mutate select rename if_else all_of
#' @importFrom rlang enquo := !! .data as_name
#' @export
update_rtime <- function(data, perm_time, perm_date, fix_time, fix_date) {

  # perm_time <- enquo(perm_time)
  # perm_date <- enquo(perm_date)
  fix_time <- enquo(fix_time)
  fix_date <- enquo(fix_date)

  remove_vars <- c("perm_dt", "fix_dt", "updated_dt", rlang::as_name(fix_time), rlang::as_name(fix_date))

  updated_df <- data |>
    dplyr::mutate(# create a datetime representing permanent date/time
                  perm_dt = as.POSIXct(paste({{ perm_date }}, {{ perm_time }}), format = "%Y-%m-%d %H:%M"),
                  fix_dt = as.POSIXct(paste({{ fix_date }}, {{ fix_time }}), format = "%Y-%m-%d %H:%M"),

                  # if to-be-fixed datetime come on or before permanent datetime, update to-be-fixed datetime
                  # else, keep 'to-be-fixed' datetime
                  updated_dt = if_else(.data$fix_dt < .data$perm_dt, .data$perm_dt + sample(seq(1500, 6300, 60), size = 1), .data$fix_dt),
                  # convert date part to character
                  updated_date = strftime(.data$updated_dt, format = "%Y-%m-%d"),
                  # convert time part to character
                  updated_time = strftime(.data$updated_dt, format = "%H:%M")) |>
    dplyr::select(!all_of(remove_vars)) |>
    dplyr::rename({{ fix_date }} := .data$updated_date, {{ fix_time }} := .data$updated_time)

}
