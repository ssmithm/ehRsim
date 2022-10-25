#' Generate random time(s)
#'
#' `rtime()` is an user-facing function that generates a random time, similarly
#' to other random number generators in base R.
#'
#' @param v name of variable.
#' @param nc name of data.table to match size of, or vector of length = 1.
#' @param format final formatting of time; valid options are "12H" and "24H"
#' @param min earliest time to sample from (in 24H format); default is "00:00"
#' @param max latest time to sample from (in 24H format); default is "23:59"
#' @param pr.wh probability of time being in 'working hours' (defined as 8 AM to
#' 5 PM); a value of 0.5 means equal probability for working vs. non-working
#' hours. Primarily used to preferentially select working-hour admit or
#' measurement times for outpatient/ambulatory visits.
#' @param wh.min earliest time to sample from (in 24H format) for working hours;
#' default is "08:00". Must be earlier than `min`.
#' @param wh.max latest time to sample from (in 24H format) for working hours;
#' default is "17:00". Must be later than `max`.
#' @returns A dataset with character column formatted as "HH:MM" (24H) or
#' "HH:MM AM" or "HH:MM PM" (12H).
#' @examples
#' rtime(v = admit_time, nc = 150, format = "24H", pr.wh = 1, wh.min = "08:00", wh.max = "17:00")
#' rtime(vital_time, mtcars, "12H", "00:00", "23:59", 0.7, "07:30", "17:30")
#' @importFrom rlang := .data
#' @importFrom dplyr filter slice_sample mutate select rename arrange
#' @importFrom lubridate ymd_hms hour minute am pm %within%
#' @export
rtime <- function(v, nc, format = "12H", min = "00:00", max = "23:59",
                  pr.wh = 0.5, wh.min = "08:00", wh.max = "17:00") {

  if (is.data.frame(nc)) {

    sample_size = nrow(nc)

  } else if (is.numeric(nc) && length(nc) == 1) {

    sample_size = nc

  } else {

    stop("`nc` needs to either be a data.frame or numeric vector of length 1.")

  }

  if ((lubridate::hm(min) > lubridate::hm(wh.min)) || (lubridate::hm(wh.max) > lubridate::hm(max))) {

    stop("Error in input times. `wh.min` must be after `min` and `wh.max` must be before `max`")

  }

  if (!is.numeric(pr.wh)) {

    stop("pr.wh must be numeric")

  } else if (pr.wh > 1 || pr.wh < 0) {

    stop("pr.wh must be in the range [0, 1]")

  } else if (pr.wh == 1) {

    sampling_frame <- data.frame(time_int = seq(as.POSIXct(paste("2022-01-01", wh.min)),
                                                as.POSIXct(paste("2022-01-01", wh.max)),
                                                by = "min"),
                                 prob = 1)

  } else {

    wh_minutes <- data.frame(time_int = seq(as.POSIXct(paste("2022-01-01", wh.min)),
                                            as.POSIXct(paste("2022-01-01", wh.max)),
                                            by = "min"),
                             prob = pr.wh/(1-pr.wh))

    nonwh_minutes <- data.frame(time_int = c(seq(as.POSIXct(paste("2022-01-01", min)),
                                                 as.POSIXct(paste("2022-01-01", wh.min)) - 60,
                                                 by = "min"),
                                             seq(as.POSIXct(paste("2022-01-01", wh.max)) + 60,
                                                 as.POSIXct(paste("2022-01-01", max)),
                                                 by = "min")),
                                prob = 1)

    sampling_frame <- rbind(wh_minutes, nonwh_minutes)

  }

#  times <- sampling_frame[order(sampling_frame$time),] |>
  times <- sampling_frame |>
    dplyr::arrange(.data$time_int) |>
    dplyr::slice_sample(n = sample_size, weight_by = .data$prob, replace = TRUE)

  if (format == "12H") {

    times <- times |>
      dplyr::mutate(hour = ifelse(lubridate::pm(.data$time_int) == TRUE, lubridate::hour(.data$time_int) - 12, lubridate::hour(.data$time_int)),
                    minute = lubridate::minute(.data$time_int),
                    ampm = ifelse(lubridate::am(.data$time_int) == TRUE, " AM", " PM"),
                    time_parsed = paste0(sprintf("%02d", .data$hour), ":", sprintf("%02d", .data$minute), .data$ampm))

  } else if (format == "24H") {

    times <- times  |>
      dplyr::mutate(hour = lubridate::hour(.data$time_int),
                    minute = lubridate::minute(.data$time_int),
                    time_parsed = paste0(sprintf("%02s", .data$hour), ":", sprintf("%02s", .data$minute)))

  } else {

    stop("Format must be `12H` or `24H`")

  }

  times <- times  |>
    dplyr::select(.data$time_parsed)  |>
    dplyr::rename({{ v }} := .data$time_parsed)

}
