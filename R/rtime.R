#' Generate random time(s)
#'
#' `rtime()` is an user-facing function that generates a random time, similarly
#' to other random number generators in base R.
#'
#' @param v name of variable.
#' @param nc name of data.table to match size of.
#' @param format final formatting of time; valid options are "12H" and "24H"
#' @param pr.wh probability of time being in 'working hours' (defined as 8 AM to
#' 5 PM); a value of 0.5 means equal probability for working vs. non-working
#' hours. Primarily used to preferentially select working-hour admit or
#' measurement times for outpatient/ambulatory visits.
#' @param min earliest time to sample from (in 24H format); default is "00:00"
#' @param max latest time to sample from (in 24H format); default is "23:59"
#' @returns A dataset with character column formatted as "HH:MM" (24H) or
#' "HH:MM AM" or "HH:MM PM" (12H).
#' @examples
#' rdate(10)
#' rdate(10, "2010-12-01", "2015-11-30", sort = FALSE)
#' @export
#'
rtime <- function(v = time, nc, format = "12H", pr.wh = 0.5, min = "00:00", max = "23:59") {

  wh_minutes <- data.frame(time = seq(as.POSIXct("2022-01-01 08:00"), as.POSIXct("2022-01-01 17:00"), by = "min"),
                           prob = pr.wh/(1-pr.wh))

  nonwh_minutes <- data.frame(time = c(seq(as.POSIXct("2022-01-01 00:00:00"), as.POSIXct("2022-01-01 07:59:00"), by = "min"),
                                       seq(as.POSIXct("2022-01-01 17:01:00"), as.POSIXct("2022-01-01 23:59:00"), by = "min")),
                              prob = 1)

  sampling_frame <- rbind(wh_minutes, nonwh_minutes) |>
    dplyr::filter(time %within%
                    lubridate::interval(lubridate::ymd_hm(paste("2022-01-01", min, "UTC")),
                                        lubridate::ymd_hm(paste("2022-01-01", max, "UTC")),
                                        tzone = "UTC"))
    # dplyr::filter(lubridate::hm(time) >= lubridate::hm(as.POSIXct(paste("2022-01-01", min))) &
    #                 lubridate::hm(time) <= lubridate::hm(as.POSIXct(paste("2022-01-01", max))))

  times <- sampling_frame[order(sampling_frame$time),] |>
    dplyr::slice_sample(n = nrow(nc), weight_by = prob, replace = TRUE)

  if (format == "12H") {

    times <- times |>
      dplyr::mutate(hour = ifelse(lubridate::pm(time) == TRUE, lubridate::hour(time) - 12, lubridate::hour(time)),
                    minute = lubridate::minute(time),
                    ampm = ifelse(lubridate::am(time) == TRUE, " AM", " PM"),
                    time_parsed = paste0(sprintf("%02d", hour), ":", sprintf("%02d", minute), ampm))

  } else if (format == "24H") {

    times <- times  |>
      dplyr::mutate(hour = lubridate::hour(time),
                    minute = lubridate::minute(time),
                    time_parsed = paste0(sprintf("%02s", hour), ":", sprintf("%02s", minute)))

  } else {

    stop("Format must be `12H` or `24H`")

  }

  times <- times  |>
    dplyr::select(time_parsed)  |>
    dplyr::rename({{ v }} := time_parsed)

}
