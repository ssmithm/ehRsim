#' Generate the base cohort
#'
#' `create_cohort()` generates a tibble with the specified number of patients
#' from a specified number of sources (e.g., for multi-site databases).
#'
#' @param source_n number of EHR sources (sites).
#' @param n number of patients per EHR source.
#' @returns A tibble with 2 columns, pid (patient id) +/- source.
#' @examples
#' create_cohort(3, 150)
#' @export
create_cohort <- function(source_n, n) {

  if (source_n == 1) {

      data.frame(pid = as.character(seq(1:n)))

  } else {

    if (source_n == length(n)) {

      for (i in 1:source_n) {

        source <- vector("list", length = as.integer(source_n))
        source[[i]] <- rep(LETTERS[i], each = n[[i]]) |> cbind()

      }

      cbind(data.frame(pid = as.character(seq(1:(sum(n))))),
            data.frame(source = as.factor(do.call("rbind", source))))

    } else if (source_n > length(n)) {

      data.frame(pid = as.character(seq(1:(source_n * n))),
                 source = as.factor(rep(LETTERS[1:source_n], each = n)))

    } else {

      stop("Something's gone wrong.")

    }

  }

}
