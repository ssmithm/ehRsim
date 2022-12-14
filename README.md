
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ehRsim

<!-- badges: start -->
<!-- badges: end -->

The goal of ehRsim is to provide a set of basic tools for creating
simulated electronic health record (EHR) datasets. Tools include
generators for continuous, categorical, date and time variables.

## Installation

You can install the development version of ehRsim from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ssmithm/ehRsim")
```

## Example

This is a basic example developing a few common datasets. Note that this
example makes fairly heavy use of [dplyr](https://dplyr.tidyverse.org/):

``` r
library(ehRsim)
library(dplyr)

#### user input ####
# number of EHR sources
n_src = 7
# number of patients per EHR source
n_p = 1000
# avg number of encounters per patient
aep = 15

# create cohort
cohort <- create_cohort(n_src, n_p)
str(cohort)
#> 'data.frame':    7000 obs. of  2 variables:
#>  $ pid   : chr  "1" "2" "3" "4" ...
#>  $ source: Factor w/ 7 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

A demographics file can then be created and joined to the cohort table:

``` r
#### demographics ####
sex <- gen_var_char(v = sex, nc = cohort, opts = c("F","M"), p = c(0.53, 0.47))

race <- gen_var_char(v = race,
                     nc = cohort,
                     opts = c("White",
                              "Black/African American",
                              "Asian",
                              "Other",
                              "Multiple Races",
                              NA_character_),
                     p = c(0.55, 0.27, 0.08, 0.03, 0.02, 0.05))

ethnicity <- gen_var_char(v = ethnicity,
                          nc = cohort,
                          opts = c("Hispanic", "Non-Hispanic", NA_character_),
                          p = c(0.18, 0.7, 0.12))

birth_date <- rdate(v = birth_date,
                    nc = cohort,
                    min = eval(Sys.Date() - (365.25*120))) # max age = 120 years

# bind them together for demographics table
demographics <- dplyr::bind_cols(cohort, sex, race, ethnicity, birth_date)
```

You might also want an encounter table, probably with multiple
encounters/person:

``` r
# first create an encounters shell that has multiple encounters per patient.
encounter_shell <- data.frame(pid = sample(cohort$pid, size = (nrow(cohort) * aep), replace = TRUE)) |>
  dplyr::arrange(pid)

# create an admission date
admit_date <- rdate(v = admit_date,
                    nc = encounter_shell,
                    min = "2012-01-01")

# create a length of stay
los <- data.frame(los = rnbinom(n = nrow(encounter_shell), size = 10, prob = 0.7))

# using the length of stay, create a discharge_date
admit_date <- dplyr::bind_cols(admit_date, los) |>
  dplyr::mutate(discharge_date = admit_date + los)

# admission time
admit_time <- rtime(v = admit_time,
                    nc = encounter_shell,
                    format = "24H",
                    pr.wh = 0.7)

# discharge time
discharge_time <- rtime(v = discharge_time,
                        nc = encounter_shell,
                        format = "24H",
                        pr.wh = 0.6)

# encounter type -- here I use values from the PCORnet common data model. 
enc_type <- gen_var_char(v = enc_type,
                         nc = encounter_shell,
                         opts = c("IP", "AV", "ED", "TH", "OT", "UN"),
                         p = c(0.2, 0.55, 0.08, 0.05, 0.07, 0.05))

# encounter disposition (Alive, Expired, Unknown, Other; again from the PCORnet
# common data model
discharge_disposition <- gen_var_char(v = discharge_disposition,
                                      nc = encounter_shell,
                                      opts = c("A","E","UN","OT"),
                                      p = c(0.8, 0.05, 0.07, 0.08))

# bind encounter table together and 
encounters <- dplyr::bind_cols(dplyr::left_join(encounter_shell, cohort, by = "pid"),
                               admit_date,
                               admit_time,
                               enc_type,
                               discharge_time,
                               discharge_disposition) |>
  dplyr::mutate(discharge_date = dplyr::if_else(enc_type %in% c("AV", "TH", "ED"), admit_date, discharge_date),
                discharge_disposition = dplyr::if_else(enc_type %in% c("AV", "TH"), "A", discharge_disposition))
```

You might have noticed in the above that discharge times are randomly
selected. For datasets that have any significant number of observations,
it???s likely there will be discharge times that come before admission
times, even though both admission date and discharge data are
equivalent. We can update this with `update_rtime()`:

``` r
encounters_fixed <- encounters |>  
  dplyr::mutate(needs_fixing = ifelse(as.POSIXct(paste(discharge_date, discharge_time), format = "%Y-%m-%d %H:%M") < as.POSIXct(paste(admit_date, admit_time), format = "%Y-%m-%d %H:%M"), "x", "")) 

encounters_fixed |> 
  dplyr::filter(admit_date == discharge_date) |>
  dplyr::select(pid, admit_date, admit_time, discharge_date, discharge_time, needs_fixing) |> 
  head(10)
#>    pid admit_date admit_time discharge_date discharge_time needs_fixing
#> 1    1 2012-11-13      23:04     2012-11-13          13:17            x
#> 2    1 2014-08-20      20:02     2014-08-20          19:46            x
#> 3    1 2013-03-22      10:54     2013-03-22          04:28            x
#> 4    1 2020-12-14      19:52     2020-12-14          09:38            x
#> 5    1 2021-03-30      03:52     2021-03-30          13:47             
#> 6    1 2013-03-24      12:06     2013-03-24          22:49             
#> 7    1 2018-05-02      16:06     2018-05-02          07:16            x
#> 8    1 2014-03-14      09:38     2014-03-14          05:57            x
#> 9    1 2022-04-22      18:01     2022-04-22          02:46            x
#> 10   1 2022-09-06      08:00     2022-09-06          04:34            x
  
encounters_fixed <- update_rtime(data = encounters_fixed, 
                                 perm_date = admit_date, 
                                 perm_time = admit_time, 
                                 fix_date = discharge_date, 
                                 fix_time = discharge_time)

encounters_fixed |> 
  dplyr::filter(admit_date == discharge_date) |>
  dplyr::select(pid, admit_date, admit_time, discharge_date, discharge_time, needs_fixing) |> 
  head(10) 
#>    pid admit_date admit_time discharge_date discharge_time needs_fixing
#> 1    1 2014-08-20      20:02     2014-08-20          21:16            x
#> 2    1 2013-03-22      10:54     2013-03-22          12:08            x
#> 3    1 2020-12-14      19:52     2020-12-14          21:06            x
#> 4    1 2021-03-30      03:52     2021-03-30          13:47             
#> 5    1 2013-03-24      12:06     2013-03-24          22:49             
#> 6    1 2018-05-02      16:06     2018-05-02          17:20            x
#> 7    1 2014-03-14      09:38     2014-03-14          10:52            x
#> 8    1 2022-04-22      18:01     2022-04-22          19:15            x
#> 9    1 2022-09-06      08:00     2022-09-06          09:14            x
#> 10   1 2018-11-07      14:35     2018-11-07          19:37

encounters_fixed <- encounters_fixed |> 
  dplyr::select(-needs_fixing)
```

We might also want to delete encounters after an Expired
discharge_disposition (this will hopefully soon be put into its own
function):

``` r
# delete encounters after an Expired discharge_disposition
encounters_death <- encounters_fixed |>
  dplyr::filter(discharge_disposition == "E") |>
  dplyr::select(pid, source, discharge_date) |>
  dplyr::rename(dead_after = discharge_date) |>
  dplyr::arrange(pid, source, dead_after) |>
  dplyr::group_by(source, pid) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

encounters_ <- dplyr::left_join(encounters_fixed, encounters_death, by = c("pid", "source")) |>
  dplyr::arrange(source, pid, admit_date)
encounters <- encounters_ |>
  dplyr::filter(is.na(dead_after) | (admit_date <= dead_after)) |>
  dplyr::select(-dead_after)

# clean up final dataset
encounters <- encounters |>
  dplyr::mutate(unique_combo = paste0(source, "-", pid, "-", admit_date),
                encounterid = openssl::md5(unique_combo)) |>
  dplyr::select(-los, -unique_combo) |>
  dplyr::relocate(pid, encounterid, enc_type, admit_date, discharge_date, discharge_disposition, source)
```

Additional planned functionality include sampling from commonly-employed
datasets and ontologies, for example, International Classification of
Diseases (ICD) codes, RXNORM or SNOMED codes, LOINC codes, and so on.
Stay tuned.
