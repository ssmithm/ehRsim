library(devtools)
devtools::load_all()
devtools::document()
devtools::install()
library(ehRsim)

#### user input ####
# number of EHR sources
n_src = 7
# number of patients per EHR source
n_p = 1000
# avg number of encounters per patient
aep = 15


#### End user input ####

# create cohort
cohort <- create_cohort(n_src, n_p)

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
# cap age at 89 years for one of the sources
birth_date <- birth_date |>
  dplyr::mutate(birth_date = dplyr::if_else(cohort$source == "B" &
                              birth_date <= (Sys.Date() - (365.25*89)),
                              (Sys.Date() - (365.25*89)),
                              birth_date))

demographics <- dplyr::bind_cols(cohort, sex, race, ethnicity, birth_date)

#### encounters ####
encounter_shell <- data.frame(pid = sample(cohort$pid, size = (nrow(cohort) * aep), replace = TRUE)) |>
  dplyr::arrange(pid)

admit_date <- rdate(v = admit_date,
                        nc = encounter_shell,
                        min = "2012-01-01")
los <- data.frame(los = rnbinom(n = nrow(encounter_shell), size = 10, prob = 0.7))
admit_date <- dplyr::bind_cols(admit_date, los) |>
  dplyr::mutate(discharge_date = admit_date + los)

admit_time <- rtime(v = admit_time,
                    nc = encounter_shell,
                    format = "24H",
                    pr.wh = 0.7)

discharge_time <- rtime(v = discharge_time,
                        nc = encounter_shell,
                        format = "24H",
                        pr.wh = 0.6)

enc_type <- gen_var_char(v = enc_type,
                         nc = encounter_shell,
                         opts = c("IP", "AV", "ED", "TH", "OT", "UN"),
                         p = c(0.2, 0.55, 0.08, 0.05, 0.07, 0.05))

discharge_disposition <- gen_var_char(v = discharge_disposition,
                                      nc = encounter_shell,
                                      opts = c("A","E","UN","OT"),
                                      p = c(0.8, 0.05, 0.07, 0.08))

encounters <- dplyr::bind_cols(dplyr::left_join(encounter_shell, cohort, by = "pid"),
                                admit_date,
                                admit_time,
                                enc_type,
                                discharge_time,
                                discharge_disposition) |>
         dplyr::mutate(discharge_date = dplyr::if_else(enc_type %in% c("AV", "TH", "ED"), admit_date, discharge_date),
                       discharge_time = update_rtime(perm_time = admit_time,
                                                     perm_date = admit_date,
                                                     fix_time = discharge_time,
                                                     fix_date = discharge_date),
                       discharge_disposition = dplyr::if_else(enc_type %in% c("AV", "TH"), "A", discharge_disposition))

# delete encounters after an Expired discharge_disposition
encounters_death <- encounters |>
  dplyr::filter(discharge_disposition == "E") |>
  dplyr::select(pid, source, discharge_date) |>
  dplyr::rename(dead_after = discharge_date) |>
  dplyr::arrange(pid, source, dead_after) |>
  dplyr::group_by(source, pid) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

encounters_ <- dplyr::left_join(encounters, encounters_death, by = c("pid", "source")) |>
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

## end encounter data ##


#### vitals ####
vitals_base <- encounters |>
  dplyr::select(pid, encounterid, source, enc_type, admit_date, discharge_date)

vitals_base_oth <- vitals_base |>
  dplyr::filter(enc_type %nin% c("AV","TH")) |>
  dplyr::group_by(source, pid, encounterid) |>
  dplyr::sample_n(., size = floor(runif(n = 1, min = 0, max = 8)), replace = TRUE)

vitals_base_avth <- vitals_base |>
  dplyr::filter(enc_type %in% c("AV","TH"))


oth_num_bp <- gen_var_num(v = num_bp, nc = vitals_base_oth, dist = "sample.integer", n = 8)

