# devtools::document()
# devtools::check()
# devtools::install()
# devtools::load_all()

#' @title Retrieve KBIT2 IQ Variables from ABC-DS Data
#' @description
#' Extracts one or more IQ variables from an ABC-DS dataset using the specified
#' codebook. The function supports optional filtering by site and cycle and allows applying
#' variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`
#'
#' @return
#' A data frame containing the selected variables and any applied filters (site and/or cycle).
#' If `apply_labels = TRUE`, variable labels are attached to the output as attributes.
#'
#' @details
#' This function provides a convenient wrapper around get_data
#' to streamline access to ABC-DS IQ variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected health variables for a specific site and cycle
#'   kbit2 <- get_iq(
#'     kbit2verbkraw,
#'     kbit2ridraw,
#'     kbit2vnonvraw,
#'     site = "Site01",
#'     cycle = "Cycle2",
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_iq
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_iq <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "abcdsiq",
    codebook = "abcdsiq",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' Check KBIT-2 Score Calculations Against Database Values
#'
#' @description
#' Validates KBIT-2 (Kaufman Brief Intelligence Test, 2nd Edition) scores stored
#' in the database against recalculated values using
#' \code{\link{calculate_kbit2_score}}. Retrieves raw scores and identifiers,
#' recalculates derived scores, and returns rows where the stored and calculated
#' values disagree.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Fetches KBIT-2 raw and derived score columns from the database via
#'     \code{get_iq()}.
#'   \item Calculates age at visit and removes records with missing age or raw
#'     scores.
#'   \item Recalculates all derived scores using
#'     \code{\link{calculate_kbit2_score}}.
#'   \item Normalises age-equivalent year fields: values of \code{"< 4"} are
#'     coerced to \code{"4"} before comparison to match database conventions.
#'   \item Pivots both the database and recalculated results to long format and
#'     joins on \code{subject_label} and \code{variable}.
#'   \item Returns only rows where the database value (\code{expected}) differs
#'     from the recalculated value (\code{actual}).
#' }
#'
#' @note
#' The function currently references an undeclared object \code{temp} in the
#' \code{dplyr::inner_join()} call. This should be replaced with
#' \code{kbit_calculations} for the function to work correctly. There is also a
#' commented-out \code{dplyr::mutate()} that coerces \code{kbit_columns} to
#' character; this may need to be reinstated depending on the column types
#' returned by \code{get_iq()}.
#'
#' @return A \link[tibble:tibble]{tibble} in long format with one row per
#'   subject–variable combination where the stored and recalculated values
#'   differ. Columns are:
#'   \describe{
#'     \item{subject_label}{Subject identifier.}
#'     \item{site_initials}{Site identifier.}
#'     \item{event_code}{Visit/event code.}
#'     \item{age_at_visit}{Age of the subject at the time of the visit.}
#'     \item{variable}{Name of the KBIT-2 score variable that mismatches.}
#'     \item{expected}{Value stored in the database.}
#'     \item{actual}{Value recalculated by \code{\link{calculate_kbit2_score}}.}
#'   }
#'   Returns an empty tibble if all stored values match the recalculations.
#'
#' @seealso
#' \code{\link{calculate_kbit2_score}} for the scoring logic,
#' \code{\link{calculate_age_at_visit}} for age derivation.
#'
#' @examples
#' \dontrun{
#' mismatches <- check_kbit_results()
#'
#' # Inspect mismatches
#' print(mismatches)
#'
#' # Count discrepancies per variable
#' mismatches |>
#'   dplyr::count(variable, sort = TRUE)
#' }
#' @export

check_kbit_results <- function() {
  identifiers <- c(
    "subject_label",
    "site_initials",
    "event_code",
    "age_at_visit"
  )

  kbit_columns <- c(
    "kbit2verbkraw",
    "kbit2ridraw",
    "kbit2vnonvraw",
    "kbit2verbraw",
    "kbit2verbstd",
    "kbit2nonvbstd",
    "kbit2nonvbaey",
    "kbit2nonvbae",
    "kbit2verbaey",
    "kbit2verbae"
  )

  kbit2 <- get_iq(!!!kbit_columns)

  kbit2 <- kbit2 |>
    atriReporter::calculate_age_at_visit() |>
    dplyr::filter_out(
      dplyr::if_any(
        c(.data$age_at_visit, .data$kbit2verbkraw:.data$kbit2vnonvraw),
        ~ is.na(.x)
      )
    ) |>
    dplyr::select(dplyr::all_of(identifiers), dplyr::all_of(kbit_columns))

  kbit_calculations <-
    atriReporter::calculate_kbit2_score(
      age_at_visit = as.numeric(kbit2$age_at_visit),
      kbit2verbkraw = as.numeric(kbit2$kbit2verbkraw),
      kbit2ridraw = as.numeric(kbit2$kbit2ridraw),
      kbit2vnonvraw = as.numeric(kbit2$kbit2vnonvraw),
      subject_label = kbit2$subject_label
    ) |>
    dplyr::select(.data$subject_label, dplyr::all_of(kbit_columns)) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ as.character(.x)
    )) |>
    dplyr::mutate(dplyr::across(
      c("kbit2verbaey", "kbit2nonvbaey"),
      ~ dplyr::if_else(.x == "< 4", "4", .x)
    )) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(kbit_columns),
      values_to = "expected",
      names_to = "variable"
    )

  kbit2 |>
    dplyr::select(dplyr::all_of(identifiers), dplyr::all_of(kbit_columns)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(kbit_columns),
      values_to = "atri",
      names_to = "variable"
    ) |>
    dplyr::inner_join(
      x = _,
      kbit_calculations,
      by = c("subject_label", "variable")
    ) |>
    dplyr::filter(.data$expected != .data$atri)
}


#' @title calculate_kbit2_score
#' @description Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @param age_at_visit Age of the participant at visit
#' @param kbit2verbkraw Verbal Knowledge Raw Score
#' @param kbit2ridraw Riddles Raw Score
#' @param kbit2vnonvraw Nonverbal Raw Score
#' @param subject_label A subject ID number is required if calculating multiple KBIT-2 scores, Default: NULL
#' @param add_premorbid_id `r lifecycle::badge("experimental")` Add a premorbid intellectual disability level using only the KBIT-2 score.
#'     This argument is currently set as FALSE for the default until the premorbid intellectual disability
#'     calculation is better understood, Default: FALSE
#' @param print_kbit Print the KBIT-2 results to the console if calculating a single KBIT-2 score, Default: TRUE
#' @param doParallel Use parallel processing to speed up the calculation of multiple KBIT-2 scores, Default: TRUE
#' @return A list containing the verbal and nonverbal standard scores, intelligent quotients,
#' @details Calculates intelligence quotient and age equivalents from the
#'     KBIT-2 score from look-up tables using the age of the participant and
#'     their verbal and nonverbal scores.
#' @seealso
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{select}}
#' @rdname calculate_kbit2_score
#' @export
#' @importFrom dplyr filter select bind_rows
#' @importFrom parallel detectCores makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach `%dopar%` foreach
#' @importFrom plyr rbind.fill

calculate_kbit2_score <- function(
  age_at_visit,
  kbit2verbkraw,
  kbit2ridraw,
  kbit2vnonvraw,
  subject_label = NULL,
  add_premorbid_id = FALSE,
  print_kbit = TRUE,
  doParallel = TRUE
) {
  files <- list.files(
    path = system.file("extdata", package = "atriReporter"),
    pattern = "kbit2",
    full.names = TRUE
  )

  for (f in files) {
    load(f)
  }

  if (length(kbit2verbkraw) > 1 & is.null(subject_label)) {
    stop(
      "A subject_label vector is required when calculating multiple KBIT-2 scores."
    )
  }

  main_kbit2_calculator <- function(
    age_at_visit,
    kbit2verbkraw,
    kbit2ridraw,
    kbit2vnonvraw,
    subject_label
  ) {
    #verbal <- nonverbal <- iq <- ae <- NULL
    # Calculate sum of standard scores

    kbit2verbraw <- kbit2verbkraw + kbit2ridraw

    verbstd <-
      verbal %>%
      dplyr::filter(
        .data$min_age <= age_at_visit &
          .data$max_age >= age_at_visit &
          .data$raw_score == kbit2verbraw
      ) %>%
      dplyr::select(-c(.data$min_age, .data$max_age))

    kbit2verbstd <- verbstd$standard_score
    kbit2verbstdci <- verbstd$confidence_interval
    kbit2verbstdpr <- verbstd$percentile_rank

    nonvbstd <-
      nonverbal %>%
      dplyr::filter(
        .data$min_age <= age_at_visit &
          .data$max_age >= age_at_visit &
          .data$raw_score == kbit2vnonvraw
      ) %>%
      dplyr::select(-c(.data$min_age, .data$max_age))

    kbit2nonvbstd <- nonvbstd$standard_score
    kbit2nonvbstdci <- nonvbstd$confidence_interval
    kbit2nonvbstdpr <- nonvbstd$percentile_rank

    sumstd <- kbit2verbstd + kbit2nonvbstd

    # Calculate IQ

    iqcomp <-
      expand_lookup(
        data = subset(iq, min_age <= age_at_visit & max_age >= age_at_visit),
        variable = "sum_of_standard_scores"
      ) %>%
      dplyr::filter(.data$sum_of_standard_scores == sumstd) %>%
      dplyr::select(-c(.data$min_age, .data$max_age))

    kbit2iqcompstd <- iqcomp$standard_score
    kbit2iqcompci <- iqcomp$confidence_interval
    kbit2iqcomppr <- iqcomp$percentile_rank

    # Calculate age equivalent
    aenonvb <-
      ae %>%
      expand_lookup(variable = "nonverbal_raw_score") %>%
      dplyr::filter(.data$nonverbal_raw_score == kbit2vnonvraw)

    kbit2nonvbaey <- strsplit(aenonvb$age_equivalent, ":")[[1]][1]
    kbit2nonvbae <- strsplit(aenonvb$age_equivalent, ":")[[1]][2]

    aeverb <-
      ae %>%
      expand_lookup(variable = "verbal_raw_score") %>%
      dplyr::filter(.data$verbal_raw_score == kbit2verbraw)

    kbit2verbaey <- strsplit(aeverb$age_equivalent, ":")[[1]][1]
    kbit2verbae <- strsplit(aeverb$age_equivalent, ":")[[1]][2]

    data <-
      data.frame(
        age_at_visit,
        kbit2verbkraw,
        kbit2ridraw,
        kbit2vnonvraw,
        kbit2verbraw,
        kbit2verbstd,
        kbit2verbstdci,
        kbit2verbstdpr,
        kbit2nonvbstd,
        kbit2nonvbstdci,
        kbit2nonvbstdpr,
        kbit2iqcompstd,
        kbit2iqcompci,
        kbit2iqcomppr,
        kbit2nonvbaey,
        kbit2nonvbae,
        kbit2verbaey,
        kbit2verbae
      )

    if (!is.null(subject_label)) {
      data <- cbind(subject_label, data)
    }

    # if (add_premorbid_id) {
    #   if (data$kbit2iqcompstd > 40) {
    #     data$prefunclevel <- ifelse(data$kbit2iqcompstd >= 50, 1, 2)
    #   } else if (data$kbit2iqcompstd == 40) {
    #     data$prefunclevel <-
    #       ifelse(
    #         aenonvb$age_equivalent[1] %in%
    #           c("< 4:0", "4:0") &
    #           aeverb$age_equivalent[1] %in% c("< 4:0", "4:0"),
    #         3,
    #         2
    #       )
    #   }
    # }
    return(data)
  }

  if (doParallel & length(kbit2verbkraw) > 1) {
    cores = parallel::detectCores()
    Ncores = cores - 1
    cl = parallel::makeCluster(Ncores)
    doParallel::registerDoParallel(cl)
    `%dopar%` = foreach::`%dopar%`
    kbitres <-
      foreach::foreach(i = 1:length(kbit2verbkraw), .packages = "abcds") %dopar%
      {
        main_kbit2_calculator(
          age_at_visit[i],
          kbit2verbkraw[i],
          kbit2ridraw[i],
          kbit2vnonvraw[i],
          subject_label[i]
        )
      } %>%
      dplyr::bind_rows() %>%
      dplyr::arrange(.data$subject_label)
  } else if (!doParallel & length(kbit2verbkraw) > 1) {
    kbitres <- data.frame()
    for (i in 1:length(kbit2verbkraw)) {
      cat("\r Processing data for ", subject_label[i])
      data <- main_kbit2_calculator(
        age_at_visit[i],
        kbit2verbkraw[i],
        kbit2ridraw[i],
        kbit2vnonvraw[i],
        subject_label[i]
      )
      kbitres <- plyr::rbind.fill(kbitres, data)
    }
    kbitres <- dplyr::arrange(kbitres, subject_label)
  } else if (length(kbit2verbkraw) == 1) {
    kbitres <- main_kbit2_calculator(
      age_at_visit,
      kbit2verbkraw,
      kbit2ridraw,
      kbit2vnonvraw,
      subject_label
    )
    kbitres$subject_label <- NULL
  } else {
    stop("An error occurred. Please check your data and try again.")
  }

  class(kbitres) <- c("tbl_df", "tbl", "data.frame")

  return(kbitres)
}

#' @title expand_lookup
#' @description Expands the look-up table in cases where there are ranges in the scores
#' @param data The data set containing the variable with the ranges
#' @param variable The variable containing the range as indicated by a dash between two numeric values
#' @return The expanded look-up table with the variable in descending order
#' @details Expands the look-up table in cases where there are ranges in the scores
#' @seealso
#'  \code{\link[rlang]{:=}}
#'  \code{\link[tibble]{tibble}}
#' @rdname expand_lookup
#' @importFrom rlang `:=`
#' @importFrom tibble tibble
#' @keywords internal

expand_lookup <- function(data, variable) {
  `:=` <- rlang::`:=`

  lookupTable <-
    do.call(
      rbind,
      lapply(seq_along(data[[variable]]), FUN = function(i) {
        if (grepl("-", data[[variable]][i])) {
          bounds <- as.numeric(strsplit(data[[variable]][i], "-")[[1]])
          tibble::tibble(
            !!variable := seq(bounds[1], bounds[2]),
            data[i, -which(colnames(data) == variable)]
          )
        } else {
          tibble::tibble(
            !!variable := as.numeric(data[[variable]][i]),
            data[i, -which(colnames(data) == variable)]
          )
        }
      })
    )

  lookupTable[order(lookupTable[[variable]], decreasing = TRUE), ]
}
