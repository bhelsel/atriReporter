#' @title Retrieve the Disclosure Variables from ATRI
#'
#' @description
#' Retrieves variables needed for the BRIDGE21 Down syndrome
#' disclosure report from the ATRI Electronic Data Capture system
#'
#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
#' @param site An optional site code or name used to filter the demographic data;
#' defaults to \code{NULL}, returning all sites.
#' @param cycle An optional study cycle identifier used to filter the data;
#' defaults to \code{NULL}, returning all cycles.
#' @param apply_labels Logical. Whether to apply variable and factor labels from
#' the ABC-DS data dictionary. Defaults to \code{FALSE}.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing selected disclosure variables,
#' optionally filtered by site or study cycle. If \code{apply_labels = TRUE},
#' the variables will include descriptive labels and factor levels.

#' @details
#' The \code{get_disclosure()} function simplifies extraction of participant-level
#' data being disclosed in the BRIDGE21 report. Users can limit results by site or
#' cycle, and automatically attach human-readable labels using
#' \code{\link{apply_labels}} and \code{\link{apply_factor_labels}}.

#' @seealso
#'  \code{\link[purrr]{reduce}}
#' @rdname get_disclosure
#' @export
#' @importFrom purrr reduce

get_disclosure <- function(
  study = c("abcds", "trcds"),
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE
) {
  study <- match.arg(study)
  key <- .disclosure_key[[study]]

  if (study == "abcds") {
    demographics <- get_demographics(
      !!!key$demographics,
      site = site,
      cycle = cycle,
      apply_labels = apply_labels
    )
  } else if (study == "trcds") {
    demographics <- get_registry(
      !!!key$registry,
      study = study,
      site = site,
      cycle = cycle,
      apply_labels = apply_labels
    )
  }

  ids <- get_ids(demographics)

  dsmse <- get_cognition(
    !!!key$dsmse,
    study = study,
    task = dsmse,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  recall <- get_cognition(
    !!!key$recall,
    study = study,
    task = recall,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  recall$frss <- ifelse(is.na(recall$frssa), recall$frssb, recall$frssa)
  recall$frssa <- recall$frssb <- NULL

  recall$crss <- ifelse(is.na(recall$crssa), recall$crssb, recall$crssa)
  recall$crssa <- recall$crssb <- NULL

  ntgedsd <- get_ntgedsd(
    !!!key$ntgedsd,
    study = study,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  exam <- get_exam(
    !!!key$exam,
    study = study,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  # ABC-DS and TRC-DS Codebook: Weight: 1 = Pounds, 2 = Kilograms; Height: 1 = Inches, 2 = Centimeters
  convert_to_metric <- function(variable, unit, type = c("height", "weight")) {
    type <- match.arg(type)
    conversion_factor <- if (type == "height") 2.54 else 1 / 2.205
    ifelse(unit == 1 & !is.na(unit), variable * conversion_factor, variable)
  }

  df_names <- c("demographics", "dsmse", "recall", "ntgedsd", "exam")

  # fmt: skip
  if (study == "abcds") {
    exam$ht <- convert_to_metric(exam$ht, exam$htu, type = "height")
    exam$wt <- convert_to_metric(exam$wt, exam$wtu, type = "weight")
    exam$htu <- exam$wtu <- NULL
  } else if (study == "trcds") {
    exam$vsheight <- convert_to_metric(exam$vsheight, exam$vshtunit, type = "height")
    exam$vsweight <- convert_to_metric(exam$vsweight, exam$vswtunit, type = "weight")
    exam$vshtunit <- exam$vswtunit <- NULL
    centiloid <- get_imaging(study = "trcds", imaging = "amymeta")
    df_names <- c(df_names, "centiloid")
  }

  data <- purrr::reduce(
    mget(df_names),
    .f = atri_join,
    join_type = full_join,
    by = ids
  )

  all_vars <- key %>%
    do.call("c", .) %>%
    {
      .[. %in% names(data)]
    } %>%
    as.character()

  data %>%
    dplyr::filter(!dplyr::if_all(all_of(all_vars), ~ is.na(.x))) %>%
    dplyr::arrange(subject_label, event_code)
}

# devtools::load_all()
# start <- Sys.time()
# get_imaging(study = "trcds", imaging = "amymeta")
# end <- Sys.time()
# difftime(end, start)
# data <- get_disclosure(study = "trcds")
