#' @title Retrieve the Disclosure Variables from ATRI
#'
#' @description
#' Retrieves variables needed for the BRIDGE21 Down syndrome
#' disclosure report from the ATRI Electronic Data Capture system
#'
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

get_disclosure <- function(site = NULL, cycle = NULL, apply_labels = FALSE) {
  key <- .disclosure_key$abcds

  demographics <- get_demographics(
    !!!key$demographics,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  ids <- get_ids(demographics)

  dsmse <- get_cognition(
    !!!key$dsmse,
    task = dsmse,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  recall <- get_cognition(
    !!!key$recall,
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
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  exam <- get_exam(
    !!!key$exam,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels
  )

  df_names <- c("demographics", "dsmse", "recall", "ntgedsd", "exam")

  data <- purrr::reduce(
    mget(df_names),
    .f = atri_join,
    join_type = inner_join,
    by = ids
  )

  return(data)
}

# data <- get_disclosure(site = "KUMC")
# readr::write_csv(data, file = "/Users/bhelsel/Desktop/Projects/ABC-DS Disclosure/abcds_data.csv")
# devtools::load_all()
# print(dplyr::select(data, frssa:crss), n = 30)
