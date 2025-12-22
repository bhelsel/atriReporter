#' @title Retrieve Physical and Neurological Exam Variables from ABC-DS Data
#' @description
#' Extracts one or more variables from the ABC-DS physical and neurological exam dataset using
#' the specified codebook. The function supports optional filtering by site and cycle and allows
#' applying variable labels for enhanced interpretability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
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
#' to streamline access to ABC-DS health variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected health variables for a specific site and cycle
#'   weight_height <- get_exam(
#'     wt, ht,
#'     dataset = phyneuro,
#'     codebook = phyneuro,
#'     site = "Site01",
#'     cycle = "Cycle2",
#'     apply_labels = TRUE
#'   )
#' }
#' }
#'
#'
#' @seealso
#'  \code{\link[rlang]{as_string}}, \code{\link[rlang]{defusing-advanced}},
#'
#' @rdname get_exam
#' @export
#' @importFrom rlang as_string enexpr ensyms

get_exam <- function(
  ...,
  study = c("abcds", "trcds"),
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  study <- match.arg(study)
  variables <- as.character(rlang::ensyms(...))
  if (study == "abcds") {
    dataset <- codebook <- "phyneuro"
  } else if (study == "trcds") {
    dataset <- codebook <- "vitals"
  }

  get_data(
    study = study,
    dataset = !!dataset,
    codebook = !!codebook,
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}
