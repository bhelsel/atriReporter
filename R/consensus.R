#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @seealso
#'  \code{\link[rlang]{defusing-advanced}}
#'  \code{\link[tidyr]{pivot_wider}}
#'  \code{\link[dplyr]{reexports}}
#' @rdname get_consensus
#' @export
#' @importFrom rlang ensyms
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of

get_consensus <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    dataset = "consensus",
    codebook = "consensus",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}
