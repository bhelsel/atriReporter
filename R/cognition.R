#' Retrieve Cognition Task Data from ABCD Study Datasets
#'
#' @description
#' Retrieves cognition-related data for a specified task (e.g., recall, dsmse)
#' from the ABC-DS study dataset. The function automatically constructs the appropriate
#' dataset and codebook names (e.g., `cogrecall`, `cogdsmse`) and optionally applies
#' variable labels for readability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param task Unquoted name of the cognition task to retrieve (e.g., `recall`, `dsmse`).
#'   Must match one of the known task identifiers listed in `.cognition_tasks`.
#' @param site Optional; a site identifier or vector of site codes to subset data by site. Default is `NULL`.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data by cycle. Default is `NULL`.
#' @param apply_labels Logical; if `TRUE`, applies variable labels from the codebook to the returned data. Default is `FALSE`.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`
#'
#' @details
#' This function validates the requested `task` against a known set of cognition task names
#' defined in `.cognition_tasks`. If a partial match (e.g., `"block"`) corresponds to multiple
#' tasks, the function prompts the user to be more specific. Once validated, the function
#' constructs the dataset and codebook names (e.g., `"cogrecall"`) and passes them to
#' [`get_abcds_data()`].
#'
#' @return
#' A tibble containing the selected variables from the specified cognition dataset, filtered
#' by `site` and/or `cycle` if provided.
#'
#' @examples
#' \dontrun{
#' # Retrieve reaction time and accuracy from the n-back task
#' get_cognition(rt_mean, accuracy, task = nback)
#'
#' }
#'
#' @seealso
#' [`get_abcds_data()`] for the underlying data retrieval logic.
#'
#' @export

get_cognition <- function(
  ...,
  task,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  task <- as.character(rlang::ensym(task))
  if (!task %in% .cognition_tasks) {
    tryCatch(
      {
        task <- .cognition_tasks[grepl(
          task,
          .cognition_tasks,
          ignore.case = TRUE
        )]
        if (length(task) > 1) {
          cli::cli_abort(c(
            "x" = glue::glue(
              "{collapse_and(task)} matched the {cli::col_cyan('tasks')} argument"
            ),
            "i" = glue::glue(
              "Please be more specific when passing one of the tasks listed below to the {cli::col_cyan('tasks')} argument in the {cli::col_cyan('get_cognition')} function:"
            ),
            "v" = paste0(.cognition_tasks, collapse = " | ")
          ))
        }
      },
      error = function(e) {
        cli::cli_abort(c(
          "x" = "Could not match the cognition task provided to a list of known tasks.",
          "i" = glue::glue(
            "Please pass one of the tasks listed below to the {cli::col_cyan('tasks')} argument in the {cli::col_cyan('get_cognition')} function:"
          ),
          "v" = paste0(.cognition_tasks, collapse = " | ")
        ))
      }
    )
  }

  task <- rlang::sym(sprintf("cog%s", task))

  get_abcds_data(
    dataset = !!task,
    codebook = !!task,
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}
