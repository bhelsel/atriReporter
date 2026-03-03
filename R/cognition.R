#' Retrieve Cognition Task Data from ABCD Study Datasets
#'
#' @description
#' Retrieves cognition-related data for a specified task (e.g., recall, dsmse)
#' from the ABC-DS study dataset. The function automatically constructs the appropriate
#' dataset and codebook names (e.g., `cogrecall`, `cogdsmse`) and optionally applies
#' variable labels for readability.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
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
#' [`get_data()`].
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
#' [`get_data()`] for the underlying data retrieval logic.
#'
#' @export

get_cognition <- function(
  ...,
  study = c("abcds", "trcds"),
  task,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  study <- match.arg(study)
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

  if (study == "abcds") {
    task <- rlang::sym(sprintf("cog%s", task))
  } else if (study == "trcds") {
    task <- rlang::sym(task)
  }

  get_data(
    study = study,
    dataset = !!task,
    codebook = !!task,
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' Retrieve Daily Living Data from the ABC-DS Study
#'
#' @description
#' Retrieves variables from the Daily Living Data (`dld`) dataset in the ABC-DS
#' study. A convenience wrapper around \code{\link{get_data}} with the dataset
#' and codebook pre-set to \code{"dld"}.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset
#'   data by site. Default is \code{NULL}.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data
#'   by cycle. Default is \code{NULL}.
#' @param apply_labels Logical; if \code{TRUE}, applies variable labels from the
#'   codebook to the returned data. Default is \code{FALSE}.
#' @param controls Logical; if \code{TRUE}, includes control participants in the
#'   returned data. Default is \code{FALSE}.
#'
#' @return A tibble containing the selected variables from the \code{dld}
#'   dataset, filtered by \code{site} and/or \code{cycle} if provided.
#'
#' @examples
#' \dontrun{
#' get_dld(dld_var1, dld_var2)
#'
#' # Filter by site and cycle
#' get_dld(dld_var1, site = "ABC", cycle = 2)
#' }
#'
#' @seealso \code{\link{get_data}} for the underlying data retrieval logic,
#'   \code{\link{get_cognition}} for cognition task data.
#'
#' @export

get_dld <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "dld",
    codebook = "dld",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}


#' Retrieve Reiss Scale Data from the ABC-DS Study
#'
#' @description
#' Retrieves variables from the Reiss Scale (\code{reiss}) dataset in the
#' ABC-DS study. The Reiss Scale assesses motivation and emotional/behavioural
#' problems in individuals with intellectual disabilities. A convenience wrapper
#' around \code{\link{get_data}} with the dataset and codebook pre-set to
#' \code{"reiss"}.
#'
#' @param ... One or more unquoted variable names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset
#'   data by site. Default is \code{NULL}.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data
#'   by cycle. Default is \code{NULL}.
#' @param apply_labels Logical; if \code{TRUE}, applies variable labels from the
#'   codebook to the returned data. Default is \code{FALSE}.
#' @param controls Logical; if \code{TRUE}, includes control participants in the
#'   returned data. Default is \code{FALSE}.
#'
#' @return A tibble containing the selected variables from the \code{reiss}
#'   dataset, filtered by \code{site} and/or \code{cycle} if provided.
#'
#' @examples
#' \dontrun{
#' get_reiss(reiss_var1, reiss_var2)
#'
#' # Filter by site and cycle
#' get_reiss(reiss_var1, site = "ABC", cycle = 2)
#' }
#'
#' @seealso \code{\link{get_data}} for the underlying data retrieval logic,
#'   \code{\link{get_cognition}} for cognition task data.
#'
#' @export

get_reiss <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "reiss",
    codebook = "reiss",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}


#' Retrieve Cancellation Task Data from the ABC-DS Study
#'
#' @description
#' Retrieves variables from the Cancellation Task (\code{cancellation}) dataset
#' in the ABC-DS study. The Cancellation Task is a measure of visual attention
#' and processing speed in which participants scan an array and mark target
#' stimuli. A convenience wrapper around \code{\link{get_data}} with the dataset
#' and codebook pre-set to \code{"cancellation"}.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more unquoted variable
#'   names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset
#'   data by site. Default is \code{NULL}.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data
#'   by cycle. Default is \code{NULL}.
#' @param apply_labels Logical; if \code{TRUE}, applies variable labels from the
#'   codebook to the returned data. Default is \code{FALSE}.
#' @param controls Logical; if \code{TRUE}, includes control participants in the
#'   returned data. Default is \code{FALSE}.
#'
#' @return A tibble containing the selected variables from the
#'   \code{cancellation} dataset, filtered by \code{site} and/or \code{cycle}
#'   if provided.
#'
#' @examples
#' \dontrun{
#' # Retrieve all cancellation task variables
#' get_cancellation(cancel_var1, cancel_var2)
#'
#' # Filter by site and cycle
#' get_cancellation(cancel_var1, site = "ABC", cycle = 2)
#' }
#'
#' @seealso \code{\link{get_data}} for the underlying data retrieval logic,
#'   \code{\link{get_cognition}} for other cognition task data.
#'
#' @export

get_cancellation <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "cancellation",
    codebook = "cancellation",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' Retrieve Neuropsychiatric Inventory (NPI) Data from the ABC-DS Study
#'
#' @description
#' Retrieves variables from the Neuropsychiatric Inventory (\code{npi}) dataset
#' in the ABC-DS study. The NPI is a validated clinical instrument that assesses
#' the frequency and severity of neuropsychiatric symptoms — such as delusions,
#' agitation, depression, and apathy — in individuals with dementia or cognitive
#' impairment, as reported by a caregiver or informant. A convenience wrapper
#' around \code{\link{get_data}} with the dataset and codebook pre-set to
#' \code{"npi"}.
#'
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> One or more unquoted variable
#'   names to retrieve from the dataset.
#' @param site Optional; a site identifier or vector of site codes to subset
#'   data by site. Default is \code{NULL}.
#' @param cycle Optional; a cycle identifier or vector of cycles to subset data
#'   by cycle. Default is \code{NULL}.
#' @param apply_labels Logical; if \code{TRUE}, applies variable labels from the
#'   codebook to the returned data. Default is \code{FALSE}.
#' @param controls Logical; if \code{TRUE}, includes control participants in the
#'   returned data. Default is \code{FALSE}.
#'
#' @return A tibble containing the selected variables from the \code{npi}
#'   dataset, filtered by \code{site} and/or \code{cycle} if provided.
#'
#' @examples
#' \dontrun{
#' # Retrieve NPI frequency and severity variables
#' get_npi(npi_var1, npi_var2)
#'
#' # Filter by site and cycle
#' get_npi(npi_var1, site = "ABC", cycle = 2)
#' }
#'
#' @seealso \code{\link{get_data}} for the underlying data retrieval logic,
#'   \code{\link{get_cognition}} for cognition task data,
#'   \code{\link{get_reiss}} for the Reiss Scale, an alternative measure of
#'   behavioural and emotional symptoms in individuals with intellectual
#'   disabilities.
#'
#' @export

get_npi <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "npi",
    codebook = "npi",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}
