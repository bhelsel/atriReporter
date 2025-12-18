#' @title Retrieve and Format ABC-DS Demographic Data
#'
#' @description
#' Retrieves participant demographic data from the ABC-DS study within the ATRI
#' Electronic Data Capture (EDC) system and returns a clean, labeled dataset
#' optionally filtered by site or study cycle.
#'
#' @param ... One or more unquoted variable names to select from the demographics
#' dataset.
#' @param site An optional site code or name used to filter the demographic data;
#' defaults to \code{NULL}, returning all sites.
#' @param cycle An optional study cycle identifier used to filter the data;
#' defaults to \code{NULL}, returning all cycles.
#' @param apply_labels Logical. Whether to apply variable and factor labels from
#' the ABC-DS data dictionary. Defaults to \code{FALSE}.
#' @param controls A boolean value that indicates whether the function should return the controls, Default: FALSE
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing selected demographic variables,
#' optionally filtered by site or study cycle. If \code{apply_labels = TRUE},
#' the variables will include descriptive labels and factor levels.
#'
#' @details
#' The \code{get_demographics()} function simplifies extraction of participant-level
#' demographic data for ABC-DS analyses. Users can specify particular variables of
#' interest, optionally limit results by site or cycle, and automatically attach
#' human-readable labels using \code{\link{apply_labels}} and
#' \code{\link{apply_factor_labels}}.
#'
#' If \code{age_at_visit} is among the requested variables, the function will
#' automatically compute age at visit based on the participant’s year of birth
#' and evaluation date (\code{de_eval_date}). This ensures accurate age
#' calculation even when the variable is not explicitly stored in the dataset.
#'
#' Internally, the function uses \pkg{purrr} for functional mapping and reshapes
#' the data with \pkg{tidyr} as needed for analysis-ready output.
#'
#' @seealso
#'  \code{\link[purrr]{map}},
#'  \code{\link[tibble]{as_tibble}},
#'  \code{\link[tidyr]{pivot_wider}},
#'  \code{\link{apply_labels}},
#'  \code{\link{apply_factor_labels}}
#'
#' @examples
#' \dontrun{
#' # Retrieve basic demographics for all sites
#' demo <- get_demographics(age_at_visit, de_race, de_gender, de_ethnicity)
#'
#' # Retrieve labeled demographics for a specific site and cycle
#' demo_site <- get_demographics(
#'   age_at_visit, de_race, de_gender, de_ethnicity,
#'   site = "KUMC", cycle = 2, apply_labels = TRUE
#' )
#' }
#'
#' @rdname get_demographics
#' @export
#' @importFrom purrr map_chr
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider

get_demographics <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_abcds_data(
    dataset = "ptdemog",
    codebook = "ptdemog",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}

#' @title Retrieve Year of Birth from ABC-DS Data
#'
#' @description
#' Extracts and formats participants’ year of birth from the ABC-DS dataset,
#' optionally filtering by site. The resulting dataset can be merged with
#' other study data or used for age calculations.
#'
#' @param data A dataset containing participant identifiers and associated
#' demographic information from the ABC-DS study.
#' @param site Optional. A site code or name used to filter results to a specific
#' study site; defaults to \code{NULL}, returning data from all sites.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with participant identifiers and corresponding
#' year of birth values.
#'
#' @details
#' The \code{retrieve_birth_year()} function retrieves the variable representing
#' year of birth from the ABC-DS demographics data and converts it into a clean,
#' analysis-ready format.
#'
#' This function is typically used internally by \code{\link{get_demographics}}
#' to calculate participant age at visit using the evaluation date
#' (\code{de_eval_date}). However, it can also be called independently when
#' users need access to year-of-birth data for custom analyses or data merging.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve year of birth for all participants
#'   birth_years <- retrieve_birth_year(demo_data)
#'
#'   # Retrieve year of birth for a specific site
#'   birth_years_ku <- retrieve_birth_year(demo_data, site = "KUMC")
#' }
#' }
#'
#' @seealso
#'  \code{\link[tibble]{as_tibble}},
#'  \code{\link{get_demographics}}
#'
#' @rdname retrieve_birth_year
#' @export
#' @importFrom tibble as_tibble

retrieve_birth_year <- function(
  data,
  site = NULL
) {
  data <- data[data$dd_field_name == "ptdob", ]

  if (!is.null(site)) {
    data <- filter_by_site(data, site)
  }

  ids <- get_ids(data)

  colnames(data)[grepl("field_value", colnames(data))] <- "ptdob"

  data <- tibble::as_tibble(data[, c(ids, "ptdob")])

  return(data)
}

#' @title Calculate Age at Visit
#'
#' @description
#' Calculates participant age at the time of evaluation based on year of birth
#' and the evaluation date (\code{de_eval_date}). Optionally filters results by
#' site or cycle.
#'
#' @param data A dataset containing year of birth and evaluation date (\code{de_eval_date}).
#' @param site Optional. A site code or name to restrict calculations to a specific
#' study site.
#' @param cycle Optional. A study cycle identifier to limit calculations to a
#' specific data collection wave.
#'
#' @return
#' A data frame or \code{\link[tibble]{tibble}} with participant identifiers and
#' their calculated age (in years) at the time of evaluation.
#'
#' @details
#' The \code{calculate_age_at_visit()} function computes age at the time of evaluation
#' in years using the formula:
#' \deqn{age = year\_of\_evaluation - year\_of\_birth}
#'
#' Here, \code{year_of_evaluation} is derived from \code{de_eval_date}.
#' It assumes that year of birth is available either directly in the input data or
#' via the \code{\link{retrieve_birth_year}} helper function.
#' This function is used internally by \code{\link{get_demographics}} when
#' \code{age_at_visit} is among the requested variables.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Calculate age for all participants
#'   age_tbl <- calculate_age_at_visit(demo_data)
#'
#'   # Calculate age for a specific site and cycle
#'   age_tbl_ku <- calculate_age_at_visit(demo_data, site = "KUMC", cycle = 2)
#' }
#' }
#'
#' @seealso
#'  \code{\link{retrieve_birth_year}},
#'  \code{\link{get_demographics}}
#'
#' @rdname calculate_age_at_visit
#' @export

calculate_age_at_visit <- function(data, site, cycle) {
  ptdoby <- retrieve_birth_year(data, site)

  data <- data[data$dd_field_name == "de_eval_date", ]

  ids <- get_ids(data)

  data$de_eval_date <- as.Date(
    data$dd_revision_field_value,
    tryFormats = c("%Y-%m-%d", "%y-%m-%d", "%m/%d/%y")
  )

  if (!is.null(site)) {
    data <- filter_by_site(data, site)
  }

  if (!is.null(cycle)) {
    data <- filter_by_cycle(data, cycle)
  }

  data <- atri_join(
    data[, c(ids, "de_eval_date")],
    ptdoby,
    by = ids,
    join_type = inner_join
  )

  data$age_at_visit <- as.numeric(format(data$de_eval_date, "%Y")) -
    as.numeric(data$ptdob)
  data$ptdob <- NULL
  data$de_eval_date <- NULL
  return(data)
}
