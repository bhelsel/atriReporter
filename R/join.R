#' Harmonize and Join ATRI and LONI Study Data
#'
#' This function performs a full join between ATRI and LONI datasets, harmonizing
#' variable types, filling missing variables, adding study and event identifiers,
#' and resolving discrepancies using LONI as the reference standard.
#'
#' The join is performed at the participant–event level, with additional shared
#' variables included to ensure consistency across data sources.
#'
#' @param atri_data A data frame containing participant- or event-level data from
#'   the ATRI source.
#'
#' @param loni_data A data frame containing participant- or event-level data from
#'   the LONI source. When discrepancies arise between ATRI and LONI values, LONI
#'   values are retained.
#'
#' @param variables A character vector of variable names to retain or align across
#'   datasets. (Currently reserved for future flexibility; not yet directly used.)
#'
#' @details
#' The function proceeds through the following steps:
#' \enumerate{
#'   \item Identifies variables common to both datasets (excluding
#'     \code{subject_label} and \code{age_at_visit}).
#'   \item Coerces mismatched variable types to character to ensure join
#'     compatibility.
#'   \item Ensures that all non-identifier ATRI variables exist in the LONI
#'     dataset, creating default columns when necessary.
#'   \item Adds standardized participant and event identifiers to the LONI data.
#'   \item Renames ATRI subject identifiers to match LONI conventions.
#'   \item Performs a full join across participant ID, event code, and shared
#'     variables.
#'   \item Resolves duplicated columns (\code{.x}/\code{.y}) by prioritizing LONI
#'     values.
#'   \item Orders the result by participant ID and event sequence.
#' }
#'
#' Event codes are returned as an ordered factor to preserve longitudinal ordering.
#'
#' @return
#' A data frame containing harmonized ATRI–LONI data with one row per
#' participant–event combination.
#'
#' @seealso
#' \code{\link[dplyr]{full_join}},
#' \code{\link[dplyr]{case_when}}
#'
#' @examples
#' \dontrun{
#' merged_data <- loni_join(
#'   atri_data = atri_df,
#'   loni_data = loni_df,
#'   variables = c("sex", "diagnosis", "education")
#' )
#' }
#'
#' @importFrom abcds add_study_ids add_event_ids
#' @importFrom dplyr case_when arrange
#'
#' @export

loni_join <- function(atri_data, loni_data, variables) {
  commonvars <- setdiff(
    intersect(colnames(loni_data), colnames(atri_data)),
    c("subject_label", "age_at_visit")
  )

  for (var in commonvars) {
    if (var != "subject_label") {
      if (typeof(loni_data[[var]]) != typeof(atri_data[[var]])) {
        loni_data[[var]] <- as.character(loni_data[[var]])
        atri_data[[var]] <- as.character(atri_data[[var]])
      }
    }
  }

  atri_ids <- get_ids(atri_data)

  check_vars <- colnames(atri_data)[which(
    !colnames(atri_data) %in% c(atri_ids, "ids")
  )]

  for (varname in check_vars) {
    if (!exists(varname, loni_data)) {
      vartype <- typeof(atri_data[[varname]])
      if (vartype == "integer") {
        loni_data[[varname]] <- 0L
      } else {
        loni_data[[varname]] <- NA
      }
    }
  }

  # Add Participant and Event Identifiers
  loni_data <- abcds::add_study_ids(loni_data, inner_join, subject_label)
  loni_data <- abcds::add_event_ids(
    loni_data,
    inner_join,
    subject_label,
    "event_sequence"
  )

  # Rename the subject_label in ATRI to match LONI
  colnames(atri_data)[which(
    colnames(atri_data) == "subject_label"
  )] <- "u19_bds_id"

  joined_df <- atri_join(
    atri_data,
    loni_data,
    by = c(
      "u19_bds_id",
      "event_code",
      commonvars
    ),
    join_type = full_join
  )

  # Resolve discrepancies by using LONI data as the standard
  cols <- colnames(joined_df)[grepl("\\.x$|\\.y$", colnames(joined_df))]

  for (vars in unique(gsub(".x$|.y$", "", cols))) {
    x <- paste0(vars, ".x")
    y <- paste0(vars, ".y")
    joined_df[[x]] <- dplyr::case_when(
      !is.na(joined_df[[x]]) & is.na(joined_df[[y]]) ~ joined_df[[x]],
      is.na(joined_df[[x]]) & !is.na(joined_df[[y]]) ~ joined_df[[y]],
      joined_df[[x]] != joined_df[[y]] ~ joined_df[[y]],
      TRUE ~ joined_df[[x]]
    )
    joined_df[[y]] <- NULL
  }

  colnames(joined_df) <- gsub("\\.x$", "", colnames(joined_df))

  joined_df$event_code = factor(
    joined_df$event_code,
    levels = c("bl", "c2", "c3", "c4", "cyc1", "cyc2", "cyc3", "cyc4")
  )

  joined_df$ids <- ifelse(
    !is.na(joined_df$u19_bds_id),
    joined_df$u19_bds_id,
    joined_df$u01_niad_adds_id
  )

  dplyr::arrange(joined_df, .data$ids, .data$event_code)
}

#' @title Flexible Join for dplyr
#'
#' @description
#' Performs a flexible join on two data frames using any \code{dplyr} join function
#' specified by the user. Supports joins specified as a string, symbol, or function.
#'
#' @inheritParams dplyr::full_join
#' @param join_type The type of \code{dplyr} join to perform. Can be a string
#'   (e.g., "inner_join"), a symbol (e.g., \code{inner_join}), or a function
#'   (e.g., \code{dplyr::inner_join}).
#' @param ... Additional arguments passed to the chosen \code{dplyr} join function.
#'
#' @return
#' A \code{\link[tibble]{tibble}} containing the joined data.
#'
#' @details
#' This function allows flexible selection of join type while joining two data
#' frames. It evaluates the provided \code{join_type} and applies it with any
#' additional arguments passed via \code{...}. Useful for programmatically
#' performing joins in pipelines.
#'
#' @seealso
#'  \code{\link[rlang]{sym}}, \code{\link[rlang]{eval_tidy}},
#'  \code{\link[rlang]{is_symbol}}, \code{\link[rlang]{is_call}},
#'  \code{\link[rlang]{abort}}, \code{\link[dplyr]{inner_join}},
#'  \code{\link[dplyr]{left_join}}, \code{\link[dplyr]{full_join}},
#'  \code{\link[dplyr]{right_join}}
#'
#' @rdname atri_join
#' @export
#' @importFrom rlang sym eval_tidy is_symbol is_call abort

atri_join <- function(x, y, by, join_type, ...) {
  join_fn <- tryCatch(
    {
      join_type <- rlang::ensym(join_type)
      rlang::eval_tidy(join_type, env = asNamespace("dplyr"))
    },
    error = function(e) {
      rlang::eval_tidy(join_type)
    }
  )

  join_fn(x, y, by = by, ...)
}
