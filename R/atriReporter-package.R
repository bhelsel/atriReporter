#' @title ATRI API Tools
#' @description
#' The **ATRI API Tools** package provides a suite of helper functions for
#' interacting with the ATRI EDC data lake API and managing study data.
#' It supports authentication, file retrieval, importing, summarization, and
#' standardized formatting of tables and reports.
#'
#' @section Environment and API Functions:
#' Functions for securely connecting to and retrieving data from the ATRI EDC.
#'
#' - \code{\link{get_atri_token}} — Retrieve the API token from the user's environment.
#' - \code{\link{get_atri_server}} — Build a formatted server URL for the ATRI data lake.
#' - \code{\link{retrieve_from_environment}} — Identify stored ATRI tokens or servers in the environment.
#' - \code{\link{atri_get}} — Execute authenticated API requests.
#' - \code{\link{get_atri_data}} — Validate and parse responses from the ATRI API.
#' - \code{\link{get_atri_files}} — Retrieve links for folders and files stored in the ATRI EDC.
#' - \code{\link{import_atri_file}} — Import CSV files from the ATRI data lake API.
#'
#' @section Data Management Functions:
#' Tools for cleaning, transforming, and preparing ATRI data sets for analysis.
#'
#' - \code{\link{calculate_age_at_visit}} — Calculate participant age from year of birth and evaluation date.
#' - \code{\link{split_factor_labels}} — Expand delimited factor variables (e.g., "1|2|3") into binary indicator columns.
#' - \code{\link{to_snake_case}} — Convert character strings to snake_case.
#' - \code{\link{collapse_and}} — Collapse a character vector with commas and “and” (Oxford comma style).
#'
#' @section Data Integration and Summarization:
#' Functions for merging, summarizing, and retrieving study-specific data.
#'
#' - \code{\link{atri_join}} — Flexible wrapper around dplyr join functions.
#' - \code{\link{get_ids}} — Identify standard ID variables (subject, site, event).
#' - \code{\link{summarize_by}} — Compute grouped summary statistics (mean, median, sd, min, max).
#' - \code{\link{get_imaging}} — Retrieve and reshape imaging metadata (amyloid PET, FDG, MRI).
#'
#' @section Table Formatting:
#' Functions to apply ABCDS themes to formatted tables for reports and outputs.
#'
#' - \code{\link{ft_add_abcds_theme}} — Apply ABCDS styling to flextables.
#' - \code{\link{gt_add_abcds_theme}} — Apply ABCDS styling to gt tables.
#'
#' @section Utility Operators:
#' JavaScript-inspired conditional operators to simplify inline logic.
#'
#' - \code{\link{\%?\%}} — Define a conditional test expression.
#' - \code{\link{\%:\%}} — Provide the else-value in a JavaScript-style conditional.
#'
#' @section Helper Functions:
#' - \code{\link{filter_by_site}} — Filter a data set by site code, initials, or label.
#' - \code{\link{filter_by_cycle}} — Filter a data set by study cycle or visit month.
#'
#' @section Author:
#' Brian Helsel (\email{bhelsel@@kumc.edu})
#'
#' @section See Also:
#' - \link[dplyr:join]{dplyr joins}
#' - \link[httr2:request]{httr2 request workflow}
#' - \link[flextable:flextable]{flextable formatting}
#' - \link[gt:gt]{gt tables}
#'
#' @importFrom rlang .data
#'
#' @docType package
#' @name atriReporter
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end

NULL
