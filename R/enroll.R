#' @title Retrieve Enrollment-Related Variables from ABC-DS Data
#' @description
#' Extracts one or more enrollment-related variables from an ABC-DS dataset using the specified
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
#' to streamline access to ABC-DS enrollment variables. Quasiquotation is used to support
#' tidy evaluation, allowing unquoted variable names and symbol references.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # Retrieve selected enrollment variables
#'   enrollment_data <- get_enrollment(
#'     enroll,
#'     site = "Site01",
#'     cycle = "Cycle2",
#'     apply_labels = TRUE
#'   )
#' }
#' }
#' @rdname get_enrollment
#' @export

get_enrollment <- function(
  ...,
  site = NULL,
  cycle = NULL,
  apply_labels = FALSE,
  controls = FALSE
) {
  variables <- as.character(rlang::ensyms(...))
  get_data(
    study = "abcds",
    dataset = "enroll",
    codebook = "enroll",
    variables,
    site = site,
    cycle = cycle,
    apply_labels = apply_labels,
    controls = controls
  )
}


#' @title Create a Coenrollment Table by Site
#'
#' @description
#' Retrieves ABC-DS coenrollment data from the ATRI Electronic Data Capture (EDC)
#' system via the API and generates a formatted \code{gt} table summarizing
#' coenrollment counts by site.
#'
#' @return
#' A \code{\link[gt]{gt}} table object displaying coenrollment counts by site.
#'
#' @details
#' This function accesses ABC-DS coenrollment data stored in the ATRI EDC system,
#' performs necessary cleaning and aggregation steps using the \pkg{dplyr} package,
#' and creates a publication-ready \code{gt} table showing the number of participants
#' coenrolled across sites.
#'
#' The resulting table can be further customized with \pkg{gt} functions for styling,
#' labeling, and formatting prior to export or inclusion in reports.
#'
#' @seealso
#'  \code{\link[dplyr]{filter}},
#'  \code{\link[dplyr]{distinct}},
#'  \code{\link[dplyr]{rename}},
#'  \code{\link[dplyr]{group_by}},
#'  \code{\link[dplyr]{summarise}},
#'  \code{\link[gt]{gt}}
#'
#' @examples
#' \dontrun{
#' # Create a coenrollment summary table by site
#' coenroll_table <- create_coenrollment_table()
#' coenroll_table
#'
#' # Save as HTML
#' gt::gtsave(coenroll_table, "coenrollment_table.html")
#' }
#'
#' @rdname create_coenrollment_table
#' @export
#' @importFrom dplyr filter distinct rename group_by summarise
#' @importFrom gt gt

create_coenrollment_table <- function() {
  files <- get_atri_files(abcds, edc, crf_data_exclude_phi, latest)
  data <- import_atri_file(abcds, files, coenroll_exclude_phi)
  coenroll <- data %>%
    dplyr::filter(
      dd_field_name == "coenroll" &
        dd_revision_field_value == 1
    ) %>%
    dplyr::distinct(subject_label, .keep_all = TRUE)

  coenroll_tbl <- coenroll %>%
    dplyr::rename(Site = site_label) %>%
    dplyr::group_by(Site) %>%
    dplyr::summarize(Coenrolled = sum(as.numeric(dd_revision_field_value))) %>%
    rbind(., c("Total", sum(.$Coenrolled))) %>%
    gt::gt() %>%
    gt_add_abcds_theme(title = "TRC-DS Coenrollment")

  return(coenroll_tbl)
}

#' @title Email the ABC-DS Coenrollment Table
#'
#' @description
#' Retrieves ABC-DS coenrollment data from the ATRI Electronic Data Capture (EDC)
#' system using \code{\link{create_coenrollment_table}} and emails the formatted
#' table to a specified list of recipients.
#'
#' @inheritParams send_email
#'
#' @return
#' An alert message in the console from \code{\link{send_email}} indicating
#' whether the email was sent successfully.
#'
#' @details
#' This function automates the process of generating and distributing the
#' ABC-DS coenrollment table. It first calls
#' \code{\link{create_coenrollment_table}} to retrieve and format the table as
#' a \code{\link[gt]{gt}} object, then converts it to HTML using
#' \code{\link[gt]{as_raw_html}} before embedding it in an email body.
#'
#' The email is sent using \code{\link{send_email}}, which must be properly
#' configured with SMTP settings and recipient information.

email_coenrollment_table <- function(sender, recipients) {
  coenroll_tbl <- create_coenrollment_table()
  send_email(
    sender,
    recipients,
    subject = "ABC-DS Participants Coenrolled in TRC-DS",
    text = "CoenrollTable",
    table = gt::as_raw_html(coenroll_tbl)
  )
}

# sender = c("Brian Helsel", "bhelsel@kumc.edu")

# recipients <- list("Brian" = "bhelsel@kumc.edu")
