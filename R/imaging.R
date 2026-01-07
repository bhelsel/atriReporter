#' @title Retrieve ABC-DS Imaging Data
#'
#' @description
#' Retrieves imaging metadata from the ABC-DS study in the ATRI EDC system
#' and returns a clean, analysis-ready dataset. Supports filtering by
#' site and study cycle, selecting specific imaging variables, and applying
#' variable and factor labels.
#'
#' @param ... One or more unquoted variable names to select from the imaging dataset.
#' @param study Character string indicating which study's ATRI EDC data to pull.
#'   Valid options are `"abcds"` and `"trcds"`.
#' @param site Optional. A site code, site initials, or partial site label to filter
#'   the dataset. Defaults to \code{NULL}, returning all sites.
#' @param cycle Optional. A numeric cycle or month value to filter the dataset.
#'   Defaults to \code{NULL}, returning all cycles.
#' @param apply_labels Logical. Whether to apply variable and factor labels from
#'   the ABC-DS data dictionary. Defaults to \code{FALSE}.
#' @param controls A boolean value that indicates whether the function should return the controls, Default is `FALSE`
#' @param imaging Character. The imaging dataset to retrieve. Must be one of
#'   \code{"amymeta"}, \code{"fdgmeta"}, or \code{"mrimeta"}. Defaults to
#'   \code{c("amymeta", "fdgmeta", "mrimeta")}, and the first matching option
#'   will be selected.
#'
#' @return
#' A \code{\link[tibble]{tibble}} with participant identifiers and the selected
#' imaging variables. If \code{apply_labels = TRUE}, variable and factor labels
#' from the ABC-DS data dictionary are applied.
#'
#' @details
#' The \code{get_imaging()} function simplifies retrieval of imaging metadata
#' for ABC-DS participants. It performs the following steps:
#' \enumerate{
#'   \item Imports the selected imaging dataset from ATRI EDC using \code{import_atri_file}.
#'   \item Filters the dataset for the requested variables, site, and cycle.
#'   \item Reshapes the dataset from long to wide format with one row per participant.
#'   \item Optionally applies labels using \code{\link{apply_labels}}.
#' }
#' The function supports multiple imaging datasets (\code{"amymeta"}, \code{"fdgmeta"},
#' \code{"mrimeta"}) and ensures that the resulting data is ready for analysis or
#' merging with other ABC-DS datasets.
#'
#' @seealso
#'  \code{\link{filter_by_site}},
#'  \code{\link{filter_by_cycle}},
#'  \code{\link{apply_labels}},
#'  \code{\link{get_ids}},
#'  \code{\link[tidyr]{pivot_wider}}
#'
#' @examples
#' \dontrun{
#' # Retrieve selected MRI metadata for all sites and cycles
#' mri_data <- get_imaging(done, imaging = "mrimeta")
#'
#' # Retrieve labeled FDG metadata for site "KU" and cycle 2
#' fdg_data <- get_imaging(
#'   done,
#'   site = "KU",
#'   cycle = 2,
#'   apply_labels = TRUE,
#'   imaging = "fdgmeta"
#' )
#' }
#'
#' @rdname get_imaging
#' @export
#' @importFrom tidyr pivot_wider

get_imaging <- function(
    ...,
    study = c("abcds", "trcds"),
    site = NULL,
    cycle = NULL,
    apply_labels = FALSE,
    controls = FALSE,
    imaging = c("amymeta", "taumeta", "fdgmeta", "mrimeta")
) {
    study <- match.arg(study)
    variables <- as.character(rlang::ensyms(...))
    imaging <- match.arg(imaging)

    data <- get_data(
        study = study,
        dataset = !!imaging,
        codebook = !!imaging,
        variables,
        site = site,
        cycle = cycle,
        apply_labels = apply_labels,
        controls = controls
    )

    # Retrieve Centiloid Values for TRC-DS study only
    if (imaging == "amymeta" & study == "trcds") {
        file <- get_atri_files(trcds, "External%20Data")
        centiloid <- import_atri_file(
            trcds,
            files = file,
            pattern = "CENTILOID"
        ) %>%
            dplyr::rename(
                subject_label = `Subject ID`,
                event_code = Scan,
                centiloid = CL
            ) %>%
            dplyr::mutate(
                event_code = ifelse(event_code == "bl", "sc2", event_code)
            ) %>%
            dplyr::select(subject_label, event_code, centiloid)

        data <- dplyr::full_join(
            data,
            centiloid,
            by = c("subject_label", "event_code")
        )
    }
    return(data)
}

#' Get MRI Sequence Data
#'
#' Retrieves and processes MRI sequence metadata from the ABCDS imaging data,
#' including information about which MRI sequences were completed. The function
#' converts sequence descriptions to abbreviated codes, creates binary indicators
#' for each sequence type, and reconstructs a coded sequence string.
#'
#' @param name Character string specifying the column naming convention for MRI
#'   sequence types. Options are:
#'   \itemize{
#'     \item \code{"abbreviation"} (default) - Uses short abbreviated names
#'       (e.g., "T1", "FLAIR", "DTI")
#'     \item \code{"description"} - Uses full descriptive names
#'       (e.g., "T1 MPRAGE/ISSPGR", "3D FLAIR")
#'   }
#' @param ... Additional parameters to pass to get_imaging such as site, cycle, apply_labels, and controls
#' @return A tibble containing MRI sequence data with the following components:
#'   \itemize{
#'     \item Standard imaging metadata columns (subject identifiers, dates, etc.)
#'     \item \code{mri_done} - Numeric indicator of MRI completion status
#'     \item \code{mrseqs} - Pipe-separated string of sequence numbers (e.g., "1|3|5")
#'       indicating which sequences were performed, where numbers correspond to:
#'       1 = T1 MPRAGE, 2 = 3D FLAIR, 3 = T2*/SWI, 4 = DTI, 5 = ASL,
#'       6 = T2 FSE, 7 = rs-fMRI
#'     \item Binary columns for each MRI sequence type (0/1 indicating
#'       presence/absence), with names determined by the \code{name} parameter
#'   }
#'
#' @details
#' The function performs several data processing steps:
#' \enumerate{
#'   \item Retrieves raw MRI metadata from the imaging database
#'   \item Renames the \code{done} column to \code{mri_done} for clarity
#'   \item Converts full sequence descriptions in \code{mrseqs} to abbreviated
#'     codes using the internal \code{.mri_sequence_key} lookup table
#'   \item Removes extraneous whitespace around pipe delimiters
#'   \item Splits the pipe-delimited sequence string into separate binary columns
#'   \item Reorders columns to place sequence indicators at the end
#'   \item Reconstructs the \code{mrseqs} column as numeric codes (1-7)
#'     separated by pipes, based on which binary indicators equal 1
#'   \item Optionally converts abbreviated column names back to full descriptions
#' }
#'
#' The function relies on an internal \code{.mri_sequence_key} data structure
#' that maps between sequence descriptions, abbreviations, and numeric codes.
#'
#' @examples
#' \dontrun{
#' # Get MRI sequences with abbreviated column names
#' mri_data <- get_mri_sequences()
#'
#' # Get MRI sequences with full descriptive column names
#' mri_data <- get_mri_sequences(name = "description")
#'
#' }
#'
#' @seealso \code{\link{get_imaging}}, \code{\link{split_factor_labels}}
#'
#' @export

get_mri_sequences <- function(
    name = c("abbreviation", "description"),
    ...
) {
    name <- match.arg(name)
    data <- get_imaging(
        "done",
        "mrseqs",
        imaging = "mrimeta",
        ...
    )

    colnames(data)[which(
        colnames(data) == "done"
    )] <- "mri_done"

    for (i in seq_len(nrow(.mri_sequence_key))) {
        data$mrseqs <- gsub(
            .mri_sequence_key$description[i],
            .mri_sequence_key$abbreviation[i],
            data$mrseqs,
            fixed = TRUE
        )
    }

    data$mrseqs <- gsub("\\s*\\|\\s*", "|", data$mrseqs)
    data <- split_factor_labels(data, "mrseqs")

    data <- data[, c(
        setdiff(names(data), .mri_sequence_key$abbreviation),
        .mri_sequence_key$abbreviation
    )]

    seq_cols <- data[, .mri_sequence_key$abbreviation]

    data$mrseqs <-
        purrr::pmap_chr(
            seq_cols,
            function(...) {
                vals <- c(...)
                keys <- which(vals == 1)
                if (length(keys) == 0) {
                    return(NA_character_)
                }
                paste(keys, collapse = "|")
            }
        )

    if (name == "description") {
        for (i in seq_len(nrow(.mri_sequence_key))) {
            idx <- which(colnames(data) == .mri_sequence_key$abbreviation[i])
            if (length(idx) > 0) {
                colnames(data)[idx] <- .mri_sequence_key$description[i]
            }
        }
    }

    return(data)
}
