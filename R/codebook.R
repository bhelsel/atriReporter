#' @title Retrieve a Study Codebook from the ATRI EDC
#'
#' @description
#' Retrieves and formats the codebook (data dictionary) for a specified study
#' from the ATRI Electronic Data Capture (EDC) system. This function imports the
#' latest data dictionary, extracts key variable information, and returns a
#' clean summary of field names, questions, and coded values.
#'
#' @param study A symbol or unquoted name of the study on the ATRI EDC from which
#' to request the codebook.
#' @param codebook A symbol or unquoted name of the codebook within the specified
#' study to request.
#'
#' @return
#' A data frame containing three columns:
#' \describe{
#'   \item{field_name}{The name of the field or variable.}
#'   \item{field_question}{The English text of the survey question or field label.}
#'   \item{field_code}{The coded values or response options for the field.}
#' }
#'
#' @details
#' This function wraps calls to `import_atri_file()` and `get_atri_files()` to
#' obtain the most recent version of a study’s data dictionary from the ATRI EDC.
#' The returned data frame excludes duplicate rows and standardizes column names
#' for downstream use. Both `study` and `codebook` arguments are treated as symbols,
#' so they should be passed without quotes (e.g., `get_codebook(MY_STUDY, CODEBOOK_NAME)`).
#'
#' @seealso
#'  \code{\link[rlang]{defusing-advanced}} for details on quasiquotation and symbol handling.
#'
#' @examples
#' \dontrun{
#' # Retrieve and view a codebook for a given study
#' cb <- get_codebook(MY_STUDY, CODEBOOK_NAME)
#' head(cb)
#' }
#'
#' @rdname get_codebook
#' @export
#' @importFrom rlang ensym

get_codebook <- function(study, codebook) {
  study <- rlang::ensym(study)
  codebook <- rlang::ensym(codebook)

  if (study == "abcds") {
    # fmt: skip
    files <- get_atri_files(abcds, s3_archive, data_lake, edc, data_dictionary, latest)
    data_dict <- import_atri_file(abcds, files, !!codebook)
  } else if (study == "trcds") {
    # fmt: skip
    files <- get_atri_files(trcds, s3_topic, data_pond_brain_health_report, "Clinical%20Data", data_dictionary)
    data_dict <- import_atri_file(trcds, files, !!codebook)
  }

  data_dict <- data.frame(
    field_name = data_dict$dd_revision_field_name,
    field_question = data_dict$dd_revision_field_question_e,
    field_code = data_dict$dd_revision_field_code_e
  )

  data_dict <- data_dict[!duplicated(data_dict), ]

  data_dict$field_question <- gsub(
    "^\\d{1,2}[a-z]\\.?\\s*|^\\d{1,2}\\.?\\s*",
    "",
    data_dict$field_question
  )

  return(data_dict)
}

#' @title Apply Variable Labels from the ABC-DS Data Dictionary
#'
#' @description
#' Applies human-readable variable labels to an existing dataset using the
#' ABC-DS (Alzheimer’s Biomarker Consortium–Down Syndrome) data dictionary
#' retrieved from the ATRI EDC system.
#'
#' @param data A dataset existing in the user's global environment for which
#' to apply variable labels.
#' @inheritParams get_codebook
#'
#' @return
#' A data frame identical to the input dataset, but with variable-level
#' `label` attributes applied based on the corresponding entries in the
#' ABC-DS data dictionary.
#'
#' @details
#' This function uses \code{\link{get_codebook}} to retrieve the specified study’s
#' data dictionary from the ATRI EDC and applies the descriptive field labels
#' to variables in the dataset.
#'
#' @seealso
#'  \code{\link{get_codebook}},
#'  \code{\link{apply_factor_labels}}
#'
#' @examples
#' \dontrun{
#' # Example: Apply variable labels to an existing dataset
#' demographics <- apply_labels(demographics, abcds, ptdemog)
#' }
#'
#' @rdname apply_labels
#' @export

apply_labels <- function(data, study, codebook) {
  study <- rlang::ensym(study)
  codebook <- rlang::ensym(codebook)
  data_dict <- get_codebook(!!study, !!codebook)
  data <- apply_factor_labels(data, dictionary = data_dict)
  for (i in names(data)) {
    if (i %in% data_dict$field_name) {
      attr(data[[i]], "label") <-
        data_dict$field_question[which(data_dict$field_name == i)]
    }
  }
  return(data)
}


#' @title Apply Factor Labels from the ABC-DS Codebook
#'
#' @description
#' Converts coded variables in a dataset into factors with meaningful levels
#' and labels, as defined in the ABC-DS codebook retrieved from the ATRI EDC.
#'
#' @param data A dataset existing in the user's global environment for which
#' to apply factor labels.
#' @param dictionary A data dictionary (e.g., returned by \code{\link{get_codebook}})
#' from the ATRI EDC containing the field names and coded values.
#'
#' @return
#' A data frame in which coded character or numeric variables are converted
#' to factors with labeled levels according to the ABC-DS codebook.
#'
#' @details
#' The \code{apply_factor_labels()} function parses coded response options
#' stored as JSON within the data dictionary and applies them to the
#' corresponding variables in the dataset.
#'
#' For multi-select fields such as \code{de_race}, the function handles
#' delimited responses (e.g., `"1|2|3"`) through \code{split_factor_labels()}.
#'
#'
#' @seealso
#'  \code{\link{apply_labels}},
#'  \code{\link{get_codebook}}
#'
#' @rdname apply_factor_labels
#' @export

apply_factor_labels <- function(data, dictionary) {
  dictionary <- dictionary[dictionary$field_code != "[]", ]

  dictionary <- dictionary[grepl("[{}]", dictionary$field_code), ]

  dictionary <- purrr::map(dictionary$field_code, function(x) {
    parsed <- jsonlite::fromJSON(gsub("'", '"', x))
    list(
      levels = as.numeric(names(parsed)),
      labels = gsub("\\/\n|\\/ ", " ", unname(unlist(parsed)))
    )
  }) %>%
    `names<-`(., dictionary$field_name)

  check_all <- c("de_race", "mrseqs")

  for (i in names(data)) {
    if (i %in% names(dictionary)) {
      if (i %in% check_all) {
        data <- split_factor_labels(
          data,
          !!i,
          dictionary,
          delim = "|"
        )
      } else {
        data[[i]] <- factor(
          data[[i]],
          levels = dictionary[[i]]$levels,
          labels = trimws(dictionary[[i]]$labels)
        )
      }
    }
  }

  return(data)
}
