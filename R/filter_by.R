#' @title Filter Dataset by Site
#'
#' @description
#' Filters a dataset to include only rows corresponding to a specific study site.
#' The function accepts numeric site codes, character site initials, or partial
#' site labels.
#'
#' @param data A data frame containing ABC-DS study data with site information
#'   (columns: \code{site_code}, \code{site_initials}, \code{site_label}).
#' @param site A numeric site code, a character site initial, or a partial site
#'   label used to filter the dataset. If the site is not recognized, all rows
#'   are returned with a message.
#'
#' @return
#' A filtered \code{data.frame} or \code{\link[tibble]{tibble}} containing only
#' rows corresponding to the specified site.
#'
#' @details
#' This function supports multiple ways of identifying a site:
#' \itemize{
#'   \item Numeric site codes (\code{site_code})
#'   \item Character site initials (\code{site_initials})
#'   \item Partial matches of the site label (\code{site_label})
#' }
#' If the specified site is not recognized, the function returns the full dataset
#' and prints a message.
#'
#' @examples
#' \dontrun{
#' # Filter dataset for site with code 3
#' filtered_data <- filter_by_site(demo_data, "033")
#'
#' # Filter dataset for site with initials "KU"
#' filtered_data <- filter_by_site(demo_data, "KUMC")
#'
#' # Filter dataset for site containing "Kansas"
#' filtered_data <- filter_by_site(demo_data, "Kansas")
#' }
#'
#' @rdname filter_by_site
#' @export

filter_by_site <- function(data, site) {
  if (is.numeric(site) & (site %in% unique(data$site_code))) {
    # needs some adjusting as site_code is returning character due to leading 0
    data <- data[data$site_code == site, ]
  } else if (is.character(site) & (site %in% unique(data$site_initials))) {
    data <- data[data$site_initials == site, ]
  } else if (is.character(site) & any(grepl(site, unique(data$site_label)))) {
    data <- data[grepl(site, data$site_label), ]
  } else {
    message(
      "Did not recognize site ",
      site,
      ". Returning all sites by default."
    )
  }
  return(data)
}

#' @title Filter Dataset by Study Cycle or Month
#'
#' @description
#' Filters a dataset to include only rows corresponding to a specific study cycle
#' or month. The function supports numeric cycles and months based on the
#' \code{event_label} column (format: "Cycle X - Month Y").
#'
#' @param data A data frame containing ABC-DS study data with an \code{event_label}
#'   column in the format "Cycle X - Month Y".
#' @param cycle A numeric value corresponding to a cycle number or month. If the
#'   cycle is not recognized, all rows are returned with a message.
#'
#' @return
#' A filtered \code{data.frame} or \code{\link[tibble]{tibble}} containing only
#' rows corresponding to the specified cycle or month.
#'
#' @details
#' The function parses the \code{event_label} column to extract the cycle and month
#' numbers. It then filters the dataset based on the provided \code{cycle} argument:
#' \itemize{
#'   \item If \code{cycle} matches a cycle number, it filters by "Cycle X".
#'   \item If \code{cycle} is a multiple of 16 and matches a month, it filters by "Month Y".
#' }
#' If the specified cycle or month is not recognized, the function returns the
#' full dataset and prints a message.
#'
#' @examples
#' \dontrun{
#' # Filter dataset for cycle 2
#' filtered_data <- filter_by_cycle(demo_data, 2)
#'
#' # Filter dataset for month 16
#' filtered_data <- filter_by_cycle(demo_data, 16)
#' }
#'
#' @rdname filter_by_cycle
#' @export

filter_by_cycle <- function(data, cycle) {
  # Detect dataset format
  has_cycle_format <- any(grepl("Cycle \\d+ - Month \\d+", data$event_label))
  has_month_only_format <- any(grepl(
    "^(Baseline|Month \\d+)$",
    data$event_label
  ))

  if (has_cycle_format) {
    # Original logic for "Cycle X - Month Y" format
    event_label_options <- strsplit(unique(data$event_label), " - ")
    cycle_options <- purrr::map_chr(event_label_options, ~ .x[[1]])
    cycle_options_num <- as.numeric(gsub("Cycle ", "", cycle_options))
    month_options <- purrr::map_chr(event_label_options, ~ .x[[2]])
    month_options_num <- as.numeric(gsub("Month ", "", month_options))

    if (is.numeric(cycle)) {
      if (cycle %% 16 == 0 & cycle %in% month_options_num) {
        data <- data[grepl(paste("Month", cycle), data$event_label), ]
      } else if (cycle %in% cycle_options_num) {
        data <- data[grepl(paste("Cycle", cycle), data$event_label), ]
      } else {
        message(
          "Did not recognize cycle ",
          cycle,
          ". Returning all cycles by default."
        )
      }
    }
  } else if (has_month_only_format) {
    # Logic for "Baseline", "Month X" format
    if (is.numeric(cycle)) {
      if (cycle == 0) {
        data <- data[data$event_label == "Baseline", ]
      } else {
        target_label <- paste("Month", cycle)
        if (target_label %in% data$event_label) {
          data <- data[data$event_label == target_label, ]
        } else {
          message(
            "Did not recognize cycle/month ",
            cycle,
            ". Returning all timepoints by default."
          )
        }
      }
    } else if (is.character(cycle) && cycle == "Baseline") {
      data <- data[data$event_label == "Baseline", ]
    }
  } else {
    warning("Could not detect event_label format. Returning all data.")
  }

  return(data)
}
