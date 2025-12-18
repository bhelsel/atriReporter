#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @param outputdir PARAM_DESCRIPTION
#' @param lonidir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[cli]{cli_abort}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[yaml]{write_yaml}}
#'  \code{\link[quarto]{quarto_render}}
#' @rdname generate_report
#' @export
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom yaml write_yaml
#' @importFrom quarto quarto_render

generate_report <- function(..., outputdir, lonidir = NULL) {
  reports <- as.character(rlang::enexprs(...))
  # if (is.null(reports)) {
  #   reports <- all_reports
  # }
  reports <- lapply(.all_reports, FUN = function(x) {
    ifelse(x %in% reports, TRUE, FALSE)
  })

  names(reports) <- .all_reports

  reportdir <- file.path(outputdir, "ABC-DS Reports")

  if (dir.exists(outputdir)) {
    if (!dir.exists(reportdir)) {
      dir.create(reportdir)
    }
  } else {
    cli::cli_abort(c(
      "x" = "The output directory was not found.",
      "i" = glue::glue("Please check that the directory exisits: {outputdir}")
    ))
  }

  qmdfolder <- system.file("qmd", package = "atriReporter")

  invisible(
    file.copy(
      from = list.files(qmdfolder, full.names = TRUE),
      to = reportdir,
      recursive = TRUE,
      overwrite = TRUE
    )
  )

  yaml::write_yaml(
    c(
      list(
        outputdir = outputdir,
        lonidir = lonidir
      ),
      reports
    ),
    file = sprintf("%s/_variables.yaml", reportdir)
  )

  # Name the report using the participant id
  pdffile <- "abcds_report.pdf"
  # Render the quarto document
  quarto::quarto_render(
    input = sprintf("%s/abcds_report.qmd", reportdir),
    output_file = pdffile
  )

  files <- list.files(reportdir, full.names = TRUE)
  non_pdf_files <- files[!grepl("\\.pdf$|\\.yaml$", files, ignore.case = TRUE)]

  invisible(unlink(non_pdf_files, recursive = TRUE))

  # # Adjust pdffile location if it is added to the id folder
  # if (!file.exists(pdffile)) {
  #   pdffile <- file.path(getwd(), id, pdffile)
  # }

  # invisible(file.copy(
  #   from = pdffile,
  #   to = file.path(persondir, basename(pdffile)),
  #   overwrite = TRUE
  # ))
  # # Remove pdf file after it is copied to the output directory
  # invisible(file.remove(pdffile))
  # # Unlink the temporary folder to remove it
  # invisible(unlink(id, recursive = TRUE))

  return(reports)
}
