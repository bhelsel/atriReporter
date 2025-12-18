# path <- "/Users/bhelsel/Desktop/Registry.csv.xlsx"

# data <- openxlsx::read.xlsx(path)
# head(data)

# data <- atriReporter::get_registry(examdate)

# library(dplyr)

# data <- df %>%
#   mutate(
#     event_code = factor(event_code, levels = paste0("cyc", 1:4)),
#     examdate = as.Date(examdate)
#   ) %>%
#   arrange(subject_label, site_label, event_code) %>%
#   group_by(subject_label, site_label) %>%
#   mutate(
#     diff_months = lubridate::time_length(
#       examdate - dplyr::lag(examdate),
#       unit = "month"
#     ),
#     diff_days = lubridate::time_length(
#       examdate - dplyr::lag(examdate),
#       unit = "days"
#     )
#   ) %>%
#   tidyr::pivot_wider(
#     id_cols = site_id:subject_label,
#     names_from = event_code,
#     values_from = examdate:diff_days
#   ) %>%
#   dplyr::ungroup()

# library(gtsummary)

# data %>%
#   select(site_label, diff_months_cyc2:diff_months_cyc4) %>%
#   group_by(site_label) %>%
#   summarise(across(
#     diff_months_cyc2:diff_months_cyc4,
#     ~ mean(.x, na.rm = TRUE)
#   )) %>%
#   rbind(
#     .,
#     c(
#       "Average",
#       colMeans(select(., diff_months_cyc2:diff_months_cyc4), na.rm = TRUE)
#     )
#   ) %>%
#   mutate(across(
#     diff_months_cyc2:diff_months_cyc4,
#     ~ round(as.numeric(.x), 2)
#   )) %>%
#   flextable::flextable() %>%
#   flextable::set_header_labels(
#     values = c(
#       "Site",
#       "Cycle 1 to\n Cycle 2",
#       "Cycle 2 to\n Cycle 3",
#       "Cycle 3 to\n Cycle 4"
#     )
#   ) %>%
#   atriReporter:::ft_add_abcds_theme() %>%
#   flextable::add_footer_row(
#     values = c("Average Number of Months Between Cycles"),
#     colwidths = 4
#   ) %>%
#   flextable::save_as_docx(
#     path = "/Users/bhelsel/Desktop/AverageBetweenABCDSCycleVisits.docx"
#   )
