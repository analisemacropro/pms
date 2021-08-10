#' Create dashboard
#'
#' @param ... Other arguments to run_model()
#' @param save_at Directory to save .html file
#' @param silent Logical
#'
#' @return Dashboard
#' @export
#'
#' @examples
#' \dontrun{
#' run_dashboard()
#' }
#'
run_dashboard <- function(..., save_at = "docs", silent = FALSE) {

  # Model step
  data_dash <- run_model(...)

  # Test and train samples
  data_pms <- data_dash %>%
    magrittr::extract2("data_samples") %>%
    magrittr::extract(1:2) %>%
    dplyr::bind_rows() %>%
    dplyr::select(date, "value" = `Valor`) %>%
    dplyr::mutate(id = "PMS Volume")


  # Forecast points
  data_frcst <- data_dash %>%
    magrittr::extract2("model_forecast") %>%
    dplyr::as_tibble() %>%
    dplyr::select(date, "value" = .data$.mean) %>%
    dplyr::mutate(id = "Previs\u00e3o")


  # Results tibble (observed and forecast points)
  tbl_results <- dplyr::bind_rows(data_pms, data_frcst) %>%
    dplyr::mutate(
      date = lubridate::as_date(date),
      mom = ((.data$value / dplyr::lag(.data$value)) - 1) * 100,      # % a.m.
      yoy = ((.data$value / dplyr::lag(.data$value, 12)) - 1) * 100,  # % a.a.
      dplyr::across(
        tidyselect::vars_select_helpers$where(is.numeric), ~round(.x, 2)
        )
      ) %>%
    tidyr::drop_na() %>%
    dplyr::as_tibble()


  # Models accuracy results
  error_metric <- data_dash %>%
    magrittr::extract2("model_accuracy") %>%
    dplyr::select(-.data$.type) %>%
    dplyr::rename("Modelo" = .data$.model) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~round(.x, 3)),
      Modelo = c("ARIMA", "ARIMA com Google Trends")
      ) %>%
    tidyr::pivot_longer(names_to = "M\u00e9trica", cols = -.data$Modelo) %>%
    tidyr::pivot_wider(names_from = .data$Modelo)


  am_message(
    paste0("\nPreparing dashboard...\n"),
    silent
  )

  # Temporarily change locale setting (usefull across OS)
  suppressWarnings(withr::local_locale(c("LC_TIME" = "pt_BR.utf8")))
  suppressWarnings(withr::local_locale(c("LC_TIME" = "pt_BR")))
  suppressWarnings(withr::local_locale(c("LC_TIME" = "pt_PT")))
  suppressWarnings(withr::local_locale(c("LC_TIME" = "Portuguese_Brazil.1252")))


  # Build dashboard
  rmd <- "rmarkdown/templates/dashboard/skeleton/skeleton.Rmd"
  file <- system.file(rmd, package = "pms")
  rmarkdown::render(input = file, output_file = "index", output_dir = save_at)

  system2("open", save_at)

  am_message(
    paste0("\nDone!\n"),
    silent
  )

  return(data_dash)

}
