#' Report model performance
#'
#' @param data Data input
#' @param silent Logical
#'
#' @return Tibble
#' @export
#'
#' @examples
#' \dontrun{
#' silent = FALSE
#'
#' # ETL etep
#' data_download <- data_etl(silent = silent)
#'
#' # Data split step (train and test)
#' data_samples <- data_split(data_download, train_end = 2019, silent = silent)
#'
#' # Fit models
#' data_fit <- model_fit(data_samples, silent = silent)
#'
#' # Model performance step (in sample)
#' data_performance <- model_accuracy(data_fit, silent = silent)
#'}
model_accuracy <- function(data, silent = FALSE) {

  am_message(
    paste0("\nStarting model accuracy step...\n"),
    silent
  )

  fit_ar <- data[["fit_ar"]]
  fit_trends <- data[["fit_trends"]]

  metrics <- dplyr::bind_rows(
    fabletools::accuracy(fit_ar),
    fabletools::accuracy(fit_trends)
    )

  am_message(
    paste0("\nFinishing model accuracy step...\n"),
    silent
  )

  return(metrics)

}
