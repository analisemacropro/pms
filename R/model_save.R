#' Save model results
#'
#' @param ... Objects to save
#' @param path Directory to save .rds file
#' @param silent Logical
#'
#' @return Save .rds file
#' @export
#'
#' @examples
#' \dontrun{
#' silent = FALSE
#'
#' # ETL
#'data_download <- retry(data_etl(silent = silent))
#'
#' # Data split (train and test)
#' data_samples <- data_split(data_download, train_end = train_end, silent = silent)
#'
#' # Fit models
#' data_fit <- model_fit(data_samples, silent = silent)
#'
#' # Model performance step (in sample)
#' data_performance <- model_accuracy(data_fit, silent = silent)
#'
#' # Forecast
#' data_forecast <- model_forecast(data_fit, data_samples, silent = silent)
#'
#' # Save results step
#' model_save(data_fit, data_performance, path = "data", silent = silent)
#'}
model_save <- function(..., path = "data", silent = FALSE) {

  am_message(
    paste0("\nSaving model results...\n"),
    silent
  )


  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE)
  }
  path <- normalizePath(path)

  save_data <- list(...)

  saveRDS(save_data, file = paste0(path, "/pms_results.rds"))

  am_message(
    paste0("\nDone!\n"),
    silent
  )

}
