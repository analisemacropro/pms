#' Run model
#'
#' @param silent Logical
#' @param train_end Date to split samples
#' @param save_results Logical
#' @param path_results Directory to save results (used when save_results = TRUE)
#'
#' @return List
#' @export
#'
#' @examples
#' \dontrun{
#' data_model <- run_model(save_results = FALSE)
#'}
run_model <- function(
  silent = FALSE,
  train_end = 2019,
  save_results = TRUE,
  path_results = "data"
) {

  # ETL
  data_download <- retry(data_etl(silent = silent))

  # Data split (train and test)
  data_samples <- data_split(data_download, train_end = train_end, silent = silent)

  # Fit models
  data_fit <- model_fit(data_samples, silent = silent)

  # Model performance step (in sample)
  data_performance <- model_accuracy(data_fit, silent = silent)

  # Forecast
  data_forecast <- model_forecast(data_fit, data_samples, silent = silent)

  data_ls = list(
    "model_fit" = data_fit,
    "model_accuracy" = data_performance,
    "model_forecast" = data_forecast
  )

  # Save results
  if(save_results) {

    model_save(
      data_ls,
      path = path_results,
      silent = silent
    )

  }

  return(data_ls)

}
