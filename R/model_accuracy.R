#' Report model performance
#'
#' @param data Data input
#' @param silent Logical
#'
#' @return List
#' @export
#'
#' @examples
#' \dontrun{
#' silent = FALSE
#'
#' # ETL etep
#' data_download <- retry(data_etl(silent = silent))
#'
#' # Stationarity step
#' data_stationarity <- data_stationarity(data_download, silent = silent)
#'
#' # Transform step
#' data_transformed <- data_transform(data_stationarity, silent = silent)
#'
#' # Data split step (train and test)
#' data_list <- data_split(data_transformed, train_end = "2019-01-01", silent = silent)
#'
#' # Train models and forecast step
#' data_model <- model_train(data_list, h = 6, silent = silent)
#'
#' # Model performance step (in sample)
#' data_performance <- model_accuracy(data_model, silent = silent)
#'}
model_accuracy <- function(data, silent = FALSE) {

  am_message(
    paste0("\nStarting model accuracy step...\n"),
    silent
  )

  fit_ar <- data[["fit_ar"]]
  fit_trends <- data[["fit_trends"]]


  fit_ar %>%
    fabletools::accuracy() %>%
    dplyr::select(RMSE, MAE)

  fit_trends  %>%
    fabletools::accuracy() %>%
    dplyr::select(RMSE, MAE)

  teste$ar <- fabletools::fitted(fit_ar)$.fitted
  teste$trends <- fabletools::fitted(fit_trends)$.fitted


  am_message(
    paste0("\nFinishing model accuracy step...\n"),
    silent
  )

  return(metrics)

}
