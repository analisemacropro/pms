#' Forecast "h" periods ahead
#'
#' @param data_fit Data input (model fit object)
#' @param data_frcst Data input (model samples object)
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
#' data_download <- data_etl(silent = silent)
#'
#' # Data split step (train and test)
#' data_samples <- data_split(data_download, train_end = 2019, silent = silent)
#'
#' # Fit models
#' data_fit <- model_fit(data_samples, silent = silent)
#'
#' # Forecast
#' data_forecast <- model_forecast(data_fit, data_samples, silent = silent)
#'}
model_forecast <- function(
  data_fit,
  data_frcst,
  silent = FALSE
) {

  am_message(
    paste0("\nStarting forecasting step...\n"),
    silent
  )

  fit_trends <- data_fit[["fit_trends"]]
  df_pred <- data_frcst[["df_previsao"]]

  # Point forecast
  fbl_frcst <- fit_trends %>%
    fabletools::forecast(df_pred)

  am_message(
    paste0("\nFinishing forecasting step...\n"),
    silent
  )

  return(fbl_frcst)

}
