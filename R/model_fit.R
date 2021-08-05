#' Fit models
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
#' data_download <- data_etl(silent = silent)
#'
#' # Data split step (train and test)
#' data_samples <- data_split(data_download, train_end = 2019, silent = silent)
#'
#' # Fit models
#' data_fit <- model_fit(data_samples, silent = silent)
#'}
model_fit <- function(data, silent = FALSE) {

  am_message(
    paste0("\nStarting model training step...\n"),
    silent
  )

  treino <- data[["df_treino"]]
  teste <- data[["df_teste"]]


  fit_ar <- treino %>%
    fabletools::model(fable::ARIMA(Valor)) %>%
    fabletools::refit(teste)


  fit_trends <- treino %>%
    fabletools::model(
      fable::ARIMA(Valor ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 + Comp.6)
      ) %>%
    fabletools::refit(teste)

  am_message(
    paste0("\nFinishing model training step...\n"),
    silent
  )

  return(
    list("fit_ar" = fit_ar, "fit_trends" = fit_trends)
    )

}
