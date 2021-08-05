#' Split samples
#'
#' @param data Data input
#' @param train_end Numeric date (YYYY) to split samples
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
#'}
data_split <- function(data, train_end = 2019, silent = FALSE) {

  am_message(
    paste0("\nStarting data split step...\n"),
    silent
  )

  treino <- data %>%
    dplyr::filter(lubridate::year(date) < train_end)

  teste <- data %>%
    dplyr::filter(lubridate::year(date) >= train_end) %>%
    tidyr::drop_na()

  previsao <- data %>%
    dplyr::filter(
      dplyr::if_any(dplyr::everything(), ~is.na(.x))
      )

  list_samples = list(
    df_treino = treino,
    df_teste = teste,
    df_previsao = previsao
  )

  am_message(
    paste0("\nFinishing data split step...\n"),
    silent
  )

  return(list_samples)

}
