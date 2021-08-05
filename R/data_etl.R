#' ETL script
#'
#' @param silent Logical
#' @importFrom rlang .data
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data_download <- data_etl(silent = FALSE)
#'}
data_etl <- function(silent = FALSE) {

  am_message(
    paste0("\nStarting ETL step...\n"),
    silent
  )


  # PMS Volume (% 12 meses)
  services <- sidrar::get_sidra(
    6443,
    period = "all",
    variable = 8677,
    classific = c("c11046", "c12355"),
    category = list(90670, 107071)
    ) %>%
    dplyr::select('M\u00eas (C\u00f3digo)', .data$Valor) %>%
    tidyr::separate('M\u00eas (C\u00f3digo)', into = c("pre", "post"), sep = -2) %>%
    dplyr::mutate(date = paste0(.data$pre, "-", .data$post, "-01")) %>%
    dplyr::select(-.data$pre,-.data$post)


  # Termos Google Trends
  lazer <- c("restaurante","bar", "cinema", "show", "parque", "academia")
  viagem <- c("hotel", "passagem de avi\u00e3o", "passagem de onibus", "aluguel de carro")
  emprego <- c("seguro desemprego", "vaga", "curr\u00edculo","empr\u00e9stimo")
  transporte <- c("onibus", "logistica", "frete", "estrada")
  outros <- c("tempo")

  termos <- c(lazer,viagem, emprego, transporte, outros)

  df_limpo <- purrr::map(
    .x = termos,
    ~gtrendsR::gtrends(
        keyword = .x,
        geo = "BR",
        time = 'all',
        onlyInterest = TRUE
      )
    ) %>%
    purrr::map(1) %>%
    dplyr::bind_rows() %>%
    dplyr::select(.data$date, .data$hits, .data$keyword) %>%
    tidyr::pivot_wider(
      names_from = .data$keyword,
      values_from = .data$hits
      ) %>%
    dplyr::mutate(
      dplyr::across(
        -.data$date,
        ~ .x - dplyr::lag(.x, n = 12)
        )
      ) %>%
    tidyr::drop_na()

  # PCA - Análise de Componentes Principais
  pca <- stats::princomp(scale(df_limpo[,-1]))

  # Tidy data
  data_mensal <- data.frame(
    "date" = df_limpo$date %>% as.character(),
    pca$scores[,c(1:6)]
    ) %>%
    dplyr::left_join(services, by = "date") %>%
    dplyr::filter(.data$date > "2012-12-01") %>%
    dplyr::mutate(date = tsibble::yearmonth(.data$date)) %>%
    tsibble::as_tsibble()

  am_message(
    paste0("\nFinishing ETL step...\n"),
    silent
  )

  return(data_mensal)
}
