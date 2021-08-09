#' Easy plot highcharter personal theme
#'
#' @param hc highchart object
#' @param title title to the plot
#' @param subtitle subtitle to the plot
#' @param source source of data
#' @param range logical, display range options?
#' @param navigator logical, display navigator zoom?
#'
#' @return plot
#' @keywords internal
#'
plot_am <- function(hc, title, subtitle = NULL, source, range = TRUE, navigator = TRUE) {

  # Create theme
  theme_am <- highcharter::hc_theme_merge(
    highcharter::hc_theme_elementary(),
    highcharter::hc_theme(
      chart = list(style = list(fontFamily = "Open Sans", color = "#333")),
      title = list(
        style = list(fontFamily = "Open Sans", color = "black", fontWeight = "bold"),
        align = "center"
      ),
      subtitle = list(
        style = list(fontFamily = "Open Sans", fontWeight = "bold"),
        align = "center"
      ),
      legend = list(align = "center", verticalAlign = "bottom"),
      xAxis = list(
        gridLineWidth      = 1,
        gridLineColor      = "#F3F3F3",
        lineColor          = "#F3F3F3",
        minorGridLineColor = "#F3F3F3",
        tickColor          = "#F3F3F3",
        tickWidth          = 1
      ),
      yAxis = list(
        gridLineColor      = "#F3F3F3",
        lineColor          = "#F3F3F3",
        minorGridLineColor = "#F3F3F3",
        tickColor          = "#F3F3F3",
        tickWidth          = 1
      )
    )
  )

  # create plot object
  am_plot <- hc %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_subtitle(text = subtitle) %>%
    highcharter::hc_credits(
      enabled = TRUE,
      text    = paste0("Fonte: ", source),
      style   = list(fontSize = "12px")
    ) %>%
    highcharter::hc_xAxis(title = FALSE) %>%
    highcharter::hc_yAxis(
      labels    = list(format = "{value}"),
      title     = FALSE,
      plotLines = list(
        list(
          value = 0,
          color = "#1a1a1a",
          width = 2
        )
      )
    ) %>%
    highcharter::hc_tooltip(shared = TRUE) %>%
    highcharter::hc_add_theme(theme_am) %>%
    highcharter::hc_navigator(enabled = navigator)

  if (range) {
    am_plot <- am_plot %>%
      highcharter::hc_rangeSelector(
        selected          = 4,
        enabled           = TRUE,
        allButtonsEnabled = TRUE,
        inputEnabled      = FALSE,
        dropdown          = "always"
      )
  } else am_plot

  am_plot

}
