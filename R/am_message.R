#' Messages in Console
#'
#' @param text String
#' @param silent Logical
#'
#' @return Message in console
#' @export
#'
#' @examples
#' \dontrun{
#' am_message(text = "Starting step...", silent = FALSE)
#'}
am_message <- function (text, silent)
{
  if (silent) {
    message("", appendLF = FALSE)
  }
  else {
    message(text, appendLF = FALSE)
  }
}
