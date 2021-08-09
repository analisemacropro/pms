#' Retry an expression
#'
#' @param foo The expression
#' @param tries Number of tries
#' @param silent Silent (logical)
#'
#' @return Output of expression or error
#' @export
#'
#' @examples
#' \dontrun{
#' retry(getwd())
#'}
retry <- function(foo, tries = 3, silent = FALSE) {

  am_message(
    paste0(paste0("Trying expression (remaining tries: ", tries, ")...\n")),
    silent
  )

  withRestarts(
    tryCatch(
      { foo },
      error = function(e) { invokeRestart("restart") }
    ),
    restart = function() {
      message("Failed...")
      stopifnot(tries > 0)
      retry(foo, tries-1)
    }
  )
}
