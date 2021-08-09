#' Accumulate a monthly percentage variation series in n periods
#'
#' @param data Column
#' @param n Period
#'
#' @return Vector
#' @export
#'
acum_p <- function(data, n){

  factor <- (1+(data/100))

  prod <- RcppRoll::roll_prodr(factor, n = n)

  final <- (prod-1)*100

  return(final)

}
