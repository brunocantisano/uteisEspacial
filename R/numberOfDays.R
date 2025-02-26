#' Retorna a quantidade de dias no mês
#'
#' Esta função recebe uma data e descobre a quantidade de dias no mês
#' @param date data para descobrir o número de dias no mês
#' @return A quantidade de dias no mês
#' @export
numberOfDays <- function(date) {
  m <- format(date, format="%m")
 
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}