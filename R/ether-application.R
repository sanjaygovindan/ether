library('zoo')

#' Rolling average
#'
#' Gets the rolling average for the time period and data specified
#' @param data, lengthOfRoll
#' @keywords sma, rolling, average
#' @export rollingAverage
#' @examples
#' rollingAverage()
rollingAverage <- function(data, lengthOfRoll){
roll <- rollmean(data,lengthOfRoll)
return (roll)
}

#' Distribution
#'
#' Gets the prcing distribution of a dataset
#' @param data, distribution
#' @keywords 
#' @export  data, distribution
#' @examples
#' rollingAverage()
priceDistribution <- function(data, distribution){
  data
  distribution
  return (data, distribution)
}


