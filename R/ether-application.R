

#' Rolling average
#'
#' Gets the rolling average for the time period and data specified
#' @param data, lengthOfRoll
#' @keywords sma, rolling, average
#' @return rollingAverage
#' @export 
#' @examples
#' rollingAverage()
library('zoo')
rollingAverage <- function(data, lengthOfRoll){
roll <- zoo::rollmean(data,lengthOfRoll)
return (roll)
}

#' Distribution
#'
#' Gets the prcing distribution of a dataset
#' @param (data, distribution)
#' @keywords distribution, analysis
#' @return newList
#' @export
#' @examples
#' rollingAverage()
priceDistribution <- function(data, distribution){
  data
  distribution
  newList <- list(data, distribution)
  return (newList)  
}


