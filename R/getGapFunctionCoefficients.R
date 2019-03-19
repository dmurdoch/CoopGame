#' @name getGapFunctionCoefficients
#' @title Compute gap function coefficients 
#' @description getGapFunctionCoefficients computes the gap function coefficients for a 
#' specified TU game
#' @aliases getGapFunctionCoefficients
#' @export getGapFunctionCoefficients
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 57
#' @template param/v
#' @return numeric vector containing the gap function coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getGapFunctionCoefficients(c(0,0,0,60,48,30,72))
#' 

getGapFunctionCoefficients=function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getGapFunctionCoefficients(paramCheckResult, v)
  A = v
  N=length(A);
  n<-getNumberOfPlayers(A)
  x <- getUtopiaPayoff(A)
  result <- -getExcessCoefficients(A,x)
  return(result);
}

initialParamCheck_getGapFunctionCoefficients=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
