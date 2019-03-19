#' @name getExcessCoefficients
#' @title Compute excess coefficients
#' @description getExcessCoefficients computes the excess coefficients for a 
#' specified TU game and an allocation x
#' @aliases getExcessCoefficients
#' @export getExcessCoefficients
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 58
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 12
#' @template param/v 
#' @template param/x
#' @return numeric vector containing the excess coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getExcessCoefficients(c(0,0,0,60,48,30,72), c(24,24,24))
#' 

getExcessCoefficients=function(v,x){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getExcessCoefficients(paramCheckResult,v,x)
  A = v
  N=length(A);
  n<-getNumberOfPlayers(A)

  excessCoefficients=sapply(1:N,function(i){
    involvedPlayers=getPlayersFromIndex(n,i)
    return(A[i]-sum(x[involvedPlayers]))
  })
  
  return(excessCoefficients);

}

initialParamCheck_getExcessCoefficients=function(paramCheckResult,v,x){
  stopOnInvalidGameVector(paramCheckResult, v)
  stopOnInvalidAllocation(paramCheckResult,x)
}
