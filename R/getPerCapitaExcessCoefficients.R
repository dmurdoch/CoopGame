#' @name getPerCapitaExcessCoefficients
#' @title Compute per capita excess coefficients
#' @description getPerCapitaExcessCoefficients computes the 
#' per capita excess coefficients for a 
#' specified TU game and an allocation x
#' @aliases getPerCapitaExcessCoefficients
#' @export getPerCapitaExcessCoefficients
#' @template author/JS
#' @template param/v
#' @template param/x
#' @return numeric vector containing the per capita excess coefficients for every coalition
#' @examples 
#' library(CoopGame)
#' getPerCapitaExcessCoefficients(c(0,0,0,60,48,30,72), c(24,24,24))
#'
getPerCapitaExcessCoefficients=function(v,x){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getPerCapitaExcessCoefficients(paramCheckResult,v,x)
  A = v
  N=length(A);
  n<-getNumberOfPlayers(A)

  excessCoefficients=sapply(1:N,function(i){
    involvedPlayers=getPlayersFromIndex(n,i)
    return(A[i]-sum(x[involvedPlayers]))
  })
  
  coeffMat<-createBitMatrix(n)
  cardS=sapply(1:N,function(ix){sum(coeffMat[ix,1:n])})
  pce = sapply(1:(N-1),function(i){
    excessCoefficients[i]/cardS[i]
  })
  return(c(pce[-N],0))
}

initialParamCheck_getPerCapitaExcessCoefficients=function(paramCheckResult,v,x){
  stopOnInvalidGameVector(paramCheckResult,v)
  stopOnInvalidAllocation(paramCheckResult,x)
}
