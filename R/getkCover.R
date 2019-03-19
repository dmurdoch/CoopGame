#' @name getkCover
#' @title Compute k-cover of game
#' @description getkCover returns the k-cover for a given TU game 
#' according to the formula on p. 173 in the book by Driessen.
#' Note that the k-cover does not exist if condition (7.2) on 
#' p. 173 in the book by Driessen is not satisfied.
#' @aliases getkCover
#' @export getkCover
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 173
#' @template param/v
#' @param k An integer specifying k in the k-cover 
#' @return numeric vector containing the k-cover of the given game 
#' if the k-cover exists, NULL otherwise 
#' @examples 
#' library(CoopGame)
#' getkCover(c(0,0,0,9,9,12,18),k=1)
#' 
#' \donttest{
#' library(CoopGame)
#' #Example from textbook by Driessen, p. 175, with alpha = 0.6 and k = 2
#' alpha = 0.6
#' getkCover(c(0,0,0,alpha,alpha,0,1), k=2)
#' #[1] 0.0 0.0 0.0 0.6 0.6 0.0 1.0
#' }
#' 
getkCover=function(v, k){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getkCover(paramCheckResult, v, k)
  A = v
  N=length(A);
  n<-getNumberOfPlayers(A)
  x <- getUtopiaPayoff(A)
  result <- NULL
  tol <- 1e-10
  # Check whether k-cover exists or not 
  startIndex = indexCoalition(n, 1:k)
  kCoverExists = FALSE
  gapFunction <- getGapFunctionCoefficients(A)
  minGapk <- min(gapFunction[startIndex:N])
  gapGrandCoalition <- gapFunction[N]
  kCoverExists = (gapGrandCoalition <= (minGapk+tol))
  if (kCoverExists)
  {
    result <- A
    for (i in 1:N)
    {
      playersInCurrCoal <- getPlayersFromIndex(n,i)
      if (length(playersInCurrCoal) < k)
      {
        result[i] <- A[i]
      }
      else
      {
        result[i] <- A[N] - sum(x[-playersInCurrCoal])  
      }
    }
  }
  else 
  {
    print("k-Cover does not exist in this case.")
  }
  return(result)
}

initialParamCheck_getkCover=function(paramCheckResult,v, k){
  stopOnInvalidGameVector(paramCheckResult, v)
  stopOnInvalidNumber(paramCheckResult, k)
}
