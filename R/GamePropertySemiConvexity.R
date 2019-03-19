#' @name isSemiConvexGame
#' @title Check if game is semiconvex
#' @description isSemiConvexGame checks if a TU game is semiconvex.
#' A TU game is semiconvex if and only if the following conditions hold true:
#' The gap function of any single player \code{i} is minimal among the gap 
#' function values of coalitions \code{S} containing player \code{i}. 
#' Also, the gap function itself is required to be nonnegative.
#' @aliases isSemiConvexGame
#' @export isSemiConvexGame
#' @template author/JS
#' @template cites/DRIESSEN_TIJS_1985
#' @templateVar DRIESSEN_TIJS_1985_P pp. 229--247
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 76
#' @template param/v
#' @return \code{TRUE} if the game is semiconvex, else \code{FALSE}. 
#' @examples
#' library(CoopGame)
#' isSemiConvexGame(c(0,0,0,1,1,1,4))
#' 
#' \donttest{
#' #Example of a semiconvex game 
#' library(CoopGame)
#' v1<-c(3,4,5,9,10,11,18)
#' isSemiConvexGame(v1)
#'
#' #Example of a game which not semiconvex 
#' library(CoopGame)
#' v2=c(1:7)
#' isSemiConvexGame(v2)
#' }
#' 
isSemiConvexGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isSemiConvexGame(paramCheckResult = paramCheckResult, v)
  A = v
  N <- length(A)
  n <- getNumberOfPlayers(A)
  bm <- createBitMatrix(n,A)
  result <- TRUE
  gapFunction <- getGapFunctionCoefficients(A)
  minGap <- min(gapFunction)
  if (minGap < 0){
    result <- FALSE
    return(result)
  }
  for (i in 1:n) {
    bmIndices=which(bm[,i]==1,1)
    gap_i = gapFunction[i]
    for (j in bmIndices){
      if (gapFunction[j] < gap_i){
        result <- FALSE
        return(result)
      }
    }
  }
  return(result)
}

initialParamCheck_isSemiConvexGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
