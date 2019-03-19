#' @name is1ConvexGame
#' @title Check if game is 1-Convex 
#' @description is1ConvexGame checks if a TU game is 1-convex.
#' A TU game is 1-convex if and only if the following condition holds true:
#' Let \code{S} be a nonempty coalition. Whenever all
#' players outside \code{S} receive their payoffs according to the
#' utopia payoff of the game, then the remaining part of the total savings 
#' is at least \code{v(S)}. 
#' @aliases is1ConvexGame
#' @export is1ConvexGame
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 73
#' @template param/v
#' @return \code{TRUE} if the game is 1-convex, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' is1ConvexGame(c(0,0,0,9,9,12,18))
#' 
#' \donttest{
#' #1-convex game (taken from book by T. Driessen, p. 75)
#' library(CoopGame)
#' v=c(0,0,0,9,9,15,18)
#' is1ConvexGame(v)
#'
#' #Example of a game which is not 1-convex 
#' library(CoopGame)
#' v=c(1:7)
#' is1ConvexGame(v)
#' }
#' 
is1ConvexGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_is1ConvexGame(paramCheckResult = paramCheckResult, v)
  A = v
  N <- length(A)
  tol <- 1e-10
  gapFunction <- getGapFunctionCoefficients(A)
  minGap <- min(gapFunction)
  gapGrandCoalition <- gapFunction[N]
  result <- FALSE
  result <- ((minGap >= 0) & (gapGrandCoalition <= (minGap+tol)))
  return(result)
}

initialParamCheck_is1ConvexGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
