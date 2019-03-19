#' @name iskConvexGame
#' @title Check if game is k-Convex 
#' @description iskConvexGame checks if a TU game is k-convex.
#' A TU game is k-convex if and only if its k-cover exists 
#' and is convex. See section 7.1 of the book by Driessen 
#' for more details
#' @aliases iskConvexGame
#' @export iskConvexGame
#' @template author/JS
#' @template cites/DRIESSEN_1998
#' @templateVar DRIESSEN_1998_P p. 171--178
#' @template param/v
#' @param k An integer specifying k
#' @return \code{TRUE} if the game is k-convex, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' iskConvexGame(v=c(0,0,0,9,9,12,18), k=1)
#' 
#' \donttest{
#' # Two examples motivated by the book by T. Driessen, p. 175:
#' #
#' # The following game is 2-convex 
#' library(CoopGame)
#' alpha = 0.4
#' v=c(0,0,0,alpha,alpha,0,1)
#' iskConvexGame(v,2)
#'
#' # The following game is not 2-convex 
#' library(CoopGame)
#' alpha = 0.7
#' v=c(0,0,0,alpha,alpha,0,1)
#' iskConvexGame(v,2)
#' }
#' 
iskConvexGame<-function(v,k){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_iskConvexGame(paramCheckResult = paramCheckResult,v,k)
  A = v
  N <- length(A)
  tol <- 1e-10
  result <- FALSE
  kCover <- getkCover(A,k)
  kCoverExists <- !(is.null(kCover))
  if (kCoverExists)
  {
    result <- isConvexGame(kCover)
  }
  return(result)
}

initialParamCheck_iskConvexGame=function(paramCheckResult,v,k){
  stopOnInvalidGameVector(paramCheckResult, v)
  stopOnInvalidNumber(paramCheckResult, k)
}
