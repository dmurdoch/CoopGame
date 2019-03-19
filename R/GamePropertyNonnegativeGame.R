#' @name isNonnegativeGame
#' @title Check if  game is nonnegative
#' @description isNonnegativeGame checks if a TU game is a nonnegative game.
#' A TU game is a nonnegative game if the game vector does not 
#' contain any negative entries.
#' @aliases isNonnegativeGame
#' @export isNonnegativeGame
#' @template author/JS
#' @template param/v
#' @return \code{TRUE} if the game is nonnegative, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' isNonnegativeGame(c(0,0,0,0.5,0.1,0.4,1))
#' 
#' \donttest{
#' #Nonnegative game
#' library(CoopGame) 
#' v1<-c(0,0,0,0,1,1,1)
#' isNonnegativeGame(v1)
#'
#' #Example for game which is not nonnegative 
#' library(CoopGame)
#' v2<-c(0,0,0,0,-1.1,1,2)
#' isNonnegativeGame(v2)
#' }
#' 
isNonnegativeGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isNonnegativeGame(paramCheckResult = paramCheckResult, v)
  A = v
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  tolerance = -1e-15
  for (i in 1:N)
  {
    if (A[i]<tolerance)    
    {
      boolRetVal = FALSE
      return(boolRetVal)
    }
  }
  return(boolRetVal)
}

initialParamCheck_isNonnegativeGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}