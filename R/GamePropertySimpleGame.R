#' @name isSimpleGame
#' @title Check if game is simple
#' @description isSimpleGame checks if a TU game is a simple game.
#' A TU game is a simple game in the sense of the book by 
#' Peleg and Sudhoelter (2007), p. 16, if and only if the 
#' game is monotonic and the values of all coalitions are 
#' either \code{0} or \code{1}. 
#' @aliases isSimpleGame
#' @export isSimpleGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 16  
#' @template param/v
#' @return \code{TRUE} if the game is essential, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' isSimpleGame(c(0,0,0,1,0,1,1))
#' 
#' \donttest{
#' #Example of a simple game
#' library(CoopGame) 
#' v1<-c(0,0,0,0,1,1,1)
#' isSimpleGame(v1)
#'
#' #Example of a game which not simple 
#' library(CoopGame)
#' v2<-c(0,0,0,0,1,1,2)
#' isSimpleGame(v2)
#' 
#' #Another example of a game which not simple 
#' #according to our definition
#' library(CoopGame) 
#' v3<-c(1,0,0,0,1,1,1)
#' isSimpleGame(v3)
#' }
#' 
isSimpleGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isSimpleGame(paramCheckResult = paramCheckResult, v)
  A = v
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  tolerance = 1e-15
  if (!isMonotonicGame(A))
  {
    boolRetVal = FALSE
    return(boolRetVal)
  }
  for (i in 1:N)
  {
    if ((abs(A[i]-0)>tolerance) && (abs(A[i]-1)>tolerance))
    {
      boolRetVal = FALSE
      return(boolRetVal)
    }
  }
  return(boolRetVal)
}

initialParamCheck_isSimpleGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
