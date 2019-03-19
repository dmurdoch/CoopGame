#' @name isConstantSumGame
#' @title Check if game is constant-sum
#' @description Checks if a TU game with n players is constant-sum. \cr
#' In a constant-sum game for any coalition 
#' \code{S} the sums of the values of the coalition 
#' \code{S} and its complement equal the value of 
#' the grand coalition \code{N}. 
#' @aliases isConstantSumGame isConstantsumGame
#' @export isConstantSumGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 11
#' @template param/v
#' @return \code{TRUE} if the game is constant-sum, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,2,2) 
#' isConstantSumGame(v)
#' 
#' \donttest{
#' #Example of a game that is not constant-sum 
#' library(CoopGame)
#' v=c(0,0,0,40,30,130,100) 
#' isConstantSumGame(v)
#' 
#' #Another example of a constant-sum game
#' library(CoopGame)
#' v=c(1,1,1,2, 2,2,2,2,2,2, 2,3,3,3, 4)
#' isConstantSumGame(v)
#' }
#' 
isConstantSumGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isConstantSumGame(paramCheckResult = paramCheckResult, v)
  A = v
  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  retVal<-TRUE

  N = length(A)
  
  B = A[1:(N-1)]+rev(A[-N])
  
  tolerance <- 1e-12
  
  for (i in 1:(N-1))
  {
    if  (abs(B[i]-A[N])>tolerance)
    {
      retVal <- FALSE
    }
  }
  return(retVal)
}

initialParamCheck_isConstantSumGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}