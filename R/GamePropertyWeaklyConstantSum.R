#' @name isWeaklyConstantSumGame
#' @title Check if game is weakly constant-sum
#' @description Checks if a TU game with n players is weakly constant-sum. \cr
#' In a weakly constant-sum game for any singleton coalition 
#' the sums of the values of that singleton coalition 
#' and its complement equal the value of 
#' the grand coalition \code{N}. 
#' @aliases isWeaklyConstantSumGame isWeaklyConstantsumGame
#' @export isWeaklyConstantSumGame
#' @template author/JS
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template param/v
#' @return \code{TRUE} if the game is weakly constant-sum, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' v1=c(0,0,0,2,2,2,2) 
#' isWeaklyConstantSumGame(v1)
#' 
#' \donttest{
#' #Example of a game that is not weakly constant-sum 
#' library(CoopGame)
#' v2=c(0,0,0,40,30,130,100) 
#' isWeaklyConstantSumGame(v2)
#' 
#' #Another example of a weakly constant-sum game
#' library(CoopGame)
#' v3=c(1,1,1,2, 7,7,7,7,7,7, 2,3,3,3, 4)
#' isWeaklyConstantSumGame(v3)
#' }
#' 
isWeaklyConstantSumGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isWeaklyConstantSumGame(paramCheckResult = paramCheckResult, v)
  A = v

  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  retVal<-TRUE

  N = length(A)
  
  B = A[1:(N-1)]+rev(A[-N])
  
  tolerance <- 1e-12
  
  for (i in 1:numberOfPlayers)
  {
    if  (abs(B[i]-A[N])>tolerance)
    {
      retVal <- FALSE
    }
  }
  return(retVal)
}

initialParamCheck_isWeaklyConstantSumGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
