#' @name isSymmetricGame
#' @title Check if game is symmetric
#' @description isSymmetricGame checks if a TU game is symmetric.
#' A TU game is symmetric if and only if the values of all  
#' coalitions containing the same number of players are identical.
#' @aliases isSymmetricGame
#' @export isSymmetricGame
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 12
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 26
#' @template param/v
#' @return \code{TRUE} if the game is symmetric, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' isSymmetricGame(c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' #Example of a symmetric game
#' library(CoopGame) 
#' v1<-c(3,3,3,10,10,10,17)
#' isSymmetricGame(v1)
#'
#' #Example of a game which is not symmetric 
#' library(CoopGame) 
#' v2=c(1:7)
#' isSymmetricGame(v2)
#' }
#' 
isSymmetricGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isSymmetricGame(paramCheckResult = paramCheckResult, v)
  A = v
  boolRetVal=TRUE
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=as.data.frame(createBitMatrix(n,A))
  players=1:n
  tolerance <- 1e-12
  i <- 1
  numberOfCurrSetOld <- 1
  valueComp <- A[i]
  while (i < N){
    i <- i+1
    # Use structure of bitMatrix bm
    # with i increasing, the number of players involved in nondecreasing
    numberOfCurrSet<-sum(bm[i,1:n])
    if (numberOfCurrSet > numberOfCurrSetOld)
    {
      valueComp <- A[i]
      numberOfCurrSetOld <- numberOfCurrSet
    }
    else 
    {
      value <- bm[[i,"cVal"]]
      if (abs(value-valueComp)>tolerance)
      {
        boolRetVal <- FALSE
      }
    }
  }
  return(boolRetVal)
}

initialParamCheck_isSymmetricGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}