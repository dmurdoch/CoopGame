#' @name isEssentialGame
#' @title Check if game is essential
#' @description Checks if a TU game with n players is essential.
#' We call a game essential, if the value of the grand coalition   
#' is greater than the sum of the values of the singleton coalitions.
#' A game is essential, if \deqn{v(N) > \sum v({i})}. \cr
#' For an essential game the imputation set is nonempty and consists of more than one point.
#' @aliases isEssentialGame
#' @export isEssentialGame
#' @template author/MM
#' @template author/JS
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 23
#' @template cites/GILLES_2015
#' @templateVar GILLES_2015_P p. 18
#' @template param/v
#' @return \code{TRUE} if the game is essential, else \code{FALSE}. 
#' @examples
#' library(CoopGame)
#' isEssentialGame(c(1,2,3,4,4,4,7))
#' 
#' \donttest{
#' # Example of an essential game
#' library(CoopGame)
#' v1 <- c(0,0,0,60,60,60,72)
#' isEssentialGame(v1)
#'
#' # Example of a game that is not essential  
#' library(CoopGame)
#' v2 <- c(30,30,15,60,60,60,72)
#' isEssentialGame(v2)
#'
#' # Example of a game that is not essential 
#' library(CoopGame)
#' v3 <- c(20,20,32,60,60,60,72)
#' isEssentialGame(v3)
#' }
#'
isEssentialGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isEssentialGame(paramCheckResult = paramCheckResult, v)
  A = v
  numberOfPlayers <- getNumberOfPlayers(A)
  isEssential <- FALSE
  scVal <- sum(A[1:numberOfPlayers])
  gcVal <- A[length(A)]

  if (gcVal > scVal) {
    isEssential <- TRUE
  } else {
    isEssential <- FALSE
  }
  return(isEssential)
}

initialParamCheck_isEssentialGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}