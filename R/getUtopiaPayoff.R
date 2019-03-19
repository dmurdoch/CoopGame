#' @name getUtopiaPayoff
#' @title Compute utopia payoff vector of game
#' @description getUtopiaPayoff calculates the utopia payoff vector for 
#' each player in a TU game. \cr
#' The utopia payoff of player i is the marginal contribution 
#' of player i to the grand coalition.
#' @aliases getUtopiaPayoff getUtopiaPayoffVector
#' @export getUtopiaPayoff
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 31
#' @template param/v
#' @return utopia payoffs for each player
#' @examples
#' library(CoopGame)
#' maschlerGame <- c(0,0,0,60,60,60,72)
#' getUtopiaPayoff(maschlerGame)
#'
getUtopiaPayoff<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_utopiaPayoff(paramCheckResult = paramCheckResult, v)
    A = v
    n=getNumberOfPlayers(A)
    N=length(A)
    
    M=A[N]-A[(N-1):(N-n)]
    return(M)
}


initialParamCheck_utopiaPayoff=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
