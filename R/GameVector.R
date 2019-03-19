#' @name getZeroOneNormalizedGameVector
#' @title Compute 0-1-normalized game vector
#' @description Computes the zero-one-normalized game for a given game specified by a game vector.
#' @aliases getZeroOneNormalizedGameVector
#' @export getZeroOneNormalizedGameVector
#' @template author/JS
#' @template author/JA
#' @template cites/GILLES_2015
#' @templateVar GILLES_2015_P p. 18
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 670
#' @template param/v 
#' @return Numeric vector of length (2^n)-1 representing the zero-one-normalized game.
#' @examples
#' library(CoopGame)
#' v<-c(1:7)
#' getZeroOneNormalizedGameVector(v)
#' 
getZeroOneNormalizedGameVector<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVector(paramCheckResult,v)
  A = v
  retVal=NULL
  AzeroNorm<-getZeroNormalizedGameVector(A)
  V_N=AzeroNorm[length(AzeroNorm)]
  if(V_N==0){
    print("Zero-one-normalized game can not be retrieved since value of grand coalition of zero-normalized game is 0.")
  }else{
    retVal=AzeroNorm/V_N
  }
  return(retVal)
}

#' @name getZeroNormalizedGameVector
#' @title Compute 0-normalized game vector
#' @description Computes the zero-normalized game for a given game specified by a game vector.
#' @aliases getZeroNormalizedGameVector
#' @export getZeroNormalizedGameVector
#' @template author/JS
#' @template author/JA
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 9
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 11
#' @template param/v
#' @return Numeric vector of length (2^n)-1 representing the zero-normalized game.
#' @examples
#' library(CoopGame)
#' v<-c(1:7)
#' getZeroNormalizedGameVector(v)
#' 
getZeroNormalizedGameVector<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVector(paramCheckResult,v)
  A=v
  N=length(A)
  n=getNumberOfPlayers(A)
  AzeroNorm=sapply(c(1:N),function(ix){
    involvedPlayers=getPlayersFromIndex(n,ix)
    A[ix]-sum(A[involvedPlayers])
  })
  return(AzeroNorm)
}

#' @name getDualGameVector
#' @title Compute dual game vector
#' @description Computes the dual game for a given TU game 
#' with n players specified by a game vector.
#' @aliases getDualGameVector
#' @export getDualGameVector
#' @template author/JS
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 125
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 7
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 737
#' @template param/v
#' @return Numeric vector of length (2^n)-1 representing the dual game.
#' @examples
#' library(CoopGame)
#' v<-unanimityGameVector(4,c(1,2))
#' getDualGameVector(v)
#'
getDualGameVector<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVector(paramCheckResult,v)
  A=v
  N=length(A)
  return(c(A[N]-rev(A[-N]),A[N]))
}



#' @name getNumberOfPlayers
#' @title Get number of players
#' @description Gets the number of players from a game vector
#' @aliases getNumberOfPlayers
#' @export getNumberOfPlayers
#' @template author/MM
#' @template author/JS
#' @template param/v
#' @return Number of players in the game (specified by game vector v) 
#' @examples
#' library(CoopGame)
#' maschlerGame=c(0,0,0,60,60,60,72)
#' getNumberOfPlayers(maschlerGame)
#'
getNumberOfPlayers <- function(v) {
  A<-v
  n <- log2(length(A) + 1)
  numberOfPlayers<-n
  return(numberOfPlayers)
}
