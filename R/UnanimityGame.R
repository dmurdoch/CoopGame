#' @name unanimityGameValue
#' @title Compute value of a coalition for a unanimity game
#' @description \strong{Coalition value for a specified unanimity game:}\cr
#' For further information see \link{unanimityGame}
#' @aliases unanimityGameValue
#' @export unanimityGameValue
#' @template author/JS
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/S
#' @param T represents coalition which is subset of grand coalition N and neccessary for generating value
#' @return  \code{1} if all players of coalition \code{T} are included in 
#' \code{S}, else \code{0}
#' @examples 
#' library(CoopGame)
#' unanimityGameValue(S=c(1,2,3),T=c(2))
#' 
unanimityGameValue<-function(S,T){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidCoalitionS(paramCheckResult,S=T)
  logicunanimityGameValue(S,T)
}


logicunanimityGameValue=function(S,T){
  retVal=0
  playersTinS = intersect(S, T)
  checkTinS = setequal(playersTinS, T)

  if (checkTinS) {
    retVal=1
  }else{
    retVal=0
  }
  return (retVal)
}

#' @name unanimityGameVector
#' @title Compute game vector for a unanimity game
#' @description \strong{Game Vector for a specified unanimity game:}\cr
#' For further information see \link{unanimityGame}
#' @aliases unanimityGameVector
#' @export unanimityGameVector
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/n
#' @param T represents coalition which is subset of grand coalition N and neccessary for generating value
#' @return  Game Vector where each element contains \code{1} if all players of coalition 'T' are included in 'S'
#' else \code{0}
#' @examples 
#' library(CoopGame)
#' unanimityGameVector(n=3,T=c(2))
#' 
unanimityGameVector<-function(n,T){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = unanimityGameValue(S=currCoal,T=T)
    i<-i+1
  }
  return(A)
}


#' @title Construct a unanimity game
#' @description \strong{Create a list containing 
#' all information about a specified unanimity game:}\cr
#' The player in coalition \code{T} are the productive players.
#' If all players from \code{T} are included, the coalition 
#' generates value \code{1}, otherwise \code{0}. \cr
#' Note that unanimity games are always simple games.
#' @template author/JS
#' @name unanimityGame
#' @template param/n
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 152
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @param T represents coalition which is subset of grand coalition and neccessary for generating value
#' @return A list with three elements 
#' representing the unanimity game (n, T, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{unanimityGameValue}, \link{unanimityGameVector}
#' @examples 
#' library(CoopGame)
#' unanimityGame(n=3,T=c(1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' unanimityGame(n=4,T=c(1,2))
#' #Output
#' #$n
#' #[1] 4
#' #
#' #$T
#' #[1] 1 2
#' 
#' #$v
#' #[1] 0 0 0 0 1 0 0 0 0 0 1 1 0 0 1
#' }
#' 
unanimityGame<-function(n,T){
  v = unanimityGameVector(n=n,T=T)
  retunanimityGame=list(n=n,T=T,v=v)  
  return(retunanimityGame)
}

