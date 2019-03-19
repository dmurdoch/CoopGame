#' @name majoritySingleVetoGameValue
#' @title Compute value of a coalition for a weighted majority game with a single veto player
#' @description \strong{Coalition value for a weighted majority game with a single veto player:} \cr
#' For further information see \link{majoritySingleVetoGame}
#' @aliases majoritySingleVetoGameValue
#' @export majoritySingleVetoGameValue
#' @template author/MM
#' @template author/JS
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/S
#' @template param/vetoPlayer
#' @return \code{1} if vetoPlayer is included in \code{S} and 
#' \code{S} is not a singleton coalition, \code{0} otherwise
#' @examples
#' library(CoopGame)
#' majoritySingleVetoGameValue(S=c(1,2), vetoPlayer=1)
#' 
majoritySingleVetoGameValue<-function(S,vetoPlayer){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidVetoPlayer(paramCheckResult,vetoPlayer)
  logicmajoritySingleVetoGameValue(S,vetoPlayer)
}

logicmajoritySingleVetoGameValue=function(S, vetoPlayer) {
  result=0
  if ( (length(S) >= 2) && (vetoPlayer %in% S) ) {
    result=1
  }

  return(result)
}


#' @name majoritySingleVetoGameVector
#' @title Compute game vector for a weighted majority game with a single veto player
#' @description \strong{Game vector for a weighted majority game with a single veto player:} \cr
#' For further information see \link{majoritySingleVetoGame} 
#' @aliases majoritySingleVetoGameVector
#' @export majoritySingleVetoGameVector
#' @template author/MM
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/n
#' @template param/vetoPlayer
#' @return Game Vector where each elements contains \code{1} if vetoPlayer is included 
#' in \code{S} and \code{S} is not a singleton coalition, \code{0} otherwise
#' @examples
#' library(CoopGame)
#' majoritySingleVetoGameVector(n=3, vetoPlayer=1)
#' 
#' 
majoritySingleVetoGameVector<-function(n,vetoPlayer){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = majoritySingleVetoGameValue(S=currCoal, vetoPlayer=vetoPlayer)
    i<-i+1
  }
  return(A)
}


#' @title Construct a weighted majority game with a single veto player
#' @description \strong{Create a list containing 
#' all information about a specified weighted majority game with a single veto player:}\cr
#' If coalition \code{S} has at least 2 members and if the veto player is part of the
#' coalition it generates a value of \code{1}, otherwise \code{0}. \cr
#' Note that weighted majority games with a single veto player are always simple games.
#' @template author/JS
#' @name majoritySingleVetoGame
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 415
#' @template param/n
#' @template param/vetoPlayer
#' @return A list with three elements 
#' representing the specified weighted majority game with 
#' a single veto player (n, vetoPlayer, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{majoritySingleVetoGameValue}, \link{majoritySingleVetoGameVector}
#' @examples
#' library(CoopGame) 
#' majoritySingleVetoGame(n=3, vetoPlayer=1)
#' 
majoritySingleVetoGame<-function(n,vetoPlayer){
  v = majoritySingleVetoGameVector(n=n,vetoPlayer=vetoPlayer)
  retmajoritySingleVetoGame=list(n=n,vetoPlayer=vetoPlayer,v=v)  
  return(retmajoritySingleVetoGame)
}




