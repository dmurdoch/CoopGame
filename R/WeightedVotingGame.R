#' @name weightedVotingGameValue
#' @title Compute value of a coalition for a weighted voting game
#' @description \strong{Coalition value for a specified weighted voting game:} \cr
#' For further information see \link{weightedVotingGame}
#' @aliases weightedVotingGameValue quotaGameValue
#' @export weightedVotingGameValue
#' @template author/JS
#' @template author/JA
#' @template author/MM
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 825--831
#' @template param/S
#' @template param/w
#' @template param/q
#' @return \code{1} if the sum of the weights of coalition 
#' \code{S} is greater or equal than quota \code{q}
#' else \code{0}
#' @examples
#' library(CoopGame)
#' weightedVotingGameValue(S=c(1,2,3),w=c(1,2,3),q=4)
#' 
weightedVotingGameValue<-function(S,w,q){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidWeightVector(paramCheckResult,n=length(w),w)
  logicWeightedVotingGameValue(S,w,q)
}


logicWeightedVotingGameValue=function(S, w, q) {

  result = 0

  for(p in S){
    result = result + w[p]
  }

  if (result >= q) {
    result = 1
  } else {
    result = 0
  }
  return(result)
}


#' @name weightedVotingGameVector
#' @title Compute game vector for a weighted voting game (aka quota game)
#' @description \strong{Game vector for a specified weighted voting game:} \cr
#' For further information see \link{weightedVotingGame}
#' @aliases weightedVotingGameVector quotaGameVector
#' @export weightedVotingGameVector
#' @template author/JS
#' @template author/JA
#' @template author/MM
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 825--831
#' @template param/n
#' @template param/w
#' @template param/q
#' @return Game Vector where each element contains \code{1} if the sum of 
#' the weights of coalition \code{S} is greater or equal than 
#' quota \code{q}, else \code{0}
#' @examples
#' library(CoopGame)
#' weightedVotingGameVector(n=3,w=c(1,2,3),q=4)
#' 
weightedVotingGameVector<-function(n,w,q){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = weightedVotingGameValue(S=currCoal,w=w,q=q)
    i<-i+1
  }
  return(A)  
}


  
#' @title Construct a weighted voting game
#' @description \strong{Create a list containing 
#' all information about a specified weighted voting game:}\cr
#' For a weighted voting game we receive a game vector 
#' where each element contains \code{1} if the sum of 
#' the weights of coalition \code{S} is greater or equal than 
#' quota \code{q}, else \code{0}. \cr
#' Note that weighted voting games are always simple games.
#' @aliases weightedVotingGame quotaGame
#' @template author/JS
#' @name weightedVotingGame
#' @template cites/PELEG_2002
#' @templateVar PELEG_2002_P pp. 195--201
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 17
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 825--831
#' @template param/n
#' @template param/w
#' @template param/q
#' @return A list with four elements 
#' representing the weighted 
#' voting game (n, w, q, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{weightedVotingGameValue}, \link{weightedVotingGameVector}
#' @examples 
#' library(CoopGame)
#' weightedVotingGame(n=3,w=c(1,2,3),q=4)
#' 
#' \donttest{
#' library(CoopGame)
#' weightedVotingGame(n=4,w=c(1,2,3,4),q=5)
#' 
#' #Output:
#' #$n
#' #[1] 4
#' 
#' #$w
#' #[1] 1 2 3 4
#' #
#' #$q
#' #[1] 5
#' #
#' #$v
#' #[1] 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1
#' }
#' 
weightedVotingGame<-function(n,w,q){
  v = weightedVotingGameVector(n=n,w=w,q=q)
  retweightedVotingGame=list(n=n,w=w,q=q,v=v)  
  return(retweightedVotingGame) 
}
  
