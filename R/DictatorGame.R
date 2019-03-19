#' @name dictatorGameValue
#' @title Compute value of a coalition for a dictator game
#' @description \strong{Coalition value for a dictator game:} \cr
#' For further information see \link{dictatorGame} 
#' @aliases dictatorGameValue
#' @export dictatorGameValue
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/S
#' @template param/dictator
#' @return \code{1} if dictator is involved in coalition, \code{0} otherwise.
#' @examples 
#' library(CoopGame)
#' dictatorGameValue(S=c(1,2,3),dictator=2)
#' 
dictatorGameValue<-function(S,dictator){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  stopOnInvalidDictator(paramCheckResult,dictator)
  logicdictatorGameValue(S,dictator)
}


logicdictatorGameValue=function(S,dictator){
  retVal=unanimityGameValue(S,T=dictator)
  return (retVal)
}

#' @name dictatorGameVector
#' @title Compute game vector for a dictator game
#' @description \strong{Game vector for a dictator game:} \cr
#' For further information see \link{dictatorGame} 
#' @aliases dictatorGameVector
#' @export dictatorGameVector
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template param/n
#' @template param/dictator
#' @return  Game vector where each element contains \code{1} if dictator is involved, \code{0} otherwise.
#' @examples 
#' library(CoopGame)
#' dictatorGameVector(n=3,dictator=2)
#' 
dictatorGameVector<-function(n,dictator){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = dictatorGameValue(S=currCoal,dictator=dictator)
    i<-i+1
  }
  return(A)
}


#' @title Construct a dictator game
#' @description \strong{Create a list containing 
#' all information about a specified dictator game:} \cr
#' Any coalitions including the dictator receive coalition 
#' value \code{1}. All the other coalitions, i.e. each and 
#' every coalition not containing the dictator, receives 
#' coalition value \code{0}. \cr
#' Note that dictator games are always simple games.
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 764
#' @template author/JA
#' @template author/JS
#' @name dictatorGame
#' @template param/n
#' @template param/dictator
#' @return A list with three elements 
#' representing the dictator game (n, dictator, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{dictatorGameValue}, \link{dictatorGameVector}
#' @examples
#' library(CoopGame) 
#' dictatorGame(n=3,dictator=2)
#' 
#' \donttest{
#' library(CoopGame) 
#' dictatorGame(n=4,dictator=2)
#' #Output:
#' #$n
#' #[1] 4
#' 
#' #$dictator
#' #[1] 2
#' 
#' #$v
#' #[1] 0 1 0 0 1 0 0 1 1 0 1 1 0 1 1
#' }
#' 
dictatorGame<-function(n,dictator){
  v = dictatorGameVector(n=n,dictator=dictator)
  retdictatorGame=list(n=n,dictator=dictator, v=v)
  return(retdictatorGame)
}