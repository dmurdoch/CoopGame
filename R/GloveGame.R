#' @name gloveGameValue
#' @title Compute value of a coalition for a glove game
#' @description \strong{Coalition value for a specified glove game: }\cr
#' For further information see \link{gloveGame}
#' @aliases gloveGameValue
#' @export gloveGameValue
#' @template author/AT
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 155--156
#' @template param/S
#' @template param/L
#' @template param/R
#' @return Number of matched pairs of gloves for given coalition \code{S}
#' @examples
#' library(CoopGame)
#' gloveGameValue(S=c(1,2), L=c(1,2), R=c(3)) 
#' 
gloveGameValue<-function(S,L,R){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=length(union(L,R)))
  stopOnInvalidLeftRightGloveGame(paramCheckResult,L=L,R=R,N=union(L,R))
  logicgloveGameValue(S,L,R)
}

#' @name gloveGameVector
#' @title Compute game vector for glove game
#' @description \strong{Game vector for glove game: }\cr
#' For further information see \link{gloveGame}
#' @aliases gloveGameVector
#' @export gloveGameVector
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 155--156 
#' @template param/n
#' @template param/L
#' @template param/R
#' @return Game vector of the specified glove game 
#' @examples
#' library(CoopGame)
#' gloveGameVector(3, L=c(1,2), R=c(3))
#'
gloveGameVector<-function(n,L,R){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = gloveGameValue(S=currCoal,L=L,R=R)
    i<-i+1
  }
  return(A)  
}


logicgloveGameValue<-function(S, L, R){
  retVal <-0

  #initialize grand coalition N
  numberOfPlayers <-length(L)+length(R)
  N<-as.numeric(1:numberOfPlayers)
  
  compareVector<-c()
  #left side
  compareVector[1]<-length(intersect(S, L))
  #right side
  compareVector[2]<-length(intersect(S, R))
  #return min value of intersected sets' length
  retVal<-min(compareVector, na.rm = TRUE)

  return (retVal)
}


#' @title Construct a glove game
#' @description \strong{Create a list containing 
#' all information about a specified glove game:} \cr
#' We have a set of players \code{L} with left-hand gloves and 
#' a set of players \code{R} with right-hand gloves.
#' The worth of a coalition \code{S} equals the number of 
#' pairs of gloves the members of \code{S} can make. 
#' Note that the sets \code{L} and \code{R} have to be disjoint.
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 155--156
#' @template param/n
#' @template param/L
#' @template param/R
#' @name gloveGame
#' @return A list with four elements 
#' representing the glove game (n, L, R, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{gloveGameValue}, \link{gloveGameVector}
#' @examples
#' library(CoopGame)
#' gloveGame(n=3,L=c(1,2), R=c(3))
#' 
#' \donttest{
#' #Example with four players: 
#' #players 1, 2 and 4 hold a left-hand glove each, 
#' #player 3 holds a right-hand glove. 
#' library(CoopGame)
#' (vv<-gloveGame(n=4,L=c(1,2,4), R=c(3)))
#' #$n
#' #[1] 3
#' 
#' #$L
#' #[1] 1 2 4
#' #
#' #$R
#' #[1] 3
#' #
#' #$v
#' #[1] 0 0 0 0 0 1 0 1 0 1 1 0 1 1 1
#' }
#' 
gloveGame<-function(n,L,R){
  v = gloveGameVector(n=n, L=L, R=R)
  retgloveGame=list(n=n,L=L, R=R, v=v)  
  return(retgloveGame)
}


