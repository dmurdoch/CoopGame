#' @name divideTheDollarGameValue
#' @title Compute value of a coalition for a divide-the-dollar game
#' @description \strong{Coalition value for a divide-the-dollar game:} \cr
#' For further information see \link{divideTheDollarGame} 
#' @aliases divideTheDollarGameValue
#' @export divideTheDollarGameValue
#' @template author/MM
#' @template author/JS
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 
#' @template param/S
#' @template param/n
#' @return value of coalition
#' @examples
#' library(CoopGame)
#' S <- c(1,2)
#' divideTheDollarGameValue(S, n = 3)
#' 
divideTheDollarGameValue<-function(S,n){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=n)
  stopOnInvalidNumberOfPlayers(paramCheckResult,n=n)
  logicdivideTheDollarGameValue(S,n)
}

#' @name divideTheDollarGameVector
#' @title Compute game vector for a divide-the-dollar game
#' @description \strong{Game vector for a divide-the-dollar game:} \cr
#' For further information see \link{divideTheDollarGame} 
#' @aliases divideTheDollarGameVector
#' @export divideTheDollarGameVector
#' @template author/JS
#' @template author/JA
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 
#' @template param/n
#' @return Game vector for the specified divide-the-dollar game 
#' @examples
#' library(CoopGame) 
#' divideTheDollarGameVector(n=3)
#' 
#' \donttest{
#' library(CoopGame)
#' (v <- divideTheDollarGameVector(n=4))
#' #Output: 
#' # [1] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
#' }
#' 
divideTheDollarGameVector<-function(n){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = divideTheDollarGameValue(S=currCoal,n=n)
    i<-i+1
  }
  return(A) 
}


logicdivideTheDollarGameValue <- function(S, n) {
  result <- 0
  if (length(S) >= n / 2) {
    result <- 1
  }
  return(result)
}


#' @title Construct a divide-the-dollar game
#' @description  \strong{Create a list containing 
#' all information about a specified divide-the-dollar game:} \cr
#' Returns a divide-the-dollar game with \code{n} players: \cr
#' This sample game is taken from the book 'Social and Economic Networks' by Matthew O. Jackson (see p. 413 ff.). 
#' If coalition \code{S} has at least \code{n/2} members it generates a value of \code{1}, otherwise \code{0}. \cr
#' Note that divide-the-dollar games are always simple games.
#' @template author/JS
#' @template author/JA
#' @template cites/JACKSON_2008
#' @templateVar JACKSON_2008_P p. 413 
#' @template param/n
#' @return A list with two elements 
#' representing the divide-the-dollar game (n, Game vector v)
#' @name divideTheDollarGame
#' @export
#' @section Related Functions: 
#' \link{divideTheDollarGameValue}, \link{divideTheDollarGameVector}
#' @examples 
#' library(CoopGame)
#' divideTheDollarGame(n=3)
#' 
#' \donttest{
#' #Example with four players
#' library(CoopGame)
#' (vv<-divideTheDollarGame(n=4))
#' #$n
#' #[1] 4
#' 
#' #$v
#' #[1] 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1
#' }
#' 
divideTheDollarGame<-function(n){
  v = divideTheDollarGameVector(n=n)
  retdivideTheDollarGame=list(n=n,v=v)  
  return(retdivideTheDollarGame)
}


