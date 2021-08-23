#' @title Compute value of a coalition for an apex game
#' @name apexGameValue
#' @description \strong{Coalition value for an apex game:} \cr
#' For further information see \link{apexGame} 
#' @aliases apexGameValue
#' @export apexGameValue
#' @template author/AT
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 164--165
#' @template param/S
#' @template param/n
#' @template param/apexPlayer
#' @return value of coalition S
#' @examples
#' library(CoopGame)
#' apexGameValue(c(1,2),3,2)
#' 
#' \donttest{
#' library(CoopGame)
#' apexGameValue(c(1,2,3,4),4,3)
#' # Output:
#' # [1] 1
#' }
#' 
apexGameValue<-function(S,n,apexPlayer){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=n)
  stopOnInvalidNumberOfPlayers(paramCheckResult,n)
  stopOnInvalidNumber(paramCheckResult,apexPlayer)
  logicapexGameValue(S,n,apexPlayer)
}

#' @title Compute game vector for an apex game
#' @name apexGameVector
#' @description \strong{Game vector for an apex game:} \cr
#' For further information see \link{apexGame} 
#' @aliases apexGameVector
#' @export apexGameVector
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 164--165
#' @template param/n
#' @template param/apexPlayer
#' @return Game vector for the apex game
#' @examples 
#' library(CoopGame)
#' apexGameVector(n=3,apexPlayer=2)
#' 
#' \donttest{
#' library(CoopGame)
#' (v <- apexGameVector(n=4,apexPlayer=3))
#' #[1] 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1
#' }
#'
apexGameVector<-function(n,apexPlayer){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = apexGameValue(S=currCoal,n=n,apexPlayer=apexPlayer)
    i<-i+1
  }
  return(A)
}


logicapexGameValue<-function(S,n,apexPlayer){
  #sort to be secure
  S<-sort(S)
  #initialize payoff for S
  retVal<-0
  #check if apexPlayer is element of S and |S| > 1
  if((apexPlayer %in% S) && (length(S) > 1)){
    return (1)
  }
  #initialize the grand coalition N
  N<-c(1:n)   #proposal by JA
  #check if S = N\{apexPlayer}
  setWithoutApex<-N[-(which(N == apexPlayer))]
  if(identical(as.numeric(S), as.numeric(setWithoutApex))){
    return (1)
  }
  return(retVal)
}


#' @title Construct an apex game
#' @description \strong{Create a list containing 
#' all information about a specified apex game:} \cr
#' A coalition can only win (and hence obtain the value \code{1}) 
#' if it \cr
#' a) contains both the apex player and one additional player \cr
#' or \cr
#' b) contains all players except for the apex player. \cr
#' Any non-winning coalitions obtain the value \code{0}. \cr
#' Note that apex games are always simple games.
#' @template author/JS
#' @template author/JA
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 164--165
#' @template param/n
#' @template param/apexPlayer
#' @return A list with three elements 
#' representing the apex game (n, apexPlayer, Game vector v)
#' @name apexGame
#' @export
#' @section Related Functions: 
#' \link{apexGameValue}, \link{apexGameVector}
#' @examples 
#' library(CoopGame)
#' apexGameVector(n=3,apexPlayer=2)
#' 
#' \donttest{
#' library(CoopGame)
#' #Example with four players, apex player is number 3
#' (vv<-apexGame(n=4,apexPlayer=3))
#' #$n
#' #[1] 4
#' 
#' #$apexPlayer
#' #[1] 4
#' 
#' #$v
#' # [1] 0 0 0 0 0 1 0 1 0 1 1 1 1 1 1
#' }
#' 
apexGame<-function(n,apexPlayer){
  v = apexGameVector(n=n,apexPlayer=apexPlayer)
  retapexGame=list(n=n,apexPlayer=apexPlayer,v=v)  
  return(retapexGame)
}


