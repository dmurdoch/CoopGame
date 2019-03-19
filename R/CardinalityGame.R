#' @name cardinalityGameValue
#' @title Compute value of a coalition for a cardinality game
#' @description \strong{Coalition value for a cardinality game:} \cr
#' For further information see \link{cardinalityGame} 
#' @export cardinalityGameValue
#' @template author/JA
#' @template author/JS
#' @template param/S
#' @return The return value is the cardinality, i.e. the number of elements, of coalition S
#' @examples
#' library(CoopGame)
#' S=c(1,2,4,5)
#' cardinalityGameValue(S)

cardinalityGameValue<-function(S){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S)
  logiccardinalityGameValue(S)
}

#' @name cardinalityGameVector
#' @title Compute game vector for a cardinality game
#' @description \strong{Game vector for a cardinality game:} \cr
#' For further information see \link{cardinalityGame}
#' @aliases cardinalityGameVector
#' @export cardinalityGameVector
#' @template author/JA
#' @template author/JS
#' @template param/n
#' @return Game vector for the cardinality game
#' @examples 
#' library(CoopGame)
#' cardinalityGameVector(n=3)
#' 
#' \donttest{
#' library(CoopGame)
#' (v <- cardinalityGameVector(n=4))
#' #[1] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 4
#' }
#' 
cardinalityGameVector<-function(n){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = cardinalityGameValue(S=currCoal)
    i<-i+1
  }
  return(A)
}

logiccardinalityGameValue=function(S){
  return(length(S))
}


#' @title Construct a cardinality game
#' @description \strong{Create a list containing 
#' all information about a specified cardinality game:} \cr
#' For a cardinality game the worth of each coalition 
#' is simply the number of the members of the coalition.
#' @template author/JS
#' @template author/JA
#' @template param/n
#' @name cardinalityGame
#' @return A list with two elements 
#' representing the cardinality game (n, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{cardinalityGameValue}, \link{cardinalityGameVector}
#' @examples 
#' library(CoopGame)
#' cardinalityGame(n=3)
#' 
#' \donttest{
#' library(CoopGame)
#' #Example: Cardinality function for four players
#' (vv<-cardinalityGame(n=4))
#' #$n
#' #[1] 4
#' 
#' #$v
#' #[1] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 4
#' }
#'
cardinalityGame<-function(n){
  v = cardinalityGameVector(n=n)
  retcardinalityGame=list(n=n,v=v)  
  return(retcardinalityGame)
}

