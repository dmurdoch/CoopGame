#' @name bankruptcyGameValue
#' @title Compute value of a coalition for a bankruptcy game
#' @description \strong{Coalition value for a specified bankruptcy game:} \cr
#' For further information see \link{bankruptcyGame}
#' @export bankruptcyGameValue
#' @template author/JS
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template cites/GURA_ET_MASCHLER_2008
#' @templateVar GURA_ET_MASCHLER_2008_P pp. 166--204
#' @template param/S
#' @template param/d
#' @template param/E
#' @return A positive value if the sum of the claims outside of coalition \code{S} is less than \code{E}
#' else \code{0}
#' @examples
#' library(CoopGame)
#' bankruptcyGameValue(S=c(2,3),d=c(1,2,3),E=4)
#' 
#' \donttest{
#' #Estate division problem from Babylonian Talmud 
#' #from paper by Aumann (2002) with E=300
#' library(CoopGame)
#' bankruptcyGameValue(S=c(2,3),d=c(100,200,300),E=300)
#' #Output
#' #[1] 200
#' }
#' 

bankruptcyGameValue<-function(S,d,E){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=length(d))
  stopOnInvalidClaimsVector(paramCheckResult,n=length(d),d)
  logicbankruptcyGameValue(S,d,E)
}


logicbankruptcyGameValue=function(S, d, E) {

  result = 0
  sumOtherClaims <- 0
  players <- 1:length(d)
  uninvolvedPlayers=players[-S]

  for(p in uninvolvedPlayers){
    sumOtherClaims = sumOtherClaims + d[p]
  }

  if (sumOtherClaims >= E) {
    result = 0
  } else {
    result = E - sumOtherClaims
  }
  return(result)
}


#' @name bankruptcyGameVector
#' @title Compute game vector for a bankruptcy game
#' @description \strong{Game vector for a specified bankruptcy game:} \cr
#' For further information see \link{bankruptcyGame}
#' @export bankruptcyGameVector
#' @template author/JS
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template cites/GURA_ET_MASCHLER_2008
#' @templateVar GURA_ET_MASCHLER_2008_P pp. 166--204
#' @template param/n
#' @template param/d
#' @template param/E
#' @return Game Vector where each element contains a positive value if the sum of the claims outside of coalition 'S' is less than \code{E}
#' else \code{0}
#' @examples
#' library(CoopGame)
#' bankruptcyGameVector(n=3, d=c(1,2,3), E=4)
#' 
#' \donttest{
#' #Estate division problem from Babylonian Talmud
#' #from paper by Aumann (2002) with E=300
#' library(CoopGame)
#' bankruptcyGameVector(n=3,d=c(100,200,300),E=300)
#' #Output
#' #[1] 0   0   0   0 100 200 300
#' }
#' 
bankruptcyGameVector<-function(n,d,E){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = bankruptcyGameValue(S=currCoal,d=d,E=E)
    i<-i+1
  }
  return(A)
}



#' @title Construct a bankruptcy game
#' @description \strong{Create a list containing 
#' all information about a specified bankruptcy game:}\cr
#' The list contains the number of players, 
#' the claims vector, the estate and the bankruptcy game vector. 
#' Bankruptcy games are defined by a vector of debts \code{d}
#' of \code{n} creditors (players) and an estate \code{E} less 
#' than the sum of the debt vector. The roots of bankruptcy 
#' games can be traced back to the Babylonian Talmud.
#' @template author/JS
#' @name bankruptcyGame
#' @template cites/ONEILL_1982
#' @templateVar ONEILL_1982_P pp. 345 -- 371
#' @template cites/AUMANN_ET_MASCHLER_1985
#' @templateVar AUMANN_ET_MASCHLER_1985_P pp. 195 -- 213
#' @template cites/AUMANN_2002
#' @template cites/GURA_ET_MASCHLER_2008
#' @templateVar GURA_ET_MASCHLER_2008_P pp. 166--204
#' @template param/n
#' @template param/d
#' @template param/E
#' @return A list with four elements 
#' representing the specified bankruptcy 
#' game (n, d, E, Game vector v)
#' @export
#' @section Related Functions: 
#' \link{bankruptcyGameValue}, \link{bankruptcyGameVector}
#' @examples 
#' library(CoopGame)
#' bankruptcyGame(n=3, d=c(1,2,3), E=4)
#' 
#' \donttest{
#' #Estate division problem from Babylonian Talmud 
#' #from paper by Aumann (2002) with E=300
#' library(CoopGame)
#' bankruptcyGame(n=3,d=c(100,200,300),E=300)
#' #Output
#' #$n
#' #[1] 3
#' 
#' #$d
#' #[1] 100 200 300
#' 
#' #$E
#' #[1] 300
#' 
#' #$v
#' #[1]   0   0   0   0 100 200 300
#' }
#' 
bankruptcyGame<-function(n,d,E){
    v = bankruptcyGameVector(n=n,d=d,E=E)
    retbankruptcyGame=list(n=n,d=d,E=E,v=v)
    return(retbankruptcyGame)
}
  