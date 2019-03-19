#' @name isMonotonicGame
#' @title Check if game is monotonic
#' @description Checks if a TU game with n players is monotonic. \cr
#' For a monotonic game a coalition \code{S} can never obtain 
#' a larger value than another coalition \code{T} if \code{S}
#' is contained in \code{T}.
#' @export isMonotonicGame
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 12
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 408 
#' @template param/v
#' @return \code{TRUE} if the game is monotonic, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' isMonotonicGame(c(0,0,0,1,0,1,1))
#' 
#' \donttest{
#' #Example of a non-monotonic game
#' library(CoopGame)
#' v1=c(4,2,5,2,3,6,10)
#' isMonotonicGame(v1)
#' 
#' #Example of a monotonic game
#' library(CoopGame)
#' v2=c(2,5,7,10, 9, 13,20)
#' isMonotonicGame(v2)
#' }
#'
isMonotonicGame<-function(v){
   paramCheckResult=getEmptyParamCheckResult()
   initialParamCheck_isMonotonicGame(paramCheckResult = paramCheckResult, v)
   A = v
   boolRetVal=TRUE
   n=getNumberOfPlayers(A)
   N=length(A)
   bm=as.data.frame(createBitMatrix(n,A))
   players=1:n
   for(i in N:indexLower(n,2)){
     involvedPlayers=getPlayersFromBMRow(bm[i,])
     uninvolvedPlayers=players[-involvedPlayers]
     corrCVals=getCorrespondingCVals(bm[1:(i-1),],uninvolvedPlayers)
     allSmallerOrEqual=all(corrCVals<=bm[i,"cVal"])
     if(!allSmallerOrEqual){
       boolRetVal=FALSE
       break
     }
   }
   return(boolRetVal)
}

#Identifies all coalition values where (by uninvolvedPlayers) specified players
#are not participating
getCorrespondingCVals<-function(bmDataFrame,uninvolvedPlayers){
  exp=paste0("(",paste(c("0",colnames(bmDataFrame)[c(uninvolvedPlayers,FALSE)],"0"),collapse = "|"),")")
  entries=subset(bmDataFrame[,],!(eval(parse(text=exp))==1),"cVal")
  return(entries)
}

initialParamCheck_isMonotonicGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
