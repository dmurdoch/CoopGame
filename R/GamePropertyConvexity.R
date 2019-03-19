#' @name isConvexGame
#' @title Check if game is convex
#' @description isConvexGame checks if a TU game is convex.
#' A TU game is convex if and only if each player's marginal 
#' contribution to any coalition is monotone nondecreasing 
#' with respect to set-theoretic inclusion.
#' @aliases isConvexGame
#' @export isConvexGame
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 329
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 717--718
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 260--261
#' @template param/v
#' @return \code{TRUE} if the game is convex, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' isConvexGame(c(0,0,0,1,1,1,5))
#' 
#' \donttest{
#' #Example of a convex game with three players
#' library(CoopGame) 
#' v=c(0,0,0,1,2,1,4)
#' isConvexGame(v)
#'
#' #Example of a nonconvex game
#' library(CoopGame) 
#' v=c(1:7)
#' isConvexGame(v)
#' }
#' 
isConvexGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isConvexGame(paramCheckResult = paramCheckResult, v)
  A = v
  result=list(isConvexGame=TRUE, setsCVal=NULL, sets=NULL)
  n=getNumberOfPlayers(A)
  bm=createBitMatrix(n)


  for(i in 1:length(A)){
    S=getPlayersFromBMRow(bm[i,])
    for(j in (1):length(A)){
      T=getPlayersFromBMRow(bm[j,])
      vS=A[indexCoalition(n,S)]
      vT=A[indexCoalition(n,T)]
      vSplusvT=vS+vT
      vSunionT=A[indexCoalition(n,union(S,T))]
      SintersectT=intersect(S,T)
      if(length(SintersectT)!=0){
        vSintersectT=A[indexCoalition(n, SintersectT)]
      }else{
        vSintersectT=0
      }

      if(!((vSunionT+vSintersectT)>=vSplusvT)){
        SunionT=union(S,T)
        result$setsCVal=c(vSunionT=vSunionT,vSintersectT=vSintersectT,vS=vS,vT=vT)
        result$sets=list(S=S,T=T,SunionT=SunionT,SintersectT=SintersectT)
        result$isConvexGame=FALSE
        return(result$isConvexGame)
      }
    }
  }
  return(result$isConvexGame)
}

initialParamCheck_isConvexGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
