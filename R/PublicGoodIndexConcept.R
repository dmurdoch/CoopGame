#' @name publicGoodIndex
#' @title Compute Public Good index
#' @description Calculates the Public Good index (aka Holler index) for a specified simple game.
#' @aliases publicGoodIndex hollerIndex
#' @export publicGoodIndex
#' @template author/JA
#' @template author/JS
#' @template cites/HOLLER_ET_PACKEL_1983
#' @templateVar HOLLER_ET_PACKEL_1983_P pp. 21--29
#' @template cites/HOLLER_1982
#' @templateVar HOLLER_1982_P pp. 262--271
#' @template cites/HOLLER_SAGE_PGI_2011
#' @templateVar HOLLER_SAGE_PGI_2011_P pp. 541--542
#' @template param/v
#' @return The return value is a vector containing the Public Good index
#' @examples
#' library(CoopGame)
#' publicGoodIndex(v=c(0,0,0,1,1,0,1))
#' 
#' \donttest{
#' #Example from Holler (2011) illustrating paradox of weighted voting
#' library(CoopGame)
#' v=weightedVotingGameVector(n=5,w=c(35,20,15,15,15), q=51)
#' publicGoodIndex(v) 
#' #[1] 0.2666667 0.1333333 0.2000000 0.2000000 0.2000000
#' }
#' 
publicGoodIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicGoodIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Good Index can be retrieved.")
  }
  else 
  {
    n=getNumberOfPlayers(A)
    mwcs=getMinimumWinningCoalitions(A)
   
    
    tempVar=sapply(c(1:n),function(i){sum(mwcs[mwcs[,i]==1,"cVal"])})
    sumPlayerValues=sum(tempVar)
    pgi=sapply(c(1:n),function(i){tempVar[i]/sumPlayerValues})
    retVal = pgi
  }
  return(retVal)
}


#' @name drawPublicGoodIndex
#' @title Draw Public Good index for 3 or 4 players
#' @description drawPublicGoodIndex draws the Public Good Index of a simple game for 3 or 4 players.
#' @aliases drawPublicGoodIndex
#' @export drawPublicGoodIndex
#' @template author/JA
#' @template author/JS
#' @template cites/HOLLER_ET_PACKEL_1983
#' @templateVar HOLLER_ET_PACKEL_1983_P pp. 21--29
#'  @template cites/HOLLER_1982
#' @templateVar HOLLER_1982_P pp. 262--271
#' @template cites/HOLLER_SAGE_PGI_2011
#' @templateVar HOLLER_SAGE_PGI_2011_P pp. 541--542
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawPublicGoodIndex(v)
drawPublicGoodIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Good Index"){
  A=v
  pgv=publicGoodIndex(A);
  visualize(A, pointsToDraw=pgv, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicGoodIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}