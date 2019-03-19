#' @name colemanCollectivityPowerIndex
#' @title Compute Coleman Power index of a Collectivity to Act
#' @description Calculates the Coleman Power index of a 
#' Collectivity to Act 
#' for a specified simple TU game.
#' Note that in general the Coleman Power index of a 
#' Collectivity to Act is not an efficient vector, 
#' i.e. the sum of its entries is not always 1. 
#' Note also that the the Coleman Power index of a 
#' Collectivity to Act is identical for each player, i.e. the 
#' result for each player is the number of winning coalitions divided by 2^n.
#' Hence no drawing routine for the Coleman Power index 
#' of a Collectivity to Act is provided.
#' @aliases colemanCollectivityPowerIndex
#' @export colemanCollectivityPowerIndex
#' @template author/JS
#' @template cites/COLEMAN_1971
#' @templateVar COLEMAN_1971_P pp. 269--300
#' @template cites/DE_KEIJZER_2008
#' @templateVar DE_KEIJZER_2008_P p. 18
#' @template cites/BERTINI_STACH_SAGE_COLEMAN_2011
#' @templateVar BERTINI_STACH_SAGE_COLEMAN_2011_P p. 117--119
#' @template param/v
#' @return Coleman Power index of a Collectivity to Act for specified simple game 
#' @examples
#' library(CoopGame) 
#' v=c(0,0,0,1,1,0,1)
#' colemanCollectivityPowerIndex(v) 
#' 
colemanCollectivityPowerIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_colemanCollectivityPowerIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Coleman Collectivity Power Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    colemanCollectivityPower =sapply(c(1:n),function(i){as.numeric(nrow(wcs))/(2^n)})
    retVal=colemanCollectivityPower
  }
  return(retVal)
}

initialParamCheck_colemanCollectivityPowerIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
