#' @name colemanInitiativePowerIndex
#' @title Compute Coleman Initiative Power index
#' @description Calculates the Coleman Initiative Power index for a specified simple TU game.
#' Note that in general the Coleman Initiative Power index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Coleman Initiative Power index is provided.
#' @aliases colemanInitiativePowerIndex
#' @export colemanInitiativePowerIndex
#' @template author/JS
#' @template cites/COLEMAN_1971
#' @templateVar COLEMAN_1971_P pp. 269--300
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 120--123
#' @template cites/DE_KEIJZER_2008
#' @templateVar DE_KEIJZER_2008_P p. 18
#' @template cites/BERTINI_STACH_SAGE_COLEMAN_2011
#' @templateVar BERTINI_STACH_SAGE_COLEMAN_2011_P p. 117--119
#' @template param/v
#' @return Coleman Initiative Power index for specified simple game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' colemanInitiativePowerIndex(v) 
#' 
colemanInitiativePowerIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_colemanInitiativePowerIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Coleman Initiative Power Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    numerator = numeric(n)
    numerator = rawBanzhafIndex(A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    commonDenominator = as.numeric(2^n - nrow(wcs))
    colemanInitiativePower =sapply(c(1:n),function(i){numerator[i]/commonDenominator})
    retVal=colemanInitiativePower
  }
  return(retVal)
}

initialParamCheck_colemanInitiativePowerIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
