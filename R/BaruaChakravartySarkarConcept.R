#' @name baruaChakravartySarkarIndex
#' @title Compute Barua Chakravarty Sarkar index
#' @description Calculates the Barua Chakravarty Sarkar index for a specified simple TU game.
#' Note that in general the Barua Chakravarty Sarkar index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Barua Chakravarty Sarkar index is provided.
#' @aliases baruaChakravartySarkarIndex
#' @export baruaChakravartySarkarIndex
#' @template author/JS
#' @template cites/BARUA_CHAKRAVARTY_ET_SARKAR_2012
#' @templateVar BARUA_CHAKRAVARTY_ET_SARKAR_2012_P pp. 81--91
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 120--123
#' @template param/v
#' @return Barua Chakravarty Sarkar index for specified simple game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' baruaChakravartySarkarIndex(v) 
#' 
baruaChakravartySarkarIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_baruaChakravartySarkarIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Barua Chakravarty Sarkar index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    numerator = numeric(n)
    numerator = rawBanzhafIndex(A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    denominator = numeric(n)
    denominator = sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    baruaChakravartySarkar =sapply(c(1:n),function(i){numerator[i]/denominator[i]})
    retVal=baruaChakravartySarkar
  }
  return(retVal)
}

initialParamCheck_baruaChakravartySarkarIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
