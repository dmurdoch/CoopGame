#' @name koenigBraeuningerIndex
#' @title Compute Koenig-Braeuninger index
#' @description Calculates the Koenig-Braeuninger index for a specified simple TU game.
#' Note that in general the Koenig-Braeuninger index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1. 
#' Hence no drawing routine for the Koenig-Braeuninger index is provided.
#' @aliases koenigBraeuningerIndex
#' @export koenigBraeuningerIndex
#' @template author/JS
#' @template cites/KOENIG_ET_BRAEUNINGER_1998
#' @templateVar KOENIG_ET_BRAEUNINGER_1998_P pp. 125--142
#' @template cites/NEVISON_ET_AL_1978
#' @templateVar NEVISON_ET_AL_1978_P pp. 130--131
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Koenig-Braeuninger index for specified simple game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' koenigBraeuningerIndex(v) 
#' 
koenigBraeuningerIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_koenigBraeuningerIndex(paramCheckResult = paramCheckResult, v)
  A = v 
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Koenig Braeuninger Index can be retrieved")
    return(retVal)
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    temp=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    koenigBraeuninger =sapply(c(1:n),function(i){temp[i]/nrow(wcs)})
    retVal = koenigBraeuninger
    return(retVal)
  }
}

initialParamCheck_koenigBraeuningerIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}