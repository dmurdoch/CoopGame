#' @name nevisonIndex
#' @title Compute Nevison index
#' @description Calculates the Nevison index for a specified simple TU game.
#' Note that in general the Nevison index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Nevison index is provided.
#' @aliases nevisonIndex
#' @export nevisonIndex
#' @template author/JS
#' @template cites/NEVISON_1979
#' @templateVar NEVISON_1979_P pp. 39--57
#' @template param/v
#' @return Nevison index for a specified simple game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' nevisonIndex(v) 
#'
nevisonIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_nevisonIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Nevison Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    temp=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    nevison =sapply(c(1:n),function(i){temp[i]/2^(n-1)})
    retVal = nevison
  }
  return(retVal)
}

initialParamCheck_nevisonIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}