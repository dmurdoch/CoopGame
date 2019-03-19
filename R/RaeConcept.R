#' @name raeIndex
#' @title Compute Rae index
#' @description raeIndex calculates the Rae index for a specified simple TU game. 
#' Note that in general the Rae index is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the Rae index is provided.
#' @aliases raeIndex
#' @export raeIndex
#' @template author/JS
#' @template cites/RAE_1969
#' @templateVar RAE_1969_P pp. 40--56
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 119--120
#' @template param/v
#' @return Rae index for specified simple game 
#' @examples
#' library(CoopGame) 
#' v=c(0,0,0,1,1,0,1)
#' raeIndex(v) 
#' 
#' \donttest{
#' library(CoopGame)
#' v=c(0,0,0,0,1,1,0,0,0,0,1,1,1,0,1)
#' raeIndex(v)
#' #result: [1] 0.875 0.625 0.625 0.500
#' }
#' 
raeIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_raeIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retval = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Rae Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    rae = rep(0,n)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    totalNumberOfWinningCoalitions = nrow(wcs)
    temp=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    for (j in 1:n)
    {
      rae[j] = 0.5 + (1/(2^n))*(2*temp[j]-totalNumberOfWinningCoalitions)
    }
    retVal = rae
  }
  return(retVal)
}

initialParamCheck_raeIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}