#' @name getGainingCoalitions
#' @title Compute gaining coalitions of a TU game
#' @description The function getGainingCoalitions identifies all 
#' gaining coalitions.
#' Coalition \code{S} is a gaining coalition if there holds: v(S) > 0
#' @export getGainingCoalitions
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return A data frame containing all gaining coalitions.
#' @examples
#' library(CoopGame)
#' getGainingCoalitions(v=c(0,0,0,2,0,2,3))
#' 
#' \donttest{
#' library(CoopGame)
#' v <- c(1,2,3,4,0,0,11)
#' getGainingCoalitions(v)
#'# Output:
#'#    V1 V2 V3 cVal
#'# 1  1  0  0    1
#'# 2  0  1  0    2
#'# 3  0  0  1    3
#'# 4  1  1  0    4
#'# 7  1  1  1   11
#'}
#'
getGainingCoalitions<-function(v){
   paramCheckResult=getEmptyParamCheckResult()
   initialParamCheck_gainingCoalitions(paramCheckResult = paramCheckResult, v)
   A = v
   n=as.numeric(getNumberOfPlayers(A))
   N=length(A)
   bm=as.data.frame(createBitMatrix(n,A))
   gcs=bm[bm[,"cVal"]>0,,drop=FALSE]
   return(gcs)
}

initialParamCheck_gainingCoalitions=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

