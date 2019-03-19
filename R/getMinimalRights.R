#' @name getMinimalRights
#' @title Compute minimal rights vector
#' @description Calculates the minimal rights vector.
#' @aliases getMinimalRights getMinimalRightsVector
#' @export getMinimalRights
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P pp. 20--21
#' @template param/v
#' @return Vector of minimal rights of each player
#' @examples
#' library(CoopGame)
#' getMinimalRights(c(0,0,0,1,0,1,1))
#' 
#' \donttest{
#' library(CoopGame)
#' v1 <- c(0,0,0,60,60,60,72)
#' getMinimalRights(v1)
#' #[1] 48 48 48
#'
#' library(CoopGame)
#' v2 <- c(2,4,5,18,14,9,24) 
#' getMinimalRights(v2)
#' #[1] 8 4 5
#' }
#' 
getMinimalRights<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_minimalRights(paramCheckResult = paramCheckResult, v)
  A = v
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=createBitMatrix(n,A)
  M=getUtopiaPayoff(A)
  
  m=sapply(1:n, function(i){
    bmIndices=which(bm[,i]==1,1)
    max(
      apply(
        bm[bmIndices,,drop=FALSE],
        1,
        function(bmRow){
          jPlayers=getPlayersFromBMRow(bmRow)
          jPlayers=jPlayers[jPlayers!=i]
          bmRow["cVal"]-sum(M[jPlayers])
        }
      )
    )
  })
  return(m)
}

initialParamCheck_minimalRights=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}