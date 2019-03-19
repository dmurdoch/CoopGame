#' @name getWinningCoalitions
#' @title Compute winning coalitions in a simple game
#' @description The function getWinningCoalitions identifies 
#' all winning coalitions of a specified simple game.
#' @aliases getWinningCoalitions
#' @export getWinningCoalitions
#' @template author/JS
#' @template cites/BERTINI_ET_AL_2008
#' @templateVar BERTINI_ET_AL_2008_P pp. 83--98
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99--110
#' @template param/v
#' @return A data frame containing all winning coalitions for a simple game.
#' @examples
#' library(CoopGame)
#' getWinningCoalitions(v=c(0,0,0,1,0,1,1))
#' 
#' \donttest{
#' library(CoopGame)
#' v=weightedVotingGameVector(n=3,w=c(1,2,3),q=5)
#' getWinningCoalitions(v)
#'# Output:
#'#   V1 V2 V3 cVal
#'# 6  0  1  1    1
#'# 7  1  1  1    1
#'# => the coalition containing player 2 and 3 and 
#'#    the grand coalition are winning coalitions
#'}
#'
getWinningCoalitions=function(v){

  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVector(paramCheckResult,v)
  A = v
  wcs = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple therefore no winning coalitions can be retrieved.")
  }else{
    n=getNumberOfPlayers(A)
    bm=as.data.frame(createBitMatrix(n,A))
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    }
  return(wcs)
}