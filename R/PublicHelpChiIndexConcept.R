#' @name publicHelpChiIndex
#' @title Compute Public Help index Chi
#' @description Calculates the Public Help index Chi for a specified simple TU game.
#' Note that the greek letter Xi (instead of Chi) was used in the 
#' original paper by Bertini and Stach (2015). 
#' @aliases publicHelpChiIndex publicHelpIndexChi publicHelpXiIndex publicHelpIndexXi
#' @export publicHelpChiIndex
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99--110
#' @template param/v
#' @return Public Help index Chi for specified simple game 
#' @examples 
#' library(CoopGame)
#' publicHelpChiIndex(v=c(0,0,0,0,1,0,1))
#' 
#' \donttest{
#' #Example from original paper by Stach (2016), p. 105:
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' publicHelpChiIndex(v) 
#' #result: 0.4583333 0.2708333 0.2708333
#' 
#' #Second example from original paper by Stach (2016), p. 105:
#' library(CoopGame)
#' v=c(0,0,0,0,1,1,0,0,0,0,1,1,1,0,1)
#' publicHelpChiIndex(v)
#' #result: 0.3981481 0.2376543 0.2376543 0.1265432
#' }
#' 
publicHelpChiIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicHelpChiIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  n=getNumberOfPlayers(A)
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Help index Chi can be retrieved.")
  }
  else
  {
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    # Loop over all winning coalitions
    factor1 <- 0
    tempVariable <- numeric(n)
    for (i in 1:nrow(wcs))
    {
      playersInCoalition = getPlayersFromBMRow(bmRow=wcs[i,])
      noPlayersInCoalition = length(playersInCoalition)
      factor1 = factor1 + 1/noPlayersInCoalition
      for (j in 1:noPlayersInCoalition)
      {
        tempVariable[playersInCoalition[j]] = tempVariable[playersInCoalition[j]] + 1/(noPlayersInCoalition^2)
      }
    }
    phiChi=(1/factor1)*tempVariable
    retVal = phiChi
  }
  return(retVal)
}


#' @name drawPublicHelpChiIndex
#' @title Draw Public Help index Chi for 3 or 4 players
#' @description drawPublicHelpChiIndex draws the Public Help index Chi for a simple game with 3 or 4 players.
#' @aliases drawPublicHelpChiIndex
#' @export drawPublicHelpChiIndex
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99--110
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawPublicHelpChiIndex(v) 
drawPublicHelpChiIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Help Chi Index"){
  A=v
  sm=publicHelpChiIndex(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicHelpChiIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}