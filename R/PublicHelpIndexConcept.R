#' @name publicHelpIndex
#' @title Compute Public Help index Theta
#' @description Calculates the Public Help index Theta for a specified simple TU game.
#' Note that the Public Help index Theta goes back to the paper by Bertini, 
#' Gambarelli and Stach (2008) and is frequently simply referred to referred 
#' to Public Help index in the literature. 
#' @aliases publicHelpIndex publicHelpThetaIndex publicHelpIndexTheta
#' @export publicHelpIndex
#' @template author/JA
#' @template author/JS
#' @template cites/BERTINI_ET_AL_2008
#' @templateVar BERTINI_ET_AL_2008_P pp. 83--98
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template cites/STACH_2016
#' @templateVar STACH_2016_P pp. 99--110
#' @template param/v
#' @return Public Help index Theta for specified simple game 
#' @examples 
#' library(CoopGame)
#' publicHelpIndex(v=c(0,0,0,0,1,0,1))
#' 
#' \donttest{
#' #Example from paper by Stach (2016), p. 105:
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' publicHelpIndex(v) 
#' #result: 0.4285714 0.2857143 0.2857143
#' 
#' #Second example from paper by Stach (2016), p. 105:
#' library(CoopGame)
#' v=c(0,0,0,0,1,1,0,0,0,0,1,1,1,0,1)
#' publicHelpIndex(v)
#' #result: 0.3529412 0.2352941 0.2352941 0.1764706
#' }
#' 
publicHelpIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicHelpIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Public Help index Theta can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the winning coalitions
    wcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    tempVar=sapply(c(1:n),function(i){sum(wcs[wcs[,i]==1,"cVal"])})
    sumPlayerValues=sum(tempVar)
    phi=sapply(c(1:n),function(i){tempVar[i]/sumPlayerValues})
    retVal = phi
  }
  return(retVal)
}


#' @name drawPublicHelpIndex
#' @title Draw Public Help index Theta for 3 or 4 players
#' @description drawPublicHelpIndex draws the Public Help index Theta for a simple game with 3 or 4 players.
#' @aliases drawPublicHelpIndex
#' @export drawPublicHelpIndex
#' @template author/JA
#' @template author/JS
#' @template cites/BERTINI_ET_AL_2008
#' @templateVar BERTINI_ET_AL_2008_P pp. 83--98
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
#' drawPublicHelpIndex(v) 
drawPublicHelpIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Public Help Index"){
  A=v
  sm=publicHelpIndex(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicHelpIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

