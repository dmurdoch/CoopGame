#' @name publicHelpChiValue
#' @title Compute (normalized) Public Help value Chi
#' @description Calculates the (normalized) Public Help value Chi 
#' by Bertini & Stach (2015) for a nonnegative TU game.
#' Note that the greek letter Xi (instead of Chi) was used in the 
#' original paper by Bertini and Stach (2015). 
#' @aliases publicHelpChiValue publicHelpValueChi publicHelpXiValue publicHelpValueXi
#' @export publicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Public Help value Chi for specified nonnegative TU game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,2,2,0,2)
#' publicHelpChiValue(v) 
publicHelpChiValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicHelpChiValue(paramCheckResult = paramCheckResult, v)
  A = v   
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) Public Help value chi.")
  }
  else
  {
    n = as.numeric(getNumberOfPlayers(A))
    N = length(A)
    phvchics=logicPublicHelpChiValue(A)
    phvres = numeric(n)
    phvres[1:n]=(phvchics[1:n]/phvchics[n+1])*A[N]
    retVal = phvres
  }
  return(retVal)
}

#' @name absolutePublicHelpChiValue
#' @title Compute absolute Public Help value Chi 
#' @description Calculates the absolute Public Help value Chi for a specified nonnegative TU game.
#' Note that in general the absolute Public Help value Chi is not an 
#' efficient vector, i.e. the sum of its entries is not always 1. Hence no 
#' drawing routine for the absolute Public Help value Chi is provided.
#' Note that the greek letter Xi (instead of Chi) was used in the 
#' original paper by Bertini and Stach (2015). 
#' @aliases absolutePublicHelpChiValue absolutePublicHelpValueChi absolutePublicHelpXiValue absolutePublicHelpValueXi
#' @export absolutePublicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Absolute Public Help value Chi for specified nonnegative game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,2,2,0,2)
#' absolutePublicHelpChiValue(v) 
#' 
absolutePublicHelpChiValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicHelpChiValue(paramCheckResult = paramCheckResult, v)
  A = v    
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any public help chi value.")
  }
  else
  {
    phvcs=logicPublicHelpChiValue(A)
    n = as.numeric(getNumberOfPlayers(A))
    retVal = phvcs[1:n]
  }
  return(retVal)
}



logicPublicHelpChiValue=function(A){
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any Public Help Value.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    retVal = numeric(n+1)
    #the gaining coalitions
    gcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    tempVar1 <- numeric(n)
    tempVar2 <- 0
    sum <- 0
    for (i in 1:nrow(gcs))
    {
      playersInCoalition = getPlayersFromBMRow(bmRow=gcs[i,])
      noPlayersInCoalition = length(playersInCoalition)
      value = gcs[[i,"cVal"]]
      for (j in 1:noPlayersInCoalition)
      {
        tempVar1[playersInCoalition[j]] = tempVar1[playersInCoalition[j]] + value/(noPlayersInCoalition^2)
      }
      tempVar2 = tempVar2 + value/noPlayersInCoalition
    }
    retVal[1:n]=tempVar1
    retVal[n+1]=tempVar2
  }
  return(retVal)
}


#' @name drawPublicHelpChiValue
#' @title Draw Public Help value Chi for 3 or 4 players
#' @description drawPublicHelpChiValue draws the (normalized) Public Help value Chi for 3 or 4 players.
#' @aliases drawPublicHelpChiValue
#' @export drawPublicHelpChiValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,0,3)
#' drawPublicHelpChiValue(v) 
drawPublicHelpChiValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Help Value Chi"){
  A=v
  sm=publicHelpChiValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicHelpChiValue=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}