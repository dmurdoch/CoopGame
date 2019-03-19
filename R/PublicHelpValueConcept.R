#' @name publicHelpValue
#' @title Compute Public Help value Theta
#' @description publicHelpValue calculates the (normalized) Public Help value Theta for a 
#' specified nonnegative TU game. Our function implements the formula from Definition 5.7, p. 20, 
#' in the paper by Bertini and Stach from 2015.
#' @aliases publicHelpValue publicHelpThetaValue publicHelpValueTheta
#' @export publicHelpValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Public Help value Theta for specified nonnegative TU game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,0.7,11,0,15)
#' publicHelpValue(v) 
#' 
publicHelpValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicGoodValue(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) public help value.")
  }
  else
  {
    phvcs=absolutePublicHelpValue(A)
    phvcsol =phvcs/sum(phvcs) * A[length(A)]
    retVal = phvcsol 
  }
  return(retVal)
}

#' @name absolutePublicHelpValue
#' @title Compute absolute Public Help value Theta
#' @description absolutePublicHelpValue calculates the absolute Public Help 
#' value Theta for a specified nonnegative TU game.
#' Note that in general the absolute Public Help value Theta is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' Hence no drawing routine for the absolute Public Help Value is provided.
#' @aliases absolutePublicHelpValue absolutePublicHelpThetaValue absolutePublicHelpValueTheta
#' @export absolutePublicHelpValue
#' @template author/JS
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Absolute Public Help value Theta for specified simple game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,0.7,11,0,15)
#' absolutePublicHelpValue(v) 
#' 
absolutePublicHelpValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicGoodValue(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any public help value.")
  }
  else
  {    
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the gaining coalitions
    gcs=bm[bm[,"cVal"]>0,,drop=FALSE]
    phv=sapply(c(1:n),function(i){sum(gcs[gcs[,i]==1,"cVal"])})
    retVal = phv
  }
  return(retVal)
}


#' @name drawPublicHelpValue
#' @title Draw Public Help value Theta for 3 or 4 players
#' @description drawPublicHelpValue draws the (normalized) Public Help value Theta for 3 or 4 players.
#' @aliases drawPublicHelpValue
#' @export drawPublicHelpValue
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
#' v=c(0,0,0,1,1,0,1)
#' drawPublicHelpValue(v) 
drawPublicHelpValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Help Value"){
  A=v
  sm=publicHelpValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicHelpValue=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
