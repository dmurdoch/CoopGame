#' @name publicGoodValue
#' @title Compute (normalized) Public Good value
#' @description Calculates the (normalized) Public Good value for a specified nonnegative TU game.
#' Note that the normalized Public Good value is sometimes also referred to as Holler value 
#' in the literature. Our function implements the formula from Definition 5.4, p. 19, in the paper 
#' by Bertini and Stach from 2015.
#' @aliases publicGoodValue hollerValue
#' @export publicGoodValue
#' @template author/JS
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257--270
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Public Good value for specified nonnegative TU game 
#' @examples 
#' library(CoopGame)
#' v=c(0,0,0,0.7,11,0,15)
#' publicGoodValue(v)
#' 
publicGoodValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicGoodValue(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Therefore we do not compute any (normalized) public Good value.")
  }
  else
  {
    pgvcs=absolutePublicGoodValue(A)
    pgvcsol =pgvcs/sum(pgvcs) * A[length(A)]
    retVal = pgvcsol
  }
  return(retVal)
}

#' @name absolutePublicGoodValue
#' @title Compute absolute Public Good value
#' @description absolutePublicGoodValue calculates the absolute 
#' Public Good value for a specified nonnegative TU game. 
#' Note that in general the absolute Public Good value is not an efficient vector, 
#' i.e. the sum of its entries is not always 1.
#' @aliases absolutePublicGoodValue absoluteHollerValue
#' @export absolutePublicGoodValue 
#' @template author/JS
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257--270
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9--25
#' @template param/v
#' @return Absolute Public Good value for specified nonnegative game 
#' @examples 
#' library(CoopGame)
#' v <- c(1,2,3,4,0,0,0)
#' absolutePublicGoodValue(v)
#' 
#' \donttest{
#' v=c(0,0,0,0.7,11,0,15)
#' absolutePublicGoodValue(v) 
#' #[1] 26.7 15.7 26.0
#' }
#' 
absolutePublicGoodValue<-function(v)
{
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_publicGoodValue(paramCheckResult = paramCheckResult, v)
  A = v  
  retVal = NULL
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. We do not compute any Public Good value in this case.")
  }
  else
  {
    n=getNumberOfPlayers(A)
    bm=createBitMatrix(n,A)
    #the real gaining coalitions
    rgcs= getRealGainingCoalitions(A)
    pgv=sapply(c(1:n),function(i){sum(rgcs[rgcs[,i]>0,"cVal"])})
    retVal = pgv
  }
  return(retVal)
}

#' @name drawPublicGoodValue
#' @title Draw Public Good value for 3 or 4 players
#' @description drawPublicGoodValue draws the (normalized) Public Good value for 3 or 4 players.
#' @aliases drawPublicGoodValue
#' @export drawPublicGoodValue
#' @template author/JS
#' @template cites/HOLLER_ET_LI_1995
#' @templateVar HOLLER_ET_LI_1995_P pp. 257 -- 270
#' @template cites/BERTINI_ET_STACH_2015
#' @templateVar BERTINI_ET_STACH_2015_P pp. 9 -- 25
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawPublicGoodValue(v) 
drawPublicGoodValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Public Good Value"){
  A=v
  sm=publicGoodValue(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_publicGoodValue=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}