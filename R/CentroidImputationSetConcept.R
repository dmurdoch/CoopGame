#' @name centroidImputationSet
#' @title Compute centroid of the imputation set
#' @description Calculates the centroid of the imputation set for specified game.
#' @aliases centroidImputationSet
#' @export centroidImputationSet
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/v
#' @return Calculates the centroid of the imputation set for a game specified by a game vector.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,3,5)
#' centroidImputationSet(v) 
#' 
centroidImputationSet<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_centroidImputationSet(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isEssentialGame(A)){
    print("Game is not essential therefore no centroid of the imputation set can be retrieved")
  }else{
    setVertices=imputationsetVertices(A)
    centroid=colSums(setVertices)/n
    retVal = centroid
  }
  return(retVal)
}


#' @name drawCentroidImputationSet
#' @title draw centroid of imputation set for 3 or 4 players
#' @description drawCentroidImputationSet draws the centroid of the imputation set for 3 or 4 players.
#' @aliases drawCentroidImputationSet
#' @export drawCentroidImputationSet
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawCentroidImputationSet(v,colour="green")
drawCentroidImputationSet<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of imputation set"){
  A=v
  pcn=centroidImputationSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_centroidImputationSet=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}