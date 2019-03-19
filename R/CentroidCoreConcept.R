#' @name centroidCore
#' @title Compute centroid of core
#' @description Calculates the centroid of the core for specified game.
#' @aliases centroidCore
#' @export centroidCore
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--49
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 686--747
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 257--275 
#' @template param/v
#' @return Calculates the centroid of the core for a game specified by a game vector v.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,3,5)
#' centroidCore(v) 
#' 
centroidCore<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_centroidCore(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  retVal=NULL
  if(!isBalancedGame(A)){
    print("Game is not balanced therefore no centroid of the core can be retrieved")
  }else{
    setVertices=coreVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @name drawCentroidCore
#' @title draw centroid of the core for 3 or 4 players
#' @description drawCentroidCore draws the centroid of the core for 3 or 4 players.
#' @aliases drawCentroidCore
#' @export drawCentroidCore
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--49
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 686--747
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 257--275 
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawCentroidCore(v,colour="green")
#' 
drawCentroidCore<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of core"){
  A=v
  pcn=centroidCore(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_centroidCore=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}