#' @name centroidReasonableSet
#' @title Compute centroid of reasonable set
#' @description Calculates the centroid of the reasonable set for specified game.
#' @aliases centroidReasonableSet
#' @export centroidReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 43--44
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/v
#' @return Calculates the centroid of the reasonable set for a game specified by a game vector.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,3,5)
#' centroidReasonableSet(v) 
#' 
centroidReasonableSet<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_centroidReasonableSet(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  setVertices=reasonableSetVertices(A)
  
  if(!isEssentialGame(A) || (nrow(setVertices) == 0) ){
    print("Reasonable set is empty and so no centroid of the reasonable set can be retrieved")
  }else{
    setVertices=reasonableSetVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}

#' @name drawCentroidReasonableSet
#' @title draw centroid of reasonable set for 3 or 4 players
#' @description drawCentroidReasonableSet draws the centroid of the reasonable set for 3 or 4 players.
#' @aliases drawCentroidReasonableSet
#' @export drawCentroidReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 43--44
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawCentroidReasonableSet(v,colour="green")
drawCentroidReasonableSet<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of reasonable set"){
  A=v
  pcn=centroidReasonableSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_centroidReasonableSet=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
