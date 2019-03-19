#' @name centroidCoreCover
#' @title Compute centroid of the core cover
#' @description Calculates the centroid of the core cover for specified game.
#' @aliases centroidCoreCover
#' @export centroidCoreCover
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 45--46
#' @template param/v
#' @return Centroid of the core cover for a quasi-balanced game specified by a game vector
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,3,5)
#' centroidCoreCover(v) 
#' 
centroidCoreCover<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_centroidCoreCover(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isQuasiBalancedGame(A)){
    print("Game is not quasi balanced therefore no centroid of the core cover can be retrieved")
  }else{
    setVertices=coreCoverVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @name drawCentroidCoreCover
#' @title draw centroid of core cover for 3 or 4 players
#' @description drawCentroidCoreCover draws the centroid of the core cover for 3 or 4 players.
#' @aliases drawCentroidCoreCover
#' @export drawCentroidCoreCover
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 45--46
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawCentroidCoreCover(v,colour="black")
#' 
drawCentroidCoreCover<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of core cover"){
  A=v
  pcn=centroidCoreCover(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_centroidCoreCover=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
