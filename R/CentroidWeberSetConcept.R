#' @name centroidWeberSet
#' @title Compute centroid of Weber set
#' @description Calculates the centroid of the Weber set for specified game.
#' @aliases centroidWeberSet
#' @export centroidWeberSet
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 327--329
#' @template param/v
#' @return Calculates the centroid of the Weber set for a game specified by a game vector.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,2,2,3,5)
#' centroidWeberSet(v) 
#' 
centroidWeberSet<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_centroidWeberSet(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  setVertices=webersetVertices(A)
  
  if(!isEssentialGame(A) || (nrow(setVertices) == 0) ){
    print("Weber set is empty and so no centroid of the Weber set can be retrieved")
  }else{
    setVertices=webersetVertices(A)
    centroid=colSums(setVertices)/nrow(setVertices)
    retVal = centroid
  }
  return(retVal)
}


#' @name drawCentroidWeberSet
#' @title draw centroid of Weber set for 3 or 4 players
#' @description drawCentroidWeberset draws the centroid of the Weber set for 3 or 4 players.
#' @aliases drawCentroidWeberSet
#' @export drawCentroidWeberSet
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 327--329
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawCentroidWeberSet(v,colour="blue")
drawCentroidWeberSet<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "centroid of Weber set"){
  A=v
  pcn=centroidWeberSet(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_centroidWeberSet=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}