#' @title Visualizing points (for games with 3 players)
#' @description Draws a point for games with 3 players
#' @aliases Visualizer3Point
#' @importFrom graphics plot
#' @param  A contains a game vector
#' @param  pointsToDraw contains the points to draw
#' @param  colour draws the polyeder with this colour
#' @param  label activates the label for the figur
#' @param  name set a name for the label
#' @template author/FM
#' @noRd

Visualizer3Point <- function(A, pointsToDraw, colour, label, name){ 
  if(is.na(colour)){
    colour="black"
  }
  
  
  #vertices for imputationset
  X=rbind(c(0,0),c(1,0),c(0.5, sqrt(0.75)))
  vertices = bary2cart(X,pointsToDraw)
  
  #Drawing the point
  #vertices of imputationset always will be labelled whenever 
  #point solution concept is drawn

  graphics::points(vertices[1],vertices[2],pch=4,col=colour)
  
  #Label
  if(label == TRUE)
  {
    setLabels(A, pointsToDraw, name)
  }
}