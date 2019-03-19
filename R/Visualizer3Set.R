#' @title Visualizing convex polyhedra (for games with 3 players)
#' @description Draws a convex polyhedron for games with 3 players
#' @aliases Visualizer3Set
#' @importFrom graphics plot plot.new polygon
#' @param  A contains a game vector
#' @param  pointsToDraw contains the points to draw
#' @param  colour draws the polyhedron with this colour
#' @param  label activates the label for the figur
#' @param  name set a name for the label
#' @template author/FM
#' @noRd

Visualizer3Set <- function(A, pointsToDraw, colour , label, name ){ 
  
  #vertices of isosceles triangle in R2
  X=rbind(c(0,0),c(1,0),c(0.5, sqrt(0.75)))
  vertices = bary2cart(X,pointsToDraw)
  vertices = t(vertices)
  
  x = vertices[1,]
  y = vertices[2,]
  

  p= graphics::polygon(x, y, density=-1, col=colour)
  
  
  #Label
  if(label == TRUE)
  {
    setLabels(A, pointsToDraw, name)
  }
  
}