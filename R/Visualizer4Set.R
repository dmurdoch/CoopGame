#' @title Visualizing convex polyhedra (for games with 4 players)
#' @description Draws a convex polyhedron for games with 4 players
#' @aliases Visualizer4Set
#' @param  A contains a game vector
#' @param  pointsToDraw contains the points to draw
#' @param  colour draws the polyhedron with this colour
#' @param  label activates the label for the figure
#' @param  name sets a name for the label
#' @template author/FM
#' @noRd

Visualizer4Set <- function(A, pointsToDraw, colour, label, name){ 
  #Coordinates for the tetrahedron
  X <- rbind(c(0, 0, 0),  
             c(1, 0, 0),
             c(0.5, 0.5 * sqrt(3), 0),
             c(0.5, 1/6 * sqrt(3), 1/3 *sqrt(6)))
  
  #Always draw the imputation set
  grandCoalition=A[length(A)]
  beta = pointsToDraw/grandCoalition
  
  #Changes the barycentric coordinates in Cartesian coordinates
  vertices <- bary2cart(X, beta)
  #Get the convex hull
  ConvHull <- convhulln(vertices)
  ts.surf <- t(ConvHull)
  #Draws with the function of rgl the set
  rgl.triangles(vertices[ts.surf, 1], vertices[ts.surf, 2], vertices[ts.surf, 3], col = colour, alpha=0.8, smooth=FALSE, lit=FALSE)
  
  #Label
  if(label == TRUE)
  {
    setLabels(A, pointsToDraw, name)
  }
  
}