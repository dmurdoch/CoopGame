#' @title Set labels for points
#' @description Set labels for the points of a convex polyhedron
#' @aliases setLabels
#' @importFrom graphics text
#' @template param/A
#' @param  pointsToDraw contains the points to draw
#' @param  name set a name for the label
#' @template author/FM
#' @template author/JS
#' @noRd

setLabels <- function(A, pointsToDraw, name = NULL){
  
  pointsToDraw = round(pointsToDraw, digits = 3)
  numberOfPlayers = ncol(pointsToDraw)
  rows = nrow(pointsToDraw)
  
  if(numberOfPlayers == 3)
  {
    X=rbind(c(-.01,-.05),c(1.01,-.05),c(0.5, sqrt(0.85)))
    vertices = bary2cart(X,pointsToDraw)
    
    for(i in 1:rows)
    {
      #Set the content of the label
      label = paste(sep="","(",pointsToDraw[i,1],",",pointsToDraw[i,2],",",pointsToDraw[i,3],")")
      
      #Set the position of the label
      labelpositionX = vertices[i,1]
      labelpositionY = vertices[i,2]
      
      #If it is only 1 point, then move the label a bit
      if(rows == 1)
      {
        #Try to position labels nicely!
        labelpositionY = labelpositionY + (A[7] - A[1] - A[2] - A[3])/13
        if(!is.null(name))
        {
          label = name
        }
      }
      
      graphics::text(labelpositionX, labelpositionY, label)
    }
    
  }
  else if (numberOfPlayers == 4)
  { 
    if (requireNamespace("rgl", quietly = TRUE)) {
      X=rbind(c(-.05, -.05, -.05),  
              c(1.25, 0, 0),
              c(0.5, 0.6 * sqrt(3), 0),
              c(0.5, 1/8 * sqrt(3), 1/3 *sqrt(6)))
      
      vertices = bary2cart(X,pointsToDraw/A[15])
      
      for(i in 1:rows)
      {
        #Set the content of the label
        label = paste(sep="","(",pointsToDraw[i,1],",",pointsToDraw[i,2],",",pointsToDraw[i,3],",",pointsToDraw[i,4],")")
        
        #Set the position of the label
        labelpositionX = vertices[i,1]
        labelpositionY = vertices[i,2]
        labelpositionZ = vertices[i,3]
        
        if(rows == 1)
        {
          if(!is.null(name))
          {
            label = name
          }
        }
        
        rgl::text3d(labelpositionX, labelpositionY, labelpositionZ, label, col = "black")
      }
    } else
    { print("Please install the package 'rgl' in order to generate plots visualizing 4-player TU games")}
  }
  
}
  