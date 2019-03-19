#' @name visualize
#' @title Visualize points or convex polyhedra
#' @description Draws a convex polyhedron of the given points or draws a point
#' @aliases visualize
# @export visualize
#' @noRd
#' @template param/A
#' @param  pointsToDraw contains the points to draw
#' @param  holdOn draws in existing plot
#' @param  colour draws the polyhedron (or point) with this colour, all colour names can be seen with "colors()"
#' @param  label activates the labels for the figure
#' @param  name set a name for the label
#' @template author/FM
visualize <- function(A, pointsToDraw, holdOn=FALSE, colour = NA , label=FALSE, name = NULL){
  if(is.vector(pointsToDraw)){
    n=getNumberOfPlayers(A)
    if(length(pointsToDraw)==n){
      pointsToDraw=matrix(pointsToDraw,nrow=1,ncol=n)
    }else{
      print("Invalid points to draw.")
      return(NULL);
    }
  }
  
  
  if(is.null(pointsToDraw) || nrow(pointsToDraw)==0){
    print("The solution you would like to draw does not exist.")
    return(NULL);
  }else{
    tryCatch({logicVisualizer(A, pointsToDraw, holdOn, colour, label, name)},
    error = function(cond){
      logicVisualizer(A, pointsToDraw, holdOn=holdOn, colour , label, name )
    })
  }
}

logicVisualizer <- function(A, pointsToDraw, holdOn, colour, label, name){ 
  
  #tryCatch({Visualizer(...,holdOn = TRUE)},
  #error = function(cond){
  #  Visualizer(...,holdOn = FALSE)
  #})
  numberOfPlayers = ncol(pointsToDraw)
  
  #Controls, if the game vector and the points have the same number of players
  if(numberOfPlayers == getNumberOfPlayers(A))
  {
    if(nrow(imputationsetVertices(A)) >= 3)
    {
    rows = nrow(pointsToDraw)
    
    #if there are 0 points, it can't be draw
    if(rows != 0)
    {
      #there are 3 players
      if(numberOfPlayers == 3)
      {
        
        if(holdOn == FALSE){
          #vertices of imputationset always will be labelled whenever 
          #subsets of the imputationset are drawn
          imputationsetDraw(A, label=TRUE)
        }
        
        #Checks the number of points and decide the kind of drawing
        if(rows == 1)
        {
          Visualizer3Point(A = A, pointsToDraw = pointsToDraw, colour = colour, label = label, name = name)
        }
        else if(rows >= numberOfPlayers)
        {
          Visualizer3Set(A = A, pointsToDraw = pointsToDraw, colour = colour, label = label, name = name)
        }
        else
        {
          print("There are too few points for a 2D-draw!")
        }
          
      }
      
      #there are 4 players
      else if(numberOfPlayers == 4)
      {
        if(holdOn==FALSE){
          #Closes all rgl windows
          while(rgl.cur()>0){
            rgl.close();
          }
          imputationsetDraw(A, label=TRUE)
        }
        
        #Checks the number of points and decide the kind of drawing
        if(rows == 1)
        {
          Visualizer4Point(A = A, pointsToDraw = pointsToDraw, colour = colour, label = label, name = name)
        }
        else if(rows >= numberOfPlayers)
        {
          Visualizer4Set(A = A, pointsToDraw = pointsToDraw, colour = colour, label = label, name = name)
        }
        else
        {
          print("There are too few points for a 3D-draw!")
        }
      }
      else
      {
        print("The program can only draw if there are 3 or 4 players - that means 3 or 4 columns")
      }
    }
    else
    {
      print("There are no points to draw!")
    }
    }
    else
    {
      print("The imputation set as frame of the drawing has too few points")
    }
  }
  else
  {
    print("The game vector does not fit with the number of players of the points of the convex hull!")
  }
}