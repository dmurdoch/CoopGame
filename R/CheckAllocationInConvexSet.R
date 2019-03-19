#' @title Check if point is element of given convex set
#' @description isElementOfConvexSet checks if a point is included 
#' in a given convex set (using the package rcdd in order to do so)
#' @name isElementOfConvexSet
#' @noRd
#' @template author/FM
#' @template author/JA
#' @template author/NC
#' @template author/JS
#' @template param/VRepMatrix
#' @template param/x
#' @return If the point is included in a convex set return \code{TRUE}, else return \code{FALSE}
# @export isElementOfConvexSet
isElementOfConvexSet <- function(VRepMatrix,x){
  pointRepMatrix = VRepMatrix
  # get number of rows of the above matrix ...
  numberOfPlayers=length(x)
  boolRetVal=FALSE
  numberOfVertices = nrow(pointRepMatrix)
  
  if(numberOfVertices!=0){
  
    if(numberOfPlayers == ncol(pointRepMatrix))
    {
      pointRepMatrix = rbind(pointRepMatrix,x)
      
      vRepresentation = makeV(points = pointRepMatrix)
      
      if(nrow(vRepresentation)>=2){
        vRepresentation = redundant(vRepresentation)
        newpos = vRepresentation$new.position
        redundancyIndices = vRepresentation$redundant
        if (length(redundancyIndices)==0)
        {
          boolRetVal = FALSE
        }
        else if (length(redundancyIndices)>=2)
        {
          boolRetVal = FALSE
        }
        else if (length(redundancyIndices)==1)
        {
          if (redundancyIndices[1]==numberOfVertices+1)
          {
            boolRetVal = TRUE
          }
          else
          {
            equality = all.equal(pointRepMatrix[redundancyIndices[1],], pointRepMatrix[numberOfVertices+1,])
            if(equality[1] == TRUE)
            {
              boolRetVal=TRUE
            }
            else
            {
              boolRetVal = FALSE
            }
          }
        }
      }
    }
  }else{
    print("The set that was specified is empty.")
  }
  return(boolRetVal)
}


