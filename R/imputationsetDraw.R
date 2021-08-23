#' @title imputationsetDraw for 3 or 4 Players
#' @name imputationsetDraw
#' @noRd
#' @description Draws the imputation of a TU game.
#' @aliases imputationsetDraw
#' @import geometry
#' @import rcdd
#' @importFrom graphics plot plot.new polygon
# @export imputationsetDraw
#' @template author/KT
#' @template author/FT
#' @template author/AM
#' @template author/FM
#' @template author/JS
#' @param A  is a numeric vector of dimension 1x7 (3 players) or 1x15 (4 players)
#' @param label  logical; is a parameter for set a label for the edge points
#' @examples
#' v=c(0,0,0,40,50,20,100) 
#' v=c(0,0,0,6,5,5,10) 
#' v=c(0,0,0,0,7,7,7,7,7,7,12,12,12,12,22)
#' 
#' imputationsetDraw(v,label=TRUE)
#' 
imputationsetDraw <- function(A, label=TRUE){ 
  #Checks, if the game vector is correct
  emptyParamCheckResult=getEmptyParamCheckResult();
  initialParamCheck_imputationsetDraw(paramCheckResult = emptyParamCheckResult,A)
  
  
  numberOfPlayers = getNumberOfPlayers(A)
  if(numberOfPlayers != 0)
  {
    # Checks if imputation set is not empty
    if(isEssentialGame(A)== TRUE)
    {
      #Calculates the matrices and vectors for the function makeH of rcdd
      matrixA1 = diag(rep(-1,numberOfPlayers))
      matrixA2 = matrix(rep(-1,numberOfPlayers),nrow = 1)
      vectorB1 = -A[1:numberOfPlayers]
      vectorB2 = -A[length(A)]
      
      #Utilize the R-Package rcdd
      hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
      vRepresentation = scdd(hRepresentation)
      
      #Transform the V-Representation into a matrix
      VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
      OutcomeMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)   
      
      if(nrow(OutcomeMatrix) >= 3)
      {
      #Drawing for 4 players
      if(numberOfPlayers == 4)
      {
        #Coordinates for the tetrahedron
        X <- rbind(c(0, 0, 0),  
                   c(1, 0, 0),
                   c(0.5, 0.5 * sqrt(3), 0),
                   c(0.5, 1/6 * sqrt(3), 1/3 *sqrt(6)))
        
        #Draws the imputations set
        
        grandCoalition=A[length(A)]
        beta = OutcomeMatrix/grandCoalition
        
        #Changes the barycentric coordinates in Cartesian coordinates
        vertices <- bary2cart(X, beta)
        #Get the convex hull
        ConvHull <- convhulln(vertices)
        ts.surf <- t(ConvHull)
        #Draw the imputation set using the function from rgl
        if (requireNamespace("rgl", quietly = TRUE)) {
          rgl::rgl.bg(color = "white")
          rgl::rgl.linestrips(vertices[ts.surf, 1], vertices[ts.surf, 2], vertices[ts.surf, 3], col = "black", alpha=0.8, smooth=FALSE, lit=FALSE)
        } else
        { print("Please install the package 'rgl' in order to generate plots visualizing 4-player TU games")}

        
        #If label=TRUE, a label is added
        if(label){
          setLabels(A, OutcomeMatrix, name = "imputationset")
        }
      }
      
      #Drawing for 3 players
      else
      {
          #Corner points of isosceles triangle in R2
          X=rbind(c(0,0),c(1,0),c(0.5, sqrt(0.75)))
          vertices = bary2cart(X,OutcomeMatrix)
          vertices = t(vertices)
          
          x = vertices[1,]
          y = vertices[2,]
          

            #Build a greater draw area
            xMark = c(x[1],x[1])
            yMark = c(y[1],y[1])
            
            for(i in 1:2)
            {
              if(x[i+1] < xMark[1])
              {
                xMark[1] = x[i+1]
              }
              if(x[i+1] > xMark[2])
              {
                xMark[2] = x[i+1]
              }
              
              if(y[i+1] < yMark[1])
              {
                yMark[1] = y[i+1]
              }
              if(y[i+1] > yMark[2])
              {
                yMark[2] = y[i+1]
              }
            }
            edgeX = c((xMark[1] - (xMark[2] - xMark[1])/ 10) , (xMark[2] + (xMark[2] - xMark[1])/ 10))
            edgeY = c((yMark[1] - (yMark[2] - yMark[1])/ 10) , (yMark[2] + (yMark[2] - yMark[1])/ 10))
            
            #Gives a new window to draw
            graphics::plot.new()
            graphics::plot(x, y,type="n", axes = FALSE, ann = FALSE, xlim = edgeX , ylim = edgeY)
          
          #Draws the polyhedron
          p= graphics::polygon(x, y, density=-1)
          
          #If label=TRUE, a label is added
          if(label){
            
            setLabels(A, OutcomeMatrix, name = "imputationset")
          }
      }
      }
      else
      {
        print("The imputation set has too few points")
      }
    }
    else
    {
      print("The imputation set is empty")
    }
  }
  else
  {
    print("Invalid number of Players")
  }
}

initialParamCheck_imputationsetDraw<-function(paramCheckResult, A){
  stopOnInvalidGameVector(paramCheckResult,A)
}