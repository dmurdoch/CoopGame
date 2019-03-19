#' @name coreVertices
#' @title Compute core vertices
#' @description Calculates the core vertices for given game vector
#' @aliases coreVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export coreVertices
#' @template author/FM
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--49
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 686--747
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 257--275
#' @template param/v
#' @return rows of the matrix are the vertices of the core
#' @examples
#' library(CoopGame)
#' coreVertices(c(0,0,0,1,1,1,3))
#' 
#' \donttest{
#' #In the following case the core consists of a single point
#' v1 = c(0,1,2,3,4,5,6)
#' coreVertices(v1)
#' #     [,1] [,2] [,3]
#' #[1,]    1    2    3
#' 
#' #Users may also want to try the following commands:
#' coreVertices(c(0,0,0,60,80,100,135))
#' coreVertices(c(5,2,4,7,15,15,15,15,15,15,20,20,20,20,35))
#' coreVertices(c(0,0,0,0,0,5,5,5,5,5,5,5,5,5,5,15,15,15,15,15,15,15,15,15,15,30,30,30,30,30,60))
#' }
#' 
coreVertices<-function(v){
  emptyParamCheckResult=getEmptyParamCheckResult();
  initialParamCheck_corevertices(paramCheckResult = emptyParamCheckResult,v)
  A = v
  numberOfPlayers = getNumberOfPlayers(A)
  retVal=NULL
    if(isEssentialGame(A) == TRUE){
      vectorA1 = c()
      
      #Calculate the matrixA1 for the function makeH
      vectorRechnung = c(rep(0,(numberOfPlayers - 1)),c(1))
      vectorA1 = c(1, rep(0,(numberOfPlayers - 1)))
      b = TRUE

      
      #Build the matrices and vectors for the function makeH
      # matrixA1b = -t(matrix(vectorA1,numberOfPlayers,(length(A) - 1)))
      matrixA1 = -createBitMatrix(n=numberOfPlayers)[-length(A),-(numberOfPlayers+1)]#-t(matrix(vectorA1,numberOfPlayers,(length(A) - 1)))
      matrixA2 = -matrix(rep(1,numberOfPlayers),1,numberOfPlayers)
      vectorB1 = -A[1:(length(A)-1)]
      vectorB2 = -A[length(A)]
      
      # Utilize the R-Package rcdd
      hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
      vRepresentation = scdd(hRepresentation)
      if (nrow(vRepresentation$output) >= 2)
      {
        vRepresentation = redundant(vRepresentation$output, representation = "V")
      }
      
      #Transform the V-Representation into a matrix
      VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)   
      
      if(numberOfPlayers == 3)
      {
      sequencevector = grDevices::chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop = FALSE]
      }
      else
      {
        OutcomeMatrix = ResultMatrix
      }
      
      retVal=OutcomeMatrix
    }
    else
    {
      # print("The core is empty")
      retVal=matrix(ncol = numberOfPlayers,nrow = 0)
    }
    return(retVal)
}


#' @name belongsToCore
#' @title Check if point is core element
#' @description belongsToCore checks if a given point is in the core
#' @export belongsToCore
#' @template author/FM
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--49
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 686--747
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 257--275
#' @template param/v
#' @template param/x
#' @return \code{TRUE} for a point belonging to the core and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' v = c(0,1,2,3,4,5,6)
#' belongsToCore(c(1,2,3),v)

belongsToCore<-function(x,v){
  A=v
  cv=coreVertices(A)
  return(isElementOfConvexSet(cv,x=x))
}


#' @name drawCore
#' @title Draw core for 3 or 4 players
#' @description drawCore draws the core for 3 or 4 players.
#' @aliases drawCore
#' @export drawCore
#' @template author/JA
#' @template author/JS
#' @template cites/GILLIES_1953
#' @template cites/AUMANN_1961
#' @templateVar AUMANN_1961_P pp. 539--552
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--49
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 686--747
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 257--275
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' drawCore(v)
#' 
drawCore<-function(v,holdOn=FALSE, colour = "red" , label=FALSE, name = "Core"){
  A=v
  co=coreVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_corevertices<-function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult,v)
}