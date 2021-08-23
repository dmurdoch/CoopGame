#' @name reasonableSetVertices
#' @title Compute vertices of reasonable set
#' @description Calculates the vertices of the reasonable set for given game vector.
#' @aliases reasonableSetVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export reasonableSetVertices
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 43--44
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/v
#' @return rows of the matrix are the vertices of the reasonable set
#' @examples
#' library(CoopGame)
#' reasonableSetVertices(c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' reasonableSetVertices(v)
#' #       [,1] [,2] [,3]
#' # [1,]    3    0    3
#' # [2,]    0    3    3
#' # [3,]    3    3    0
#' }
#' 
reasonableSetVertices<-function(v){
    emptyParamCheckResult=getEmptyParamCheckResult();
    initialParamCheck_reasonableSetVertices(paramCheckResult = emptyParamCheckResult,v)
    A = v
    numberOfPlayers = getNumberOfPlayers(A)
    if(isEssentialGame(A) == TRUE){
      m <- A[1:numberOfPlayers]
      M <- rep(0,numberOfPlayers)
      marginalContributions <- getMarginalContributions(A)$marginal_values
      for (i in 1:numberOfPlayers)
      {
        M[i] <- max(marginalContributions[,i])
      }
      matrixA1_help1 = diag(rep(-1,numberOfPlayers))
      matrixA1_help2 = diag(rep(1,numberOfPlayers))
      matrixA1 <- rbind(matrixA1_help1, matrixA1_help2)
      matrixA2 = matrix(rep(-1,numberOfPlayers),nrow = 1)
      vectorB1 <- c(-m,M)
      vectorB2 = -A[length(A)]
      
      # Utilize the R-Package rcdd
      hRepresentation = makeH(matrixA1,vectorB1,matrixA2,vectorB2)
      vRepresentation = scdd(hRepresentation)
      
      #Transform the V-Representation into a matrix
      VectorCounter = length(vRepresentation[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRepresentation[[1]][(VectorCounter * 2 + 1):(length(vRepresentation[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers)   
      
      if(numberOfPlayers == 3)
      {
        sequencevector = grDevices::chull(ResultMatrix)
        OutcomeMatrix = ResultMatrix[sequencevector,,drop=FALSE]
      }
      else
      {
        OutcomeMatrix = ResultMatrix
      }
    }
    else
    {
      print("The reasonable set is empty")
      OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
    }
  return(OutcomeMatrix)
}


#' @name belongsToReasonableSet
#' @title Check if point is element of reasonable set
#' @description  belongsToReasonableSet checks if the point is in the reasonable set
#' @aliases belongsToReasonableSet
#' @import geometry
#' @import rcdd
#' @export belongsToReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 43--44
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/x
#' @template param/v
#' @return \code{TRUE} if point belongs to reasonable set, \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' belongsToReasonableSet(x=c(1,0.5,0.5), v=c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' belongsToReasonableSet(x=c(2,2,2),v)
#' #[1] TRUE
#' belongsToReasonableSet(x=c(1,2,4),v)
#' #[1] FALSE
#' }

belongsToReasonableSet<-function(x,v){
  A=v
  ccv=reasonableSetVertices(A) 
  return(isElementOfConvexSet(ccv,x=x))
}

#' @name drawReasonableSet
#' @title Draw reasonable set for 3 or 4 players
#' @description drawReasonableSet draws the reasonable set for 3 or 4 players.
#' @aliases drawReasonableSet
#' @export drawReasonableSet
#' @template author/JS
#' @template cites/MILNOR_1953
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 43--44
#' @template cites/GERARD_VARET_ET_ZAMIR_1987
#' @templateVar GERARD_VARET_ET_ZAMIR_1987_P pp. 123--143
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' drawReasonableSet(v)
drawReasonableSet<-function(v,holdOn=FALSE, colour = NA , label=FALSE, name = "Reasonable Set"){
  A=v
  co=reasonableSetVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_reasonableSetVertices<-function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult,v)
}
