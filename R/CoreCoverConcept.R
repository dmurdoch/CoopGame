#' @name coreCoverVertices
#' @title Compute core cover vertices
#' @description Calculates the core cover for a given game vector
#' @aliases coreCoverVertices
#' @import rcdd
#' @importFrom grDevices chull
#' @export coreCoverVertices
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 45--46
#' @template param/v
#' @return rows of the matrix are the vertices of the core cover
#' @examples
#' library(CoopGame)
#' coreCoverVertices(c(0,0,0,1,1,1,3))
#' 
#' \donttest{
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' coreCoverVertices(v)
#' #       [,1] [,2] [,3]
#' # [1,]    3    0    3
#' # [2,]    0    3    3
#' # [3,]    3    3    0
#' }
coreCoverVertices<-function(v){
    emptyParamCheckResult=getEmptyParamCheckResult();
    initialParamCheck_coreCoverVertices(paramCheckResult = emptyParamCheckResult,v)
    A = v
    numberOfPlayers = getNumberOfPlayers(A)
    if(isEssentialGame(A) == TRUE){
      M <- getUtopiaPayoff(A)
      m <- getMinimalRights(A)
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
      print("The core cover is empty")
      OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
    }
  return(OutcomeMatrix)
}

#' @name belongsToCoreCover
#' @title Check if point is core cover element
#' @description belongsToCoreCover checks if the point is in the core cover
#' @aliases belongsToCoreCover
#' @import geometry
#' @import rcdd
#' @export belongsToCoreCover
#' @template author/JA
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 45--46
#' @template param/x
#' @template param/v
#' @return \code{TRUE} if point belongs to core cover, \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' belongsToCoreCover(x=c(1,1,1), v=c(0,0,0,1,1,1,3))
#' 
#' \donttest{
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' belongsToCoreCover(x=c(2,2,2),v)
#' #[1] TRUE
#' belongsToCoreCover(x=c(1,2,4),v)
#' #[1] FALSE
#' }
#' 
belongsToCoreCover<-function(x,v){
  A = v
  ccv=coreCoverVertices(A) 
  return(isElementOfConvexSet(ccv,x))
}

#' @name drawCoreCover
#' @title Draw core cover for 3 or 4 players
#' @description drawCoreCover draws the core cover for 3 or 4 players.
#' @aliases drawCoreCover
#' @export drawCoreCover
#' @template author/JA
#' @template author/JS
#' @template cites/TIJS_LIPPERTS_1982
#' @templateVar TIJS_LIPPERTS_1982_P pp. 27--37
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 21 
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 45--46
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <- c(0,0,0,3,3,3,6)
#' drawCoreCover(v)
drawCoreCover<-function(v,holdOn=FALSE, colour = NA , label=FALSE, name = "Core Cover"){
  A=v
  co=coreCoverVertices(A);
  visualize(A, pointsToDraw=co, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_coreCoverVertices<-function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult,v)
}
