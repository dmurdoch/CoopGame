#' @name webersetVertices
#' @title Compute vertices of Weber Set
#' @description Calculates the Weber Set for given game vector with n players.
#' @aliases webersetVertices
#' @importFrom grDevices chull
#' @export webersetVertices
#' @template author/AM
#' @template author/FM
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 327--329
#' @template param/v
#' @return rows of the matrix are the vertices of the Weber Set
#' @examples
#' library(CoopGame)
#' webersetVertices(c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' #Example of a 3-player TU game (with a Weber Set with 6 vertices)
#' library(CoopGame)
#' v = c(0,1,2,3,4,5,6)
#' webersetVertices(v)
#'
#' #Example of a 4-player TU game (with a Weber Set with 14 vertices)
#' library(CoopGame)
#' v = c(5,2,4,7,15,15,15,15,15,15,20,20,20,20,35)
#' webersetVertices(v)
#' }
#' 
webersetVertices<-function(v){
  emptyParamCheckResult=getEmptyParamCheckResult();
  initialParamCheck_weberSetVertices(paramCheckResult = emptyParamCheckResult,v)
  A = v
  numberOfPlayers = getNumberOfPlayers(A)
  OutcomeMatrix = matrix(ncol = numberOfPlayers,nrow = 0)
  if(!isNonnegativeGame(A)){
    print("Game is not nonnegative. Vertices of Weber Set are only computed for nonnegative games.")
  }
  else if (!isMonotonicGame(A)){
    print("Game is not monotonic. Vertices of Weber Set are only computed for monotonic games.")
  }
  else
  {
    #getMarginalContributions gives the marginal vectors
    ResultList = getMarginalContributions(A)
    ResultMatrix = ResultList[[3]]
    
    if(numberOfPlayers == 3)
    {
      sequencevector = grDevices::chull(ResultMatrix)
      OutcomeMatrix = ResultMatrix[sequencevector,,drop = FALSE]
    }
    else
    {
      #Delete redundant points
      vRep = makeV(points = ResultMatrix)
      if (nrow(vRep) >= 2)
      {
        vRep = redundant(vRep, representation = "V")
      }
      
      VectorCounter = length(vRep[[1]]) / (numberOfPlayers + 2)
      OutcomeVector = vRep[[1]][(VectorCounter * 2 + 1):(length(vRep[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, numberOfPlayers) 
      OutcomeMatrix = ResultMatrix
    }
  }
  return (OutcomeMatrix)
}


#' @name belongsToWeberset
#' @title Check if point is element of Weber Set
#' @description belongsToWeberset checks if the point is in the Weber Set
#' @aliases belongsToWeberset
#' @export belongsToWeberset
#' @template author/JA
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 327--329
#' @template param/x
#' @template param/v
#' @return \code{TRUE} if point belongs to Weber Set and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' belongsToWeberset(x=c(1,0.5,0.5), v=c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' v=c(0,1,2,3,4,5,6)
#' 
#' #Point belongs to Weber Set:
#' belongsToWeberset(x=c(1.5,1,3.5),v)
#' 
#' #Point does not belong to Weber Set:
#' belongsToWeberset(x=c(2.05,2,2),v)
#' }
#' 
belongsToWeberset<-function(x,v){
  A = v
  wv=webersetVertices(A) 
  return(isElementOfConvexSet(wv,x=x))
}


#' @name drawWeberset
#' @title Draw Weber Set for 3 or 4 players
#' @description drawWeberset draws the Weber Set for 3 or 4 players.
#' @aliases drawWeberset
#' @export drawWeberset
#' @template author/JA
#' @template author/JS
#' @template cites/WEBER_1988
#' @templateVar WEBER_1988_P pp. 101--119
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 327--329
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v = c(0,1,2,3,4,5,6)
#' drawWeberset(v, colour ="yellow")
drawWeberset<-function(v,holdOn=FALSE, colour = NA , label=FALSE, name = "Weber Set"){
  A=v
  wv=webersetVertices(A);
  visualize(A, pointsToDraw=wv, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_weberSetVertices<-function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult,v)
}