#' @name imputationsetVertices
#' @title Compute vertices of imputation set
#' @description imputationsetVertices calculates the imputation set vertices 
#' for given game vector.
#' @aliases imputationsetVertices imputationSetVertices
#' @import rcdd
#' @export imputationsetVertices
#' @template author/MM 
#' @template author/FM
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/v
#' @return rows of the matrix are the vertices of the imputation set
#' @examples
#' library(CoopGame)
#' imputationsetVertices(c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' v = c(2, 4, 5, 18, 24, 9, 24)
#' 
#' imputationsetVertices(v)
#' 
#' #      [,1] [,2] [,3]
#' #[1,]   15    4    5
#' #[2,]    2   17    5
#' #[3,]    2    4   18
#' }
#' 
imputationsetVertices<-function(v){
  emptyParamCheckResult=getEmptyParamCheckResult();
  initialParamCheck_impvertices(paramCheckResult = emptyParamCheckResult,v)
  A = v
  numberOfPlayers = getNumberOfPlayers(A)
  
  # Checks if imputation set is not empty
  if(isEssentialGame(A)== TRUE || isDegenerateGame(A))
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
  }
  else
  {
    print("The imputation set is empty")
    OutcomeMatrix=(matrix(nrow=0,ncol=numberOfPlayers))
  }
  return (OutcomeMatrix)
}


#' @name belongsToImputationset
#' @title Check if point is imputation
#' @description belongsToImputationset checks if the point belongs to the imputation set
#' @aliases belongsToImputationset
#' @export belongsToImputationset
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/x
#' @template param/v
#' @return \code{TRUE} for a point belonging to the imputation set and \code{FALSE} otherwise
#' @examples
#' library(CoopGame)
#' belongsToImputationset(x=c(1,0.5,0.5), v=c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' library(CoopGame)
#' v=c(0,1,2,3,4,5,6)
#'
#' #Point belongs to imputation set:
#' belongsToImputationset(x=c(1.5,1,3.5),v)
#'
#' #Point does not belong to imputation set:
#' belongsToImputationset(x=c(2.05,2,2),v)
#' }
#' 
belongsToImputationset<-function(x,v){
  isv=imputationsetVertices(v) 
  return(isElementOfConvexSet(isv,x))
}


#' @name drawImputationset
#' @title Draw imputation set for 3 or 4 players
#' @description drawImputationset draws the imputation set for 3 or 4 players.
#' @aliases drawImputationset drawImputationSet
#' @export drawImputationset
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 20
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 674 
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 278
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 407
#' @template param/v
#' @template param/label
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,1,2,3,4,5,6)
#' drawImputationset(v)

drawImputationset<-function(v, label=TRUE){
  A=v
  n = getNumberOfPlayers(A)
  canDraw = ((n==3) | (n==4))
  if (canDraw){
    imputationsetDraw(A, label)
  }
  else{
    print("The program can only draw if there are 3 or 4 players")
  }
}

initialParamCheck_impvertices<-function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult,v)
}