#' @name isAdditiveGame
#' @title Check if game is additive
#' @description Checks if a TU game with n players is additive. \cr
#' In an additive game for any two disjoint coalitions 
#' \code{S} and \code{T} the value of the union of 
#' \code{S} and \code{T} equals the sum of the values  
#' of \code{S} and \code{T}. In other words, additive games 
#' are constant-sum and the imputation set of an 
#' additive game consists of exactly one point.
#'
#' @aliases isAdditiveGame
#' @export isAdditiveGame
#' @template author/AT
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 11
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 292
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P p. 261 
#' @template param/v
#' @return \code{TRUE} if the game is additive, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' isAdditiveGame(c(1,1,1,2,2,2,3))
#' 
#' \donttest{
#' #The following game is not additive
#' library(CoopGame)
#' v=c(0,0,0,40,50,20,100)
#' isAdditiveGame(v) 
#' 
#' #The following game is additive
#' library(CoopGame)
#' v=c(1,1,1,1, 2,2,2,2,2,2, 3,3,3,3, 4)
#' isAdditiveGame(v)
#' }
#'
isAdditiveGame<-function(v){
  # validate parameter
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isAdditiveGame(paramCheckResult = paramCheckResult, v)
  A = v
  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  result<-TRUE

  #create bitmatrix (for each possible coalition of players)
  bitMatrix=createBitMatrix(n = numberOfPlayers, A)

  #start checking at a set of two
  i<-numberOfPlayers+1
  end<-nrow(bitMatrix)

  bounds<-factorial(numberOfPlayers)/(factorial(1:(numberOfPlayers-1))*factorial((numberOfPlayers-1):1))

  while(i<=end){

    #number of players in the set which is checked
    numberOfCurrSet<-sum(bitMatrix[i,1:numberOfPlayers])
    #upper bound for combinations which has to be checked
    currCheckComb<-sum(bounds[1:(numberOfCurrSet-1)])
    #lower bound for combinations to check for current set
    if(numberOfCurrSet/2==1){
      lowerBound = 0
    }else{
      lowerBound<-sum(bounds[1:(ceiling(numberOfCurrSet/2)-1)])
    }

    #payoff for the current set of players
    payOffUnion = bitMatrix[[i,"cVal"]]

    while(currCheckComb > lowerBound){

      hitSetCount<-sum(bitMatrix[i,1:numberOfPlayers]&bitMatrix[currCheckComb,1:numberOfPlayers])
      hitSetCountSubset<-sum(bitMatrix[currCheckComb,1:numberOfPlayers])
      #check only those combinations which hit the current combi at least for the half
      #of the set which is checked
      if(hitSetCount>=numberOfCurrSet/2 && hitSetCount==hitSetCountSubset){

        #go through combinations for the current set to check if it's additive
        payOffS = bitMatrix[[currCheckComb,"cVal"]]
        payOffT = A[indexCoalition(numberOfPlayers, setdiff(which(bitMatrix[i,1:numberOfPlayers]&1), which(bitMatrix[currCheckComb,1:numberOfPlayers]&1)))]
        result = (payOffUnion == payOffS + payOffT)
        if(!result){
          return(result)
        }
      }

      currCheckComb<-currCheckComb-1
    }

    i<-i+1
  }

  return(result)
}

initialParamCheck_isAdditiveGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

