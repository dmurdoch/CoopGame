#' @name isSuperadditiveGame
#' @title Check if game is superadditive
#' @description Checks if a TU game with n players is superadditive. \cr
#' In a superadditive game for any two disjoint coalitions 
#' \code{S} and \code{T} the value of the union of 
#' \code{S} and \code{T} is always greater or equal 
#' the sum of the values of \code{S} and \code{T}. 
#' In other words, the members of any two disjoint 
#' coalitions \code{S} and \code{T} will never be 
#' discouraged from collaborating.
#' @aliases isSuperadditiveGame isSuperAdditiveGame
#' @export isSuperadditiveGame
#' @template author/AT
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P p. 295
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P p. 671
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P p. 408 
#' @template param/v
#' @return \code{TRUE} if the game is superadditive, else \code{FALSE}.
#' @examples
#' library(CoopGame)
#' isSuperadditiveGame(c(0,0,0,1,1,1,2))
#' 
#' \donttest{
#' #Example of a superadditive game
#' library(CoopGame)
#' v1=c(0,0,0,40,50,20,100) 
#' isSuperadditiveGame(v1)
#' 
#' #Example of a game that is not superadditive 
#' library(CoopGame)
#' v2=c(0,0,0,40,30,130,100) 
#' isSuperadditiveGame(v2)
#' 
#' #Another example of a superadditive game
#' library(CoopGame)
#' v3=c(1,1,1,1, 2,2,2,2,2,2, 3,3,3,3, 4)
#' isSuperadditiveGame(v3)
#' }
#' 
isSuperadditiveGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isSuperadditiveGame(paramCheckResult = paramCheckResult, v)
  A = v
  #get number of players
  numberOfPlayers=getNumberOfPlayers(A)

  #result value
  result<-TRUE

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

        #go through combinations for the current set to check if it's superadditive
        payOffS = bitMatrix[[currCheckComb,"cVal"]]
        payOffT = A[indexCoalition(numberOfPlayers, setdiff(which(bitMatrix[i,1:numberOfPlayers]&1), which(bitMatrix[currCheckComb,1:numberOfPlayers]&1)))]
        result = (payOffUnion >= payOffS + payOffT)
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

initialParamCheck_isSuperadditiveGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
