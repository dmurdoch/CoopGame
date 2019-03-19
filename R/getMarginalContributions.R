#' @name getMarginalContributions
#' @title Compute marginal contributions
#' @description Calculates the marginal contributions for all permutations of the players
#' @aliases getMarginalContributions
#' @export getMarginalContributions
#' @importFrom gtools permutations
#' @template author/AT
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 156--159
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 6 
#' @template param/v
#' @return a list with given game vector, a matrix of combinations
#'          used and a matrix with the marginal contributions
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' getMarginalContributions(v)
#' 
getMarginalContributions <- function(v) {
  # validate parameter
  paramCheckResult <- getEmptyParamCheckResult()
  initialParamCheck_marginalContributions(paramCheckResult = paramCheckResult, v)
  A = v
  retVal<-0
  numberOfPlayers=getNumberOfPlayers(A)
  
  #initialize the result matrix
  marginalValue=matrix()
  #initialize matrix/array with all permutations for the order of the players
  #p<-permn(numberOfPlayers)
  p<-gtools::permutations(n = numberOfPlayers, r = numberOfPlayers)
  #initialize matrix with marginal values for each order-permutation for each player
  marginalValue = matrix(nrow = (nrow(p)), ncol = numberOfPlayers)

  #initialize column and row variables for matrix of marginal contributions
  i<-1
  j<-1
  #number of rows and number of permutations is n!+1
  #numRows<-length(p)
  numRows<-nrow(p)
  while (i<=numRows) {
    #size of each permutation is number of players
    while(j<=numberOfPlayers){
      #check if current player is first player in order
      #if(j == p[[i]][1]){
      if(j == p[i,1]){
        marginalValue[i,j]<-A[j]
      }else{
        #get index position for current player in current permutation
        #curPos<-which(p[[i]]==j)
        curPos<-which(p[i,]==j)
        #save vector with players including current player
        #setWithCurPlayer <- p[[i]][1:curPos]
        setWithCurPlayer <- p[i,1:curPos]
        #save vector without current player
        #setWithoutCurPlayer <- p[[i]][1:(curPos-1)]
        setWithoutCurPlayer <- p[i,1:(curPos-1)]
        #sort sets
        setWithCurPlayer<-sort(setWithCurPlayer, decreasing = FALSE)
        setWithoutCurPlayer<-sort(setWithoutCurPlayer, decreasing = FALSE)
        #get index of current coalition of players in 'all combination'-list
        indxWithCurPlayer<-indexCoalition(numberOfPlayers, setWithCurPlayer)
        indxWithoutCurPlayer<-indexCoalition(numberOfPlayers, setWithoutCurPlayer)
        #calculate marginalvalue for current player
        marginalValue[i,j] <- A[indxWithCurPlayer]-A[indxWithoutCurPlayer]
      }
      j=j+1
    }
    j=1
    i=i+1
  }
  retVal=list(A=A,combinations=p,marginal_values=marginalValue)
  
  return (retVal)
}

initialParamCheck_marginalContributions=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
