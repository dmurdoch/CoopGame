#' @title create bit matrix
#' @name createBitMatrix
#' @description createBitMatrix creates a bit matrix with 
#' (numberOfPlayers+1) columns and (2^numberOfPlayers-1) rows 
#' which contains all possible 
#' coalitions (apart from the null coalition) for the set of all players.  
#' Each player is represented by a column which describes if this player 
#' is either participating (value 1) or not participating (value 0).
#' The last column (named cVal) contains the values of each coalition.
#' Accordingly, each row of the bit matrix expresses a coalition as a subset of all players.
#'
#' @aliases createBitMatrix
#' @export createBitMatrix
#' @importFrom utils combn
#' @template author/JA
#' @template author/JS
#' @template param/n
#' @template param/A
#' @return The return is a bit matrix containing all possible coalitions apart from the empty coalition
#' @examples
#' library(CoopGame)
#' createBitMatrix(3,c(0,0,0,60,60,60,72))
#' 
#' \donttest{
#' library(CoopGame)
#' A=weightedVotingGameVector(n=3,w=c(1,2,3),q=5)
#' bm=createBitMatrix(3,A)
#' bm
#'# Output:
#'#            cVal
#'# [1,] 1 0 0    0
#'# [2,] 0 1 0    0
#'# [3,] 0 0 1    0
#'# [4,] 1 1 0    0
#'# [5,] 1 0 1    0
#'# [6,] 0 1 1    1
#'# [7,] 1 1 1    1
#'}

createBitMatrix=function(n,A=NULL){
  
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidNumberOfPlayers(paramCheckResult, n)
  #Create all possible subsets from set of all players
  # bm=hier.part::combos(n)$binary
  N=2^n-1
  bm <- matrix(rep(0, N * n), nrow = N, ncol = n, byrow = TRUE)
  # assign actual row position
  rownum <- 1
  
  # store every possible coalition of given n-person game in coefficient matrix
  # by overwriting row-wise the default value at the position of each coalition
  # partner with one
  for (i in 1:n) {
    combo <- utils::combn(n, i)
    
    for (j in 1:ncol(combo)) {
      for (k in 1:nrow(combo)) {
        bm[rownum, combo[k, j]] <- 1
      }
      
      rownum <- rownum + 1
    }
  }

  #Add column named cVal which is intended to contain the values each created by the accordingly coalition
  bm=cbind(bm,cVal=0)

  #Fill column named cVal by values of game vector A
  if(methods::hasArg(A)){
    bm[1:length(A),"cVal"]=A
  }
  #return bit matrix containing all possible coalition apart from the null coalition
  return (bm)
}

#' @title Extract players from bit vector
#' @name getPlayersFromBitVector
#' @description getPlayersFromBitVector determines players involved in a coalition
#' from a binary vector.
#' @aliases getPlayersFromBitVector
#' @export getPlayersFromBitVector
#' @template author/JA
#' @template author/JS
#' @param bitVector represents the binary vector 
#' @return playerVector contains the numbers of the players involved in the coalition
#' @examples 
#' library(CoopGame)
#' myBitVector <-c(1,0,1,0)
#' (players<-getPlayersFromBitVector(myBitVector))
#' 
getPlayersFromBitVector=function(bitVector){
  numberOfPlayers=length(bitVector)
  #changed identification of players for the playerVector on proposal of Alexandra Tiukkel to line below
  playerVector=which(bitVector&1)
  return(playerVector)
}

#' @title Extract players from bit matrix row
#' @name getPlayersFromBMRow
#' @description getPlayersFromBMRow determines players involved 
#' in a coalition from the row of a bit matrix 
#' @aliases getPlayersFromBMRow
#' @export getPlayersFromBMRow
#' @template author/JA
#' @template author/JS
#' @param bmRow represents the bit matrix row
#' @return playerVector contains involved players (e.g. c(1,3), see example below for bitIndex=5 and n=3)
#' @examples
#' library(CoopGame)
#' bm=createBitMatrix(n=3,A=c(0,0,0,1,1,1,2))
#' getPlayersFromBMRow(bmRow=bm[4,])
#' 
#' \donttest{
#' library(CoopGame)
#' bm=createBitMatrix(n=3,A=c(1:7))
#' #Corresponding bit matrix:
#' #           cVal
#' #[1,] 1 0 0    1
#' #[2,] 0 1 0    2
#' #[3,] 0 0 1    3
#' #[4,] 1 1 0    4
#' #[5,] 1 0 1    5 <=Specified bit index
#' #[6,] 0 1 1    6
#' #[7,] 1 1 1    7
#'
#' #Determine players from bit matrix row by index 5
#' players=getPlayersFromBMRow(bmRow=bm[5,])
#' #Result:
#' players 
#' #[1] 1 3
#' }
#' 
getPlayersFromBMRow=function(bmRow){
  players=getPlayersFromBitVector(bmRow[1:(which(names(bmRow)=="cVal")-1)])
  #Remove col and rownames
  
  return(unname(players))
}
##Second Section: END
