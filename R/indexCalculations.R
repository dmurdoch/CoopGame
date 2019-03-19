#' @name IndexCalculations_CoopGame
#' @family IndexCalculations_CoopGame
#' @title Index Calculation Functions
#' @noRd
#' @description Index Calculation Functions aim to offer
#' global functions which allow the user to determine
#' the corresponding index of a row in a bit matrix or a game vector 
#' with respect to specified coalition properties.
#' But also the goal of our Index Calculation Functions is to offer 
#' functions which help to identify properties of a coalition 
#' referenced by an index.
#' @section Index search functions:
#' 
#' \tabular{lll}{
#'    \strong{Function Name}                        \tab \strong{Short Description}                                                  \cr
#'    \code{\link{indexCoalition}}                  \tab  Finds out index to specified coalition S                                   \cr
#'    \code{\link{indexCoalitionByBitVector}}       \tab  Finds out index to specified coalition S when S is specified as bit vector \cr
#'  }
#'  
#' @section Search helper functions:
#'  
#' \tabular{lll}{
#'    \strong{Function Name}                        \tab \strong{Short Description}                                                                                         \cr
#'    \code{\link{getPlayersFromIndex}}             \tab Identifes by specified index the players involved in the corresponding coalition                                  \cr
#    \code{\link{calcDistance}}                    \tab Identifies distance between to indices when n choose b players left to set and difference of players is specified  \cr
#    \code{\link{getStartPosition}}                \tab Identifies index for coalition where either 'player' is specified as first player involved and n choose b players are involved in complete coalition.\cr
#    \code{\link{indexLower}}                      \tab Identifies index of lower bound for first occurrence of n choose b players involved\cr
#    \code{\link{indexUpper}}                      \tab Identifies index of upper bound for last occurrence of n choose b players involved\cr
#'  }
NULL


#' @name indexCoalition
#' @title Calculate a coalition's index
#' @family IndexCalculations_CoopGame
#' @description indexCoalition calculates the index a certain coalition S
#'  has either in a bit matrix or in a game vector for TU game with n players.
#' @aliases indexCoalition
# @export indexCoalition
#' @noRd
#' @template author/JA
#' @template author/JS
#' @param n represents number of players in the game
#' @param S is a vector containing a subset of the players in the game and represents
#'        the coalition whose corresponding index is searched
#' @return index of the coalition
#' @examples
#' library(CoopGame)
#' A=weightedVotingGameVector(n=3,w=c(1,2,3),q=5)
#' bm=createBitMatrix(3,A)
#' ix_c=indexCoalition(3,S=c(1,3)) #delivers ix_c=5
#' bm
#'#Output (with added comment in line):
#'#           cVal
#'#[1,] 1 0 0    0
#'#[2,] 0 1 0    0
#'#[3,] 0 0 1    0
#'#[4,] 1 1 0    0
#'#[5,] 1 0 1    0 <=
#'#[6,] 0 1 1    1
#'#[7,] 1 1 1    1
#'
indexCoalition=function(n,S){
  S=as.numeric(S) #Marked as to be removed when all changes applied
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_indexCoalition(paramCheckResult,n,S)
  #index referencing coalition in corresponding bit matrix for n players
  ix_c=NULL
  b=length(S)
  S=sort(S)
  if(b==1){
    #index for row in bit matrix for coalition is equal to number of player
    #if there is only one player involved
    ix_c=S[1]
  }else{
    #Calculate index in bit matrix for n choose b where bit for player S[1] is first set bit
    ix_c=getStartPosition(firstPlayer = S[1],n,b)

    #Loop variable for current pair of players
    i_tmp=1
    #Loop over all pairs of players
    for(i in S[1:(length(S)-1)]){
      #compare the difference of two neighbouring players
      dif=S[i_tmp+1]-S[i_tmp]
      #Get distance of two neighbours (in rows) and add the result
      #to index referencing the coalition in the bit matrix
      ix_c=ix_c+calcDistance(dif,n-i,b-i_tmp)
      i_tmp=i_tmp+1
    }
  }
  return (ix_c)
}

#' @name initialParamCheck_indexCoalition
#' @noRd
#' @title Parameter check for indexCoalition
#' @family IndexCalculations_CoopGame
#' @description initialParamCheck_indexCoalition validates parameters 
#' passed to indexCoalition
#' @aliases initialParamCheck_indexCoalition
#' #@export initialParamCheck_indexCoalition
#' @template author/JA
#' @param paramCheckResult represents object where errCode and errMessage are assigned to after validation.
#' @param n represents number of players involved in whole game.
#' @param S represents coalition as subset of N.
#' @examples
#'   paramCheckResult=getEmptyParamCheckResult()
#'   initialParamCheck_indexCoalition(paramCheckResult,n,S)
initialParamCheck_indexCoalition=function(paramCheckResult,n,S){
  stopOnInvalidCoalitionS(paramCheckResult,S,n=n)
}

#' @name indexCoalitionByBitVector
#' @title Calculate a coalition's index from a bit vector
#' @family IndexCalculations_CoopGame
#' @description indexCoalitionByBitVector calculates the index of 
#' a certain coalition S from a bit vector  
#' for a TU game with n players
#' @aliases indexCoalitionByBitVector
# @export indexCoalitionByBitVector
#' @noRd
#' @template author/JA
#' @template author/JS
#' @template param/n
#' @param bitVector is a vector containing a subset of players 
#'        (number of bit set depicts involved players) and represents
#'        the coalition for which the corresponding index is searched
#' @return ix_c as the returned index value
#' @examples 
#' library(CoopGame)
#' # The coalition formed by players 1 and 3 has index 6 
#' # in a 4-player TU game 
#' myBitVector <- c(1,0,1,0)
#' indexCoalitionByBitVector(4,myBitVector)
#' #[1] 6
#
indexCoalitionByBitVector=function(n,bitVector){
  S=which(bitVector&1)
  return(indexCoalition(n,S))
}
#
## First section: END

## Second section: START


#' @name getPlayersFromIndex
#' @title Calculate the indices of players from coalition's index
#' @description getPlayersFromIndex calculates the indices of 
#' players in a certain coalition S
#' specified by the coalition's index for a TU game with n players
#' @aliases getPlayersFromIndex
#' @importFrom utils combn
#' @family IndexCalculations_CoopGame
# @export getPlayersFromIndex
#' @noRd
#' @template author/JA
#' @param bitIndex is row index of a bit matrix where involved players should get identified.
#' @template param/n
#' @return A vector of indices of players 
#' @examples
#' #Finds indices of players of a row specified by 'bitIndex' 
#' #in a bit matrix for 'n' players
#' #Corresponding bit matrix for 3 players with marked row (4th column represents coalition value)
#' #             cVal
#' # [1,] 1 0 0    1
#' # [2,] 0 1 0    2
#' # [3,] 0 0 1    3
#' # [4,] 1 1 0    4
#' # [5,] 1 0 1    5
#' # [6,] 0 1 1    6 <=
#' # [7,] 1 1 1    7
#' #Here: For bitIndex 6 the involved players should be determined (players 2 and 3)
#' library(CoopGame)
#' getPlayersFromIndex(n=3,6)
#' #Result: [1] 2 3
#' 
getPlayersFromIndex=function(n, bitIndex){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getPlayersFromIndex(paramCheckResult, n, bitIndex)

  ix_lb=NULL
  ix_ub=NULL
  dif=NULL
  players=NULL

  #Find out how many players b are involved within the bit matrix' row specified
  #by bitIndex
  for(b in 1:n){
    #Determine lower and upper bound of range within b players are involved
    ix_lb=indexLower(n,b)
    ix_ub=indexUpper(n,b)
    #if bitIndex is within in range the correct number of players was identified
    #and loop gets quited
    if(ix_lb<=bitIndex & bitIndex<=ix_ub){
      #dif saves value for difference between bitIndex and index in bit matrix
      #where first occurence of a coalition with b choose players arises
      dif=bitIndex-ix_lb
      break
    }
  }
  players=t(utils::combn(c(1:n),m=b))[1+dif,]
  return(players)
}

#' @name initialParamCheck_getPlayersFromIndex - parameter check for getPlayersFromIndex
#' @noRd
#' @title initialParamCheck_getPlayersFromIndex
#' @family IndexCalculations_CoopGame
#' @description initialParamCheck_getPlayersFromIndex validates parameters passed to getPlayersFromIndex
#' @aliases initialParamCheck_getPlayersFromIndex
#' #@export initialParamCheck_getPlayersFromIndex
#' @template author/JA
#' @param paramCheckResult represents object where errCode and errMessage are assigned to after validation.
#' @param param n represents number of the game's players
#' @param param bitIndex represents the index referencing the corresponding row in the bit matrix for which involved players gets identified.


initialParamCheck_getPlayersFromIndex=function(paramCheckResult, n, bitIndex){
  stopOnInvalidNumberOfPlayers(paramCheckResult,n)
  stopOnInvalidIndex(paramCheckResult,index = bitIndex, n=n)
}

## Second section: END


## Third section: START
## From here onwards no functionality is used or exported
#' @name calcDistance
#' @noRd 
#' @title calcDistance - get distance between 2 players
#' @description calcDistance calculates distance in a bit matrix for n players beetween 2 defined players (in rows) when .
#' @aliases calcDistance
#' @family IndexCalculations_CoopGame
#' #@export calcDistance
#' @template author/JA
#' @param paramCheckResult represents object where errCode and errMessage are assigned to after validation.
#' @param n represents here number of players left to set.
#' @param b represents number of players which get set.
#' @examples
#' #Example: Bit matrix for 3 players (4th column represents coalition value)
#' #look for start Position for firstPlayer=1, n=3 and b=2
#' #getStartPosition(firstPlayer=1, n=3, b=2) delivers 4 as index
#' #Now check rows it takes that bit for next player 3 is set
#' #- here n is 2 (two players left '2' and '3') and b is 1 (1 bit left to set)
#' calcDistance(dif=(3-1),2,1)
#' #Now add 4 (old index) to 1 and get 5 as index where bit for player 1 and 3 is set
#' #       [,1] [,2] [,3] [,4]
#' # [1,]    1    0    0    0
#' # [2,]    0    1    0    0
#' # [3,]    0    0    1    0
#' # [4,]    1    1    0    0
#' # [5,]    1    0    1    0 <=
#' # [6,]    0    1    1    0
#' # [7,]    1    1    1    0
#' # [8,]    0    0    0    0
calcDistance=function(dif,n,b){
  ix=0
  if(dif>1){
    for(j in 1:(dif-1)){
      n_tmp=n-j+1
      b_tmp=b
      ix=ix+(choose(n_tmp,b_tmp)*(b_tmp/n_tmp))
    }
  }
  return(ix)
}

#' @name getStartPosition
#' @noRd
#' @title getStartPosition - get index where n choose b players are involved
#' @description getStartPosition calculates index for row in a bit matrix for n players with n choose b players involved
#' and firstPlayer is the player where bit is set for the first time
#' @aliases getStartPosition
#' @family IndexCalculations_CoopGame
#' #@export getStartPosition
#' @template author/JA
#' @param firstPlayer represents object where errCode and errMessage are assigned to after validation.
#' @param n represents number of players involved in whole game.
#' @param b represents coalition as subset of N.
#' @examples
#' # Bit matrix for 3 players (4th column represents coalition value)
#' # look for start Position for firstPlayer=2, n=3 and b=2
#' #=> searched index is 6:
#' getStartPosition(firstPlayer=2,n=3,b=2)
#' # Result:> [1] 6
#' # Corresponding bit matrix
#'
#'#       [,1] [,2] [,3] [,4]
#'# [1,]    1    0    0    0
#'# [2,]    0    1    0    0
#'# [3,]    0    0    1    0
#'# [4,]    1    1    0    0
#'# [5,]    1    0    1    0
#'# [6,]    0    1    1    0 <=
#'# [7,]    1    1    1    0
#'# [8,]    0    0    0    0

getStartPosition=function(firstPlayer,n,b){
  ix_s=0
  ix_l=indexLower(n,b)
  dif=firstPlayer[1]
  ix_s=calcDistance(dif,n,b)
  return (ix_s+ix_l)
}






#' @name indexLower
#' @noRd
#' @title indexLower - calculates index for lower bound where n choose b players are involved
#' @description indexLower calculates in a bit matrix for n players the the first row where n choose b are involved in the game.
#' @aliases indexLower
#' @family IndexCalculations_CoopGame
#' #@export indexLower
#' @template author/JA
#' @param param n represents number of the game's players
#' @param param b represents number of players involved.
#' @examples
#' #Generate bit matrix for n=3 players
#' createBitMatrix(n=3,A=c(1:7))
#' #Correponding bit matrix
#' #           cVal
#' #[1,] 1 0 0    1
#' #[2,] 0 1 0    2
#' #[3,] 0 0 1    3
#' #[4,] 1 1 0    4 <= Searched row
#' #[5,] 1 0 1    5
#' #[6,] 0 1 1    6
#' #[7,] 1 1 1    7
#' #Get index of row where 3 choose 2 players are involved
#' indexLower(n=3,b=2)
#' #Result: [1] 4

indexLower=function(n,b){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_indexLower(paramCheckResult, n,b)
  ix=0
  for (i in 1:b) {
    ix=ix+choose(n,i-1)
  }
  return(ix)
}

#' @name initialParamCheck_indexLower
#' @noRd
#' @title initialParamCheck_indexLower - parameter check for  indexLower
#' @description initialParamCheck_indexLower validates parameters passed to indexLower
#' @aliases initialParamCheck_indexLower
#' @family IndexCalculations_CoopGame
#' #@export initialParamCheck_indexLower
#' @template author/JA
#' @param paramCheckResult represents object where errCode and errMessage are assigned to after validation.
#' @param param n represents number of the game's players
#' @param param b represents number of players involved.
#' @examples
#' #Valid parameters:
#' paramCheckResult=getEmptyParamCheckResult()
#' initialParamCheck_indexLower(paramCheckResult,n=3,b=2)
#' #Invalid parameters (as n<b):
#' paramCheckResult=getEmptyParamCheckResult()
#' initialParamCheck_indexLower(paramCheckResult,n=2,b=3)

initialParamCheck_indexLower=function(paramCheckResult, n, b){
  stopOnInvalidNChooseB(paramCheckResult,n,b)
}


#' @name indexUpper
#' @noRd
#' @title indexUpper - calculates index for upper bound where n choose b players are involved
#' @description indexUpper calculates in a bit matrix for n players index referencing the the last row where n choose b are involved in the game.
#' @aliases indexUpper
#' @family IndexCalculations_CoopGame
#' #@export indexUpper
#' @template author/JA
#' @param param n represents number of the game's players
#' @param param b represents number of players involved.
#' @examples
#' #Generate bit matrix for n=3 players
#' createBitMatrix(n=3,A=c(1:7))
#' #Correponding bit matrix
#' #           cVal
#' #[1,] 1 0 0    1
#' #[2,] 0 1 0    2
#' #[3,] 0 0 1    3
#' #[4,] 1 1 0    4
#' #[5,] 1 0 1    5
#' #[6,] 0 1 1    6 <= Searched row
#' #[7,] 1 1 1    7
#' #Get index of last row where 3 choose 2 players are involved
#' indexUpper(n=3,b=2)
#' #Result: [1] 6

indexUpper=function(n,b){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_indexUpper(paramCheckResult,n,b)
  index=0
  for (i in 1:b) {
    index=index+choose(n,i)
  }
  return (index)
}

#' @name initialParamCheck_indexUpper
#' @noRd
#' @title initialParamCheck_indexUpper - parameter check for indexUpper
#' @description initialParamCheck_indexUpper validates parameters passed to indexUpper
#' @aliases initialParamCheck_indexUpper
#' @family IndexCalculations_CoopGame
#' #@export initialParamCheck_indexUpper
#' @template author/JA
#' @param paramCheckResult represents object where errCode and errMessage are assigned to after validation.
#' @param param n represents number of the game's players
#' @param param b represents number of players involved.
#' @examples
#' #Valid parameters:
#' paramCheckResult=getEmptyParamCheckResult()
#' initialParamCheck_indexUpper(paramCheckResult,n=3,b=2)
#' #Invalid parameters (as n<b):
#' paramCheckResult=getEmptyParamCheckResult()
#' initialParamCheck_indexUpper(paramCheckResult,n=2,b=3)

initialParamCheck_indexUpper=function(paramCheckResult, n, b){
  stopOnInvalidNChooseB(paramCheckResult,n,b)
}
## Third section: END




