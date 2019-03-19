#' @name johnstonIndex
#' @title Compute Johnston index
#' @description johnstonIndex calculates the Johnston index for a simple game.
#' @aliases johnstonIndex
#' @export johnstonIndex
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/JOHNSTON_1978
#' @templateVar JOHNSTON_1978_P pp. 907--914
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 124
#' @template param/v
#' @return Johnston index for a specified simple game
#' @examples
#' library(CoopGame)
#' johnstonIndex(c(0,0,0,1,0,0,1))
#' 
#' \donttest{
#' #player 1 has 3 votes
#' #player 2 has 2 votes
#' #player 3 has 1 vote
#' #majority for the decision is 4 (quota)
#'
#' library(CoopGame)
#' #function call generating the game vector:
#' v <- weightedVotingGameVector(n = 3, w = c(3,2,1), q = 4)
#'
#' johnstonIndex(v)
#' #[1] 0.6666667 0.1666667 0.1666667
#' }
#' 
johnstonIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_johnstonIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Johnston Index can be retrieved")
  }
  else 
  {
    n <- getNumberOfPlayers(A)
    bm <- createBitMatrix(n, A)
    johnstonIndexVal <- rep.int(0, n)
    
    # gets the winning coalitions
    winningCoalitions=bm[bm[,"cVal"]>0,,drop=FALSE]
  
    if (length(winningCoalitions)==0) {
      print("There are no winning coalitions in this game")
    }
    else
    {
  
      mxRecVals <- matrix(ncol = n, nrow = nrow(winningCoalitions))
    
      for (r in 1:nrow(winningCoalitions)) {
        tmpRow <- winningCoalitions[r, ]
    
        cntSwing <- 0
        vecSwingPlayers <- c()
    
        for (i in 1:n) {
          tmptmpRow <- tmpRow[1:n]
          # set player i to zero - 1 1 0 -> 0 1 0
          tmptmpRow[i] <- 0
          # compare bit pattern with entry in bitmatrix and get corresponding coalition value
          # null coalition generates no value
          if (sum(tmptmpRow != 0)) {
            ix <- indexCoalitionByBitVector(n, tmptmpRow)
            cVal <- bm[ix, 'cVal']
          } else {
            cVal <- -1
          }
    
          # check for each player in vulnerable coalition if player is critical (so called swing)
          # count number of players being in swing position
          if (cVal == 0) {
            cntSwing <- cntSwing + 1
            vecSwingPlayers <- c(vecSwingPlayers, 1)
          } else {
            vecSwingPlayers <- c(vecSwingPlayers, 0)
          }
        }
        if (cntSwing == 0) {
          recVal <- 0
        } else{
          # get reciprocal of number of swing players in coalition
          recVal <- 1 / cntSwing
        }
        # multiply vector containing swing players with reciprocal of number of swing players
        mxRecVals[r, ] <- vecSwingPlayers * recVal
      }
    
      # sum up the reciprocal for each player
      total <- colSums(mxRecVals)
    
      mxRecVals <- rbind(mxRecVals, total)
    
      totalNumberOfReciprocals <- sum(total)
    
      # The Johnston power of player i is the sum of the reciprocal of number of swings in
      # vulnerable  coalition c in  which i is  critical, divided by the total number of reciprocal of
      # number of swings in vulnerable coalition c of all players
      if (totalNumberOfReciprocals != 0) {
        johnstonIndexVal <- total / totalNumberOfReciprocals
      }
      retVal = johnstonIndexVal
      }
  }
  return(retVal)
}


#' @name drawJohnstonIndex
#' @title Draw Johnston index for 3 or 4 players
#' @description drawJohnstonIndex draws the Johnston index for 3 or 4 players.
#' @aliases drawJohnstonIndex
#' @export drawJohnstonIndex
#' @template cites/JOHNSTON_1978
#' @templateVar JOHNSTON_1978_P pp. 907--914
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P p. 124
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <- c(0,0,0,1,1,0,1)
#' drawJohnstonIndex(v)
drawJohnstonIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Johnston index"){
  A=v
  lgv=johnstonIndex(A);
  visualize(A, pointsToDraw=lgv, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_johnstonIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
