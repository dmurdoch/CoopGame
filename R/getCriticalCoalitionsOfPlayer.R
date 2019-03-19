#' @name getCriticalCoalitionsOfPlayer
#' @title Compute critical coalitions of a player for simple games
#' @description getCriticalCoalitionsOfPlayer identifies all coalitions for one player
#' in which that player is critical (within a simple game).
#' These coalitions are characterized by the circumstance that without this player the other players
#' generate no value (then also called a losing coalition) - therefore this player is also described as a critical player.
#' @aliases getCriticalCoalitionsOfPlayer
#' @export getCriticalCoalitionsOfPlayer
#' @template author/JA
#' @template author/JS
#' @param player represents the observed player
#' @template cites/DEEGAN_ET_PACKEL_1978
#' @templateVar DEEGAN_ET_PACKEL_1978_P pp. 151--161
#' @template param/v 
#' @return A data frame containing all minimal winning coalitions for one special player
#' @examples 
#' library(CoopGame)
#' getCriticalCoalitionsOfPlayer(2,v=c(0,0,0,0,0,1,1))
#' 
#' \donttest{
#' library(CoopGame)
#' v=c(0,1,0,1,0,1,1)
#' 
#' #Get coalitions where player 2 is critical:
#' getCriticalCoalitionsOfPlayer(2,v)
#' #Output are all coalitions where player 2 is involved.
#' #Observe that player 2 is dictator in this game.
#'#
#'#     V1 V2 V3 cVal bmRow
#'#  2  0  1  0    1     2
#'#  4  1  1  0    1     4
#'#  6  0  1  1    1     6
#'#  7  1  1  1    1     7
#'}
#'
getCriticalCoalitionsOfPlayer<-function(player,v){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidGameVector(paramCheckResult,v)
  stopOnInvalidNumber(paramCheckResult, player)
  getCoalitionsWherePlayerCritical(player,v)
}

getCoalitionsWherePlayerCritical=function(player,v){
  bmTemp = NULL
  A = v
  if(!isSimpleGame(A)){
    print("Game is not simple therefore no minimum winning coalitions can be retrieved.")
  }
  else
  {
    n=as.numeric(getNumberOfPlayers(A))
    bitMatrix = as.data.frame(createBitMatrix(n,A))
    
    #rowsToBeRemoved is intended for containing all coalitions where  player is not critical
    #and which finally get removed before returning a matrix with additional column for referencing according entries in the bit matrix
    rowsToBeRemoved=c()
    ci=c(rep(0,n))
    bm=as.data.frame(bitMatrix)
    #Get coalitions where player is involved
    bmTemp=bm[bm[,player]==1,,drop=FALSE]
    #Ignore these coalitions where coalition value is 0
    bmTemp=bmTemp[bmTemp[,n+1]!=0,,drop=FALSE]
    
    if(nrow(bmTemp)!=0){
      #add extra column for reference to corresponding bitMatrix row
      bmTemp=cbind(bmTemp,bmRow=0)
      
      for(j in 1:nrow(bmTemp)){
        bmTempRow=bmTemp[j,]
        #Get involved players: players u {player}
        players=getPlayersFromBMRow(bmTempRow)
        #Remove player from players: players / {player}
        players=setdiff(players,player)
        
        #check if players / {player} is empty
        if(length(players)==0){
          #if players without player is empty check if coalition value for only player as coalition's member is equal or smaller than 0
          #in case condition applies mark for removal and else keep row
          if(bm[player,"cVal"]<=0){
            rowsToBeRemoved=c(rowsToBeRemoved,j) #mark j for removal
          }else{
            ci[player]=ci[player]+bmTempRow["cVal"] # keep row
            bmTemp[j,"bmRow"]=indexCoalition(n,S=player)
          }
        }else{
          ix=indexCoalition(n,S=players) #identify index for corresponding coalition to bmTempRow without player i
          if(bm[ix,"cVal"]==0){
            ci[player]=ci[player]+bmTempRow["cVal"] # keep row
            #Write reference for row in bit matrix to bmRow
            bmTemp[j,"bmRow"]=indexCoalition(n,S=c(players,player))
          }else{
            rowsToBeRemoved=c(rowsToBeRemoved,j) #mark j for removal
          }
        }
      }
      if(!is.null(rowsToBeRemoved)){
        bmTemp=rbind(bmTemp)[-rowsToBeRemoved, , drop = FALSE] # remove rows
      }
    }else{
      bmTemp=NULL
    }
  } 
  return(bmTemp)
}



