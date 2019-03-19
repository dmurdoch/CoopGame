#' @name deeganPackelIndex
#' @title Compute Deegan-Packel index
#' @description deeganPackelIndex calculates the Deegan-Packel index for simple games
#' @aliases deeganPackelIndex
#' @export deeganPackelIndex
#' @template author/JA
#' @template author/MM
#' @template author/JS
#' @template cites/DEEGAN_ET_PACKEL_1978
#' @templateVar DEEGAN_ET_PACKEL_1978_P pp. 151--161
#' @template cites/HOLLER_ET_ILLING_2006
#' @templateVar HOLLER_ET_ILLING_2006_P pp. 323--324
#' @template param/v
#' @return Deegan-Packel index for a specified simple game
#' @examples
#' library(CoopGame)
#' deeganPackelIndex(c(0,0,0,0,1,0,1))
#' 
#' \donttest{
#' #Example from HOLLER & ILLING (2006), chapter 6.3.3
#' #Expected result: dpv=(18/60,9/60,11/60,11/60,11/60)
#' library(CoopGame)
#' v1=weightedVotingGameVector(n = 5, w=c(35,20,15,15,15), q=51)
#' deeganPackelIndex(v1)
#' #Output (as expected, see HOLLER & ILLING chapter 6.3.3) :
#' #[1] 0.3000000 0.1500000 0.1833333 0.1833333 0.1833333
#'}
#'
deeganPackelIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_deeganPackelIndex(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Deegan Packel Index can be retrieved.")
  }
  else
  {
    n=getNumberOfPlayers(A)
  
  
    #Determine minimum winning coalitions in bit matrix
    minimumWinningCoalitions=getMinimumWinningCoalitions(A)
  
    #determine cardinality of minimum winning coalitions in bit matrix
    cardMWC=nrow(minimumWinningCoalitions)
    dpv=c()
  
    if(cardMWC==0){
      #if there are no minimum winning coalitions (e.g for A=rep(0,15))
      #each players gets a deegan packel index of 0
      # Well, actually CoopGame is not supposed to accept null games ...:-)
      dpv=rep(0,n)
    }else{
      #if there are minimum winning coalitions, they determine Deegan Packel Index
  
      #Determine constant factor which is the same for each player
      #=> 1 divided by number of minimum winning coalitions
      constFactor=1/cardMWC
  
      #Loop over all players and determine each player's specific deegan packel index
      for(i in 1:n){
        sumvKDivk=0
        minimumWinningCoalitionsOfI=minimumWinningCoalitions[minimumWinningCoalitions[,i]==1,,drop=FALSE]
        if(nrow(minimumWinningCoalitionsOfI)!=0){
          #Loop over all minimum winning coalitions of player i
          for(j in 1:nrow(minimumWinningCoalitionsOfI)){
            #determine k as sum of players involved in current row of minimumWinningCoalitionsOfI
            k=sum(minimumWinningCoalitionsOfI[j,1:n] == 1)
            #determine each player's specific part of the deegan packel equation
            sumvKDivk=sumvKDivk+minimumWinningCoalitionsOfI[j,"cVal"]/k
          }
  
          #Deegan-Packel Index is calculated by multiplying constant factor and specific part of each player
          dpv[i]=constFactor*(sumvKDivk)
        }else{
          #if player i is not involved in any minimum winning coalition he receives 0 
          dpv[i]=0
        }
      }
    }
    retVal = dpv
  }
  return(retVal)
}


#' @name drawDeeganPackelIndex
#' @title draw Deegan-Packel index for 3 or 4 players
#' @description drawDeeganPackelIndex draws the Deegan-Packel index for 3 or 4 players.
#' @aliases drawDeeganPackelIndex
#' @export drawDeeganPackelIndex
#' @template author/JS
#' @template cites/DEEGAN_ET_PACKEL_1978
#' @templateVar DEEGAN_ET_PACKEL_1978_P pp. 151--161
#' @template cites/HOLLER_ET_ILLING_2006
#' @templateVar HOLLER_ET_ILLING_2006_P pp. 323--324
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawDeeganPackelIndex(v) 
drawDeeganPackelIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Deegan Packel Index"){
  A=v
  sm=deeganPackelIndex(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_deeganPackelIndex=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

