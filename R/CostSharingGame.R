#' @name costSharingGameValue
#' @title Compute value of a coalition for a cost game
#' @description \strong{Coalition value for a cost sharing game:} \cr
#' For further information see \link{costSharingGame} 
#' @aliases costSharingGameValue
#' @export costSharingGameValue
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 14--16
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 667--668
#' @template param/S
#' @template param/Costs
#' @return Cost savings of coalition S as compared to singleton coalitions
#' @examples
#' library(CoopGame)
#' costSharingGameValue(S=c(1,2), Costs=c(2,2,2,3,3,3,4))
#' 
#' \donttest{
#' #Example with 3 students sharing an appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings when A and B share appartment
#' library(CoopGame)
#' costSharingGameValue(S=c(1,2),Costs=c(300,270,280,410,410,410,550))
#' #Output: 
#' #[1] 160
#' }
#'
costSharingGameValue<-function(S,Costs){
  paramCheckResult=getEmptyParamCheckResult()
  stopOnInvalidCoalitionS(paramCheckResult,S, n=getNumberOfPlayers(Costs))
  logiccostSharingGameValue(S,Costs)
}


#' @name costSharingGameVector
#' @title Compute game vector for a cost sharing game
#' @description \strong{Coalition vector for a cost sharing game:} \cr
#' For further information see \link{costSharingGame} 
#' @aliases costSharingGameVector
#' @export costSharingGameVector
#' @template author/JS
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 14--16
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 667--668
#' @template param/n
#' @template param/Costs
#' @return Game vector with cost-savings of each coalition S as compared to singleton coalitions.
#' @examples
#' library(CoopGame)
#' costSharingGameVector(n=3, Costs=c(2,2,2,3,3,3,4))
#' 
#' \donttest{
#' #Example with 3 students sharing an appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings for all combinations sharing appartments
#' library(CoopGame)
#' (v=costSharingGameVector(n=3, Costs=c(300,270,280,410,410,410,550)))
#' #Output: 
#' #[1]   0   0   0 160 170 140 300
#' }
#' 
costSharingGameVector<-function(n,Costs){
  bitMatrix = createBitMatrix(n)[,1:n];
  #initialize game vector
  A<-c()
  i<-1
  end<-((2^n)-1)
  
  while(i<=end){
    currCoal<-which(bitMatrix[i,]&1)  
    A[i] = costSharingGameValue(S=currCoal,Costs=Costs)
    i<-i+1
  }
  return(A)
}


logiccostSharingGameValue=function(S,Costs){
  # C same structure as game vector A therefore number of player can be determined by same structure
  numberOfPlayers=getNumberOfPlayers(Costs)
  
  #Saves costs for all players out of set S under the single coalition assumption  
  costsOfSingleCoaltions=0
  
  #Saves costs for coalition S
  costsOfCoaltionS=0
  
  #win of coalition S
  winOfCoalitionS=0
  
  #Determines possible savings/losses by taking part in coalition S for all players of S
  indexOfS=indexCoalition(n = numberOfPlayers, S)
  costsOfSingleCoaltions=sum(Costs[as.numeric(S)])
  costsOfCoaltionS=Costs[indexOfS]
  winOfCoalitionS=costsOfSingleCoaltions-costsOfCoaltionS
  
  return(winOfCoalitionS)
}



#' @title Construct a cost sharing game
#' @description \strong{Create a list containing 
#' all information about a specified cost sharing game:} \cr
#' The user may specify the cost function of a cost allocation
#' problem. A corresponding savings game will be calculated.
#' The savings game specified by the game vector \code{v} 
#' will work like an ordinary TU game.
#' @template author/JS
#' @template author/JA
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 14--16
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 667--668
#' @template param/n
#' @template param/Costs
#' @return  A list with three elements 
#' representing the specified cost sharing 
#' game (n, Costs, Game vector v)
#' @name costSharingGame
#' @export
#' @section Related Functions: 
#' \link{costSharingGameValue}, \link{costSharingGameVector}
#' @examples 
#' library(CoopGame)
#' costSharingGame(n=3, Costs=c(2,2,2,3,3,3,4))
#' 
#' \donttest{
#' #Example with 3 students sharing an appartment:
#' #-------------------------------
#' #| costs     |  A  |  B  |  C  |
#' #- -----------------------------
#' #|single     | 300 | 270 | 280 |
#' #|appartment |     |     |     |
#' #-------------------------------
#' #
#' #Appartment for 2 persons => costs: 410
#' #Appartment for 3 persons => costs: 550
#' 
#' #Savings for all combinations sharing appartments
#' library(CoopGame)
#' (vv <- costSharingGame(n=3, Costs=c(300,270,280,410,410,410,550)))
#' #Output:
#' #$n
#' #[1] 3
#' 
#' #$Costs
#' #[1] 300 270 280 410 410 410 550
#'
#' #$v
#' #[1]   0   0   0 160 170 140 300
#' }
#'
costSharingGame<-function(n,Costs){
  v = costSharingGameVector(n=n,Costs=Costs)
  retcostSharingGame=list(n=n,Costs=Costs,v=v)
  return(retcostSharingGame)
}

