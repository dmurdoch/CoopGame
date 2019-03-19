#' @name isBalancedGame
#' @title Check if game is balanced
#' @description Checks if a game is balanced. A game is 
#' balanced if the core is a nonempty set. 
#' @aliases isBalancedGame
#' @export isBalancedGame
#' @template author/JS
#' @template cites/BONDAREVA_1963
#' @templateVar BONDAREVA_1963_P pp. 119--139
#' @template cites/SHAPLEY_1967
#' @templateVar SHAPLEY_1967_P pp. 453--460
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 27--32
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 691--702
#' @template cites/SLIKKER_ET_NOUWELAND_2001
#' @templateVar SLIKKER_ET_NOUWELAND_2001_P pp. 6--7
#' @template cites/OSBORNE_ET_RUBINSTEIN_1994
#' @templateVar OSBORNE_ET_RUBINSTEIN_1994_P pp. 262--263
#' @template param/v
#' @return \code{TRUE} if the game is balanced, else \code{FALSE}
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,40,50,20,100)
#' isBalancedGame(v)
#' 
#' \donttest{
#' #Example of an unbalanced game with 3 players
#' library(CoopGame)
#' v=c(1,1,1,2,3,4,3)
#' isBalancedGame(v)
#' 
#' #Example of an unbalanced game with 4 players
#' library(CoopGame)
#' v=c(0,0,0,0,1,0,0,0,0,3,3,3,3,3,4)
#' isBalancedGame(v)
#' 
#' #Example of a balanced game with 4 players
#' library(CoopGame)
#' v= c(0,0,0,0,1,0,0,0,0,2,2,2,2,2,4)
#' isBalancedGame(v)
#' }
#' 
isBalancedGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isBalancedGame(paramCheckResult = paramCheckResult, v)
  A = v
  #Return value 
  retVal=T
  cv = coreVertices(A)
  if (nrow(cv) == 0)
  { 
    retVal = F
  }
  return(retVal)
}

initialParamCheck_isBalancedGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
