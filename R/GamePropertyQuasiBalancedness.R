#' @name isQuasiBalancedGame
#' @title Check if game is quasi-balanced
#' @description Checks if a TU game is quasi-balanced. \cr
#' A TU game is quasi-balanced if \cr
#' a) the components of its minimal rights vector are 
#' less or equal than the components of its utopia payoff 
#' vector \cr
#' and \cr
#' b) the sum of the components of its minimal rights vector 
#' is less or equal the value of the grand coalition which 
#' in turn is less or equal than the sum of the components 
#' of its utopia payoff vector. \cr
#' Note that any balanced game is also quasi-balanced, 
#' but not vice versa. \cr
#' Note that the quasi-balanced games are those games with 
#' a non-empty core cover.
#' Note also that quasi-balancedness is sometimes in the 
#' literature also referred to as compromise-admissibility.
#' @aliases isQuasiBalancedGame
#' @export isQuasiBalancedGame
#' @template author/JA
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 31 
#' @template param/v
#' @return \code{TRUE} if the game is quasi-balanced, else \code{FALSE}.
#' @examples 
#' library(CoopGame)
#' isQuasiBalancedGame(c(0,0,0,1,1,1,4))
#' 
#' \donttest{
#' #Example of a quasi-balanced game:
#' library(CoopGame)
#' v1=c(1,1,2,6,8,14,16)
#' isQuasiBalancedGame(v1)
#' 
#' #Example of a game which is not quasi-balanced:
#' library(CoopGame)
#' v2=c(1:7)
#' isQuasiBalancedGame(v2)
#' }
#' 
isQuasiBalancedGame<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_isQuasiBalancedGame(paramCheckResult = paramCheckResult, v)
  A = v
  retBoolQB=TRUE
  N=length(A)
  n=getNumberOfPlayers(A)
  
  mc=matrix(nrow=2,ncol=n)
  rownames(mc)<-c("M","m")
  mc["M",]<-getUtopiaPayoff(A)
  mc["m",]<-getMinimalRights(A)
  
  sum_m=sum(mc["m",])
  sum_M=sum(mc["M",])
  
  if(!all(mc["m",]<=mc["M",])){
    retBoolQB=FALSE
  }
  if(!(sum_m<=A[N])){
    retBoolQB=FALSE
  }
  if(!(A[N]<=sum_M)){
    retBoolQB=FALSE
  }
  return(retBoolQB)
}

initialParamCheck_isQuasiBalancedGame=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
