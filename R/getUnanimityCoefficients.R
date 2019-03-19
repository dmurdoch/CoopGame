#' @name getUnanimityCoefficients
#' @title Compute unanimity coefficients of game
#' @description getUnanimityCoefficients calculates to unanimity coefficients of 
#' a specified TU game. Note that the unanimity coefficients are also frequently 
#' referred to as Harsanyi dividends in the literature.
#' @aliases getUnanimityCoefficients getHarsanyiDividends
#' @export getUnanimityCoefficients
#' @template author/JA
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P p. 153
#' @template cites/GILLES_2015
#' @templateVar GILLES_2015_P pp. 15--17 
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template param/v 
#' @return numeric vector containing the unanimity coefficients
#' @examples 
#' library(CoopGame)
#' getUnanimityCoefficients(c(0,0,0,60,48,30,72))

getUnanimityCoefficients=function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_getUnanimityCoefficients(paramCheckResult, v)
  A = v
  n=getNumberOfPlayers(A)
  N=length(A);
  bm=createBitMatrix(n,A);
  unanimityCoefficients=c();

  for(i in 1:N){
    bmIndices=getCorrespondingRows(bm,n,index=i);
    signs=getSigns(bm,n, bmIndices);
    #this line realises formula in Harald Wiese "Kooperative Spieltheorie" p.124
    lambda_T_v=sum(bm[bmIndices,"cVal"]*signs);
    unanimityCoefficients=c(unanimityCoefficients,lambda_T_v);
  }
  return(unanimityCoefficients);

}

initialParamCheck_getUnanimityCoefficients=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

#Function figures out only signs neccessary for calculating unanimity coefficients
#with formula provided in book by Gilles p. 15 f. subject to the referenced rows of bit matrix
getSigns<-function(bm,n, indices){
  
  cardOfKs=c(apply(bm[indices,,drop=FALSE],1,function(x){sum(x[1:n])}));
  #Check how many entries in vector indices are true corresponds to length of considered entries
  numberOfRows=length(which(indices&1));
  cardOfT=cardOfKs[numberOfRows];
  signs=unlist(lapply(cardOfKs,function(x){(-1)^(cardOfT-x)}));
  return(signs);
}

#Function figures out which rows of bit matrix are neccessary for calculating unanimity coefficients
#with formula provided in book by Gilles p. 15 f. for a certain coalition T (referenced by parameters index and n)
getCorrespondingRows<-function(bm,n,index){
  N=2^n-1;
  #Get all players who are not contained in referenced row of bit matrix
  playersNotInvolved=!as.vector(bm[index,1:n]);
  #pattern matrix for boolean arithmetic
  pm=matrix(playersNotInvolved,nrow=index,ncol=n,byrow = TRUE);
  #AND binary and pattern matrix
  resolvedPlayerMatrix=bm[1:index,1:n]&pm
  #Get indices where the special players are not involved
  indices=!apply(resolvedPlayerMatrix,1,function(x){any(x)})
  indices=c(indices,rep(FALSE,N-index))
  return(indices);
}
