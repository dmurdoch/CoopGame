#' @name banzhafValue
#' @title Compute Banzhaf value
#' @description banzhafValue computes the Banzhaf value for a specified TU game
#'              The Banzhaf value itself is an alternative to the Shapley value. \cr
#'              Conceptually, the Banzhaf value is very similar to the Shapley value. 
#'              Its main difference from the Shapley value is that the Banzhaf value is coalition
#'              based rather than permutation based. 
#'              Note that in general the Banzhaf vector is not efficient!
#'              In this sense this implementation of the Banzhaf value could also 
#'              be referred to as the non-normalized Banzhaf value, see formula (20.6) in 
#'              on p. 368 of the book by Hans Peters (2015).
#' @aliases banzhafValue nonNormalizedBanzhafValue
#' @export banzhafValue
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 367--370
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template cites/GAMBARELLI_SAGE_BANZHAF_2011
#' @templateVar GAMBARELLI_SAGE_BANZHAF_2011_P pp. 53--54
#' @template param/v
#' @return The return value is a numeric vector which contains the Banzhaf value for each player.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,2,1,4)
#' banzhafValue(v)
#' 
#' \donttest{
#' #Example from paper by Gambarelli (2011)
#' library(CoopGame)
#' v=c(0,0,0,1,2,1,3)
#' banzhafValue(v)
#' #[1] 1.25 0.75 1.25
#' }
#' 
banzhafValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_banzhafValue(paramCheckResult = paramCheckResult, v)
  A = v
  n = getNumberOfPlayers(A)
  banzhafFactor=1/(2^(n-1))
  bv=logicRawBanzhafValue(A)
  return(bv*banzhafFactor)
}

#' @name rawBanzhafValue
#' @title Compute raw Banzhaf Value
#' @description raw Banzhaf Value, i.e. the Banzhaf Value without the division by the scaling factor \eqn{2^{(n-1)}} 
#' @aliases rawBanzhafValue
#' @export rawBanzhafValue
#' @template author/JS
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template param/v
#' @return The return value is a numeric vector which contains the raw Banzhaf value for each player.
#' @examples 
#' library(CoopGame)
#' v = c(0,0,0,1,1,2,5)
#' rawBanzhafValue(v)
#' 
#' \donttest{
#' library(CoopGame)
#' v = c(0,0,0,2,2,3,5)
#' rawBanzhafValue(v)
#' #[1] 6 8 8
#' }
#' 
rawBanzhafValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_banzhafValue(paramCheckResult = paramCheckResult, v)
  A = v
  n = getNumberOfPlayers(A)
  bi = logicRawBanzhafValue(A)
  return(bi)
}

#' @name rawBanzhafIndex
#' @title Compute raw Banzhaf Index
#' @description Raw Banzhaf Index for a specified simple game, see 
#' formula (7.4) on p. 118 of the book by Chakravarty, Mitra and Sarkar
#' @aliases rawBanzhafIndex
#' @export rawBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template param/v
#' @return The return value is a numeric vector which contains the raw Banzhaf index for each player.
#' @examples 
#' library(CoopGame)
#' rawBanzhafIndex(apexGameVector(n=3, apexPlayer=1))
#' 
#' \donttest{
#' v<- apexGameVector(n = 4,apexPlayer=3)
#' rawBanzhafIndex(v)
#' #[1] 2 2 6 2
#' 
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' v=weightedVotingGameVector(n=3, w=c(50,49,1),q=51)
#' rawBanzhafIndex(v)
#' #[1] 3 1 1
#' 
#' v<-weightedVotingGameVector(n=3,w=c(50,30,20),q=c(67))
#' rawBanzhafIndex(v)
#' #[1] 3 1 1
#' }
#' 
rawBanzhafIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_banzhafValue(paramCheckResult = paramCheckResult, v)
  A = v
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no raw Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    n = getNumberOfPlayers(A)
    bi = logicRawBanzhafValue(A)
    return(bi)
  }
}



logicRawBanzhafValue=function(A){

  retVal=NULL
  bI=c()
  
  numberOfPlayers=getNumberOfPlayers(A)
  bm=createBitMatrix(n=numberOfPlayers,A)
  
  for(i in 1:numberOfPlayers){
    #Get all coalitions K where player i takes part
    K=bm[bm[,i]==1,]
    
    #Get all coalitions K \ {i}
    KwithoutI=K
    KwithoutI[,i]=0
    #set v({})=0
    KwithoutI[1,"cVal"]=0
    
    
    for(k in 2:nrow(K)){
      ix=indexCoalition(n=numberOfPlayers, S=getPlayersFromBMRow(KwithoutI[k,]))
      KwithoutI[k, "cVal"]=bm[ix,"cVal"]
    }
    sumMarginalContributions=sum(K[,"cVal"]-KwithoutI[,"cVal"])
    bI[i]=sumMarginalContributions#*banzhafFactor
  }
  return (bI)
}


#' @name drawNormalizedBanzhafIndex
#' @title draw normalized Banzhaf Index for 3 or 4 players
#' @description drawNormalizedBanzhafIndex draws the Banzhaf Value for 3 or 4 players. \cr
#'              Drawing any kind of Banzhaf values only makes sense from our point of view 
#'              for the normalized Banzhaf index for simple games, because 
#'              only in this case will the Banzhaf index be efficient.
#' @aliases drawNormalizedBanzhafIndex
#' @export drawNormalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 367--370
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template cites/BERTINI_STACH_SAGE_BANZHAF_2011
#' @templateVar BERTINI_STACH_SAGE_BANZHAF_2011_P pp. 54--55
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v<-weightedVotingGameVector(n=3,w=c(50,30,20),q=c(67))
#' drawNormalizedBanzhafIndex(v)
#' 
drawNormalizedBanzhafIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Banzhaf index"){
  A=v
  bv=normalizedBanzhafIndex(A);
  visualize(A, pointsToDraw=bv, holdOn=holdOn, colour = colour , label=label, name = name)
}

##normalized Banzhaf index
#' @name normalizedBanzhafIndex
#' @title Compute normalized Banzhaf index
#' @description Normalized Banzhaf index for a specified simple game, see 
#' formula (7.6) on p. 119 of the book by Chakravarty, Mitra and Sarkar 
#' @aliases normalizedBanzhafIndex
#' @export normalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 367--370
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template cites/BERTINI_STACH_SAGE_BANZHAF_2011
#' @templateVar BERTINI_STACH_SAGE_BANZHAF_2011_P pp. 54--55
#' @template param/v
#' @return The return value is a numeric vector which contains the normalized Banzhaf index for each player.
#' @examples 
#' library(CoopGame)
#' normalizedBanzhafIndex(dictatorGameVector(n=3, dictator=1))
#' 
#' \donttest{
#' library(CoopGame)
#' v<-weightedVotingGameVector(n=4,w=c(8,6,4,2),q=c(12))
#' normalizedBanzhafIndex(v)
#' #[1] 0.41666667 0.25000000 0.25000000 0.08333333
#'
#' library(CoopGame) 
#' v<- apexGameVector(n = 4,apexPlayer=3)
#' normalizedBanzhafIndex(v)
#' #[1] 0.1666667 0.1666667 0.5000000 0.1666667
#' 
#' library(CoopGame)
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' v=weightedVotingGameVector(n=3, w=c(50,49,1),q=51)
#' normalizedBanzhafIndex(v)
#' #[1] 0.6 0.2 0.2
#' 
#' library(CoopGame)
#' v<-weightedVotingGameVector(n=3,w=c(50,30,20),q=c(67))
#' normalizedBanzhafIndex(v)
#' #[1] 0.6 0.2 0.2
#' }
#' 
normalizedBanzhafIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_banzhafValue(paramCheckResult = paramCheckResult, v)
  A=v
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Normalized Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    bidcs=logicRawBanzhafValue(A)
    bidcs=bidcs/sum(bidcs)
    return(bidcs)
  }
}

## non-normalized Banzhaf index
#' @name nonNormalizedBanzhafIndex
#' @title Compute non-normalized Banzhaf index
#' @description non-normalized Banzhaf index for a specified simple game, see 
#' formula (7.5) on p. 119 of the book by Chakravarty, Mitra and Sarkar
#' @aliases nonNormalizedBanzhafIndex
#' @export nonNormalizedBanzhafIndex
#' @template author/JA
#' @template author/JS
#' @template cites/CHAKRAVARTY_ET_AL_2015
#' @templateVar CHAKRAVARTY_ET_AL_2015_P pp. 118--119
#' @template cites/BERTINI_STACH_SAGE_BANZHAF_2011
#' @templateVar BERTINI_STACH_SAGE_BANZHAF_2011_P pp. 54--55
#' @template param/v
#' @return The return value is a vector which contains the non-normalized Banzhaf index for each player.
#' @examples 
#' library(CoopGame)
#' nonNormalizedBanzhafIndex(dictatorGameVector(n=3, dictator=1))
#' 
#' \donttest{
#' library(CoopGame)
#' v<-weightedVotingGameVector(n=4,w=c(8,6,4,2),q=c(12))
#' nonNormalizedBanzhafIndex(v)
#' #[1] 0.625 0.375 0.375 0.125
#' 
#' library(CoopGame)
#' v<- apexGameVector(n = 4,apexPlayer=3)
#' nonNormalizedBanzhafIndex(v)
#' #[1] 0.25 0.25 0.75 0.25
#' 
#' library(CoopGame)
#' #N=c(1,2,3), w=(50,49,1), q=51   
#' v=weightedVotingGameVector(n=3, w=c(50,49,1),q=51)
#' nonNormalizedBanzhafIndex(v)
#' #[1] 0.75 0.25 0.25
#'
#' library(CoopGame) 
#' v<-weightedVotingGameVector(n=3,w=c(50,30,20),q=c(67))
#' nonNormalizedBanzhafIndex(v)
#' #[1] 0.75 0.25 0.25
#' }
#' 
nonNormalizedBanzhafIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_shapleyValue(paramCheckResult = paramCheckResult, v)
  A = v
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no non-normalized Banzhaf Index can be retrieved")
    return(NULL)
  }
  else
  {
    bidcs=logicRawBanzhafValue(A)
    facBv=2^(getNumberOfPlayers(A)-1)
    bidcs=bidcs/facBv
    return(bidcs)
  }
}

#' @name normalizedBanzhafValue
#' @title Compute normalized Banzhaf value
#' @description normalizedBanzhafValue computes the normalized Banzhaf value 
#' for a specified TU game. The corresponding formula can e.g. be found 
#' in the article by Stach (2017), p. 77.
#' @aliases normalizedBanzhafValue
#' @export normalizedBanzhafValue
#' @template author/JS
#' @template cites/GAMBARELLI_SAGE_BANZHAF_2011
#' @templateVar GAMBARELLI_SAGE_BANZHAF_2011_P pp. 53--54
#' @template cites/STACH_SUBCOAL_2017
#' @templateVar STACH_SUBCOAL_2017_P pp. 74--86
#' @template param/v 
#' @return The return value is a numeric vector which contains the normalized Banzhaf value for each player.
#' @examples
#' library(CoopGame)
#' normalizedBanzhafValue(c(0,0,0,1,2,3,6))
#' 
#' \donttest{
#' #Example from paper by Gambarelli (2011)
#' library(CoopGame)
#' v=c(0,0,0,1,2,1,3)
#' normalizedBanzhafValue(v)
#' #[1] 1.1538462 0.6923077 1.1538462
#' #Expected Result: 15/13  9/13  15/13
#' }
#' 
normalizedBanzhafValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_banzhafValue(paramCheckResult = paramCheckResult, v)
  A = v
  n = getNumberOfPlayers(A)
  grandCoalValue = A[length(A)]
  rawVal = rawBanzhafValue(A)
  retVal = (grandCoalValue/sum(rawVal)) * rawVal
  return(retVal)
}

#' @name drawNormalizedBanzhafValue
#' @title draw normalized Banzhaf value for 3 or 4 players
#' @description drawNormalizedBanzhafValue draws the Banzhaf Value for 3 or 4 players. \cr
#'              Drawing any kind of Banzhaf values only makes sense from our point of view 
#'              for the normalized Banzhaf value, because 
#'              only in this case will the Banzhaf value be efficient.
#' @aliases drawNormalizedBanzhafValue
#' @export drawNormalizedBanzhafValue
#' @template author/JS
#' @template cites/GAMBARELLI_SAGE_BANZHAF_2011
#' @templateVar GAMBARELLI_SAGE_BANZHAF_2011_P pp. 53--54
#' @template cites/STACH_SUBCOAL_2017
#' @templateVar STACH_SUBCOAL_2017_P pp. 74--86
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' drawNormalizedBanzhafValue(c(0,0,0,1,2,3,6))
#' 
#' \donttest{
#' #Example from paper by Gambarelli (2011)
#' library(CoopGame)
#' v=c(0,0,0,1,2,1,3)
#' drawNormalizedBanzhafValue(v)
#' }
#' 
drawNormalizedBanzhafValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Normalized Banzhaf value"){
  A=v
  bv=normalizedBanzhafValue(A);
  visualize(A, pointsToDraw=bv, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_banzhafValue=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
