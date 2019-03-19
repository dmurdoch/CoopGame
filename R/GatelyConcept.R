#' @name gatelyValue
#' @title Compute Gately point
#' @description gatelyValue calculates the Gately point for a given TU game
#' @aliases gatelyValue gatelyPoint
#' @export gatelyValue
#' @template author/JS
#' @template cites/GATELY_1974
#' @templateVar GATELY_1974_P pp. 195--208
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P  pp. 455--456
#' @template param/v
#' @return Gately point of the TU game or NULL in case the Gately point is not defined
#' @examples
#' library(CoopGame)
#' gatelyValue(c(0,0,0,1,1,1,3.5))
#' 
#' \donttest{
#' library(CoopGame)
#' v=c(0,0,0,4,0,3,6)
#' gatelyValue(v)
#'
#' #Output (18/11,36/11,12/11):
#' #1.636364 3.272727 1.090909
#' 
#' #Example from original paper by Gately (1974)
#' library(CoopGame)
#' v=c(0,0,0,1170,770,210,1530)
#' gatelyValue(v)
#' 
#' #Output:
#' #827.7049 476.5574 225.7377 
#' }
#' 
gatelyValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_GatelyValue(paramCheckResult = paramCheckResult, v)
  A = v 
  # assign number of coalitions
  N <- length(A)

  # assign number of players
  n <- getNumberOfPlayers(A)
  
  gatelyValue <- NULL
  
  if(isDegenerateGame(A))
  {
    gatelyValue <- A[1:n]
  }
  else if(isWeaklyConstantSumGame(A)){
    print("Gately point does not exist as game is weakly constant-sum.")
    gatelyValue <- NULL
  }
  else if(!isEssentialGame(A)){
    print("Gately point does not exist as singleton coalitions sum up to more than the grand coalition.")
    gatelyValue <- NULL
  }
  else{
    eptd=equalPropensityToDisrupt(A,k=1)
    singletonCoalitions <- A[1:n]
    utopiaPayoffs <- getUtopiaPayoff(A)
    vecTrue <- rep(TRUE,n)
    vecFalse <- rep(FALSE,n)
    compareBooleans <- (singletonCoalitions <= utopiaPayoffs)
    myBoolean1 <- ((all.equal(compareBooleans, vecTrue)) == TRUE)
    myBoolean2 <- ((all.equal(compareBooleans, vecFalse)) == TRUE)
    if ((myBoolean1 == TRUE) | (myBoolean2 == TRUE))
    {
      sumSingletonCoalitions = sum(singletonCoalitions)
      sumUtopiaPayoffs = sum(utopiaPayoffs)
      diffSums = sumUtopiaPayoffs - sumSingletonCoalitions
      for(i in 1:n)
      {
        gatelyValue[i] = A[i] + (A[N] - sumSingletonCoalitions) * (utopiaPayoffs[i] - singletonCoalitions[i])/diffSums
      }
    }
    else
    {
      print("Gately point does not exist as Gately formula would not provide an imputation.")
      gatelyValue <- NULL
    }
    
  }
  names(gatelyValue)<-NULL
  return(gatelyValue)
}


#' @name equalPropensityToDisrupt
#' @title Compute equal propensity to disrupt
#' @description equalPropensityToDisrupt calculates the 
#' equal propensity to disrupt for a TU game with n players 
#' and a specified coalition size k. See the original paper by 
#' Littlechild & Vaidya (1976) for the formula with general k and 
#' the paper by Staudacher & Anwander (2019) for the specific expression 
#' for k=1 and interpretations of the equal propensity to disrupt.
#' @aliases equalPropensityToDisrupt
#' @export equalPropensityToDisrupt
#' @template author/JA
#' @template author/JS
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template param/v
#' @param k is the fixed coalition size to be considered when calculating the equal propensity to disrupt
#' @return the value for the equal propensity to disrupt
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,4,0,3,6)
#' equalPropensityToDisrupt(v, k=1)
#'

equalPropensityToDisrupt<-function(v,k=1){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_equalPropensityToDisrupt(paramCheckResult,v)
  A = v
  n=as.numeric(getNumberOfPlayers(A))
  bm=createBitMatrix(n)

  bm[,"cVal"]=apply(bm,1,sum)
  pos_n_min_k=which(bm[,"cVal"]==(n-k))
  pos_k=which(bm[,"cVal"]==(k))


  vN=A[length(A)]

  factorNumerator=factorial(n-1)/(factorial(k)*factorial(n-k-1))
  sum_N_min_S=sum(A[pos_n_min_k])

  factorDenominator=factorial(n-1)/(factorial(k-1)*factorial(n-k))
  sum_S=sum(A[pos_k])

  numerator=factorNumerator*vN - sum_N_min_S
  denominator=factorDenominator*vN - sum_S

  if(denominator==0){
    dk=Inf
  }else{
    dk=numerator/denominator
  }

  return(dk)

}

initialParamCheck_equalPropensityToDisrupt<-function(paramCheckResult,A){
  stopOnInvalidGameVector(paramCheckResult,A)
}


#' @name propensityToDisrupt
#' @title Compute propensity to disrupt
#' @description propensityToDisrupt for calculating the propensity of disrupt for game vector v, an allocation x and a specified coalition S
#' @aliases propensityToDisrupt
#' @export propensityToDisrupt
#' @template author/JA
#' @template author/JS
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template param/v
#' @template param/x
#' @template param/S
#' @return propensity to disrupt as numerical value
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,4,0,3,6)
#' x=c(2,3,1)
#' propensityToDisrupt(v,x,S=c(1))
#' 
propensityToDisrupt<-function(v,x,S){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_propensityToDisrupt(paramCheckResult,v, x, S)
  A = v
  retVal = 0
  
  n=as.numeric(getNumberOfPlayers(A))
  N=2^n
  # return 0 for grand coalition
  if (length(S) < n)
  {
    N_min_S=c(1:n)[-S]
  
    nominator=(sum(x[N_min_S])-A[indexCoalition(n,N_min_S)])
    denominator=(sum(x[S])-A[indexCoalition(n,S)])
    di_x=nominator/denominator
  
    # if(is.nan(di_x)){
    #   di_x=0
    # }
    retVal = di_x
 }
 return(retVal)
}

initialParamCheck_propensityToDisrupt<-function(paramCheckResult,A,x,S){
  stopOnInvalidGameVector(paramCheckResult,A)
  stopOnInvalidCoalitionS(paramCheckResult, S)
  stopOnInvalidAllocation(paramCheckResult, x)
}

#' @name getVectorOfPropensitiesToDisrupt
#' @title Compute vector of propensities to disrupt
#' @description getVectorOfPropensitiesToDisrupt computes a vector of 
#' propensities to disrupt for game vector v and an allocation x
#' @aliases getVectorOfPropensitiesToDisrupt
#' @export getVectorOfPropensitiesToDisrupt
#' @template author/JS
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template param/v
#' @template param/x
#' @return a numerical vector of propensities to disrupt at a given allocation \code{x}
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,4,0,3,6)
#' x=c(2,3,1)
#' getVectorOfPropensitiesToDisrupt(v,x)
#'

getVectorOfPropensitiesToDisrupt<-function(v,x){
  paramCheckResult=getEmptyParamCheckResult()
  A = v
  n=getNumberOfPlayers(A);
  propensitiesVector = numeric(length(A))
  for (i in 1:length(A))
  {
    S = as.vector(getPlayersFromIndex(n,i))
    propensitiesVector[i] = propensityToDisrupt(A,x,S)
  }
  # Note that vector of propensities to disrupt set to zero for grand coalition
  
  return(propensitiesVector)
}


#' @name drawGatelyValue
#' @title draw Gately point for 3 or 4 players
#' @description drawGatelyValue draws the Gately point for 3 or 4 players.
#' @aliases drawGatelyValue
#' @export drawGatelyValue
#' @template author/JA
#' @template author/JS
#' @template cites/GATELY_1974
#' @templateVar GATELY_1974_P pp. 195--208
#' @template cites/STAUDACHER_ET_ANWANDER_2019
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template cites/NARAHARI_2015
#' @templateVar NARAHARI_2015_P  pp. 455--456
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' drawGatelyValue(c(0,0,0,1,1,1,3.5))
#' 
#' \donttest{
#' #Example from original paper by Gately (1974):
#' library(CoopGame)
#' v=c(0,0,0,1170,770,210,1530)
#' drawGatelyValue(v)
#' }
drawGatelyValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Gately Value"){
  A=v
  gatelyValue=gatelyValue(A);
  visualize(A, pointsToDraw=gatelyValue, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_GatelyValue=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
