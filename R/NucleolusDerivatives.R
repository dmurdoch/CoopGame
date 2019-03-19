#' @name nucleolus
#' @title Compute nucleolus 
#' @description Computes the nucleolus of a TU game with a non-empty imputation set and n players.
#' @aliases nucleolus 
#' @import rcdd
#' @export nucleolus
#' @template author/JS
#' @template cites/SCHMEIDLER_1969
#' @templateVar SCHMEIDLER_1969_P pp. 1163--1170
#' @template cites/KOHLBERG_1971
#' @templateVar KOHLBERG_1971_P pp. 62--66
#' @template cites/KOPELOWITZ_1967
#' @template cites/MEGIDDO_1974
#' @templateVar MEGIDDO_1974_P pp. 355--358
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 82--86
#' @template param/v
#' @return Numeric vector of length n representing the nucleolus.
#' @examples
#' library(CoopGame)
#' nucleolus(c(1, 1, 1, 2, 3, 4, 5))
#'
#' \donttest{
#' library(CoopGame)
#' nucleolus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21))
#' #[1] 3.5 4.5 5.5 7.5
#' 
#' #Final example:
#' #Estate division problem from Babylonian Talmud with E=300,
#' #see e.g. seminal paper by Aumann & Maschler from 1985 on
#' #'Game Theoretic Analysis of a Bankruptcy Problem from the Talmud'
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=300)
#' nucleolus(v)
#' #[1]  50 100 150
#' }
#' 
nucleolus<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
  A = v  
  isImp = T
  isNT = T
  N <- length(A)
  n <- getNumberOfPlayers(A)
  excess = rep(1,N-1)
  excessR = cbind(rep(0,N-1), -A[1:N-1], createBitMatrix(n,excess)[1:N-1,])
  furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
  furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
  return(logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT))
}

logicNucleolusDerivatives<-function(A, ER, FR, isImput, isNType){
  retVal = NULL
  N <- length(A)
  n <- getNumberOfPlayers(A)
  hasConverged = FALSE
  
  if (isDegenerateGame(A) && isImput)
  {
    retVal = A[1:n]
    hasConverged = TRUE
  }
  
  if ((!(isDegenerateGame(A))) && (!(isEssentialGame(A))) && isImput)
  {
    print("Solution concept is only provided for games with a nonempty imputation set.")
    retVal = NULL
    hasConverged = TRUE
  }
  
  nRowEx = nrow(ER)
  objCoeff <- c(rep(0,n),1)
  individualVariables <- rep(F, n)
  nEqual = 1
  loop = 0
  
  while ((loop < N) && (!hasConverged)){
    
    loop = loop + 1
    outLPCDD <- lpcdd(rbind(ER,FR),objCoeff)
    #
    if (!(outLPCDD$solution.type=="Optimal"))
    {
      print("Problem in Linear Program for nucleolus derivative solution.")
      print("Aborting!")
      retVal = NULL
      break
    }
    primalSolution <- outLPCDD$primal.solution
    dualSolution <- outLPCDD$dual.solution
    newObjValue <- outLPCDD$optimal.value
  
    # Determine positions to be updated
    #
    posUpdated = which((abs(dualSolution) >= 1e-14))
    for (position in 1:length(posUpdated))
    {
      rowUp = posUpdated[position]
      if ((rowUp <= nRowEx) && (ER[rowUp,1] ==0))
      {
        ER[rowUp,1] = 1
        ER[rowUp,2] = ER[rowUp,2] + ER[rowUp,ncol(ER)] * newObjValue
        ER[rowUp, ncol(ER)] = 0
        nEqual = nEqual + 1
      }
      if (isNType && (rowUp <= n))
      {
        individualVariables[rowUp] = T
      }
    }   
    #
    # Check if last row in ER is all zeros
    if (all((ER[,ncol(ER)]==0))){   
      hasConverged = TRUE
      retVal = primalSolution[-length(primalSolution)]
      break
    }
    # Check for individualVariables
    if(isNType && (sum(individualVariables)>=(n-1))) 
    {
      hasConverged = TRUE
      retVal = primalSolution[-length(primalSolution)]
      break
    }
    
    # Check if first row in ER is all ones
    subVec = ER[,1]
    if (sum(subVec)>=(nRowEx-1)) {
      hasConverged = TRUE
      retVal = primalSolution[-length(primalSolution)]
      break
    }
    
    # Computation must not take more than n steps
    if ((loop >= (n-1)) || (nEqual >= (n+1)))
    {
      vRep = scdd(rbind(ER[ER[,1]==1,],FR)[,1:(ncol(ER)-1)])
      VectorCounter = length(vRep[[1]]) / (n + 2)
      OutcomeVector = vRep[[1]][(VectorCounter * 2 + 1):(length(vRep[[1]]))]
      ResultMatrix = matrix(OutcomeVector, VectorCounter, n)   
      if (nrow(ResultMatrix) == 1)
      {
        hasConverged = TRUE
        retVal = as.vector(ResultMatrix)
        break
      }
    }
  }
  return(retVal)
}


#' @name Prenucleolus
#' @title Compute prenucleolus
#' @description Computes the prenucleolus of a TU game with n players.
#' @aliases prenucleolus 
#' @import rcdd
#' @export prenucleolus
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 107--132
#' @template param/v
#' @return Numeric vector of length n representing the prenucleolus.
#' @examples
#' library(CoopGame)
#' prenucleolus(c(1, 1, 1, 2, 3, 4, 5))
#' 
#' \donttest{
#' #Example 5.5.12 from Peleg/Sudhoelter, p. 96
#' library(CoopGame)
#' prenucleolus(c(0,0,0,10,0,0,2))
#' #Output
#' #[1]  3  3 -4
#' #In the above example nucleolus and prenucleolus do not coincide!
#' 
#' library(CoopGame)
#' prenucleolus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21))
#' # [1] 3.5 4.5 5.5 7.5
#' 
#' #Final example:
#' #Estate division problem from Babylonian Talmud with E=200,
#' #see e.g. seminal paper by Aumann & Maschler from 1985 on
#' #'Game Theoretic Analysis of a Bankruptcy Problem from the Talmud'
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=200)
#' prenucleolus(v)
#' #[1]  50 75 75
#' #Note that nucleolus and prenucleolus need to coincide for the above game
#' }
#' 
prenucleolus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    isImp = F
    isNT = T
    N <- length(A)
    n <- getNumberOfPlayers(A)
    excess = rep(1,N-1)
    excessR = cbind(rep(0,N-1), -A[1:N-1], createBitMatrix(n,excess)[1:N-1,])
    furtherR = c(1, A[N], rep(-1,n),0)
    return(logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT))
}

#' @name perCapitaNucleolus
#' @title Compute per capita nucleolus
#' @description perCapitaNucleolus calculates the per capita nucleolus for 
#' a TU game with a non-empty imputation set specified by a game vector.
#' @aliases perCapitaNucleolus 
#' @import rcdd
#' @export perCapitaNucleolus
#' @template author/JS
#' @template cites/YOUNG_1985
#' @templateVar YOUNG_1985_P pp. 65--72
#' @template param/v
#' @return per capita nucleolus for a specified TU game with n players
#' @examples
#' library(CoopGame)
#' perCapitaNucleolus(c(1, 1, 1, 2, 3, 4, 5))
#' 
#' \donttest{
#' #Example from YOUNG 1985, p. 68
#' v<-costSharingGameVector(n=3,C=c(15,20,55,35,61,65,78))
#' perCapitaNucleolus(v)
#' #[1]  0.6666667  1.1666667 10.1666667
#' }
#' 
perCapitaNucleolus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    isImp = T
    isNT = T
    N <- length(A)
    n <- getNumberOfPlayers(A)
    excess = as.vector(cardinalityGameVector(n)[1:N-1])
    excessR = cbind(rep(0,N-1), -A[1:N-1], createBitMatrix(n,excess)[1:N-1,])
    furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
    furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
    return(logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT))
}


#' @name proportionalNucleolus
#' @title Compute proportional nucleolus 
#' @description proportionalNucleolus calculates the proportional 
#' nucleolus for a TU game with a non-empty imputation set and 
#' n players specified by game vector.
#' @aliases proportionalNucleolus 
#' @import rcdd
#' @export proportionalNucleolus
#' @template author/JS
#' @template cites/YOUNG_ET_AL_1982
#' @templateVar YOUNG_ET_AL_1982_P pp. 463--475
#' @template param/v
#' @return proportional nucleolus for specified TU game with n players
#' @examples
#' library(CoopGame)
#' v<-c(0,0,0,48,60,72,140)
#' proportionalNucleolus(v)
#' 
proportionalNucleolus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    retValue = NULL
    if(!isNonnegativeGame(A)){
      print("Game is not nonnegative. Therefore we do not compute the proportional nucleolus.")
    }
    else
    {
      isImp = T
      isNT = T
      N <- length(A)
      n <- getNumberOfPlayers(A)
      excess = A[1:N-1]
      excessR = cbind(rep(0,N-1), -A[1:N-1], createBitMatrix(n,excess)[1:N-1,])
      furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
      furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
      retValue = logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT)
    }
    return(retValue)
}


#' @name disruptionNucleolus
#' @title Compute disruption nucleolus
#' @description Computes the disruption nucleolus of a balanced TU game with n players
#' @aliases disruptionNucleolus 
#' @import rcdd
#' @export disruptionNucleolus
#' @template author/JS
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template param/v
#' @return Numeric vector of length \code{n} 
#' representing the disruption nucleolus of the specified TU game
#' @examples
#' library(CoopGame)
#' v<-c(0, 0, 0, 1, 1, 0, 1)
#' disruptionNucleolus(v)
#' 
#' \donttest{
#' library(CoopGame)
#' exampleVector<-c(0,0,0,0,2,3,4,1,3,2,8,11,6.5,9.5,14)
#' disruptionNucleolus(exampleVector)
#' #[1] 3.193548 4.754839 2.129032 3.922581
#' }
#' 
disruptionNucleolus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    retValue = NULL
    if(!isBalancedGame(A)){
      print("Disruption nucleolus is only provided for games with a non-empty core.")
    }
    else {
      isImp = T
      isNT = T
      N <- length(A)
      n <- getNumberOfPlayers(A)
      tfac=sapply(1:((N-1)/2),function(ix){A[N]-A[ix]-A[N-ix]})
      tfac=c(tfac,rev(tfac),0)
      excessR = cbind(rep(0,N-1), -A[1:N-1], createBitMatrix(n,tfac)[1:N-1,])
      furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
      furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
      furtherR = rbind(furtherR, c(0, -1e-15, rep(0,n), -1))
      furtherR = rbind(furtherR, c(0, 1, rep(0,n), 1))
      retValue = logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT)
    }
    return(retValue)
}


#' @name modiclus
#' @title Compute modiclus
#' @description Calculates the modiclus of a TU game with a non-empty imputation set and n players.
#' Note that the modiclus is also know as the modified nucleolus in the literature.
#' Due to complexity of modiclus computation we recommend to use this function for at most n=11 players.
#' @aliases modiclus 
#' @import rcdd
#' @export modiclus
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 124--132
#' @template cites/SUDHOELTER_1997
#' @templateVar SUDHOELTER_1997_P pp. 147--182
#' @template cites/SUDHOELTER_1996
#' @templateVar SUDHOELTER_1996_P pp. 734--756
#' @template param/v
#' @return Numeric vector of length n representing the modiclus (aka modified nucleolus)
#' of the specified TU game.
#' @examples
#' library(CoopGame)
#' modiclus(c(1, 1, 1, 2, 3, 4, 5))
#' 
#' \donttest{
#' library(CoopGame)
#' modiclus(c(0, 0, 0, 0, 5, 5, 8, 9, 10, 8, 13, 15, 16, 17, 21))
#' #[1] 4.25 5.25 5.75 5.75
#' }
#' 
modiclus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    isImp = T
    isNT = F
    N <- length(A)
    n <- getNumberOfPlayers(A)
    rhs = determineModiclusRlb(A)
    restrMat = determineModiclusMatrix(A)
    excessR = cbind(rep(0,nrow(restrMat)), -rhs, restrMat)
    furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
    furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
    return(logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT))
}

determineModiclusMatrix<-function(A){
  
  n=getNumberOfPlayers(A)
  N=length(A)
  
  tempBM<-as.data.frame(createBitMatrix(n))
  lpMatrix<-matrix(ncol=(n+1),nrow=0)
  for(i in 1:(nrow(tempBM)-1)){ #original (nrow(tempBM)-1))
    currEntry=tempBM[i,]
    corrEntries=tempBM[c(-i,-N),] #original c(-i,-N)
    matTemp=matrix(unlist(apply(corrEntries,1,FUN=function(x){currEntry-x})),ncol=(n+1),byrow = TRUE)
    lpMatrix=rbind(lpMatrix,matTemp)
  }
  lpMatrix[,(n+1)]=1
  # lpMatrix=rbind(lpMatrix,c(rep(1,n),0))
  colnames(lpMatrix)[(n+1)]<-"cVal"
  return(lpMatrix)
}

determineModiclusRlb<-function(A){
  rlb<-c()
  N=length(A)
  for(i in 1:(N-1)){   #originial (N-1)
    valuesOfT=A[c(-i,-N)] #original c(-i,-N)
    rlbValues=sapply(valuesOfT,function(x){A[i]-x})
    rlb<-c(rlb,rlbValues)
  }
  
  # rlb<-c(rlb,A[N])
  return(rlb)
}

#' @name simplifiedModiclus
#' @title Compute simplified modiclus 
#' @description Computes the simplified modiclus of a TU game with a non-empty imputation set and n players.
#' @aliases simplifiedModiclus 
#' @import rcdd
#' @export simplifiedModiclus
#' @template author/JS
#' @template cites/TARASHNINA_2011
#' @templateVar TARASHNINA_2011_P pp. 150--166
#' @template param/v
#' @return Numeric vector of length n representing the simplified modiclus 
#' of the specified TU game.
#' @examples
#' library(CoopGame)
#' simplifiedModiclus(c(0, 0, 0, 1, 1, 0, 1))
#' 
#' \donttest{
#' #Second example:
#' #Estate division problem from Babylonian Talmud with E=100,
#' #see e.g. seminal paper by Aumann & Maschler from 1985 on
#' #'Game Theoretic Analysis of a Bankruptcy Problem from the Talmud'
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=100)
#' simplifiedModiclus(v)
#' #[1]  33.33333 33.33333 33.33333
#' }
#' 
simplifiedModiclus<-function(v){
    paramCheckResult=getEmptyParamCheckResult()
    initialParamCheck_Nuc(paramCheckResult = paramCheckResult, v)
    A = v  
    isImp = T
    isNT = F
    N <- length(A)
    n <- getNumberOfPlayers(A)
    rhs = determineSimplifiedModiclusRlb(A)
    restrMat = determineSimplifiedModiclusMatrix(A)
    excessR = cbind(rep(0,nrow(restrMat)), -rhs, restrMat)
    furtherR = cbind(rep(0,n),-A[1:n], diag(n),rep(0,n))
    furtherR = rbind(c(1, A[N], rep(-1,n),0), furtherR)
    return(logicNucleolusDerivatives(A, excessR, furtherR, isImp, isNT))
}

determineSimplifiedModiclusMatrix<-function(A){
  
  n=getNumberOfPlayers(A)
  N=length(A)
  
  lpMatrix=createBitMatrix(n)
  lpMatrix[lpMatrix==0]=-1
  lpMatrix[,(n+1)]=1
  # lpMatrix[N,(n+1)]=0
  return(lpMatrix[1:(N-1),])
}

determineSimplifiedModiclusRlb<-function(A){
  rlb<-c()
  N=length(A)
  rlb<-sapply(1:(N-1), FUN=function(ix){A[ix]-A[N-ix]})
  #rlb<-c(rlb,A[N])
  return(rlb)
}

#' @name drawNucleolus
#' @title Draw nucleolus for 3 or 4 players
#' @description drawNucleolus draws the nucleolus for 3 or 4 players.
#' @aliases drawNucleolus
#' @export drawNucleolus
#' @template author/JA
#' @template cites/SCHMEIDLER_1969
#' @templateVar SCHMEIDLER_1969_P pp. 1163--1170
#' @template cites/KOHLBERG_1971
#' @templateVar KOHLBERG_1971_P pp. 62--66
#' @template cites/KOPELOWITZ_1967
#' @template cites/MEGIDDO_1974
#' @templateVar MEGIDDO_1974_P pp. 355--358
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 82--86
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,3)
#' drawNucleolus(v) 
#' 
#' \donttest{
#' #Visualization for estate division problem from Babylonian Talmud with E=300,
#' #see e.g. seminal paper by Aumann & Maschler from 1985 on
#' #'Game Theoretic Analysis of a Bankruptcy Problem from the Talmud'
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=300)
#' drawNucleolus(v)
#' }
#' 
drawNucleolus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Nucleolus"){
  A=v
  nuc=nucleolus(A);
  visualize(A, pointsToDraw=nuc, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawPrenucleolus
#' @title Draw prenucleolus for 3 or 4 players
#' @description drawPrenucleolus draws the prenucleolus for 3 or 4 players.
#' @aliases drawPrenucleolus
#' @export drawPrenucleolus
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 107--132
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,3)
#' drawPrenucleolus(v) 
#' 
#' \donttest{
#' #Visualization for estate division problem from Babylonian Talmud with E=200,
#' #see e.g. seminal paper by Aumann & Maschler from 1985 on
#' #'Game Theoretic Analysis of a Bankruptcy Problem from the Talmud'
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=200)
#' drawPrenucleolus(v)
#' }
#' 
drawPrenucleolus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Prenucleolus"){
  A=v
  pcn=prenucleolus(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawPerCapitaNucleolus
#' @title Draw per capita nucleolus for 3 or 4 players
#' @description drawPerCapitaNucleolus draws the per capita nucleolus for 3 or 4 players.
#' @aliases drawPerCapitaNucleolus
#' @export drawPerCapitaNucleolus
#' @template author/JS
#' @template cites/YOUNG_1985
#' @templateVar YOUNG_1985_P pp. 65--72
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,3)
#' drawPerCapitaNucleolus(v) 
#' 
#' \donttest{
#' #Example from YOUNG 1985, p. 68
#' library(CoopGame)
#' v=c(0,0,0,0,9,10,12)
#' drawPerCapitaNucleolus(v)
#' }
#' 
drawPerCapitaNucleolus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Per Capita Nucleolus"){
  A=v
  pcn=perCapitaNucleolus(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawProportionalNucleolus
#' @title Draw proportional nucleolus for 3 or 4 players
#' @description drawProportionalNucleolus draws the proportional nucleolus for 3 or 4 players.
#' @aliases drawProportionalNucleolus
#' @export drawProportionalNucleolus
#' @template author/JS
#' @template cites/YOUNG_ET_AL_1982
#' @templateVar YOUNG_ET_AL_1982_P pp. 463--475
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v<-c(0,0,0,48,60,72,140)
#' drawProportionalNucleolus(v)
drawProportionalNucleolus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Proportional Nucleolus"){
  A=v
  pgv=proportionalNucleolus(A);
  visualize(A, pointsToDraw=pgv, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawDisruptionNucleolus
#' @title draw disruption nucleolus for 3 or 4 players
#' @description drawDisruptionNucleolus draws the disruption nucleolus for 3 or 4 players.
#' @aliases drawDisruptionNucleolus
#' @export drawDisruptionNucleolus
#' @template author/JS
#' @template cites/LITTLECHILD_ET_VAIDYA_1976
#' @templateVar LITTLECHILD_ET_VAIDYA_1976_P pp. 151--161
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v<-bankruptcyGameVector(n=3,d=c(100,200,300),E=200)
#' drawDisruptionNucleolus(v)
#' 
drawDisruptionNucleolus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Disruption Nucleolus"){
  A=v
  dn=disruptionNucleolus(A);
  visualize(A, pointsToDraw=dn, holdOn=holdOn, colour = colour , label=label, name = name)
}


#' @name drawModiclus
#' @title Draw modiclus for 3 or 4 players
#' @description drawModiclus draws the modiclus for 3 or 4 players.
#' @aliases drawModiclus
#' @export drawModiclus
#' @template author/JS
#' @template cites/PELEG_ET_SUDHOELTER_2007
#' @templateVar PELEG_ET_SUDHOELTER_2007_P pp. 124--132
#' @template cites/SUDHOELTER_1997
#' @templateVar SUDHOELTER_1997_P pp. 147--182
#' @template cites/SUDHOELTER_1996
#' @templateVar SUDHOELTER_1996_P pp. 734--756
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(1, 1, 1, 2, 3, 4, 5)
#' drawModiclus(v)
drawModiclus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Modiclus"){
  A=v
  mod=modiclus(A);
  visualize(A, pointsToDraw=mod, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawSimplifiedModiclus
#' @title Draw simplified modiclus for 3 or 4 players
#' @description drawSimplifiedModiclus draws the simplified modiclus for 3 or 4 players.
#' @aliases drawSimplifiedModiclus
#' @export drawSimplifiedModiclus
#' @template author/JS
#' @template cites/TARASHNINA_2011
#' @templateVar TARASHNINA_2011_P pp. 150--166
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0, 0, 0, 1, 1, 0, 1)
#' drawSimplifiedModiclus(v)
drawSimplifiedModiclus<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Simplified Modiclus"){
  A=v
  sm=simplifiedModiclus(A);
  visualize(A, pointsToDraw=sm, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_Nuc=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
