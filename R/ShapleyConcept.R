#' @name shapleyValue
#' @title Compute Shapley value 
#' @description Calculates the Shapley value for n players with formula from Lloyd Shapley.
#' @aliases shapleyValue
#' @export shapleyValue
#' @template author/AT
#' @template author/JS
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/AUMANN_2010
#' @templateVar AUMANN_2010_P pp. 3--10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 156--159
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 748--781
#' @template cites/BERTINI_SAGE_SHAPLEY_2011
#' @templateVar BERTINI_SAGE_SHAPLEY_2011_P p. 600--603
#' @template param/v
#' @return Shapley value for given game vector with n players
#' @examples
#' library(CoopGame)
#' shapleyValue(v=c(0,0,0,1,2,3,7.5))
#' 
#' \donttest{
#' #Example of a non-superadditive game,
#' #i.e. the inheritance problem due to Ibn Ezra (1146),
#' #from paper by Robert Aumann from 2010 on
#' #'Some non-superadditive games, and their Shapley values, in the Talmud'
#' library(CoopGame)
#' Aumann2010Example<-c(120,60,40,30,120,120,120,60,60,40,120,120,120,60,120)
#' shapleyValue(Aumann2010Example)
#' #[1] 80.83333 20.83333 10.83333  7.50000
#' }
#' 
shapleyValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_shapleyValue(paramCheckResult = paramCheckResult, v)
  A = v  
  return(logicShapleyValue(A))
}

#' @name shapleyShubikIndex
#' @title Compute Shapley-Shubik index 
#' @description Calculates the Shapley-Shubik index for a specified simple game with n players. 
#' Note that no separate drawing routine for the Shapley-Shubik index is provide 
#' as users can always resort to \link{drawShapleyValue}
#' @aliases shapleyShubikIndex
#' @export shapleyShubikIndex
#' @template author/AT
#' @template author/JS
#' @template cites/SHAPLEY_SHUBIK_1954
#' @templateVar SHAPLEY_SHUBIK_1954_P pp. 787--792
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 156--159
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 748--781
#' @template cites/STACH_SAGE_SHUBIK_2011
#' @templateVar STACH_SAGE_SHUBIK_2011_P pp. 603--606
#' @template param/v
#' @return Shapley-Shubik index for given simple game
#' @examples
#' library(CoopGame)
#' shapleyShubikIndex(v=c(0,0,0,0,1,0,1))
#' 
#' \donttest{
#' #Example from Stach (2011):
#' library(CoopGame)
#' v=weightedVotingGameVector(n=4,q=50,w=c(10,10,20,30))
#' shapleyShubikIndex(v)
#' #[1] 0.08333333 0.08333333 0.25000000 0.58333333
#' }
#' 
shapleyShubikIndex<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_shapleyValue(paramCheckResult = paramCheckResult, v)
  A = v  
  retVal = NULL
  if(!isSimpleGame(A)){
    print("Game is not simple. Therefore no Shapley Shubik Index can be retrieved")
  }
  else
  {
    retVal=logicShapleyValue(A)
  }  
  return(retVal)
}

logicShapleyValue <- function(A) {
  numberOfPlayers=getNumberOfPlayers(A)
  
  shapley<-c()
  
  coalitions<-list()

  
  bitMatrix=createBitMatrix(n = numberOfPlayers, A)
  bitMatrix<-rbind(bitMatrix, 0)
  
  i<-1
  j<-1
  #loop over all players
  while(i<=numberOfPlayers){
    #get all coalitions the current player is not part of
    vectorWithIndx<-which(!(bitMatrix[, i]&1))
    #save in a list for result
    currCoal<-bitMatrix[vectorWithIndx,]
    coalitions[[i]]<-currCoal
    
    shapleyForPlayer = 0
    while(j<=nrow(currCoal)){
      curCoalSet = which(currCoal[j,1:numberOfPlayers]&1)
      mightiness = sum(currCoal[j,1:numberOfPlayers])
      #formula:sum(|C|!(n-|C|-1)!/n! *(v(Cv{i})-v(C)))
      shapleyForPlayer = shapleyForPlayer +
        ((factorial(mightiness)*(factorial(numberOfPlayers-mightiness-1))/factorial(numberOfPlayers))
         *(A[indexCoalition(numberOfPlayers,union(i,curCoalSet))]
           -currCoal[[j, "cVal"]]))
      
      j<-j+1
    }
    
    j<-1
    shapley[i] = shapleyForPlayer
    
    i<-i+1
  }
  
  # return(list(coalitions = coalitions, shapleyValue=shapley))
  return(shapley)
}


#' @name drawShapleyValue
#' @title Draw Shapley value for 3 or 4 players
#' @description drawShapleyValue draws the Shapley value for 3 or 4 players.
#' @aliases drawShapleyValue
#' @export drawShapleyValue
#' @template author/AT
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/AUMANN_2010
#' @templateVar AUMANN_2010_P pp. 3--10
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 156--159
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 748--781
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawShapleyValue(v) 
drawShapleyValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Shapley value"){
  A=v
  shapleyValue=shapleyValue(A);
  visualize(A, pointsToDraw=shapleyValue, holdOn=holdOn, colour = colour , label=label, name = name)
}

#' @name drawShapleyShubikIndex
#' @title Draw Shapley-Shubik index for 3 or 4 players
#' @description drawShapleyShubik draws the Shapley-Shubik index 
#' simple game with 3 or 4 players.
#' @aliases drawShapleyShubikIndex
#' @export drawShapleyShubikIndex
#' @template author/JS
#' @template cites/SHAPLEY_SHUBIK_1954
#' @templateVar SHAPLEY_SHUBIK_1954_P pp. 787--792
#' @template cites/SHAPLEY_1953
#' @templateVar SHAPLEY_1953_P pp. 307--317
#' @template cites/PETERS_2015
#' @templateVar PETERS_2015_P pp. 156--159
#' @template cites/MASCHLER_ET_SOLAN_ET_ZAMIR_2013
#' @templateVar MASCHLER_ET_SOLAN_ET_ZAMIR_2013_P pp. 748--781
#' @template cites/STACH_SAGE_SHUBIK_2011
#' @templateVar STACH_SAGE_SHUBIK_2011_P pp. 603--606
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v=c(0,0,0,1,1,0,1)
#' drawShapleyShubikIndex(v) 
drawShapleyShubikIndex<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Shapley-Shubik index"){
  A=v
  shapleyShubik=shapleyShubikIndex(A);
  visualize(A, pointsToDraw=shapleyShubik, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_shapleyValue=function(paramCheckResult,v){
  stopOnInvalidGameVector(paramCheckResult, v)
}

