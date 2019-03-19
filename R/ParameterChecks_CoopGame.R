##First Section: START
#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidGameVector
#' @description stopOnInvalidGameVector checks if game vector v 
#'  is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/v
#' @template param/n
#' @export stopOnInvalidGameVector
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1000 \tab Game vector is invalid as 'NULL' \cr
#' 1001 \tab Number of elements in game vector is invalid \cr
#' 1002 \tab Type of game vector is not numeric \cr
#' 1003 \tab Game vector has different number of players than n \cr
#' 1004 \tab Null game specified, value for every player is 0 }
#' @examples
#' library(CoopGame)
#' validGameVector=c(0,0,0,60,60,60,72)
#' stopOnInvalidGameVector(paramCheckResult,validGameVector)

stopOnInvalidGameVector=function(paramCheckResult, v, n=NULL){
  checkResult=getEmptyParamCheckResult()
  A = v
  numberOfPlayersA=log2(length(A)+1)
  #Check if A is null
  if(is.null(A)){
    checkResult$errMessage="Game vector is invalid as 'NULL'"
    checkResult$errCode = 1000
    #Check if number of players deduced from v is valid integer
  }else if(numberOfPlayersA%%1!=0){
    checkResult$errMessage="Number of elements in game vector is invalid"
    checkResult$errCode = 1001
    #Check if game vector v is numeric
  }else if(!is.numeric(A)){
    checkResult$errMessage="Type of game vector is not numeric"
    checkResult$errCode = 1002
    #Check if n is specified if game vector v has n players
  }else if(!is.null(n)){
    if(numberOfPlayersA!=n){
      checkResult$errMessage="Game vector has different number of players than specified number of players."
      checkResult$errCode = 1003
    }
  }else if( (length(unique(A)) == 1 & A[1]==0) ){
    checkResult$errMessage="Null game specified, value for every player is 0"
    checkResult$errCode = 1004
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidGrandCoalitionN
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/GrandCoalitionN
#' @export stopOnInvalidGrandCoalitionN
#' @description stopOnInvalidGrandCoalitionN checks if grand coalition N is specified 
#' correctly and causes calculation to stop otherwise.
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1010 \tab Grand coalition vector N is invalid as 'NULL' \cr
#' 1011 \tab Grand coalition vector N is invalid as not numeric }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validGrandCoalition = c(1,2,3,4,5)
#' stopOnInvalidGrandCoalitionN(paramCheckResult, N=validGrandCoalition)
#' 
stopOnInvalidGrandCoalitionN=function(paramCheckResult, N){
  checkResult=getEmptyParamCheckResult()
  numberOfPlayersN=length(N)
  if(is.null(N)){
    checkResult$errMessage="Grand coalition vector N is invalid as 'NULL'"
    checkResult$errCode=1010
    #Check if coalition vector N is numeric
  }else if(!is.numeric(N)){
    checkResult$errMessage="Grand coalition vector N is invalid as not numeric"
    checkResult$errCode=1011
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidCoalitionS
#' @description stopOnInvalidCoalitionS checks if coalition S as subset of grand coalition N 
#'              is specified correctly and causes calculation to stop otherwise.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/S
#' @template param/GrandCoalitionN
#' @template param/n
#' @template param/v
#' @export stopOnInvalidCoalitionS
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1020 \tab Coalition vector S is invalid as 'NULL' \cr
#' 1021 \tab Coalition vector S is invalid as not numeric \cr
#' 1022 \tab Coalition vector S no subset of grand coalition N \cr
#' 1023 \tab The number of players in S cannot be greater than the number of players in N \cr
#' 1024 \tab Specified coalition is inconsistent with game vector}
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validCoalition = c(1,2,3)
#' stopOnInvalidCoalitionS(paramCheckResult, S=validCoalition, N=c(1,2,3,4,5))
#' 
stopOnInvalidCoalitionS=function(paramCheckResult, S,N=NULL,n=NULL,v=NULL){
  checkResult=getEmptyParamCheckResult()
  numberOfPlayersS=length(S)
  numberOfPlayersN=length(N)
  A = v
  #Check if S is 'NULL'
  if(is.null(S)){
    checkResult$errMessage="Coalition vector S is invalid as 'NULL'"
    checkResult$errCode=1020
    #Check if coalition vector S is numeric
  }else if(!is.numeric(S)){
    checkResult$errMessage="Coalition vector S is invalid as not numeric"
    checkResult$errCode=1021
    #In case grand coalition N is specified, check ...
  }else if(!is.null(N)){
    #Check if S subset of N
    if(!all(S %in% N)){
      checkResult$errMessage="Coalition vector S is no subset of grand coalition N"
      checkResult$errCode=1022
    }
  }else if(!is.null(n)){
    if((length(S)>n)){
      checkResult$errCode = 1023
      checkResult$errMessage = "The number of players in S cannot be greater than the number of players in N"
    }
  }else if(!is.null(A)){
    if(is.numeric(A) && (max(S)>getNumberOfPlayers(A))){
      checkResult$errCode = 1024
      checkResult$errMessage = "Specified coalition is inconsistent with game vector"
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}



#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNumberOfPlayers
#' @description stopOnInvalidNumberOfPlayers checks if number of players is 
#' specified correctly and causes calculation to stop otherwise.
#' @export stopOnInvalidNumberOfPlayers
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/n
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1050 \tab Number of players is invalid as below 2}
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validNumberOfPlayers = 10
#' stopOnInvalidNumberOfPlayers(paramCheckResult, n=validNumberOfPlayers)
#' 
stopOnInvalidNumberOfPlayers=function(paramCheckResult, n){
  checkResult=getEmptyParamCheckResult()
  if(n<2){
    checkResult$errMessage="Number of players is invalid as below 2."
    checkResult$errCode=1050
  # There is no maximal number of players from version 0.2.0 onwards (J.St.)
  #}else if(n>24){
  #  checkResult$errMessage="Number of players is invalid as above 24."
  #  checkResult$errCode=1051
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidIndex
#' @description stopOnInvalidIndex checks if coalition function 
#' (in the form of either v or A) is specified correctly and causes causes calculation 
#' to stop otherwise.
#' @export stopOnInvalidIndex
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @param index index which is checked to be a valid index
#' @template param/n
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1070 \tab Index is 'NULL'. \cr
#' 1071 \tab Index is 'not numeric'. \cr
#' 1072 \tab Index is within the wrong range according to number of players n.}
#' @examples
#' library(CoopGame)
#' v=c(1:7)
#' paramCheckResult=getEmptyParamCheckResult()
#' validIndex = 5
#' stopOnInvalidIndex(paramCheckResult, index=validIndex, n=3)
#' 
stopOnInvalidIndex=function(paramCheckResult, index, n=NULL){
  checkResult=getEmptyParamCheckResult()
  if(is.null(index)){
    checkResult$errMessage="Index is 'NULL'."
    checkResult$errCode=1070
  }else if(!is.numeric(index)){
    checkResult$errMessage="Index is 'not numeric'."
    checkResult$errCode=1071
  }else if(!is.null(n)&is.numeric(n)){
    numberOfCoalitions=(2^n)-1
    if(!(1<=index & index<=numberOfCoalitions)){
      checkResult$errMessage="Index is within the wrong range according to specified number of players n."
      checkResult$errCode=1072
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNChooseB
#' @description stopOnInvalidNChooseB checks if definition of n choose b is 
#' specified correctly and causes stop otherwise.
#' @export stopOnInvalidNChooseB
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/n
#' @param b number of players in subset
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1080 \tab Number of players 'n' is 'NULL' \cr
#' 1081 \tab Number of involved players 'b' is 'NULL' \cr
#' 1082 \tab Number of players 'n' is not 'numeric' \cr
#' 1083 \tab Number of involved players 'b' is not 'numeric \cr
#' 1084 \tab Number of involved players 'b' is greater than of players 'n'}
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validN = 3
#' validAndConsistentB = 2
#' stopOnInvalidNChooseB(paramCheckResult, n=validN, b=validAndConsistentB)
#' 
stopOnInvalidNChooseB=function(paramCheckResult, n, b){
  checkResult=getEmptyParamCheckResult()
  if(is.null(n)){
    checkResult$errMessage="Number of players 'n' is 'NULL'"
    checkResult$errCode=1080
  }else if(is.null(b)){
    checkResult$errMessage="Number of players 'b' is 'NULL'"
    checkResult$errCode=1081
  }else if(!is.numeric(n)){
    checkResult$errMessage="Number of players 'n' is 'not numeric'"
    checkResult$errCode=1082
  }else if(!is.numeric(b)){
    checkResult$errMessage="Number of players 'b' is 'not numeric'"
    checkResult$errCode=1083
  }else if(n<b){
    checkResult$errMessage="Number of involved players 'b' is greater than number of players 'n'"
    checkResult$errCode=1084
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidBoolean
#' @title stopOnInvalidBoolean - check definition is a boolean
#' @description stopOnInvalidBoolean checks definition is the parameter a boolean
#' @export stopOnInvalidBoolean
#' @template author/JS
#' @template author/FM
#' @template param/paramCheckResult
#' @param boolean parameter which is checked if it is a valid boolean.
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1120 \tab Parameter is not a boolean value \cr
#' 1121 \tab Parameter is not of length 1 }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validBoolean = TRUE
#' stopOnInvalidBoolean(paramCheckResult, validBoolean)
#' 
stopOnInvalidBoolean<-function(paramCheckResult, boolean){
  checkResult=getEmptyParamCheckResult();
  if(!is.logical(boolean)){
    checkResult$errMessage="Parameter is not a boolean value"
    checkResult$errCode=1120
  }else if(length(boolean)>1){
    checkResult$errMessage="Parameter is not of length 1"
    checkResult$errCode=1121
  }
  eval.parent(substitute(paramCheckResult<-checkResult));
  stopOnParamCheckError(paramCheckResult);
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidNumber
#' @description stopOnInvalidNumber checks definition is the parameter a number
#' @export stopOnInvalidNumber
#' @template author/JS
#' @template author/FM
#' @template param/paramCheckResult
#' @param number input which is checked to be valid number
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1130 \tab Parameter is not a number \cr
#' 1131 \tab Parameter is not of length 1 }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validNumber = 5
#' stopOnInvalidNumber(paramCheckResult, validNumber)
#'
stopOnInvalidNumber<-function(paramCheckResult, number){
  checkResult=getEmptyParamCheckResult();
  
  if(!is.numeric(number)){
    checkResult$errMessage="Parameter is not a number"
    checkResult$errCode=1130
  }else if(length(number)>1){
    checkResult$errMessage="Parameter is not of length 1"
    checkResult$errCode=1131
  }
  eval.parent(substitute(paramCheckResult<-checkResult));
  stopOnParamCheckError(paramCheckResult);
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidLeftRightGloveGame
#' @description stopOnInvalidLeftRightGloveGame checks if L (left gloves) 
#'  and R (right gloves) are specified as parameter correctly (also regarding grand coalition).
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/L
#' @template param/R
#' @template param/GrandCoalitionN
#' @export stopOnInvalidLeftRightGloveGame
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1140 \tab Not all players in L and R included. \cr
#' 1141 \tab L must have size > 0. \cr
#' 1142 \tab R must have size > 0. \cr
#' 1143 \tab L and R have to be disjoint sets. }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validL=c(1,3)
#' validR=c(2)
#' stopOnInvalidLeftRightGloveGame(paramCheckResult, L=validL,R=validR,N=c(1,2,3))
#' 
stopOnInvalidLeftRightGloveGame=function(paramCheckResult, L, R, N){
  
  checkResult=getEmptyParamCheckResult()
  #check if given numer of players in L und R is correct
  #and if L und R are disjoint
  unionLR = sort(union(L,R))
  if(!setequal(unionLR, N)){
    checkResult$errMessage = "Not all players in L and R included."
    checkResult$errCode = 1140
  }else if(!(length(L) > 0)){
    checkResult$errMessage = "L must have size > 0."
    checkResult$errCode = 1141
  }else if(!(length(R)) > 0){
    checkResult$errMessage = "R must have size > 0."
    checkResult$errCode = 1142
  }else if(length(intersect(L, R)) > 0){
    checkResult$errMessage = "L and R have to be disjoint sets."
    checkResult$errCode = 1143
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidVetoPlayer
#' @description stopOnInvalidVetoPlayer checks if vetoPlayer
#'  is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/vetoPlayer
#' @export stopOnInvalidVetoPlayer
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1190 \tab At least one veto player has to be specified \cr
#' 1191 \tab Only a single veto player is allowed for this game }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validVetoPlayer = 3
#' stopOnInvalidVetoPlayer(paramCheckResult, vetoPlayer=validVetoPlayer)
#' 
stopOnInvalidVetoPlayer=function(paramCheckResult, vetoPlayer) {
  checkResult=getEmptyParamCheckResult()
  if (is.null(vetoPlayer)) {
    checkResult$errMessage="At least one veto player has to be specified"
    checkResult$errCode = 1190
  } else if (length(vetoPlayer) > 1) {
    checkResult$errMessage="Only a single veto player is allowed for this game"
    checkResult$errCode = 1191
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidQuota
#' @description stopOnInvalidQuota checks if qutoa
#'  in a weighted voting game is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/q
#' @export stopOnInvalidQuota
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1030 \tab Invalid quota as q is NULL \cr
#' 1031 \tab Quota must be greater than zero! \cr
#' 1032 \tab Quota must be numeric!  }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validQuota = 3
#' stopOnInvalidQuota(paramCheckResult, q=validQuota)
#' 
stopOnInvalidQuota=function(paramCheckResult,q){
  checkResult=getEmptyParamCheckResult()
  if(is.null(q)){
    checkResult$errCode=1030
    checkResult$errMessage="Invalid quota as q is NULL"
  }else if (q < 0) {
    checkResult$errCode=1031
    checkResult$errMessage="Quota must be greater than zero!"
  }else if(!is.numeric(q)){
    checkResult$errCode=1032
    checkResult$errMessage="Quota must be numeric!"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidWeightVector
#' @description stopOnInvalidWeightVector checks if weight vector 
#'  in a weighted voting game is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/w
#' @template param/n
#' @export stopOnInvalidWeightVector
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1110 \tab Number of weights must be equal or greater than number of players in coalition!  \cr
#' 1111 \tab Invalid weight vector as w is not numeric }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validWeightVector = c(1,2,3)
#' stopOnInvalidWeightVector(paramCheckResult, n=3, w=validWeightVector)
#' 
stopOnInvalidWeightVector=function(paramCheckResult,n,w){
  checkResult=getEmptyParamCheckResult()
  if(n > length(w)){
    checkResult$errCode=1110
    checkResult$errMessage="Number of weights must be equal or greater than number of players in coalition!"
  }else if(!is.numeric(w)){
    checkResult$errCode=1111
    checkResult$errMessage="Invalid weight vector as w is not numeric"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidEstate
#' @description stopOnInvalidBankruptcy checks if estate
#'  is specified correctly (as parameter in a bankruptcy game).
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JS
#' @template param/paramCheckResult
#' @template param/E
#' @export stopOnInvalidEstate
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1150 \tab Estate must be nonnegative! \cr
#' 1151 \tab Estate must be numeric! \cr
#' 1152 \tab Invalid estate as E is NULL}
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validEstate = 55
#' stopOnInvalidEstate(paramCheckResult, E=validEstate)
#' 
stopOnInvalidEstate=function(paramCheckResult,E){
  checkResult=getEmptyParamCheckResult()
  if (E < 0) {
    checkResult$errCode=1150
    checkResult$errMessage="Estate must be nonnegative!"
  }else if(!is.numeric(E)){
    checkResult$errCode=1151
    checkResult$errMessage="Estate must be numeric!"
  }else if(is.null(E)){
    checkResult$errCode=1152
    checkResult$errMessage="Invalid estate as E is NULL"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidClaimsVector
#' @description stopOnInvalidClaimsVector checks if claims vector
#'  in a bankruptcy game is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes stop otherwise.
#' @template author/JS
#' @template param/paramCheckResult
#' @template param/d
#' @template param/n
#' @export stopOnInvalidClaimsVector
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1160 \tab Number of claims must equal the number of players in the bankruptcy game! \cr
#' 1161 \tab Invalid claims vector as d must be numeric }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validClaimsVector = c(100,150,200)
#' stopOnInvalidClaimsVector(paramCheckResult, n=3, d=validClaimsVector)
#' 
stopOnInvalidClaimsVector=function(paramCheckResult,n,d){
  checkResult=getEmptyParamCheckResult()
  if(n != length(d)){
    checkResult$errCode=1160
    checkResult$errMessage="Number of claims must equal the number of players in the bankruptcy game!"
  }else if(!is.numeric(d)){
    checkResult$errCode=1161
    checkResult$errMessage="Invalid claims vector as d must be numeric"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidDictator
#' @description stopOnInvalidDictator checks if dictator
#'  is specified correctly in a dictator game.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/dictator
#' @template param/n
#' @export stopOnInvalidDictator
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1090 \tab 'dictator' does not contain only one single element \cr
#' 1091 \tab Representation of 'dictator' is not 'numeric' \cr
#' 1092 \tab 'dictator' is not element of grand coalition \cr
#' 1093 \tab 'dictator' is 'NULL' }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validDictator = 3
#' stopOnInvalidDictator(paramCheckResult,dictator=validDictator,n=3)
#' 
stopOnInvalidDictator=function(paramCheckResult,dictator,n=NULL){
  checkResult=getEmptyParamCheckResult()
  if(is.null(dictator)){
    checkResult$errCode=1093
    checkResult$errMessage ="'dictator' is 'NULL'"
  }else if(!is.numeric(dictator)){
    checkResult$errCode=1091
    checkResult$errMessage ="Representation of 'dictator' is not 'numeric'"
  }else if(length(dictator)!=1){
    checkResult$errCode=1090
    checkResult$errMessage ="'dictator' does not contain only one single element"
  }else if(!is.null(n)){
    if(length(intersect(dictator,c(1:n)))!=1){
      checkResult$errCode=1092
      checkResult$errMessage = "'dictator' is not element of grand coalition"
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}


#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInvalidAllocation
#' @description stopOnInvalidAllocation checks if allocation
#'  is specified correctly.
#'  Validation result gets stored to object paramCheckResult 
#'  in case an error occured and causes calculation to stop.
#' @template author/JS
#' @template author/JA
#' @template param/paramCheckResult
#' @template param/x
#' @template param/n
#' @template param/v
#' @export stopOnInvalidAllocation
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1100 \tab Allocation 'x' is NULL \cr
#' 1101 \tab Allocation 'x' is not of type numeric. \cr
#' 1102 \tab Allocation 'x' has wrong number of elements as compared to number of players. \cr
#' 1103 \tab Allocation is inconsistent with game vector. }
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' validAllocation=c(1,2,3)
#' stopOnInvalidAllocation(paramCheckResult,x=validAllocation,n=3)
#' 
stopOnInvalidAllocation=function(paramCheckResult,x,n=NULL,v=NULL){
  checkResult=getEmptyParamCheckResult()
  A=v
  if(is.null(x)){
    checkResult$errCode=1100 
    checkResult$errMessage = "Allocation 'x' is NULL"
  }else if(!is.numeric(x)){
    checkResult$errCode=1101 
    checkResult$errMessage = "Allocation 'x' is not of type numeric."
  }else if(!is.null(n)){
    if(length(x)!=n){
      checkResult$errCode=1102  
      checkResult$errMessage = "Allocation 'x' has wrong number of elements as compared to number of players."
    }
  }else if(!is.null(A)){
    if(is.numeric(A)&&(length(x)!=getNumberOfPlayers(A))){
      checkResult$errCode=1103  
      checkResult$errMessage = "Allocation is inconsistent with game vector."
    }
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

#' @template Templates/ParameterCheckFunction
#' @templateVar PARAMETERCHECK_NAME stopOnInconsistentEstateAndClaimsVector
#' @description stopOnInconsistentEstateAndClaimsVector
#' checks if 
#' sum of claims is greater or equal estate (in bankruptcy games).
#' Calculation stops with an error message if claims vector and estate are inconsistent.
#' @template author/JS
#' @template param/paramCheckResult
#' @template param/E
#' @template param/d
#' @export stopOnInconsistentEstateAndClaimsVector
#' @section Error Code Ranges:
#' Error codes and messages shown to user if error on parameter check occurs
#' \tabular{lll}{
#' \strong{Error Code} \tab \strong{Message} \cr
#' 1170 \tab Estate E must be less or equal the sum of claims!}
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' consistentClaims= c(26,27,55,57)
#' consistentE = 110
#' stopOnInconsistentEstateAndClaimsVector(paramCheckResult, d=consistentClaims, E=consistentE)
#' 
stopOnInconsistentEstateAndClaimsVector=function(paramCheckResult,E,d){
  checkResult=getEmptyParamCheckResult()
  if(E > sum(d)){
    checkResult$errCode=1170 
    checkResult$errMessage = "Estate E must be less or equal the sum of claims!"
  }
  eval.parent(substitute(paramCheckResult<-checkResult))
  stopOnParamCheckError(paramCheckResult)
}

##First Section: END

##Second Section: START

#' @name getEmptyParamCheckResult
#' @title getEmptyParamCheckResult for generating stucture according to parameter check results
#' @description Returns a defined data structure which is intended to store an error code 
#' and a message after the check of function parameters was executed.
#'  In case parameter check was successfull the error code has the value '0'
#'  and the message is 'NULL'.
#' @family ParameterChecks_CoopGame
#' @aliases getEmptyParamCheckResult
#' @export getEmptyParamCheckResult
#' @template author/JA
#' @return list with 2 elements named errCode which contains an integer
#' representing the error code ('0' if no error) and errMessage for the error message ('NULL' if no error)
#' @examples
#' library(CoopGame)
#' 
#' initParamCheck_example=function(numberOfPlayers){
#'  paramCheckResult=getEmptyParamCheckResult()
#'  if(numberOfPlayers!=3){
#'    paramCheckResult$errMessage="The number of players is not 3 as expected"
#'    paramCheckResult$errCode=1
#'  }
#'  return(paramCheckResult)
#' }
#'
#' initParamCheck_example(3)
#' #Output:
#' #$errCode
#' #[1] 0
#' #$errMessage
#' #NULL
#' 
getEmptyParamCheckResult=function(){
  #Return code which gives further information for testing
  #Initialized with 0 means no error
  retCode=0
  #Message which is shown user for error
  #Initialized with NULL as no error
  errMsg=NULL
  return(list(errCode=retCode,errMessage=errMsg))
}


#' @name stopOnParamCheckError
#' @title stopOnParamCheckError - stop and create error message on error
#' @description stopOnParamCheckError causes and creates error message 
#' on base of paramCheckResult parameter where 'errCode' <> '0' in case error occured.
#' @family ParameterChecks_CoopGame
#' @aliases stopOnParamCheckError
#' @export stopOnParamCheckError
#' @template author/JA
#' @template param/paramCheckResult
#' @examples
#' library(CoopGame)
#' paramCheckResult=getEmptyParamCheckResult()
#' stopOnParamCheckError(paramCheckResult)
#' 
stopOnParamCheckError=function(paramCheckResult){
  #boolean variable: TRUE => stop is executed, FALSE => no stop is executed
  boolStop=FALSE

  if(paramCheckResult$errCode!=0){
    #in case error occured set boolStop to true
    boolStop=TRUE
  }

  #Execute stop in case boolStop was set to TRUE in previous steps and generate adequate error message
  if(boolStop){
    errMsg=paste("Error Code ",paramCheckResult$errCode,": ",paramCheckResult$errMessage, sep="")
    stop(errMsg)
  }
}

##Second Section: END


