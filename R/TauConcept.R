#' @name tauValue
#' @title Compute tau-value
#' @description Calculates the tau-value for a quasi-balanced TU game with n players.
#' @aliases tauValue tijsValue
#' @export tauValue
#' @template author/JA
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 32
#' @template cites/TIJS_1981
#' @templateVar TIJS_1981_P pp. 123--132
#' @template cites/STACH_SAGE_TIJS_2011
#' @templateVar STACH_SAGE_TIJS_2011_P pp. 667--670
#' @template param/v
#' @return tau-value for a quasi-balanced TU game with n players
#' @examples
#' library(CoopGame)
#' tauValue(v=c(0,0,0,0,1,0,1))
#' 
#' \donttest{
#' #Example from article by Stach (2011)
#' library(CoopGame)
#' v=c(0,0,0,1,2,1,3)
#' tauValue(v) 
#' #[1] 1.2 0.6 1.2
#' }
#' 
tauValue<-function(v){
  paramCheckResult=getEmptyParamCheckResult()
  initialParamCheck_tauValue(paramCheckResult = paramCheckResult, v)
  A = v
  retVal=NULL
  n=getNumberOfPlayers(A)
  N=length(A)
  
  if(!isQuasiBalancedGame(A)){
    print("Game is not quasi-balanced therefore no tau-value can be retrieved.")
  }else{
    mc=matrix(nrow=2,ncol=n)
    rownames(mc)<-c("M","m")
    mc["M",]<-getUtopiaPayoff(A)
    mc["m",]<-getMinimalRights(A)
    
    if(identical(mc["M",],mc["m",])){
      retVal=mc["M",]
    }else{
      diffM_m=mc["M",]-mc["m",]
      tDiagMatrix=diag(1,nrow=n,ncol = n)
      coeffMat=cbind(tDiagMatrix,diffM_m)
      coeffMat=rbind(coeffMat,c(rep(1,n),0))
      tauResult=solve(coeffMat,c(mc["M",],A[N]))
      retVal = unname(tauResult[1:n])
    }
  }
  return(retVal)
}


remainder<-function(A){
  n=getNumberOfPlayers(A)
  N=length(A)
  bm=createBitMatrix(n=n,A)
  mc=getUtopiaPayoff(A)
  bm[,1:n]=bm[,1:n]*mc
  remainder=matrix(
    unlist(
      apply(bm,1,
            FUN = function(x,N,n){
              return(sapply(1:(n),function(ix){x["cVal"]-sum(x[-c(ix,(n+1))])}))
            },
            N=N,
            n=n
      )
    ),
    ncol=(n),
    byrow=TRUE
  )
  return(remainder)
}



#' @name drawTauValue
#' @title Draw tau-value for 3 or 4 players
#' @description drawTauValue draws the tau-value for 3 or 4 players.
#' @aliases drawTauValue drawTijsValue
#' @export drawTauValue
#' @template author/JA
#' @template author/JS
#' @template cites/BRANZEI_ET_AL_2006
#' @templateVar BRANZEI_ET_AL_2006_P p. 32
#' @template cites/TIJS_1981
#' @templateVar TIJS_1981_P pp. 123--132
#' @template cites/STACH_SAGE_TIJS_2011
#' @templateVar STACH_SAGE_TIJS_2011_P pp. 667--670
#' @template param/v
#' @template param/holdOn 
#' @template param/colour
#' @template param/label
#' @template param/name
#' @return None.
#' @examples
#' library(CoopGame)
#' v <-c(1,2,3,60,60,60,142)
#' drawTauValue(v,colour="green")
drawTauValue<-function(v,holdOn=FALSE, colour = NA , label=TRUE, name = "Tau value"){
  A=v
  pcn=tauValue(A);
  visualize(A, pointsToDraw=pcn, holdOn=holdOn, colour = colour , label=label, name = name)
}

initialParamCheck_tauValue=function(paramCheckResult,v=v){
  stopOnInvalidGameVector(paramCheckResult, v)
}
