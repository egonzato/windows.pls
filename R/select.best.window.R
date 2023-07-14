#' Selection of the best window computed with cv.wpls
#' @description
#' Takes as input the object containing metrics of the several models computed
#' with cv.wpls and selects the best basing on the lowest RMSE available; then computes
#' PLS and gives as output an object containing results.
#'
#' @param wpls, object obtained from cv.wpls.
#' @returns  An object containing results of the best model. Has the same content of a model obtained from the function **pls** of **mdatools**.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' #' data(beer)
#' conc=beer[,1]
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' conc=unlist(conc)
#' mywpls=cv.wpls(sp, conc,mode='wpls', windows = 5)
#' best.pls=sel.best.window(mywpls)

sel.best.window=function(wpls=NULL){
  ncp.old=wpls[[6]]
  scale.old=wpls[[7]]
  cv.old=wpls[[8]]
  cv=as.data.frame(wpls[[4]][1])

  perf.rmse.cv.v=round(unlist(cv),3)

  perf.names=substring(names(cv),9,nchar(names(cv))[1])

  perf.rep=rep(perf.names,dim(cv)[1])

  perf.rep=perf.rep[order(perf.rep)]

  comp.rep=rep(seq(1,dim(cv)[1],1),dim(cv)[2])

  perf.rmse.t=as.data.frame(cbind(perf.rmse.cv.v,perf.rep,comp.rep))

  perf.rmse.t[,c(1,3)]=lapply(perf.rmse.t[,c(1,3)],as.numeric)
  names(perf.rmse.t)[c(1,3)]=c('RMSE','Component')

  condition.window=wpls[[5]]
  best.model=perf.rmse.t[which.min(perf.rmse.t$RMSE),'perf.rep']
  wavel=condition.window[which(best.model==condition.window$nwindows),'wl.window']
  print(paste('The best model is the',best.model,'model which has range',wavel))

  best.j=as.numeric(condition.window[which(condition.window$nwindows==best.model),'j'])
  best.J=as.numeric(condition.window[which(condition.window$nwindows==best.model),'J'])

  xtrain.original=as.data.frame(wpls[[1]])
  ytrain.original=as.data.frame(wpls[[2]])

  xtrain.best=xtrain.original[,best.j:best.J]
  ytrain.best=ytrain.original
  best.pls=pls(xtrain.best,
               ytrain.best,
               ncomp=ncp.old,
               scale=scale.old,
               cv=cv.old)
  return(best.pls)
}
