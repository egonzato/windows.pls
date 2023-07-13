#' Cross-validation for segmented spectral regions of the original spectra.
#' @description
#' Computes and stores cross-validation metrics for one of the three possible modes ‘wpls’, ‘epls’, ‘swpls’.
#'
#' @param xblock A matrix containing one spectra for each observation.
#' @param yblock A vector containing the concentration associated to each spectra in the *xblock* matrix.
#' @param windows Parameter used when either ‘wpls’ or ‘ewpls’ is chosen. Points out how many windows the user wants to divide the spectra in.
#' @param window.size Parameter used when ‘swpls’ is chosen. Indicates the width of the window that slides along the spectra.
#' @param increment Parameter used when ‘swpls’ is chosen. Indicates how many steps the window slides forward.
#' @param cv Number of segments used for cross-validation.
#' @param scale logical, asks to perform standardization.
#' @param ncp Maximum number of principal components to be computed for each model.
#' @param mode 'wpls','ewpls' or 'swpls', see **Details** for more.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @export
#' @returns
#' Returns a list containing:
#'
#' \item{xblock}{Matrix containing spectra used to train the model.}
#' \item{yblock}{Vector containing values of the dependent variable.}
#' \item{cal}{List containing RMSE and R2 of calibratrion.}
#' \item{cv}{List containing RMSE and R2 of cross-validation.}
#' \item{ncp}{Number of components used to compute the model.}
#' \item{scale}{Contains logical condition used for standardization.}
#' \item{cv.segment}{ Number of segments used for cross-validation.}
#'
#' @details NIR and Vis-NIR technologies are used to obtain spectra which might
#' contain helpful information about the content of the samples the user is investigating.
#' Since this method has been combined with multivariate statistical methods, researchers have
#' been questioning the importance of using spectra in its entirety or if it might be a better
#'  solution to divide it in smaller regions which can guarantee higher performance in terms of predictions.
#'  Several methods have been proposed, from selecting only some regions to selecting combinations of those which are performing the best.
#' This function provides three possibilities:
#'
#'1. **‘wpls’**, which stands for *Window PLS*, divides the original spectra into several windows, computes PLS and stores metrics of interest such as RMSE and R2 for calibration and cross-validation both.
#'
#'2. **‘ewpls’**, which stands for *Evolving Window PLS*, divides the original spectra into several windows, but each new window incorporates the previous ones, so that we are comparing smaller windows with the entire spectra.
#'
#'3. **’swpls’**, which stands for *Sliding Window Window PLS*, ,asks the width of the window that will be used to compute the model and the step that the window will make forward in the spectra so that a new model is calculated. In this way the window slides along spectra and computes several models, which will be compared with metrics.
#'
#'
#'This function proposes a simpler version of **iPLS**, that can be found in the **mdatools** package, which divides the spectra in smaller segments and tries to find the combination with the lowest RMSE in cross-validation.
#'
#' @references
#' 1. Chen, J., Yin, Z., Tang, Y. et al. Vis-NIR spectroscopy with moving-window PLS method applied to rapid analysis of whole blood viscosity. Anal Bioanal Chem 409, 2737–2745 (2017).
#'
#' 2. Y.P. Du, Y.Z. Liang, J.H. Jiang, R.J. Berry, Y. Ozaki, Spectral regions selection to improve prediction ability of PLS models by changeable size moving window partial least squares and searching combination moving window partial least squares, Analytica Chimica Acta, Volume 501, Issue 2, 2004, Pages 183-191,
#'
#' 3. **mdatools** package, https://github.com/svkucheryavski/mdatools
#' @examples
#' data(beer)
#' conc=beer[,1]
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' conc=unlist(conc)
#' mywpls=cv.wpls(sp, conc,mode='wpls', windows = 5)

cv.wpls=function(xblock=NULL,
                 yblock=NULL,
                 windows=3,
                 window.size=30,
                 increment=10,
                 cv=10,
                 scale=F,
                 ncp=10,
                 mode='wpls'){
  if( !(mode%in% c('wpls','ewpls','swpls'))){stop('Mode not available')}
  limits=round(seq(1,ncol(xblock),length.out=windows+1))
  size=seq(1,windows,1)
  # initialize rmse matrix
  perf.rmse.cv=matrix(NA,nrow = ncp)
  perf.rmse.cal=matrix(NA,nrow = ncp)
  # initialize r2 matrix
  perf.r2.cv=matrix(NA,nrow = ncp)
  perf.r2.cal=matrix(NA,nrow = ncp)
  # matrix that contains column index
  index=matrix(NA,ncol = 2)
  wl.window=c()
  nwindows=c()
  mode=mode
  for (i in size){
    # define columns
    if(mode=='swpls'){break}
    j=ifelse(mode=='wpls',limits[i],1)
    J=limits[i+1]
    index.temporary=cbind(j,J)
    x.mw.train=xblock[,j:J]
    # compute model
    pls.mw=pls(x=x.mw.train,
               y=yblock,
               cv=cv,
               ncomp=ncp,
               scale=scale)
    # store rmse
    rmse.cv=as.data.frame(t(pls.mw$cvres$rmse))
    rmse.cal=as.data.frame(t(pls.mw$calres$rmse))
    # store r2
    r2.cv=as.data.frame(t(pls.mw$cvres$r2))
    r2.cal=as.data.frame(t(pls.mw$calres$r2))
    # name of the interval
    nwindow=paste('Window',i,sep='')
    nwindows=c(nwindows,nwindow)
    wl.temporary=paste(make.names(names(xblock)[j]),
                       ':',
                       make.names(names(xblock)[J]),
                       sep='')
    # change name of the first column
    names(rmse.cv)=nwindow
    names(rmse.cal)=nwindow
    # same for r2
    names(r2.cv)=nwindow
    names(r2.cal)=nwindow
    # bind cols rmse
    perf.rmse.cv=cbind(perf.rmse.cv,rmse.cv)
    perf.rmse.cal=cbind(perf.rmse.cal,rmse.cal)
    # bind cols r2
    perf.r2.cv=cbind(perf.r2.cv,r2.cv)
    perf.r2.cal=cbind(perf.r2.cal,r2.cal)
    # store windows
    wl.window=c(wl.window,wl.temporary)
    # store indexes
    index=rbind(index,index.temporary)
  }
  ############ swpls ################
  window.size=window.size
  increment=increment
  j.v=NULL
  J.v=NULL
  for (i in 1:ncol(xblock)){
    if(mode%in% c('wpls','ewpls')){break}
    # define columns
    j=ifelse(i==1,1,j.v[i-1]+increment)
    J=ifelse(i==1,1+window.size,J.v[i-1]+increment)
    if(J>ncol(xblock)){J=ncol(xblock)}
    j.v=c(j.v,j)
    J.v=c(J.v,J)
    if(i>1 && J.v[i-1]==ncol(xblock)){break}
    index.temporary=cbind(j,J)
    x.mw.train=xblock[,j:J]
    # compute model
    pls.mw=pls(x=x.mw.train,
               y=yblock,
               cv=cv,
               ncomp=ncp,
               scale=scale)
    # store rmse
    rmse.cv=as.data.frame(t(pls.mw$cvres$rmse))
    rmse.cal=as.data.frame(t(pls.mw$calres$rmse))
    # store r2
    r2.cv=as.data.frame(t(pls.mw$cvres$r2))
    r2.cal=as.data.frame(t(pls.mw$calres$r2))
    # name of the interval
    nwindow=paste('Window',i,sep='')
    nwindows=c(nwindows,nwindow)
    wl.temporary=paste(make.names(names(xblock)[j]),
                       ':',
                       make.names(names(xblock)[J]),
                       sep='')
    # change name of the first column
    names(rmse.cv)=nwindow
    names(rmse.cal)=nwindow
    # same for r2
    names(r2.cv)=nwindow
    names(r2.cal)=nwindow
    # bind cols rmse
    perf.rmse.cv=cbind(perf.rmse.cv,rmse.cv)
    perf.rmse.cal=cbind(perf.rmse.cal,rmse.cal)
    # bind cols r2
    perf.r2.cv=cbind(perf.r2.cv,r2.cv)
    perf.r2.cal=cbind(perf.r2.cal,r2.cal)
    # store windows
    wl.window=c(wl.window,wl.temporary)
    # store indexes
    index=rbind(index,index.temporary)
  }
  index=index[-1,]
  # bind indexes and cols
  window.index=as.data.frame(cbind(nwindows,wl.window,index))
  window.index[nrow(window.index)+1,]=c('Complete',
                                        paste(make.names(names(xblock)[1]),
                                              ':',
                                              make.names(names(xblock)[ncol(xblock)]),
                                              sep=''),
                                        1,
                                        ncol(xblock))
  if(mode=='ewpls'){window.index=window.index[-(nrow(window.index)-1),]}
  # compute entire model
  complete=pls(xblock,
               yblock,
               ncomp=ncp,
               scale=scale,
               cv=cv)
  # for cross validation of the complete model
  perf.rmse.cv[,1]=t(complete$cvres$rmse)
  names(perf.rmse.cv)[1]='Complete'
  # for calibration of the complete model
  perf.rmse.cal[,1]=t(complete$calres$rmse)
  names(perf.rmse.cal)[1]='Complete'
  # for cross validation of the complete model
  perf.r2.cv[,1]=t(complete$cvres$r2)
  names(perf.r2.cv)[1]='Complete'
  # for calibration of the complete model
  perf.r2.cal[,1]=t(complete$calres$r2)
  names(perf.r2.cal)[1]='Complete'
  # store cv results in a list
  metrics.cv=list('rmse.cv'=perf.rmse.cv,'r2.cv'=perf.r2.cv)
  # store cal results in a list
  metrics.cal=list('rmse.cal'=perf.rmse.cal,'r2.cal'=perf.r2.cal)
  ## create object that contains rmse and r2 results
  metrics=list('xblock'=xblock,
               'yblock'=yblock,
               'cal'=metrics.cal,
               'cv'=metrics.cv,
               'windows'=window.index,
               'ncp'=ncp,
               'scale'=scale,
               'cv.segment'=cv)
  return(metrics)
}
