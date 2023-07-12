#' Plots in a single window the R2 of each model.
#'
#' @param wpls, object obtained from **cv.wpls**.
#' @param col.cal, color for the calibration line.
#' @param col.cv, color for the cross-validation line.
#' @param col.strip.background, color of the banner for each window.
#' @param xlab, title of the x axis.
#' @param ylab, title of the y axis.
#' @param title, title of the plot.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @export
#' @returns Plot of R2 of each spectra region used to compute PLS.
#' @examples
#' data(beer)
#' conc=beer[,1]
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' conc=unlist(conc)
#' mywpls=cv.wpls(sp, conc,mode='wpls', windows = 5)
#' global.r2(mywpls,col.cal='navy',
#'                col.cv='red',
#'                col.strip.background='orange',
#'                xlab='Component',
#'                ylab=expression(R^2))
#'

global.r2=function(wpls=NULL,
                        col.cal='blue',
                        col.cv='red',
                        col.strip.background='orange',
                        xlab=NULL,
                        ylab=NULL,
                        title=NULL){

  ncp=wpls[[6]]
  cv=as.data.frame(wpls[[4]][2])
  cal=as.data.frame(wpls[[3]][2])

  perf.r2.cv.v=round(unlist(cv),3)
  perf.r2.cal.v=round(unlist(cal),3)

  perf.names=substring(names(cv),7,nchar(names(cv))[1])

  perf.rep=rep(perf.names,dim(cv)[1])

  perf.rep=perf.rep[order(perf.rep)]

  comp.rep=rep(seq(1,dim(cv)[1],1),dim(cv)[2])

  perf.r2.t=as.data.frame(cbind(perf.r2.cal.v,perf.r2.cv.v,perf.rep,comp.rep))
  perf.r2.t[,c(1,2,4)]=lapply(perf.r2.t[,c(1,2,4)],as.numeric)
  names(perf.r2.t)[c(1,4)]=c('R2','Component')

  ggplot()+
    geom_point(data=perf.r2.t,aes(x=Component,y=R2, col='Cal'))+
    geom_point(data=perf.r2.t,aes(x=Component,y=perf.r2.cv.v, col='Cv'))+
    geom_path(data=perf.r2.t,aes(x=Component,y=R2, col='Cal'))+
    geom_path(data=perf.r2.t,aes(x=Component,y=perf.r2.cv.v, col='Cv'))+
    facet_wrap(~perf.rep, scales='free')+
    scale_color_manual(name='Legend',
                       breaks=c('Cal', 'Cv'),
                       values=c('Cal'=col.cal, 'Cv'=col.cv))+
    scale_x_continuous(breaks=c(1:ncp))+
    labs(x=xlab,
         y=ylab,
         title = title)+
    theme_bw()+
    theme(strip.background = element_rect(fill=col.strip.background))
}
