#' Plots RMSE of calibration and cross-validation of a single wnindow.
#'
#' @param wpls, object obtained from **cv.wpls**.
#' @param condition, name of the Window the user wants to plot.
#' @param shape.cal, shape of the point of the calibration line.
#' @param shape.cv, shape of the point of the cross-validation line.
#' @param width, width of the line.
#' @param size, size of the points of calibration and cross-validation.
#' @param col.cal, color for the calibration line.
#' @param col.cv, color for the cross-validation line.
#' @param xaxis.title, title of the x axis.
#' @param yaxis.title, title of the y axis.
#' @param title, title of the plot.
#' @param legend.name, displays legend and its name.
#' @param x.legend, position of the legend on the x axis, ranges from 0 to 1.
#' @param y.legend, position of the legend on the y axis, ranges from 0 to 1.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @returns Plot of RMSE of the region requested by the user.
#' @export
#' @examples
#' data(beer)
#' conc=unlist(beer[,1])
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' mywpls=cv.wpls(sp, conc,mode='wpls', windows = 5)
#' rmse.single.window(mywpls,'Window2')

rmse.single.window=function(wpls=NULL,
                                 condition='Complete',
                                 shape.cal=19,
                                 shape.cv=19,
                                 width=1,
                                 size=2,
                                 col.cal='blue',
                                 col.cv='red',
                                 xaxis.title='Component',
                                 yaxis.title='RMSE',
                                 title=paste('Plot of RMSE for the',condition,'model'),
                                 legend.name=NULL,
                                 x.legend=0.1,
                                 y.legend=0.2){

  cv=as.data.frame(wpls[[4]][1])
  cal=as.data.frame(wpls[[3]][1])

  perf.rmse.cv.v=round(unlist(cv),3)
  perf.rmse.cal.v=round(unlist(cal),3)

  perf.names=substring(names(cv),9,nchar(names(cv))[1])

  perf.rep=rep(perf.names,dim(cv)[1])

  perf.rep=perf.rep[order(perf.rep)]

  comp.rep=rep(seq(1,dim(cv)[1],1),dim(cv)[2])

  perf.rmse.t=as.data.frame(cbind(perf.rmse.cal.v,perf.rmse.cv.v,perf.rep,comp.rep))
  perf.rmse.t[,c(1,2,4)]=lapply(perf.rmse.t[,c(1,2,4)],as.numeric)
  names(perf.rmse.t)[c(1,4)]=c('RMSE','Component')
  perf.rmse.t=perf.rmse.t[which(perf.rmse.t$perf.rep==condition),]
  min=perf.rmse.t[which.min(perf.rmse.t$perf.rmse.cv.v),'Component']

  ggplot()+
    geom_point(data=perf.rmse.t,aes(x=Component,y=RMSE, col='Cal', fill='Cal'),shape=shape.cal,size=size)+
    geom_point(data=perf.rmse.t,aes(x=Component,y=perf.rmse.cv.v, col='Cv', fill='Cv'),shape=shape.cv,size=size)+
    geom_path(data=perf.rmse.t,aes(x=Component,y=RMSE, col='Cal'),linewidth=width)+
    geom_path(data=perf.rmse.t,aes(x=Component,y=perf.rmse.cv.v, col='Cv'),linewidth=width)+
    scale_color_manual(name=legend.name,
                       breaks=c('Cal', 'Cv'),
                       values=c('Cal'=col.cal, 'Cv'=col.cv))+
    scale_x_continuous(breaks=c(1:dim(cv)[1]))+
    labs(x=xaxis.title,
         y=yaxis.title,
         title=title)+
    guides(fill='none')+
    geom_vline(xintercept=min, linewidth=1, linetype=2,col='gray')+
    theme_bw()+
    theme(legend.position = c(x.legend,y.legend),
          legend.text = element_text(colour="black",
                                     size=10,
                                     face="bold"),
          legend.background = element_rect(linetype="solid",
                                           color='black'))
}
