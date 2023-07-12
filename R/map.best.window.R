#' Plots spectra highlighting windows with the best performance.
#'
#' @param wpls, object obtained from **cv.wpls**.
#' @param fade, opacity of the window.
#' @param col.window, color of the window that highlights the region.
#' @param xlab, title of the x axis.
#' @param ylab title of the y axis.
#' @param title, title of the plot.
#' @param legend, description description
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @returns Plot of the spectra with a window that highlights the region with the lowest cross-validation error.
#' @export
#' @examples
#' data(beer)
#' conc=beer[,1]
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' conc=unlist(conc)
#' mywpls=cv.wpls(sp, conc,mode='wpls', windows = 5)
#' map.best.window(mywpls)

map.best.window=function(wpls=NULL,
                          fade=0.7,
                          col.window='steelblue',
                          xlab='Wavelengths',
                          ylab='Absorbance',
                          title=NULL,
                          legend=NULL){
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

  xtrain.original=as.data.frame(wpls[[1]])
  ytrain.original=unlist(as.data.frame(wpls[[2]]))

  name.j=as.numeric(condition.window[which(condition.window$nwindows==best.model),'j'])
  name.J=as.numeric(condition.window[which(condition.window$nwindows==best.model),'J'])

  index.best.j=as.numeric(sub('.','',names(xtrain.original)[name.j]))
  index.best.J=as.numeric(sub('.','',names(xtrain.original)[name.J]))
  back=as.data.frame(cbind(index.best.j,index.best.J))

  data.wide=as.data.frame(cbind(xtrain.original,ytrain.original))
  transposed=data.wide %>%
    mutate(row = row_number()) %>%
    gather(nm, value, -ytrain.original, -row) %>%
    mutate(nm = parse_number(nm))

  print(ggplot()+
          geom_line(transposed,mapping=aes(x = nm,
                                           y = value,
                                           group = row,
                                           color=ytrain.original),

                    stat="identity",
                    linewidth=0.75)+
          scale_colour_gradientn(colours=rev(topo.colors(10)),
                                 name=stringr::str_wrap(paste(legend),
                                                        width = 10))+
          labs(x = xlab,
               y=ylab,
               title=title)+
          geom_rect(data=back,
                    aes(xmin=index.best.j,
                        xmax=index.best.J,
                        ymin=-Inf,
                        ymax=+Inf,),
                    alpha=fade,
                    fill='steelblue')+
          guides(fill='none',color='none')+
          theme_bw())
}
