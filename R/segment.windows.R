#' Displays how  spectra are divided in windows
#'
#' @param xblock A matrix containing one spectra for each observation.
#' @param yblock A vector containing the concentration associated to each spectra in the *xblock* matrix.
#' @param windows Number of windows the spectra has to be divided in.
#' @param fade Opacity of the window.
#' @param xlab Title of the x axis.
#' @param ylab Title of the y axis.
#' @param title Title of the plot.
#' @param legend Name of the substance which drives the gradient of spectra’s mapping.
#' @param grad Number of colors that are used to build the gradient.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @export
#' @returns Plot of spectra in which segments have a different background color.
#' @examples
#' data(beer)
#' conc=unlist(beer[,1])
#' sp=beer[,2:ncol(beer)]
#' names(sp)=convert.names.wl(1100,2250,2)
#' segment.windows(sp,conc,windows=7,fade=0.25)

segment.windows=function(xblock=NULL,
                      yblock=NULL,
                      windows=3,
                      fade=0.3,
                      xlab='Wavelength',
                      ylab='Absorbance',
                      title=paste('Spectra divided in',
                                  windows,
                                  'segments',
                                  sep=' '),
                      legend=NULL,
                      grad=10){

  data.wide=as.data.frame(cbind(xblock,yblock))
  transposed=data.wide %>%
    mutate(row = row_number()) %>%
    gather(nm, value, -yblock, -row) %>%
    mutate(nm = parse_number(nm))

  cols=dim(xblock)[2]
  s=round(seq(1,cols,length.out=windows+1))
  len=substring(make.names(names(xblock)[s]),2)
  min=as.numeric(substring(make.names(names(xblock)[s]),2)[-length(len)])
  max=as.numeric(substring(make.names(names(xblock)[s]),2)[-1])
  back=as.data.frame(cbind(min,max))

  print(ggplot()+
          geom_line(transposed,mapping=aes(x = nm,
                                           y = value,
                                           group = row,
                                           color=yblock),

                    stat="identity",
                    linewidth=0.75)+
          scale_colour_gradientn(colours=rev(topo.colors(grad)),
                                 name=stringr::str_wrap(paste(legend),
                                                        width = 10))+
          labs(x = xlab,
               y=ylab,
               title=title)+
          geom_rect(data=back,
                    aes(xmin=min,
                        xmax=max,
                        ymin=-Inf,
                        ymax=+Inf,),
                    color='white',
                    alpha=fade,
                    fill=heat.colors(length(len)-1))+
          guides(fill='none',color='none')+
          theme_bw())
}
