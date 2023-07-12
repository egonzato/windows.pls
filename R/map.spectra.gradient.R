#' Colors and plots each spectra based on the associated concentration of the outcome variable
#'
#' @param xblock A matrix containing one spectra for each observation.
#' @param yblock A vector containing the concentration associated to each spectra in the *xblock* matrix.
#' @param legend.title Title of the legend which displays the gradient.
#' @param plot.title Title of the plot.
#' @param xlab Title of the x axis.
#' @param ylab Title of the y axis.
#' @param grad Number of colors for the gradient's palette.
#' @param l.width Width of each spectra.
#' @param col.legend Deletes presence of the legend.
#' @import ggplot2
#' @import mdatools
#' @import tidyverse
#' @import readr
#' @import stringr
#' @import grDevices
#' @import dplyr
#' @import tidyr
#' @export
#' @returns Plot with spectra of all observations, mapped with the intensity of the associated concentration.
#' @examples
#' data(beer)
#' X=beer[,2:ncol(beer)]
#' names(X)=convert.names.wl(1100,2250,2)
#' Y=unlist(beer[,1])
#' map.spectra.gradient(X,Y)

map.spectra.gradient=function(xblock=NULL,
                     yblock=NULL,
                     legend.title='Gradient',
                     plot.title='Spectra with gradient based on Y variable',
                     xlab='Wavelength',
                     ylab='Absorbance',
                     grad=10,
                     l.width=0.75,
                     col.legend=NULL){
  data.wide=as.data.frame(cbind(xblock,yblock))
  transposed=data.wide %>%
    mutate(row = row_number()) %>%
    gather(nm, value, -yblock, -row) %>%
    mutate(nm = parse_number(nm))
  #
  print(ggplot(transposed, aes(x = nm, y = value, group = row))+
          geom_line(aes(color=yblock),stat="identity",linewidth=l.width)+
          scale_colour_gradientn(colours=rev(topo.colors(grad)),
                                 name=str_wrap(paste(legend.title),
                                               width = 0.8))+
          guides(color=col.legend)+
          labs(x = xlab,
               y=ylab,
               title=plot.title)+
          theme_bw())
}
