#' Turns wavelengths into variable's names
#'
#' @param start First wavelength of the spectra.
#' @param stop Last wavelength of the spectra.
#' @param step Distance between each recorded wavelength.
#' @returns Returns vector with syntactically valid names for each wavelength
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
#' data(beer)
#' X=beer[,2:ncol(beer)]
#' head(names(X))
#' names(X)=convert.names.wl(1100,2250,2)
#' head(names(X))

convert.names.wl=function(start=NULL,
                       stop=NULL,
                       step=2){
  s=seq(start,stop,step)
  wl=paste('X',s,sep='')
}
