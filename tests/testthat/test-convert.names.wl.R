library(testthat);
source("C:\\Users\\Elia Gonzato\\Desktop\\windows.pls\\R\\convert.names.wl.R");


data("beer")
conc=unlist(beer[,1])
spectra=beer[,2:ncol(beer)]
names(spectra)=convert.names.wl(1100,2250,2)

#
test_that(desc="The best window is Window1",code ={
  name4=names(spectra)[4];
  expect_equal(object=name4,expected='X1106');
  expect_equal(object = is.character(name4),expected = TRUE)

} )
