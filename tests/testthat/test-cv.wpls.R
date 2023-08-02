library(testthat);
source("C:\\Users\\Elia Gonzato\\Desktop\\windows.pls\\R\\cv.wpls.R");


data("beer")
conc=unlist(beer[,1])
spectra=beer[,2:ncol(beer)]
names(spectra)=convert.names.wl(1100,2250,2)
mywpls=cv.wpls(spectra,conc,mode='wpls',windows = 3);
#
test_that(desc="The best window is Window1",code ={
  comp=mywpls$windows$nwindows[4];
  r2=as.numeric(round(as.data.frame(mywpls[[4]][2])[1,1],2))
  rmse=as.numeric(round(as.data.frame(mywpls[[3]][1])[1,1],2))
  expect_equal(object=comp,expected='Complete');
  expect_equal(object=r2,expected=0.81);
  expect_equal(object=rmse,expected=0.95);
  expect_equal(object=is.numeric(r2),expected=TRUE);
  expect_equal(object=is.numeric(rmse),expected=TRUE);
  expect_equal(object = is.character(comp),expected = TRUE)
} )
