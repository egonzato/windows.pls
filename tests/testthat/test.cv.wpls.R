library(testthat);
context("Cross-validation Window pls testing");
source("cv.wpls.R");


data("beer")
conc=unlist(beer[,1])
spectra=beer[,2:ncol(beer)]
names(spectra)=convert.names.wl(1100,2250,2)
mywpls=cv.wpls(spectra,conc,mode='wpls',windows = 3);

#
test_that(desc="The best window is Window1",code ={
    comp=mywpls$windows$nwindows[4];
    r2=round(as.data.frame(mywpls[[4]][2])[1,1],2)
    rmse=round(as.data.frame(mywpls[[3]][1])[1,1],2)
    expect_that(object=comp,condition=equals('Complete'));
    expect_that(object=r2,condition=equals(0.81));
    expect_that(object=rmse,condition=equals(0.95));
    expect_that(object=is.numeric(r2),condition=equals(TRUE));
    expect_that(object=is.numeric(rmse),condition=equals(TRUE));
    expect_that(object = is.character(comp),condition = equals(TRUE))

} )
