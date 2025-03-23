coms1<-sprintf("aria2c --allow-overwrite=false -s 8 -x 8 https://s3-us-west-2.amazonaws.com/mrlc/Annual_NLCD_LndCov_%d_CU_C1V0.tif", c(1985:2023))
coms2<-sprintf("aria2c --allow-overwrite=false -s 8 -x 8 https://s3-us-west-2.amazonaws.com/mrlc/Annual_NLCD_LndChg_%d_CU_C1V0.tif", c(1985:2023))
coms<-c(coms1, coms2)
writeLines(coms, "../Data/LandCover/usgs/download.sh")
