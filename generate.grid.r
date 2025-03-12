library(terra)
library(dggridR)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")
#10, 20, 50, 100km
resolutions<-c(100, 50, 20, 10)
for (resolution in resolutions){
  print(resolution)
  rrr<-dg_closest_res(dggs, col="spacing_km", val=resolution)
  dggs<-dgconstruct(res = rrr)
  earth<-dgearthgrid(dggs)
  
  shpfname = sprintf("../Shape/grids/grids_%dkm.shp", resolution)
  write_sf(earth, shpfname)
  
}


