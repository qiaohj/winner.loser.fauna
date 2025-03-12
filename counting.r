library(data.table)
library(sf)
library(ggplot2)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")
group<-"Amphibia"
#taxa<-readRDS(sprintf("/media/huijieqiao/WD10T_12/datasets_harvest/GBIF/202207/Taxa/%s.rda", group))

base<-"/media/huijieqiao/WD10T_12/datasets_harvest/GBIF/202207/RAW"
df<-fread(sprintf("%s/%s/%s.csv", base, group, group), sep = "\t", quote="")
df<-df[!is.na(decimalLongitude) & !is.na(decimalLatitude)]
df<-df[!is.na(year)]
df$lon<-round(df$decimalLongitude, 1)
df$lat<-round(df$decimalLatitude, 1)
saveRDS(df, sprintf("../Data/RAW/%s_full.rda", group))
df_ll<-df[, c("lon", "lat")]
df_ll<-unique(df_ll)
sf_points <- st_as_sf(df_ll, coords = c("lon", "lat"), crs = 4326)
resolutions<-c(100, 50, 20, 10)

for (res in resolutions){
  target<-sprintf("../Data/RAW/%s_%dkm.rda", group, res)
  print(target)
  if (file.exists(target)){
    next()
  }
  sf<-read_sf(sprintf("../Shape/grids/grids_%dkm.shp", res))
  relation<-st_contains(sf, sf_points)
  names(relation)<-sf$seqnum
  
  dt_index <- rbindlist(lapply(names(relation), function(index) {
    data.table(index = as.numeric(index), value = relation[[as.numeric(index)]])
  }))
  sf_points[dt_index$value, "grid_index"]<-dt_index$index
  sf_points$lon<-st_coordinates(sf_points)[,1]
  sf_points$lat<-st_coordinates(sf_points)[,2]
  points<-data.table(sf_points[, c("grid_index", "lon", "lat")])
  points$geometry<-NULL
  
  df_n<-df[, c("kingdom", "phylum", "class", "order", "family", "genus", "species", "year", "lon", "lat")]
  df_n<-df_n[species!=""]
  df_n<-df_n[, .(N_OCC=.N), by=c("kingdom", "phylum", "class", "order", "family", "genus", "species", "year", "lon", "lat")]
  
  df_index<-merge(df_n, points, by=c("lon", "lat"))
  df_grid<-df_index
  df_grid$lon<-NULL
  df_grid$lat<-NULL
  df_grid<-df_grid[, .(N_OCC=sum(N_OCC)),
                   by=c("kingdom", "phylum", "class", "order", 
                        "family", "genus", "species", "year", 
                        "grid_index")]
  saveRDS(df_grid, target)
}


Count<-df_index[, .(N_OCC=sum(N_OCC), N_SP=length(unique(species))),
                by=list(grid_index, year)]


Count_test<-Count[year %in% c(1900, 2000, 2010, 2020)]
Count_test<-data.frame(Count_test)
Count_test<-merge(sf, Count_test, by.y="grid_index", by.x="seqnum")

table(Count_test$year)
ggplot(Count_test)+geom_sf(aes(fill=log(N_OCC)), color=NA)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 5,
    limits = c(0, 10)
  )+
  facet_wrap(~year)+
  theme_bw()

ggplot(Count_test)+geom_sf(aes(fill=N_SP), color=NA)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 25,
    limits = c(0, 50)
  )+
  facet_wrap(~year)+
  theme_bw()
