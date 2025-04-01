library(data.table)
library(sf)
library(ggplot2)
library(necountries)
library(units)
library(ggh4x)
library(ggnewscale)
library(ks)
library(spatialEco)
library(terra)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")
crs_america<-"+proj=laea +lat_0=30 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

res<-10
window_size<-1
target<-sprintf("../Data/Loss.Gain/%s/res.%d_window.%d.rda",
                "ALL", res, window_size)
sf<-read_sf(sprintf("../Shape/target_grids/target_grids_%dkm.shp", res))

lc_lc<-readRDS(sprintf("../Data/LandCover/Change/by_LC.window_%d.%dkm.rda", window_size, res))
table(lc_lc$year)
range(sf$seqnum)
range(lc_lc$grid_id)

sf_usa<-merge(sf, lc_lc[LC==1 & year==2020], by.x="seqnum", by.y="grid_id")
ggplot(sf_usa)+geom_sf()

lc_all<-readRDS(sprintf("../Data/LandCover/Change/ALL.window_%d.%dkm.rda", window_size, res))
if (window_size==1){
  lc_all$window_size<-window_size
  lc_all$N_Diff<-NULL
  lc_all<-lc_all[, c("grid_id", "res", "ALL_N", "year", "window_size", "Change_ratio_ALL")]
}
sf_usa<-merge(sf, lc_all[year==2020], by.x="seqnum", by.y="grid_id")
ggplot(sf_usa)+geom_sf()

colnames(lc_all)[c(1, 4)]<-c("grid_index", "Year")

lc_all_sum<-lc_all[, .(Change_ratio_ALL==sum(Change_ratio_ALL)),
                   by=list(grid_index, res, ALL_N, window_size)]


result_list<-readRDS(target)


setindex(result_all, "grid_index")


result_list_with_year<-merge(result_list, lc_all, by=c("Year", "grid_index"))

result_all<-result_list_with_year[group %in% c("Aves", "Apidae", "Mammalia") & 
                                  between(Year, 1986, 2019), 
                        .(N=.N, 
                          SP_LOSS_RATE=mean(SP_LOSS_RATE),
                          SP_GAIN_RATE=mean(SP_GAIN_RATE),
                          SP_Jaccard=mean(SP_Jaccard),
                          Change_ratio_ALL=mean(Change_ratio_ALL)
                        ),
                        by=list(group, grid_index, ALL_SP)]
sf_all<-merge(sf, result_all[group %in% c("Aves", "Apidae", "Mammalia")], 
              by.x="seqnum", by.y="grid_index")
sf_with_year<-merge(sf, result_list_with_year[group %in% c("Aves", "Apidae", "Mammalia")], 
                  by.x="seqnum", by.y="grid_index")


result_list_with_year[Change_ratio_ALL>1]
cor(sf_all$SP_Jaccard, sf_all$Change_ratio_ALL)
ggplot(sf_all)+geom_point(aes(x=Change_ratio_ALL, y=SP_LOSS_RATE, size=ALL_SP), alpha=0.2, shape=1)+
  facet_wrap(~group)
ggplot(sf_all)+geom_point(aes(x=Change_ratio_ALL, y=SP_GAIN_RATE, size=ALL_SP), alpha=0.2, shape=1)+
  facet_wrap(~group)
ggplot(sf_all)+geom_point(aes(x=Change_ratio_ALL, y=SP_Jaccard, size=ALL_SP), alpha=0.2, shape=1)+
  facet_wrap(~group)


ggplot(sf_with_year)+geom_point(aes(x=Change_ratio_ALL, y=SP_Jaccard, size=ALL_SP), alpha=0.2, shape=1)+
  facet_grid(~group)

ggplot(sf_all[which(sf_all$group=="Aves"),])+
  geom_sf(aes(fill=SP_Jaccard), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_grid(~group)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 0.4
  )+theme_bw()

kdf_df<-data.table(sf_all[which(sf_all$group=="Aves"),])
kdf_df$geometry<-NULL

kdf_df <- st_as_sf(kdf_df, coords = c("lon", "lat"), crs = 4326) 
meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 4326, 
                  agr = "constant") 
cadmium.kde.500 <- sf.kde(x = meuse, y = meuse$cadmium, res=40, 
                          bw = 500, standardize = TRUE)
kde_result <- sp.kde(x=kdf_df, y=kdf_df$SP_Jaccard, standardize = T, 
                     res=0.05) 
plot(kde_result)
kdf_df<-sf_all[which(sf_all$group=="Aves"),]
kdf_df$KDE<-extract(kde_result, data.frame(x=kdf_df$lon, y=kdf_df$lat), ID=F)$z
cor(kdf_df$KDE, kdf_df$Change_ratio_ALL)

ppp<-as.data.table(kde_result, xy=T)
ggplot(kdf_df)+
  geom_sf(aes(fill=SP_Jaccard), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_grid(~group)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 0.4
  )+theme_bw()

ggplot(kdf_df[which(kdf_df$group=="Aves" & kdf_df$Change_ratio_ALL<0.1),])+
  geom_sf(aes(fill=Change_ratio_ALL), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_grid(~group)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 0.01
  )+theme_bw()

ggplot(kdf_df)+
  geom_sf(aes(fill=KDE), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_grid(~group)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 5000
  )+theme_bw()

ggplot(sf_all[which(sf_all$group=="Aves"),], 
       aes(x=lon, y=lat, weight=SP_Jaccard))+
  geom_density_2d(aes(color = ..level..), 
                  geom = "tile", 
                  contour = FALSE)
  
  coord_sf(crs = st_crs(crs_america))+
  facet_grid(~group)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 0.4
  )+theme_bw()


result_list_with_lc_sampled<-result_list_with_lc[sample(nrow(result_list_with_lc), 1e4)]
#glm(data=result_list_with_lc, SP_LOSS_RATE~Change_ratio_ALL+group)

p1<-ggplot(result_list_with_lc_sampled[group %in% c("Aves", "Apidae", "Mammalia")])+
  geom_point(aes(x=Change_ratio_ALL, y=SP_LOSS_RATE, size=N_SP_Curr), shape=1, alpha=0.2)+
  #scale_x_log10()+
  facet_wrap(~group, nrow=1)

p1
p2<-ggplot(result_list_with_lc_sampled[group %in% c("Aves", "Apidae", "Mammalia")])+
  geom_point(aes(x=Change_ratio_ALL, y=SP_GAIN_RATE, size=N_SP_Curr), shape=1, alpha=0.2)+
  facet_wrap(~group, nrow=1)

p2

p3<-ggplot(result_list_with_lc_sampled[group %in% c("Aves", "Apidae", "Mammalia")])+
  geom_point(aes(x=Change_ratio_ALL, y=SP_Jaccard, size=N_SP_Curr), shape=1, alpha=0.2)+
  #scale_x_log10()+
  facet_wrap(~group, nrow=1)
#hist(result_list_with_lc$Change_ratio_ALL)
p3
ggpubr::ggarrange(plotlist = list(p1, p2, p3),
                  ncol=1)
year_threshold<-1950
result_se<-result_list[Year>year_threshold, .(
  Turnover_SP=mean(SP_LOSS_RATE + SP_GAIN_RATE),
  SP_LOSS_RATE=mean(SP_LOSS_RATE, na.rm=T),
  SP_GAIN_RATE=mean(SP_GAIN_RATE, na.rm=T),
  
  OCC_LOSS_RATE=mean(OCC_LOSS_RATE, na.rm=T),
  OCC_GAIN_RATE=mean(OCC_GAIN_RATE, na.rm=T),
  Stability_SP=mean(SP_Jaccard),
  Turnover_OCC=mean(OCC_LOSS_RATE + OCC_GAIN_RATE),
  Stability_OCC=mean(OCC_Jaccard)
),
by=list(grid_index)]
N_SP<-target_df[year>year_threshold, .(N_SP=length(unique(species))), by=list(grid_index)]
result_se<-merge(result_se, N_SP, by="grid_index")
hist(result_se$Stability_SP)

ggplot(result_se)+geom_point(aes(x=N_SP, y=Stability_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 50
  )+theme_bw()

p0<-ggplot(result_se)+geom_point(aes(x=Stability_SP, y=Stability_OCC, color=N_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 50
  )+theme_bw()
sf_target<-merge(sf, result_se, by.x="seqnum", by.y="grid_index")

p1<-ggplot(sf_target)+
  geom_sf(aes(fill=Stability_SP), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  #xlim(-150, -50)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 0.75
  )
p1

p2<-ggplot(sf_target)+
  geom_sf(aes(fill=Stability_OCC), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  #xlim(-150, -50)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 0.75
  )
p2

p3<-ggplot(sf_target)+
  geom_sf(aes(fill=N_SP), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  #xlim(-150, -50)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 50
  )
p3


p<-ggpubr::ggarrange(p0, p1, p2, p3)
p

p3<-ggplot(result_se)+geom_point(aes(x=SP_LOSS_RATE, y=SP_GAIN_RATE, color=N_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 50
  )+theme_bw()

p3

p4<-ggplot(result_se)+geom_point(aes(x=OCC_LOSS_RATE, y=OCC_GAIN_RATE, color=N_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 50
  )+theme_bw()

p4
p<-ggpubr::ggarrange(p3, p4)
p

result_list[Jaccard>10]
if (F){
  
  df_list<-list()
  for (col in c("N_SP_Gain", "N_OCC_Gain", "N_SP_Loss", "N_OCC_Loss",
                #"N_SP_Curr", "N_OCC_Curr", "N_SP_Prev", "N_OCC_Prev",
                "N_SP_Diff", "N_OCC_Diff")){
    ccc<- c("grid_index", "Year", col)
    item<-result_list[, ..ccc]
    colnames(item)[3]<-"V"
    item$Var<-gsub("_OCC", "", gsub("_SP", "", col))
    item$Type<-ifelse(grepl("SP", col), "SP", "OCC")
    df_list[[length(df_list)+1]]<-item
  }
  df_list<-rbindlist(df_list)
  ggplot(df_list)+geom_line(aes(x=Year, y=V, group=grid_index))+
    facet_grid2(Var~Type, scale="free_y", independent="y")+
    theme_bw()
  coords<-st_coordinates(st_centroid(sf))
  sf$lon<-coords[,1]
  sf$lat<-coords[,2]
  sf_target<-merge(sf, df_list, by.x="seqnum", by.y="grid_index")
  #sf_target<-data.table(sf_target)
  #sf_target$geometry<-NULL
  p1<-ggplot(sf_target[which(sf_target$Year==2010) & sf_target$Type=="SP",])+
    geom_sf(aes(fill=V), color=NA)+
    coord_sf(crs = st_crs(crs_america))+
    facet_grid2(Var~Type)+
    #xlim(-150, -50)+
    scale_fill_gradient2(
      low = "#0072B2",
      mid = "#FFFFFF", 
      high = "#CC79A7",
      midpoint = 0
    )
  p2<-ggplot(sf_target[which(sf_target$Year==2010) & sf_target$Type=="OCC",])+
    geom_sf(aes(fill=log(V)), color=NA)+
    coord_sf(crs = st_crs(crs_america))+
    facet_grid2(Var~Type)+
    #xlim(-150, -50)+
    scale_fill_gradient2(
      low = "#0072B2",
      mid = "#FFFFFF", 
      high = "#CC79A7",
      midpoint = 8
    )
  p<-ggpubr::ggarrange(p1, p2)
  ggsave(p, filename="../Figures/test.png", width=10, height=10,
         bg="white")
}