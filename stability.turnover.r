library(data.table)
library(sf)
library(ggplot2)
library(necountries)
library(units)
library(ggh4x)
library(ggnewscale)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")

res<-100
window_size<-5
target<-sprintf("../Data/Loss.Gain/%s/res.%d_window.%d.rda",
                "ALL", res, window_size)
lc_lc<-readRDS(sprintf("../Data/LandCover/Change/by_LC.window_%d.%dkm.rda", window_size, res))
lc_all<-readRDS(sprintf("../Data/LandCover/Change/ALL.window_%d.%dkm.rda", window_size, res))

colnames(lc_all)[c(1, 4)]<-c("grid_index", "Year")
result_list<-readRDS(target)

result_list_with_lc<-merge(result_list, lc_all, by=c("Year", "grid_index"))

glm(data=result_list_with_lc, SP_LOSS_RATE~Change_ratio_ALL+group)

p1<-ggplot(result_list_with_lc[group %in% c("Aves", "Apidae", "Mammalia")])+
  geom_point(aes(x=Change_ratio_ALL, y=SP_LOSS_RATE, size=N_SP_Curr), shape=1, alpha=0.2)+
  facet_wrap(~group, nrow=1)

p1
p2<-ggplot(result_list_with_lc[group %in% c("Aves", "Apidae", "Mammalia")])+
  geom_point(aes(x=Change_ratio_ALL, y=SP_GAIN_RATE, size=N_SP_Curr), shape=1, alpha=0.2)+
  facet_wrap(~group, nrow=1)

p2

p3<-ggplot(result_list_with_lc[group %in% c("Aves", "Apidae", "Mammalia")])+
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