library(data.table)
library(sf)
library(ggplot2)
library(necountries)
library(units)
library(ggh4x)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")
group<-"Aves"
res<-100


if (F){
  df<-readRDS(sprintf("../Data/RAW/%s_%dkm.rda", group, res))
  target<-countries(name=c("Canada", "United States of America"))
  target<-st_cast(target, "POLYGON")
  threshold<-area <- set_units(1e12, "m^2")
  crs_america<-"+proj=laea +lat_0=30 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  target<-target[which(st_area(target)>threshold),]
  sf<-read_sf(sprintf("../Shape/grids/grids_%dkm.shp", res))
  
  index<-st_intersects(target, sf)
  index<-unlist(index)
  
  sf_target<-sf[index,]
  coords<-st_coordinates(st_centroid(sf_target))
  sf_target$lon<-coords[,1]
  sf_target$lat<-coords[,2]
  sf_target<-sf_target[which(sf_target$lon<= -55 & sf_target$lon>= -150 & sf_target$seqnum<3e4),]
  write_sf(sf_target, "../Shape/target_grids/target_grids.shp")
  hist(sf_target[which(sf_target$lon<= -20 & sf_target$lon>= -150),]$lon)
  ggplot(sf_target[which(sf_target$lon<= -55 & sf_target$lon>= -150 & sf_target$seqnum<3e4),])+
    geom_sf(aes(fill=lon))+coord_sf(crs = st_crs(crs_america))
  target_df<-df[grid_index %in% sf_target$seqnum]
  saveRDS(target_df, "../Data/American_Land.rda")
}

target_df<-readRDS("../Data/American_Land.rda")

Count<-target_df[, .(N_OCC=sum(N_OCC), N_SP=length(unique(species))),
                by=list(year)]
ggplot(Count[between(year, 1999, 2021)])+
  geom_line(aes(x=year, y=N_OCC))

ggplot(Count[between(year, 1999, 2021)])+
  geom_line(aes(x=year, y=N_SP))

Count<-target_df[, .(N_OCC=sum(N_OCC), N_SP=length(unique(species))),
          by=list(year, grid_index)]
Count_SE<-Count[, .(N_OCC=mean(N_OCC), N_SP=mean(N_SP),
                    N_OCC_SD=sd(N_OCC), N_SP_SD=sd(N_SP)),
                by=list(year)]

ggplot(Count_SE[between(year, 1950, 2021)])+
  geom_ribbon(aes(x=year, y = N_SP, ymin = N_SP - N_SP_SD, ymax = N_SP + N_SP_SD), alpha = .2) +
  geom_line(aes(x=year, y=N_SP))

ggplot(Count_SE[between(year, 1950, 2021)])+
  #geom_ribbon(aes(x=year, y = N_OCC, ymin = N_OCC - N_OCC_SD, ymax = N_OCC + N_OCC_SD), alpha = .2) +
  geom_line(aes(x=year, y=N_OCC))

sf_target<-merge(sf, Count, by.x="seqnum", by.y="grid_index")
crs_america<-"+proj=laea +lat_0=30 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
ggplot(sf_target[which(sf_target$year %in% c(1995, 2000, 2005, 2010, 2015, 2020)),])+
  geom_sf(aes(fill=N_SP), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_wrap(~year)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 250,
    limits = c(0, 500)
  )

ggplot(sf_target[which(sf_target$year %in% c(1995, 2000, 2005, 2010, 2015, 2020)),])+
  geom_sf(aes(fill=log(N_OCC)), color=NA)+
  coord_sf(crs = st_crs(crs_america))+
  facet_wrap(~year)+
  scale_fill_gradient2(
    low = "#0072B2",
    mid = "#FFFFFF", 
    high = "#CC79A7",
    midpoint = 8,
    limits = c(0, 15)
  )


window_size<-5
years<-c(1900:2022)
y<-years[1]
result_list<-list()
for (y in years){
  print(y)
  item_prev<-target_df[between(year, y-window_size, y-1), 
                  c("species", "grid_index", "N_OCC")]
  item_prev<-item_prev[, .(N_OCC_Prev=sum(N_OCC)),
             by=c("species", "grid_index")]
  
  item_curr<-target_df[between(year, y-window_size+1, y), 
                       c("species", "grid_index", "N_OCC")]
  item_curr<-item_curr[, .(N_OCC_Curr=sum(N_OCC)),
             by=c("species", "grid_index")]
  
  item_all<-merge(item_prev, item_curr, by=c("species", "grid_index"),
                  all=T)
  item_all[is.na(N_OCC_Prev), "N_OCC_Prev"]<-0
  item_all[is.na(N_OCC_Curr), "N_OCC_Curr"]<-0
  
  
  gain<-item_all[N_OCC_Prev==0]
  gain_N<-gain[, .(N_SP_Gain=length(unique(species)),
                   N_OCC_Gain=sum(N_OCC_Curr)),
               by=list(grid_index)]
  loss<-item_all[N_OCC_Curr==0]
  loss_N<-loss[, .(N_SP_Loss=length(unique(species)),
                   N_OCC_Loss=sum(N_OCC_Prev)),
               by=list(grid_index)]
  
  curr_N<-item_curr[, .(N_SP_Curr=length(unique(species)),
                        N_OCC_Curr=sum(N_OCC_Curr)),
                    by=list(grid_index)]
  prev_N<-item_prev[, .(N_SP_Prev=length(unique(species)),
                   N_OCC_Prev=sum(N_OCC_Prev)),
               by=list(grid_index)]
  
  all_N<-merge(gain_N, loss_N, all=T) |> merge(curr_N, all=T) |> merge(prev_N, all=T)
  
  all_N[is.na(all_N)]<-0
  all_N$N_SP_Diff<-all_N$N_SP_Curr - all_N$N_SP_Prev
  all_N$N_OCC_Diff<-all_N$N_OCC_Curr - all_N$N_OCC_Prev
  all_N$Year<-y
  result_list[[length(result_list)+1]]<-all_N
  if (F){
    ggplot(all_N)+geom_histogram(aes(x=N_OCC_Diff))
    ggplot(all_N)+geom_histogram(aes(x=N_SP_Diff))
  }
}
result_list<-rbindlist(result_list)
result_list$N_SP_Overlap<-(result_list$N_SP_Curr+result_list$N_SP_Prev-result_list$N_SP_Gain-result_list$N_SP_Loss)/2
result_list$SP_Jaccard<-result_list$N_SP_Overlap/(result_list$N_SP_Overlap+result_list$N_SP_Gain+result_list$N_SP_Loss)
result_list$SP_LOSS_RATE<-result_list$N_SP_Loss/(result_list$N_SP_Overlap+result_list$N_SP_Loss+result_list$N_SP_Gain)
result_list$SP_GAIN_RATE<-result_list$N_SP_Gain/(result_list$N_SP_Overlap+result_list$N_SP_Loss+result_list$N_SP_Gain)

result_list$N_OCC_Overlap<-(result_list$N_OCC_Curr+result_list$N_OCC_Prev-result_list$N_OCC_Gain-result_list$N_OCC_Loss)/2
result_list$OCC_Jaccard<-result_list$N_OCC_Overlap/(result_list$N_OCC_Overlap+result_list$N_OCC_Gain+result_list$N_OCC_Loss)
result_list$OCC_LOSS_RATE<-result_list$N_OCC_Loss/(result_list$N_OCC_Overlap+result_list$N_OCC_Loss+result_list$N_OCC_Gain)
result_list$OCC_GAIN_RATE<-result_list$N_OCC_Gain/(result_list$N_OCC_Overlap+result_list$N_OCC_Loss+result_list$N_OCC_Gain)

saveRDS(result_list, sprintf("../Data/Loss.Gain/%s/res.%d_window.%d.rda",
                             group, res, window_size))

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

p0<-ggplot(result_se)+geom_point(aes(x=Stability_SP, y=Stability_OCC, color=N_SP))+
  scale_color_gradient2(
  low = "#0072B2",
  mid = "white", 
  high = "#CC79A7",
  midpoint = 300
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

p<-ggpubr::ggarrange(p0, p1, p2)
p

p3<-ggplot(result_se)+geom_point(aes(x=SP_LOSS_RATE, y=SP_GAIN_RATE, color=N_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 300
  )+theme_bw()

p3

p4<-ggplot(result_se)+geom_point(aes(x=OCC_LOSS_RATE, y=OCC_GAIN_RATE, color=N_SP))+
  scale_color_gradient2(
    low = "#0072B2",
    mid = "white", 
    high = "#CC79A7",
    midpoint = 300
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