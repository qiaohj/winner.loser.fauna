library(data.table)
library(sf)
library(ggplot2)
library(necountries)
library(units)
library(ggh4x)
library(ggnewscale)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")




if (F){
  #df<-readRDS(sprintf("../Data/RAW/%s_%dkm.rda", group, res))
  #if (group=="Insecta"){
  #  df<-df[family=="Apidae"]
  #  group<-"Apidae"
  #}
  
  for (res in c(10, 20, 50, 100)){
    print(res)
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
    
    if (res==100){
      id_threshold<-3e4
      
    }
    if (res==10){
      id_threshold<-25e5
      
    }
    if (res==20){
      id_threshold<-84e4
      
    }
    if (res==50){
      id_threshold<-93e3
      
    }
    
    sf_target<-sf_target[which(sf_target$lon<= -55 & sf_target$lon>= -150 
                               & sf_target$seqnum<id_threshold),]
    if (F){
      ggplot(sf_target[which(sf_target$lon<= -55 & sf_target$lon>= -150 & 
                               sf_target$seqnum<id_threshold),])+
        geom_sf(aes(fill=lon))+coord_sf(crs = st_crs(crs_america))
    }
    write_sf(sf_target, sprintf("../Shape/target_grids/target_grids_%dkm.shp", res))
  }
  
  
  hist(sf_target[which(sf_target$lon<= -20 & sf_target$lon>= -150),]$lon)
  ggplot(sf_target[which(sf_target$lon<= -55 & sf_target$lon>= -150 & sf_target$seqnum<3e4),])+
    geom_sf(aes(fill=lon))+coord_sf(crs = st_crs(crs_america))
  target_df<-df[grid_index %in% sf_target$seqnum]
  saveRDS(target_df, sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", group, res))
  
  
  
  coms<-data.table(expand.grid(res=c(10, 20, 50, 100),
                               group=c("Amphibia", "Aves", "Mammalia", "Insecta", "Reptilia")))
  #coms<-data.table(expand.grid(res=c(20, 50, 100),
  #                             group=c("Insecta")))
  for (i in c(1:nrow(coms))){
    item<-coms[i]
    print(paste(i, "/", nrow(coms), ":", item$res, item$group))
    sf_target<-read_sf(sprintf("../Shape/target_grids/target_grids_%dkm.shp", item$res))
    if (item$group=="Insecta"){
      target<-sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", 
                      "Apidae", item$res)
    }else{
      target<-sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", 
                      item$group, item$res)
    }
    
    
    if (file.exists(target)){
      next()
    }
    df<-readRDS(sprintf("../Data/RAW/%s_%dkm.rda", item$group, item$res))
    if (item$group=="Insecta"){
      df<-df[family=="Apidae"]
    }
    target_df<-df[grid_index %in% sf_target$seqnum]
    saveRDS(target_df, target)
    
  }
}

if (F){
  for (res in c(10)){
    
    df_list<-list()
    for (group in c("Amphibia", "Aves", "Mammalia", "Apidae", "Reptilia")){
      print(paste(res, group))
      df<-readRDS(sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", group, res))
      df$group<-group
      df_list[[group]]<-df
    }
    df_all<-rbindlist(df_list)
    saveRDS(df_all, sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", "ALL", res))
  }
  
}
for (res in c(10, 20, 50, 100)){
  print(res)
  group<-"ALL"
  target_df<-readRDS(sprintf("../Data/Target_Area/American_Land_%s_%dkm.rda", group, res))
  target_df<-target_df[between(year, 1985, 2019)]
  #Count<-target_df[, .(N_OCC=sum(N_OCC), N_SP=length(unique(species))),
  #                 by=list(year, group)]
  
  
  if (F){
    ggplot(Count[between(year, 1950, 2022)])+
      geom_line(aes(x=year, y=N_OCC, color=group))+
      scale_y_log10()
    
    ggplot(Count[between(year, 1950, 2022)])+
      geom_line(aes(x=year, y=N_SP,  color=group))
  }
  Count<-target_df[, .(N_OCC=sum(N_OCC), N_SP=length(unique(species))),
                   by=list(year, grid_index, group)]
  
  CountALL<-target_df[, .(ALL_OCC=sum(N_OCC), ALL_SP=length(unique(species))),
                      by=list(grid_index, group)]
  
  CountFULL<-target_df[, .(FULL_OCC=sum(N_OCC), FULL_SP=length(unique(species))),
                       by=list(group)]
  
  Count<-merge(merge(Count, CountALL, by=c("grid_index", "group")), CountFULL, by="group")
  #hist(Count[group=="Aves"]$ALL_SP)
  Count_SE<-Count[, .(N_OCC=mean(N_OCC), N_SP=mean(N_SP),
                      N_OCC_SD=sd(N_OCC), N_SP_SD=sd(N_SP)),
                  by=list(year, group)]
  if (F){
    ggplot(Count_SE[between(year, 1950, 2022)])+
      #geom_ribbon(aes(x=year, y = N_SP, ymin = N_SP - N_SP_SD, ymax = N_SP + N_SP_SD), alpha = .2) +
      geom_line(aes(x=year, y=N_SP, color=group))+
      scale_y_log10()
    
    ggplot(Count_SE[between(year, 1950, 2021)])+
      #geom_ribbon(aes(x=year, y = N_OCC, ymin = N_OCC - N_OCC_SD, ymax = N_OCC + N_OCC_SD), alpha = .2) +
      geom_line(aes(x=year, y=N_OCC, color=group))+
      scale_y_log10()
    
    sf<-read_sf(sprintf("../Shape/grids/grids_%dkm.shp", res))
    sf_target<-merge(sf, Count, by.x="seqnum", by.y="grid_index")
    crs_america<-"+proj=laea +lat_0=30 +lon_0=-120 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
    
    target_years<-sf_target[which(sf_target$year %in% c(1950, 2000, 2019)),]
    
    p1<-ggplot()+
      geom_sf(data=target_years[which(target_years$group=="Aves"),],
              aes(fill=N_SP), color=NA)+
      coord_sf(crs = st_crs(crs_america))+
      facet_wrap(~year, ncol=1)+
      labs(title="Aves")+
      scale_fill_gradient2(
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group=="Aves"),]$N_SP)),
        midpoint = max(target_years[which(target_years$group=="Aves"),]$N_SP)/2
      )+
      theme()
    p1
    p2<-  ggplot()+geom_sf(data=target_years[which(target_years$group=="Mammalia"),],
                           aes(fill=N_SP), color=NA)+
      coord_sf(crs = st_crs(crs_america))+
      facet_wrap(~year, ncol=1)+
      labs(title="Mammalia", fill="Number of species")+
      scale_fill_gradient2(
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group=="Mammalia"),]$N_SP)),
        midpoint = max(target_years[which(target_years$group=="Mammalia"),]$N_SP)/2
      )+
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_blank())
    p2
    
    p3<-  ggplot()+geom_sf(data=target_years[which(target_years$group=="Apidae"),],
                           aes(fill=N_SP), color=NA)+
      coord_sf(crs = st_crs(crs_america))+
      facet_wrap(~year, ncol=1)+
      labs(title="Apidae", fill="Number of species")+
      scale_fill_gradient2(
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group=="Apidae"),]$N_SP)),
        midpoint = max(target_years[which(target_years$group=="Apidae"),]$N_SP)/2
      )+
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_blank())
    p3
    
    p<-ggpubr::ggarrange(p1, p2, p3, nrow=1)
    p
    
    target_years$group2 <- factor(target_years$group, 
                                  levels = c("Apidae","Aves",  "Mammalia", "Amphibia", "Reptilia")) 
    
    ggplot()+
      geom_sf(data=target_years[which(target_years$group %in% c("Mammalia")),],
              aes(fill=N_SP), color=NA)+
      coord_sf(crs = st_crs(crs_america))+
      facet_grid(rows=vars(group2), cols=vars(year))+
      scale_fill_gradient2(
        name="Apidae",
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group %in% c("Mammalia")),]$N_SP)),
        midpoint = max(target_years[which(target_years$group %in% c("Mammalia")),]$N_SP)/2)+
      ggnewscale::new_scale_fill() +
      geom_sf(data=target_years[which(target_years$group %in% c("Apidae")),],
              aes(fill=N_SP), color=NA)+
      scale_fill_gradient2(
        name="Aves",
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group %in% c("Apidae")),]$N_SP)),
        midpoint = max(target_years[which(target_years$group %in% c("Apidae")),]$N_SP)/2)+
      ggnewscale::new_scale_fill() +
      geom_sf(data=target_years[which(target_years$group %in% c("Aves")),],
              aes(fill=N_SP), color=NA)+
      scale_fill_gradient2(
        name="Mammalia",
        low = "#0072B2",
        mid = "#FFFFFF", 
        high = "#CC79A7",
        limit=c(0, max(target_years[which(target_years$group %in% c("Aves")),]$N_SP)),
        midpoint = max(target_years[which(target_years$group %in% c("Aves")),]$N_SP)/2)
  }
  for (window_size in c(1, 5, 10)){
    print(paste(group, res, window_size))
    target<-sprintf("../Data/Loss.Gain/%s/res.%d_window.%d.rda",
                    group, res, window_size)
    if (file.exists(target)){
      next()
    }
    years<-c(1985:2019)
    y<-years[1]
    result_list<-list()
    for (y in years){
      print(y)
      item_prev<-target_df[between(year, y-window_size, y-1), 
                           c("species", "grid_index", "N_OCC", "group")]
      item_prev<-item_prev[, .(N_OCC_Prev=sum(N_OCC)),
                           by=c("species", "grid_index", "group")]
      
      item_curr<-target_df[between(year, y-window_size+1, y), 
                           c("species", "grid_index", "N_OCC", "group")]
      item_curr<-item_curr[, .(N_OCC_Curr=sum(N_OCC)),
                           by=c("species", "grid_index", "group")]
      
      item_all<-merge(item_prev, item_curr, by=c("species", "grid_index", "group"),
                      all=T)
      item_all[is.na(N_OCC_Prev), "N_OCC_Prev"]<-0
      item_all[is.na(N_OCC_Curr), "N_OCC_Curr"]<-0
      
      
      gain<-item_all[N_OCC_Prev==0]
      gain_N<-gain[, .(N_SP_Gain=length(unique(species)),
                       N_OCC_Gain=sum(N_OCC_Curr)),
                   by=list(grid_index, group)]
      loss<-item_all[N_OCC_Curr==0]
      loss_N<-loss[, .(N_SP_Loss=length(unique(species)),
                       N_OCC_Loss=sum(N_OCC_Prev)),
                   by=list(grid_index, group)]
      
      curr_N<-item_curr[, .(N_SP_Curr=length(unique(species)),
                            N_OCC_Curr=sum(N_OCC_Curr)),
                        by=list(grid_index, group)]
      prev_N<-item_prev[, .(N_SP_Prev=length(unique(species)),
                            N_OCC_Prev=sum(N_OCC_Prev)),
                        by=list(grid_index, group)]
      
      all_N<-merge(gain_N, loss_N, all=T) |> merge(curr_N, all=T) |> merge(prev_N, all=T)
      all_N<-merge(all_N, CountALL, by=c("grid_index", "group"))
      all_N<-merge(all_N, CountFULL, by=c("group"))
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
    
    saveRDS(result_list, target)
  }
}
