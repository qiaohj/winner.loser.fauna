library(terra)
library(sf)
library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/WD10T_12/winner.loser.fauna/winner.loser.fauna")


for (y in c(1985:2022)){
  
  for (res in c(100, 20, 50, 10)){
    target<-sprintf("../Data/LandCover/grided/year.%d.res.%dkm.rda", y, res)
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    lc<-rast(sprintf("../Data/LandCover/usgs/RAW/Land.Cover/Annual_NLCD_LndCov_%d_CU_C1V0.tif", y))
    r_extent <- ext(lc)
    print(paste("year", y, "res", res))
    shp<-read_sf(sprintf("../Shape/target_grids/target_grids_%dkm.shp", res))
    shp_proj <- st_transform(shp, st_crs(lc))
    
    masked_list <- list() 
    for (i in 1:nrow(shp_proj)) {
      polygon <- shp_proj[i, ]
      shp_extent <- ext(polygon)
      if (!relate(r_extent, shp_extent, "intersects")) {
        next()
      }
      if ((i %% 1e3)==0){
        print(paste("year", y, "res", res, i, nrow(shp_proj)))
      }
      
      cr<-crop(lc, shp_extent)
      ma<-mask(cr, polygon)
      v<-values(ma)
      v<-v[!is.na(v)]
      if (length(v)==0){
        next()
      }
      
      v_df<-data.table(table(v))
      v_df$year<-y
      v_df$grid_id<-shp_proj[i, ]$seqnum
      v_df$res<-res
      masked_list[[length(masked_list)+1]] <- v_df
    }
    
    masked_df<-rbindlist(masked_list)
    saveRDS(masked_df, target)
  }
}

if (F){
  for (res in c(100, 20, 50, 10)){
    dflist<-list()
    for (y in c(1985:2022)){
      print(paste(res, y))
      target<-sprintf("../Data/LandCover/grided/year.%d.res.%dkm.rda", y, res)
      df<-readRDS(target)
      dflist[[length(dflist)+1]]<-df
    }
    dfdf<-rbindlist(dflist)
    if (F){
      table(dfdf$v)
    }
    dfdf[, LC := substr(v, 1, 1)]
    dfdf_large<-dfdf[, .(N=sum(N)), by=list(year, grid_id, res, LC)]
    dfdf_all<-dfdf_large[, .(ALL_N=sum(N)), by=list(year, grid_id, res)]
    dfdf_large<-merge(dfdf_large, dfdf_all, by=c("year", "grid_id", "res"))
    
    dfdf_prev<-dfdf_large
    dfdf_prev$year<-dfdf_prev$year+1
    colnames(dfdf_prev)[5]<-"Prev_N"
    dfdf_large_full<-merge(dfdf_large, dfdf_prev, by=c("year", "grid_id", "res", "LC", "ALL_N"))
    setorderv(dfdf_large_full, c("LC", "grid_id", "year"))
    dfdf_large_full$N_Diff<-abs(dfdf_large_full$N-dfdf_large_full$Prev_N)
    
    dfdf_large_full[, Change_ratio_LC := (N - Prev_N) / ALL_N, by = .(grid_id, LC, year, res, ALL_N)]
    #dfdf_large_full[Change_ratio_LC>=0.5]
    dfdf_all<-dfdf_large_full[, .(N_Diff=sum(N_Diff)), by=list(grid_id, year, res, ALL_N)]
    dfdf_all[, Change_ratio_ALL := N_Diff / ALL_N, by = .(grid_id, year, res, ALL_N)]
    #hist(dfdf_all$Change_ratio_ALL)
    #dfdf_all[Change_ratio_ALL>1]
    
    setorderv(dfdf_large_full, c("grid_id", "year", "LC"))
    saveRDS(dfdf_large_full, sprintf("../Data/LandCover/Change/by_LC.window_1.%dkm.rda", res))
    saveRDS(dfdf_all, sprintf("../Data/LandCover/Change/ALL.window_1.%dkm.rda", res))
    for (window_size in c(5, 10)){
      from_y<-1986+window_size-1
      to_y<-2022
      ddd_lc<-list()
      ddd_all<-list()
      while (from_y<=to_y) {
        print(paste(res, from_y, window_size))
        item<-dfdf_large_full[between(year, from_y-4, from_y)]
        item<-item[, .(year=from_y, window_size=window_size,
                       Change_ratio_LC=sum(Change_ratio_LC),
                       Change_ratio_LC_ABS=sum(abs(Change_ratio_LC))),
                   by=list(grid_id, res, LC, ALL_N)]
        ddd_lc[[length(ddd_lc)+1]]<-item
        
        item<-dfdf_all[between(year, from_y-4, from_y)]
        item<-item[, .(year=from_y, window_size=window_size,
                       Change_ratio_ALL=sum(Change_ratio_ALL)),
                   by=list(grid_id, res, ALL_N)]
        ddd_all[[length(ddd_all)+1]]<-item
        from_y<-from_y+1
      }
      ddd_lc<-rbindlist(ddd_lc)
      ddd_all<-rbindlist(ddd_all)
      saveRDS(ddd_lc, sprintf("../Data/LandCover/Change/by_LC.window_%d.%dkm.rda", 
                              window_size, res))
      saveRDS(ddd_all, sprintf("../Data/LandCover/Change/ALL.window_%d.%dkm.rda", 
                                window_size, res))
      
    }
  }
}