
# merged_map <- snh
# values(merged_map)[values(crop1)==1]=2 # late crop
# values(merged_map)[values(crop2)==1]=3 # early crop

#plot(snh)
#plot(crop1)#
#plot(crop2)#
#plot(merged_map)

drange <- range(unlist(lapply(flor_val,function(x){x$week})))
season = list(timeperiods = drange[2]-drange[1]+1,weeks = seq(drange[1],drange[2]))# The season is specified according to the range of the flower period (flor_val)

if(!SA){
raster_list_no_drought <- 
  lapply(1:max(drange),function(week){
    f <- unlist(lapply(flor_val,function(x){
      if(sum(x$week==week)==0){f=0}else{
        f = x$flres[x$week==week]}
      return(f)
    }))
    reclassify(merged_map, matrix(c(0,0,1,f[1],2,f[2],3,f[3]),ncol = 2, byrow = T))
  })

raster_list_drought <- 
  lapply(1:max(drange),function(week){
    f <- unlist(lapply(1:3,function(i){
      if(sum(flor_val_D[[i]]$week==week)==0){f=0}else{
        f = flor_val_D[[i]]$flres[flor_val_D[[i]]$week==week]}
      return(f)
    }))
    reclassify(merged_map, matrix(c(0,0,1,f[1],2,f[2],3,f[3]),ncol = 2, byrow = T))
  })

  save(raster_list_no_drought,raster_list_drought,file = paste0('flor',row_id,'.Rdata'))
}else{
  load(paste0('flor',row_id,'.Rdata'))
}



