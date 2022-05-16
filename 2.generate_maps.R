#set.seed(1000)
mapsize = 201

m = matrix(0, mapsize, mapsize) 
r = raster(m, xmn=0, xmx=10, ymn=0, ymx=10)

if(!SA){
  snh <- makeClass(r,ceiling(sim_design$snh_fract[row_id]*mapsize*mapsize/sim_design$snh_size[row_id]/100), sim_design$snh_size[row_id], val=1) 
  snh <- rmSingle(snh)
  prop_snh <- mean(values(snh)) 
  
  patchsize_crop=50
  prop_crop=(1-prop_snh-0.4)
  
  tolerance=0.05
  npatches_crop=ceiling((prop_crop*mapsize*mapsize)/patchsize_crop)
  target=prop_crop
  i=0
  repeat{
    i=i+1
    if(i>100){break}
    else {
      try(landuse<-makeClass(snh,npatches_crop,patchsize_crop,bgr=0,val=2),silent = TRUE)
      prop_obs=sum(values(landuse)==2)/length(values(landuse))
      if(abs(prop_obs -prop_crop)< tolerance){
        print(paste('suitable landuse raster created after',i,'iterations'));
        break
      }}}  
  
landuse <- rmSingle(landuse)
crop = landuse
values(crop)=0
values(crop)[values(landuse)==2]=1 
  
npatches_crop_late=ceiling((mean(values(crop))*mapsize*mapsize)/patchsize_crop*runif(1))
landuse2<-makeClass(crop,npatches_crop_late,patchsize_crop,bgr=1,val=2)
  
crop2 = landuse2
values(crop2)=0
values(crop2)[values(landuse2)==2]=1 
  
crop1 = crop
values(crop1)[values(crop2)==1]=0 
  
merged_map <- snh
values(merged_map)[values(crop1)==1]=2 # early crop
values(merged_map)[values(crop2)==1]=3 # late crop
  
prop_late_crop = mean(values(crop2))
prop_crop = mean(values(crop1))
  
save(snh, merged_map, prop_snh, prop_late_crop, prop_crop, file=paste0('lu',row_id,'.Rdata'))
}else{
  load(paste0('lu',row_id,'.Rdata'))
}
# update 01.04.22 - keeps overwriting in lu1.Rdata

# save to output
sim_df$prop_snh[row_id] <- prop_snh
sim_df$prop_late_crop[row_id] <- prop_late_crop
sim_df$prop_crop[row_id] <- prop_crop



