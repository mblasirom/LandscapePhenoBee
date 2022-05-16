
season$weeks
length(season$weeks)
paramList = data.frame(hab_names = c("snh", "floral_crop", "non_bee"), distance = 100)
nr <- nrow(nestmap)
nc <- ncol(nestmap) 
cell.size = 10
number_cells = nr*nc
landscape_area = nr*nc*(cell.size^2)

cutoff <- 0.99
kernelFor <- kerncalc(paramList$distance, cell.size, maxSize = floor(min((nr - 1)/2, (nc - 1)/2)), decaycut = cutoff)$decay

land_param = list(
  nr = nr,
  nc = nc,
  cell.size = cell.size,
  kernelFor = kernelFor)

pop_param$Ne <- sum(values(nests), na.rm = TRUE) 

no_more_resources = 0.01 #to simulate the lack of resources in the landscape


popgrowth <- function(t, vars, pop_param, land_param, nest_rast, floral_rast_list) {
  if(t %in% c(1,2))
  { 
    #A.1 first two periods only queens foraging, no workers 
    #print('A1')
    foraging_q <- latfordisp (X = raster::as.matrix(nest_rast),
                              FLOR = raster::as.matrix(floral_rast_list[[t]]),
                              decay = land_param$kernelFor) 
    
    VRt <- as.vector(t(foraging_q$visitation_rate)) 
    vars$Rvect[t] <- sum(foraging_q$Racc_per_nest)/pop_param$Ne 
    vars$Wvect[t] = 0 
    vars$stat[t] = 0
    vars$stage[t] = "A1"
    
  }
  else if(
    vars$stat[t-1] == 0 & 
    (season$weeks[t] < week.workerforage)
   )
  { #A.2 queens foraging and produce workers 
    #print('A2')
   vars$stage[t] = "A2"
   foraging_q <- latfordisp (X =  raster::as.matrix (nest_rast),
                              FLOR = raster::as.matrix(floral_rast_list[[t]]),
                              decay = kernelFor) 
    
    VRt <- as.vector(t(foraging_q$visitation_rate)) 
    vars$Rvect[t] <- sum(foraging_q$Racc_per_nest)/pop_param$Ne 
    wt <- pop_param$alpha*(1 - exp(-(vars$Rvect[t-1] + vars$Rvect[t-2])/2*pop_param$beta))
    vars$Wvect[t] <- pop_param$delta*vars$Wvect[t-1] + wt 
  }
  else if(
    vars$stat[t-1] == 0  & 
    (season$weeks[t] >= week.workerforage)
    )
  { 
    #B.1 workers foraging, and being produced
    #print('B.1a')
    vars$stage[t] = "B1a"
    vars$stat[t] = 1
    foraging_wt <- latfordisp (X =  raster::as.matrix (nest_rast),
                               FLOR = raster::as.matrix(floral_rast_list[[t]]),
                               decay = kernelFor) 
    
    VRt <- as.vector(t(foraging_wt$visitation_rate))*vars$Wvect[t-1] * pop_param$pw  
    vars$Rvect[t] <- sum(foraging_wt$Racc_per_nest)*vars$Wvect[t-1] * pop_param$pw/pop_param$Ne 
    wt <- pop_param$alpha*(1 - exp(-(vars$Rvect[t-1] + vars$Rvect[t-2])/2*pop_param$beta)) 
    vars$Wvect[t] <-  pop_param$delta*vars$Wvect[t-1] + wt 
  }
  else if(vars$stat[t-1] == 1 & (season$weeks[t] < week.prodqueen))
  {
    #B.1 workers foraging, and being produced
    #print('B.1b')
    vars$stage[t] = "B1b"
    vars$stat[t] = 1
    foraging_wt <- latfordisp (X =  raster::as.matrix (nest_rast),
                               FLOR = raster::as.matrix(floral_rast_list[[t]]),
                               decay = kernelFor) 
    VRt <- as.vector(t(foraging_wt$visitation_rate))*vars$Wvect[t-1] * pop_param$pw 
    vars$Rvect[t] <- sum(foraging_wt$Racc_per_nest)*vars$Wvect[t-1] * pop_param$pw/pop_param$Ne 
    wt <- pop_param$alpha*(1 - exp(-(vars$Rvect[t-1] + vars$Rvect[t-2])/2*pop_param$beta))
    vars$Wvect[t] <-  pop_param$delta*vars$Wvect[t-1] + wt
    vars$Qvect[t] <- 0
  }
  else if(vars$stat[t-1] == 1 & (season$weeks[t] >= week.prodqueen))
  {
    #B.2 workers foraging, and workers and queens being produced
    #print('B.2a')
    vars$stage[t] = "B2a"
    vars$stat[t] = 2
    foraging_wt <- latfordisp (X =  raster::as.matrix (nest_rast),
                               FLOR = raster::as.matrix(floral_rast_list[[t]]),
                               decay = kernelFor) 
    VRt <- as.vector(t(foraging_wt$visitation_rate))*vars$Wvect[t-1] * pop_param$pw 
    vars$Rvect[t] <- sum(foraging_wt$Racc_per_nest)*vars$Wvect[t-1] * pop_param$pw/pop_param$Ne 
    wt <- pop_param$alpha*(1 - exp(-(vars$Rvect[t-1] + vars$Rvect[t-2])/2*pop_param$beta))
    vars$Wvect[t] <-  pop_param$delta*vars$Wvect[t-1] + pop_param$epsilon*wt
    vars$Qvect[t] <-  pop_param$delta*vars$Qvect[t-1] + (1-pop_param$epsilon)*wt
    
  }
  else if(vars$stat[t-1] == 2 & sum(vars$Rvect[t-1] + vars$Rvect[t-2]) > no_more_resources )
  {
    #B.2 workers foraging, and queens being produced
    #print('B.2b')
    vars$stage[t] = "B2b"
    vars$stat[t] = 2
    foraging_wt <- latfordisp (X =  raster::as.matrix (nest_rast),
                               FLOR = raster::as.matrix(floral_rast_list[[t]]),
                               decay = kernelFor) 
    VRt <- as.vector(t(foraging_wt$visitation_rate))*vars$Wvect[t-1] * pop_param$pw
    vars$Rvect[t] <- sum(foraging_wt$Racc_per_nest)*vars$Wvect[t-1] * pop_param$pw/pop_param$Ne
    wt <- pop_param$alpha*(1 - exp(-(vars$Rvect[t-1] + vars$Rvect[t-2])/2*pop_param$beta))
    vars$Wvect[t] <-  pop_param$delta*vars$Wvect[t-1] + pop_param$epsilon*wt
    vars$Qvect[t] <-  pop_param$delta*vars$Qvect[t-1] + (1-pop_param$epsilon)*wt

  }
  else if(vars$stat[t-1] == 2 & mean(vars$Rvect[t-1] + vars$Rvect[t-2]) <= no_more_resources )
  {
    #B.3 
    #print('B.3a')
    vars$stage[t] = "B3a"
    vars$stat[t] = 3
    VRt <- 0
    vars$Rvect[t] <- 0
    t3 <- min(which(vars$stat==3))
    vars$Wvect[t] <-  pop_param$delta^(t-t3+1) * vars$Wvect[t-1]
    vars$Qvect[t] <-  pop_param$delta^(t-t3+1) * vars$Qvect[t-1]    
  }
  else if(vars$stat[t-1] == 3)
  {
    #B.3 
    #print('B.3b')
    vars$stage[t] = "B3b"
    vars$stat[t] = 3
    VRt <- 0
    vars$Rvect[t] <- 0
    t3 <- min(which(vars$stat==3))
    vars$Wvect[t] <-  pop_param$delta^(t-t3+1) * vars$Wvect[t-1]
    vars$Qvect[t] <-  pop_param$delta^(t-t3+1) * vars$Qvect[t-1] 
  }
  
 
  vars$VR[t] = sum(VRt/values(floral_rast_list[[t]]), na.rm=TRUE)
  ft = floral_rast_list[[t]]
  values(ft) = values(floral_rast_list[[t]])*(1-values(snh))
  vars$poll[t] = sum(values(ft)*
                    (1-exp(-pop_param$kappa*VRt/values(ft))), na.rm=TRUE)
  if(!is.finite(vars$VR[t])){vars$VR[t] = 0}
  return(list(vars = vars, VRt = VRt))
}


##### Results per each period in one season #######################################################

run_season <- function(floral_rast_list){
  for (t in 1:length(season$weeks)){ # 
    if(t == 1){
      Rvect <- Wvect <-  Qvect <- stat <- VR <- stage <- poll <- rep(0, length(season$weeks))
      vars = data.frame(Rvect = Rvect, Wvect = Wvect, Qvect = Qvect,stat = stat, stage=stage, VR = VR, poll = poll)
      out <- popgrowth(t, vars, pop_param, land_param, 
                       nest_rast = nests,
                       floral_rast_list = floral_rast_list)
    }else{
      out <- popgrowth(t, vars = out$vars, pop_param, land_param, 
                       nest_rast = nests,
                       floral_rast_list = floral_rast_list)
    }
  }
out$vars$flor <- NA
  

flower_sum <- for(t in 1:length(season$weeks)){
    out$vars$flor[t] <- sum(values(floral_rast_list[[t]]),na.rm=TRUE)/landscape_area
  }
  return(out)
}
out <- run_season(floral_rast_list=raster_list_no_drought[season$weeks]) 
out_D <- run_season(floral_rast_list=raster_list_drought[season$weeks])  

if(row_id == 5){
  save(out,out_D,file = 'outrowid5.Rdata')
}
if(TRUE){
  png(paste('pop',row_id,'.png'))
  plot(c(1,season$timeperiods), c(0,1), type='n', xlab = 'Time', ylab = '')
  lines(1:length(season$weeks),out$vars$Rvect/max(out$vars$Rvect,out_D$vars$Rvect),col='magenta')
  lines(1:length(season$weeks),out$vars$Wvect/max(out$vars$Wvect,out_D$vars$Wvect),col='blue')
  lines(1:length(season$weeks),out$vars$Qvect/max(out$vars$Qvect,out_D$vars$Qvect),col='black')
  lines(1:length(season$weeks),out$vars$flor/max(out$vars$flor,out_D$vars$flor),col='red')
  lines(1:length(season$weeks),out$vars$poll/max(out$vars$poll,out_D$vars$poll),col='#1b6f75')
  
  lines(1:length(season$weeks),out_D$vars$Rvect/max(out$vars$Rvect,out_D$vars$Rvect),col='magenta',lty=2)
  lines(1:length(season$weeks),out_D$vars$Wvect/max(out$vars$Wvect,out_D$vars$Wvect),col='blue',lty=2)
  lines(1:length(season$weeks),out_D$vars$Qvect/max(out$vars$Qvect,out_D$vars$Qvect),col='black',lty=2)
  lines(1:length(season$weeks),out_D$vars$flor/max(out$vars$flor,out_D$vars$flor),col='red',lty=2)
  lines(1:length(season$weeks),out_D$vars$poll/max(out$vars$poll,out_D$vars$poll),col='#1b6f75', lty = 2)
  legend('topleft',c('Resources','Workers','Queens'),col = c('magenta','blue','black'), lty = rep(1,3), cex = 0.8)
  mtext(paste("rowID",row_id))
  segments(x0=drought_start-min(season$weeks),x1=drought_end-min(season$weeks),y0=0,col='red',lwd=3)
  dev.off()
}  

if(FALSE){
  sewe <- c(17, 23, 27, 33) #Example of different weeks
  par(mfrow = c(1, 4))
  a= plot(raster_list_no_drought[[sewe[1]]], col=rev(scico(100, alpha = 1, palette = "oslo")), xlim=c(0,10), ylim=c(0,10), breaks = seq(0, 8, by = 0.1))
  b= plot(raster_list_no_drought[[sewe[2]]], col=rev(scico(100, alpha = 1, palette = "oslo")), xlim=c(0,10), ylim=c(0,10),breaks = seq(0, 8, by = 0.1))
  c= plot(raster_list_no_drought[[sewe[3]]], col=rev(scico(100, alpha = 1, palette = "oslo")), xlim=c(0,10), ylim=c(0,10),breaks = seq(0, 8, by = 0.1))
  d= plot(raster_list_no_drought[[sewe[4]]], col=rev(scico(100, alpha = 1, palette = "oslo")), xlim=c(0,10), ylim=c(0,10),breaks = seq(0, 8, by = 0.1))
} 

##### summarize the whole season ##################################################################

sim_df$VR[row_id] <- trapz(1:length(season$weeks),out$vars$VR) ##area under the curve calculation, call library "caTools"
sim_df$Q_Ne[row_id] <- out$vars$Qvect[length(out$vars$Qvect)] # sum number queens per nest 
sim_df$Wmax_Ne[row_id] <- max(out$vars$Wvect) # max number or Wvect to get the number of workers on a peak production
sim_df$Wtot_Ne[row_id] <- trapz(1:length(season$weeks),out$vars$Wvect) # sum number or Wvect to get the number of workers on the season
sim_df$poll[row_id] <- trapz(1:length(season$weeks),out$vars$poll) # Pollination
sim_df$flor[row_id] <- trapz(1:length(season$weeks),out$vars$flor)/landscape_area # floral resources


sim_df$VR_D[row_id] <- trapz(1:length(season$weeks),out_D$vars$VR)
sim_df$Q_Ne_D[row_id] <- out_D$vars$Qvect[length(out_D$vars$Qvect)] 
sim_df$Wmax_Ne_D[row_id] <- max(out_D$vars$Wvect)
sim_df$Wtot_Ne_D[row_id] <- trapz(1:length(season$weeks),out_D$vars$Wvect) 
sim_df$poll_D[row_id] <- trapz(1:length(season$weeks),out_D$vars$poll)
sim_df$flor_D[row_id] <- trapz(1:length(season$weeks),out_D$vars$flor)/landscape_area 

