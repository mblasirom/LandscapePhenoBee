
## Floral resources over the season, first derived per day and summed over land use types, then aggregated over weeks

## 1. Use generalised season progression for each land use to identify the start of flowering
## 2. Specify floral values as a function over the fraction of the flowering season and the maximum flowers during a season (without drought)
## 3. Modify with respect to drought - floral resources will decrease
## 4. Derive averages over weeks

# source('GDDdata.R') ## if there is daily temperature available to calculate GDD, load here, and replace with a sample of ST
#load("gdd_data.RData")
#ST = as.numeric(gdd[[1]])

## theoretical generalised season progression, that mimics GDD
ST = pnorm((1:365-200)/65)*2500/25

# Identify weeks
{
  first.dayperweek <- seq(1,365,by=7)
  d.beemerge <- which(ST > pop_param$gdd.emerge)[1] # day that bees emerge
  first.week = max(which(first.dayperweek < d.beemerge)) # transform days into weeks
  
  d.workerforage <- which(ST > pop_param$gdd.workerforage)[1] 
  week.workerforage <- max(which(first.dayperweek < d.workerforage))
  
  d.prodqueen <- which(ST > pop_param$gdd.queenprod)[1] 
  week.prodqueen <- max(which(first.dayperweek < d.prodqueen))

}


growth_no_drought <- lapply(1:nrow(resource_param),
                            function(lutype,ST,resource_param){
                              d.fstart <- which(ST > resource_param$gdd.start.flowering[lutype])[1] # day that the flowering starts
                              d.fend <- 10+which(ST > resource_param$gdd.end.flowering[lutype])[1] # day that the flowering ends
                              f.par <- resource_param[lutype,]
                              d.01 <- (0:(d.fend-d.fstart))/(d.fend-d.fstart)#
                              get.pn2 <- function(xx){(1-4*(xx-0.5)^2)*f.par$max.flowers}
                              get.diffpn2 <- function(xx){((1-4*(xx-0.5)^2) - c(0,(1-4*(xx[-length(xx)]-0.5)^2)))*f.par$max.flowers}
                              temp <- seq(d.fstart,d.fend)
                              return(
                                list(growth=get.diffpn2(d.01),
                                     day=temp,
                                     week=as.numeric(cut(temp,breaks=first.dayperweek))))
                            },
                            ST=ST,
                            resource_param=resource_param)
class(growth_no_drought) <- c('gr','list')

plot.gr <- function(gr){
  days <- lapply(gr,function(x){x$day})
  vals <- lapply(gr,function(x){cumsum(x$growth)})
  plot(range(unlist(days)),range(unlist(vals)),type='n',xlab='days',ylab='floral resource')
  lines(days[[1]],(vals[[1]]),col='green')
  lines(days[[2]],(vals[[2]]),col='yellow')
  lines(days[[3]],(vals[[3]]),col='red')
  abline(h=0)
}
if(FALSE){
  plot(growth_no_drought)
}

weekly_growth_no_drought <- lapply(growth_no_drought,function(gr){
  temp <- aggregate(gr$growth,by=list(week=gr$week),'sum')
  colnames(temp)[2] <- "growth" 
  return(temp)
})

class(weekly_growth_no_drought) <- c('grw','list')
plot.grw <- function(gr){
  days <- lapply(gr,function(x){x$week})
  vals <- lapply(gr,function(x){cumsum(x$growth)})
  plot(range(unlist(days)),range(unlist(vals)),type='n',xlab='weeks',ylab='floral resource')
  lines(days[[1]],(vals[[1]]),col='green')
  lines(days[[2]],(vals[[2]]),col='yellow')
  lines(days[[3]],(vals[[3]]),col='red')
  abline(h=0)
}
if(FALSE){
  plot(weekly_growth_no_drought)
}

###### Simulation of drought #####

## if in normal conditions the growth is positive, the growth in drought conditions will be close to zero,
## while if the growth is negative, under drought conditions this will translate in a 50% larger reduction of growth.

drought_start <- min(growth_no_drought[[2]]$week)
drought_end <- drought_start + sim_design$d_length[row_id] 

weekly_growth_drought <- lapply(weekly_growth_no_drought,function(gr,drought_start,drought_end){
  id <- (drought_start < gr$week) & (gr$week < (drought_end+1))
  gr$growth[id] <-  min(gr$growth[id]*1.5,0) # 
  return(gr)  
},drought_start=drought_start,drought_end=drought_end)
class(weekly_growth_drought) <- c('grw','list')
if(FALSE){
  plot(weekly_growth_no_drought)
  plot(weekly_growth_drought)
  abline(v = drought_start)
  abline(v = drought_end)
}


flor_val <- lapply(weekly_growth_no_drought,function(gr){
  gr$flres = cumsum(gr$growth)
  gr$flres[gr$flres<0] = 0
  return(gr)
})
flor_val_D <- lapply(weekly_growth_drought,function(gr){
  gr$flres = cumsum(gr$growth)
  gr$flres[gr$flres<0] = 0
  return(gr)
})




