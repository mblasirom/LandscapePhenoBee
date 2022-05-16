
nest_intensity = pop_param$nest_int
nestmap = snh 
values(nestmap)=0
values(nestmap)[values(snh)==1]=1 
av_number_of_nests <- sum(values(nestmap))*nest_intensity
actual_number_of_nests <- rpois(1,av_number_of_nests)# total number of nests in this landscape
nest_id <- sample.int(length(values(nestmap)),size=actual_number_of_nests,prob=values(nestmap))

nests = nestmap
values(nests) <- 0
values(nests)[nest_id] <- 1
values(nests)[values(nestmap)==0] = NA
nestshape <- rasterToPolygons(nests,fun=function(x){x==1})
#plot(nestmap)
#plot(nestshape,add=TRUE, border = 'red')
#table(values(nests))   
