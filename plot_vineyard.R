vineyard <- function(data,p=.5,hd=1,range=c(-Inf,Inf),min_death=4,side=FALSE,id=FALSE,col='red'){
  # data := list of ripsdiag outputs with ripsDiag(...,"cyclelocation" = TRUE)
  # p := minimum persistence to be displayed
  # hd := homological dimension to appear in vineyard
  # range := range of time slices desired in the vineyard
  # col := color of points
  # min_death := min death of features of interest
  
  max_death<- max(data[[1]]$diagram[,'Death']) + 1
  
  open3d(zoom=.7)
  par3d(userMatrix=matrix(c(0.910520375,0.010782093,-0.413324028,0.000000000,0.009738045,0.998823464,0.047507871,0.000000000,0.413350046,-0.047281798,0.909343719,0.000000000,0.000000000,0.000000000,0.000000000,1.000000000),nrow=4))
  t= length(data)
  
  ids<-c()
  for( i in (2:t)){
    if (i < range[1]){next}
    
    if (i >range[2]) {next}
    
    data[[i-1]]$diagram <- cbind(data[[i-1]]$diagram,i-1)
    colnames(data[[i-1]]$diagram)[4] <-'time'
    data[[i]]$diagram <- cbind(data[[i]]$diagram,i)
    colnames(data[[i]]$diagram)[4] <-'time'
    

    t_1_hd_indices<- which(data[[i-1]]$diagram[,'dimension']==hd & (data[[i-1]]$diagram[,'Death']-data[[i-1]]$diagram[,'Birth'] >p ) & (lengths(data[[i-1]]$cycleLocation)!=0)&(data[[i-1]]$diagram[,'Death']>min_death))
    
    t_2_hd_indices<- which(data[[i]]$diagram[,'dimension']==hd & (data[[i]]$diagram[,'Death']-data[[i]]$diagram[,'Birth'] >p )& (lengths(data[[i]]$cycleLocation)!=0) & (data[[i]]$diagram[,'Death']>min_death))
    
    
    if (i == 2){ids<-rbind(ids,cbind(data[[i-1]]$diagram[t_1_hd_indices,2:3],data[[i-1]]$diagram[t_1_hd_indices,4],paste('H1 at',i-1,':',t_1_hd_indices)))}
    
    ids<-rbind(ids,cbind(data[[i]]$diagram[t_2_hd_indices,2:3],data[[i]]$diagram[t_2_hd_indices,4],paste('H1 at',i,':',t_2_hd_indices)))
    
    
    data[[i-1]]$verts_dist <- data[[i-1]]$verts_dist[data[[i-1]]$verts_dist[,1] %in% t_1_hd_indices ,]
    data[[i-1]]$verts_dist <- data[[i-1]]$verts_dist[data[[i-1]]$verts_dist[,2] %in% t_2_hd_indices ,] #get rid homclasses below pers and death
    min_dist<-data[[i-1]]$verts_dist[which.min(data[[i-1]]$verts_dist[,3]),3]
    max_dist<- data[[i-1]]$verts_dist[which.max(data[[i-1]]$verts_dist[,3]),3]
    

    data[[i-1]]$verts_dist[,3] <- (1-((data[[i-1]]$verts_dist[,3] - min_dist)/(max_dist - min_dist)))^10 #transparency between 0 and 1
    
    
    #endpoints:= x1, y1, z1, transparency, x2, y2, z2, transparency
    endpoints<-cbind(cbind(data[[i-1]]$diagram[data[[i-1]]$verts_dist[,1],2:3],i-1,data[[i-1]]$verts_dist[,3]), cbind(data[[i]]$diagram[data[[i-1]]$verts_dist[,2],2:3],i,data[[i-1]]$verts_dist[,3]))
    spheres3d(endpoints[,1:3],pch=2, color=col,cex = .2,radius = .1)
    spheres3d(endpoints[,5:7],pch=2, color=col,cex = .2,radius = .1)
    xpoints<- as.vector(t(endpoints[,c(1,5)]))
    ypoints<- as.vector(t(endpoints[,c(2,6)]))

    zpoints<- as.vector(t(endpoints[,c(3,7)])) 
    
    transparency <- as.vector(t(endpoints[,c(4,8)]))

    less_noise<- which(transparency>.1)

    
    
    segments3d(xpoints[less_noise],ypoints[less_noise],zpoints[less_noise],col='black',alpha=transparency[less_noise],lwd=3 )
    
    lines3d(x=c(0,max_death),y=c(0,max_death),z=c((i-1),(i-1)),lwd=1)

    lines3d(x=c(0,max_death),y=c(0,max_death),z=c(i,i),lwd=1)
    
    
  }
  
  

  if (side==TRUE){
    axes3d(edges = c('x-+','y--'))}
  else{
      axes3d(edges = c('x-+','y--','z'))
      rgl.texts(text='Time',x=-2,y=-2,z=min((range[2]/2),(t/2)),col='black')
      }
    
  rgl.texts(text='Death',x=-4,y=6,z=-4,col='black')
  
  rgl.texts(text='Birth',x=6,y=-2,z=min(range[2]+3,t+3),col='black')
  
  if (id==TRUE){identify3d(ids[,1:3],labels = ids[,4],n=6)}
 
  
}




vineyard_no_lines<- function(data,p=.5,hd=1,range=c(-Inf,Inf),min_death=4,side=FALSE,id=FALSE,col='red'){
  # data := list of ripsdiag outputs with ripsDiag(...,"cyclelocation" = TRUE)
  # p := minimum persistence to be displayed
  # hd := homological dimension to appear in vineyard
  # range := range of time slices desired in the vineyard
  # col := color of points
  # min_death := min death of features of interest
  
  max_death<- max(data[[1]]$diagram[,'Death']) + 1
  
  open3d(zoom=.7)
  par3d(userMatrix=matrix(c(0.910520375,0.010782093,-0.413324028,0.000000000,0.009738045,0.998823464,0.047507871,0.000000000,0.413350046,-0.047281798,0.909343719,0.000000000,0.000000000,0.000000000,0.000000000,1.000000000),nrow=4))
  t= length(data)
  
  ids<-c()
  for( i in (2:t)){
    if (i < range[1]){next}
    
    if (i >range[2]) {next}
    
    data[[i-1]]$diagram <- cbind(data[[i-1]]$diagram,i-1)
    colnames(data[[i-1]]$diagram)[4] <-'time'
    data[[i]]$diagram <- cbind(data[[i]]$diagram,i)
    colnames(data[[i]]$diagram)[4] <-'time'
    
    
    t_1_hd_indices<- which(data[[i-1]]$diagram[,'dimension']==hd & (data[[i-1]]$diagram[,'Death']-data[[i-1]]$diagram[,'Birth'] >p ) & (lengths(data[[i-1]]$cycleLocation)!=0)&(data[[i-1]]$diagram[,'Death']>min_death))
    
    t_2_hd_indices<- which(data[[i]]$diagram[,'dimension']==hd & (data[[i]]$diagram[,'Death']-data[[i]]$diagram[,'Birth'] >p )& (lengths(data[[i]]$cycleLocation)!=0) & (data[[i]]$diagram[,'Death']>min_death))
    
    
    if (i == 2){ids<-rbind(ids,cbind(data[[i-1]]$diagram[t_1_hd_indices,2:3],data[[i-1]]$diagram[t_1_hd_indices,4],paste('H1 at',i-1,':',t_1_hd_indices)))}
    
    ids<-rbind(ids,cbind(data[[i]]$diagram[t_2_hd_indices,2:3],data[[i]]$diagram[t_2_hd_indices,4],paste('H1 at',i,':',t_2_hd_indices)))
    
    
    data[[i-1]]$verts_dist <- data[[i-1]]$verts_dist[data[[i-1]]$verts_dist[,1] %in% t_1_hd_indices ,]
    data[[i-1]]$verts_dist <- data[[i-1]]$verts_dist[data[[i-1]]$verts_dist[,2] %in% t_2_hd_indices ,] #get rid homclasses below pers and death
    min_dist<-data[[i-1]]$verts_dist[which.min(data[[i-1]]$verts_dist[,3]),3]
    max_dist<- data[[i-1]]$verts_dist[which.max(data[[i-1]]$verts_dist[,3]),3]
    
    
    data[[i-1]]$verts_dist[,3] <- (1-((data[[i-1]]$verts_dist[,3] - min_dist)/(max_dist - min_dist)))^10 #transparency between 0 and 1
    
    
    #endpoints:= x1, y1, z1, transparency, x2, y2, z2, transparency
    endpoints<-cbind(cbind(data[[i-1]]$diagram[data[[i-1]]$verts_dist[,1],2:3],i-1,data[[i-1]]$verts_dist[,3]), cbind(data[[i]]$diagram[data[[i-1]]$verts_dist[,2],2:3],i,data[[i-1]]$verts_dist[,3]))
    spheres3d(endpoints[,1:3],pch=2, color=col,cex = .2,radius = .1)
    spheres3d(endpoints[,5:7],pch=2, color=col,cex = .2,radius = .1)

    lines3d(x=c(0,max_death),y=c(0,max_death),z=c((i-1),(i-1)),lwd=1)
    
    lines3d(x=c(0,max_death),y=c(0,max_death),z=c(i,i),lwd=1)
    
    
  }
  
  
  #title3d(xlab = 'Birth',ylab='Death',zlab='Time')
  if (side==TRUE){
    axes3d(edges = c('x-+','y--'))}
  else{
    axes3d(edges = c('x-+','y--','z'))
    rgl.texts(text='Time',x=-2,y=-2,z=min((range[2]/2),(t/2)),col='black')
  }
  
  rgl.texts(text='Death',x=-4,y=6,z=-4,col='black')
  
  rgl.texts(text='Birth',x=6,y=-2,z=min(range[2]+3,t+3),col='black')
  
  if (id==TRUE){identify3d(ids[,1:3],labels = ids[,4],n=1)}
  
  
}







