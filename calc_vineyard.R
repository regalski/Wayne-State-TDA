calc_vineyard<- function(data,file_names=c('2512rdACC_masked_time_','_patient1.rds'),p=.5,hd=1){
  ##This function appends the rips_diag cycleLocations in the form needed by calc_vineyard as diag$verts and saves it in a new directory
  ## that has a naming scheme pers_PERSISTANCECUTOFF_HOMDIM
  # data := list of ripsdiag outputs with ripsDiag(...,"cyclelocation" = TRUE)
  # p := minimum persistence to be displayed
  # hd := homological dimension to appear in vineyard
  out_dir<- paste('vine',p,hd,sep='_')
  if (!dir.exists(out_dir)){dir.create(out_dir)}
  
  t=length(data)
  for (i in 1:t){
    data[[i]]$verts<-rep(list(NA),dim(data[[i]]$diagram)[1])  
    for (cycle in which(data[[i]]$diagram[,1]==hd & lengths(data[[i]]$cycleLocation)!=0)){
          verts<-NULL
        
          for (j in 1:nrow(data[[i]][["cycleLocation"]][[cycle]][,,1])){
          
            verts <- rbind(verts,data[[i]][["cycleLocation"]][[cycle]][j,,])
                      }
        data[[i]]$verts[[cycle]]<- verts
    }

  }
  

  for( i in 2:t){
    data[[i-1]]$diagram <- cbind(data[[i-1]]$diagram,i-1)
    colnames(data[[i-1]]$diagram)[4] <-'time'
    data[[i]]$diagram <- cbind(data[[i]]$diagram,i)
    colnames(data[[i]]$diagram)[4] <-'time'
    
    
    t_1_hd_indices<- which(data[[i-1]]$diagram[,'dimension']==hd & (data[[i-1]]$diagram[,'Death']-data[[i-1]]$diagram[,'Birth'] >p ) & (lengths(data[[i-1]]$cycleLocation)!=0))
    
    t_2_hd_indices<- which(data[[i]]$diagram[,'dimension']==hd & (data[[i]]$diagram[,'Death']-data[[i]]$diagram[,'Birth'] >p )& (lengths(data[[i]]$cycleLocation)!=0))
    
    
    
    verts_dist<-NULL
    for (v1 in t_1_hd_indices) {
      for (v2 in t_2_hd_indices) {
        
        verts_dist<-rbind(verts_dist,c(v1,v2,hausdorff_dist(data[[i-1]]$verts[[v1]],data[[i]]$verts[[v2]])))
        
      }
      
    }
    
    data[[i-1]]$verts_dist<- verts_dist
    
    
    saveRDS(data[[i-1]],file=paste0(out_dir,'/',file_names[1],i-1,file_names[2]))
    if( i == t){saveRDS(data[[i]],file=paste0(out_dir,'/',file_names[1],i,file_names[2]))}
    
  }
    
}

