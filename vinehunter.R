library("pracma")

vinehunter_primitive <- function(cycle_index,diags){
  
vine <- list() 
cycle <- diags[[1]]$verts[[cycle_index]][,1:3]
vine[[1]]<- diags[[1]]$cycleLocation[[cycle_index]]


for (i in 2:length(diags)){
  
  distances <- rep(NA,length(diags[[i]]$verts))
  
  for (j in 1:length(diags[[i]]$verts)){
    
    if(is.matrix(diags[[i]]$verts[[j]]))
      distances[j] <- hausdorff_dist(cycle, diags[[i]]$verts[[j]][,1:3])
      
  }
  vine[[i]] <- diags[[i]]$cycleLocation[[which.min(distances)]]
  
}

return(vine)

}



