rdata_to_rds <-
  function(dir_names = c('./weight25scale12/',
                         '2512rdACC_masked_time_',
                         '_patient1.Rdata'),
           number_of_files = 290) {
    for (i in 1:number_of_files) {
      load(paste(dir_names[1], dir_names[2], i, dir_names[3], sep = ''))
      saveRDS(y, file = paste(
        dir_names[1],
        'rds/',
        dir_names[2],
        i,
        gsub('.Rdata', '.rds', dir_names[3]),
        sep = ''
      ))
      rm(y)
    }
  }

read_in_data <-
  function(dir_names = c('./vine_0.5_1/', '2512rdACC_masked_time_', '_patient1.rds'),
           number_of_files = 10) {
    data <- list()
    for (i in 1:number_of_files) {
      data[[i]] <- readRDS(file = paste0(dir_names[1], dir_names[2], i, dir_names[3]))
    }
    
    return(data)
  }





normalize_bold<- function(points_bold){
  
  points_bold<- data.matrix(cbind(points_bold,NA))
  
  points<-dim(points_bold)[1]
  
  for (p in 1:points){
    
    distances <- sapply(as.vector(1:nrow(points_bold))[-p], function(x) dist(rbind(points_bold[x,],points_bold[p,])))
    points_bold[p,5]<-sum(abs(points_bold[p,4] - points_bold[-p,4])/distances)/(sum(1/distances))
    print(points_bold[p,5])
  }
  return(points_bold[,c(1,2,3,5)])
  
}








