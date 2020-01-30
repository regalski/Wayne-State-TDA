plot_loops <-
  function(loops,
           data,
           colors = c('red', 'pink', 'purple', 'green', 'blue', 'yellow'),loop_perspective=NULL) {
    if (is.null(loop_perspective)) {
      loop_perspective <-
        matrix(
          c(
            0.3630731,
            -0.5154495,
            0.7762021,
            0.0000000,
            0.7040736,
            0.6974096,
            0.1337916 ,
            0.0000000,-0.6102936,
            0.4979273,
            0.6161250,
            0.0000000,
            0.0000000,
            0.0000000,
            0.0000000,
            1.0000000
          ),
          nrow = 4,
          byrow = TRUE
        )
    }
    open3d()
    points3d(data)
    par3d(userMatrix = loop_perspective)
    col <- 1
    for (loop in loops) {
      for (i in 1:dim(loop)[1]) {
        lines3d(loop[i, , ], lwd = 5, col = colors[col])
      }
      col <- col + 1
      
    }
  }



plot_loops_animated <- function(loops, data, loop_perspective = NULL) {
  if (is.null(loop_perspective)) {
    loop_perspective <-
      matrix(
        c(
          0.3630731,
          -0.5154495,
          0.7762021,
          0.0000000,
          0.7040736,
          0.6974096,
          0.1337916 ,
          0.0000000,-0.6102936,
          0.4979273,
          0.6161250,
          0.0000000,
          0.0000000,
          0.0000000,
          0.0000000,
          1.0000000
        ),
        nrow = 4,
        byrow = TRUE
      )
  }
  
  
  ls <- list()
  for (loop in loops) {
    l <- NULL
    for (i in 1:dim(loop)[1]) {
      l <- rbind(l, loop[i, , ])
    }
    
    ls <- list.append(ls, l)
  }
  open3d()
  par3d(userMatrix = loop_perspective)
  for (i in ls) {
    ###change me
    rgl.clear()
    
    points3d(data)
    segments3d(i, lwd = 3, col = 'red')
    
    Sys.sleep(1)
    
  }
}

plot_loops_pictures_with_rotation <-
  function(loops, data, dir_out, loop_perspective = NULL) {
    if (is.null(loop_perspective)) {
      loop_perspective <-
        matrix(
          c(
            -0.023078129,
            -0.999727488,
            0.003478826,
            0.000000000,
            0.012772111,
            0.003184580,
            0.999913454,
            0.000000000,-0.999652088,
            0.023120541,
            0.012695219,
            0.000000000,
            0.000000000,
            0.000000000,
            0.000000000,
            1.000000000
          ),
          nrow = 4,
          byrow = TRUE
        )
    }
    
    dir.create(dir_out)
    
    ls <- list()
    
    for (loop in loops) {
      l <- NULL
      for (i in 1:dim(loop)[1]) {
        l <- rbind(l, loop[i, , ])
      }
      
      ls <- list.append(ls, l)
    }
    r3dDefaults$windowRect = c(0, 0, 1920, 1080)
    open3d()
    
    for (i in seq(1, 30, .5)) {
      points3d(data)
      rgl.texts(
        text = 'ACC',
        x = 40,
        y = 55,
        z = 60,
        cex = 1,
        col = 'black'
      )
      view3d(userMatrix = rotationMatrix((pi / 2) * (i / 30), 0, 1, 1) %*% loop_perspective ,
             zoom = 1)
      
      rgl.snapshot(paste0(dir_out, '/', sprintf('%03d', i * 2), '.png'))
      
    }
    
    
    for (i in 1:length(loops)) {
      rgl.clear()
      
      view3d(userMatrix = rotationMatrix(pi / 2, 0, 1, 1) %*% loop_perspective ,
             zoom = 1)#,cex=2)
      points3d(data)
      segments3d(ls[[i]], lwd = 3, col = 'red')
      
      rgl.texts(
        text = paste('Time', i, sep = ' '),
        x = 40,
        y = 55,
        z = 60,
        cex = 1,
        col = 'black'
      )
      
      rgl.snapshot(paste0(dir_out, '/', sprintf('%03d', i + 60), '.png'))
    }
  }

