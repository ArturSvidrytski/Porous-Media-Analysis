##############################################################################################################
#####
## Create folder with name of *.dis file and copy the file into the folder with name input.dis (starts with
## a specified path and then works recursively)
## "d:/1_Work/ImagesAndData/Mesoporous monoliths (from Daniela, for P. Monson)/Input_files_dis/"
#####

any.dis.into.input.dis <- function(path){
  
  names.of.all.files.in.dir.full <- sort( list.files(path = path, full.names = T, pattern = '*\\.dis', recursive = T, include.dirs = F) )
  names.of.all.files.in.dir.full <- names.of.all.files.in.dir.full[basename(names.of.all.files.in.dir.full)!='input.dis']
  names.of.dirs <- character( length = length( names.of.all.files.in.dir.full ) )
  
  for (i in 1:length(names.of.all.files.in.dir.full) ) {
    names.of.dirs[i] <- substr( names.of.all.files.in.dir.full[i], 1,  nchar( names.of.all.files.in.dir.full[i] )-4 )
    if (!dir.exists(names.of.dirs[i])){
      # print(names.of.all.files.in.dir.full[i])
      # print(names.of.dirs[i])
      dir.create( names.of.dirs[i] )
      file.copy( from = names.of.all.files.in.dir.full[i], to = names.of.dirs[i], overwrite = F )
      file.rename( from = paste0(names.of.dirs[i], '/', basename(  names.of.all.files.in.dir.full[i] ) ), to = paste0(names.of.dirs[i], '/', 'input.dis') )
    }
  }
  
}


##############################################################################################################
#####
## Porosity calculation splitting object into stacks of slices. If nslice = 0 than all object will be taken
#####

porosity.nslice <- function( object.3D, nslice = 0 ){
  
  obj.dim <- dim(object.3D)
  
  if (nslice == 0){
    
    porosity.perc <- 100*length(object.3D[object.3D==as.integer(0)])/prod(dim(object.3D))
    
  } else {
    
    solid.voxels.number <- 0
    empty.voxels.number <- 0
    
    for ( starting.layer.number.axe.z in seq(from = 1, to = obj.dim[3], by = nslice )){
      print(starting.layer.number.axe.z)
      if ( starting.layer.number.axe.z+nslice <= obj.dim[3] ){
        gc(); object.3D.part <- object.3D[,,starting.layer.number.axe.z:(starting.layer.number.axe.z+nslice-1)]
      } else {
        gc(); object.3D.part <- object.3D[,,starting.layer.number.axe.z:obj.dim[3]]
      }
      
      gc(); solid.voxels.number <- solid.voxels.number + length(object.3D.part[ object.3D.part == as.integer(1) ])
      gc(); empty.voxels.number <- empty.voxels.number + length(object.3D.part[ object.3D.part == as.integer(0) ])
      
    }
    rm(object.3D.part); gc()
    porosity.perc <- 100 * empty.voxels.number /  as.numeric(prod(obj.dim))
    
  }
  gc()
  return(porosity.perc)
}

##############################################################################################################
#####
## Plot Black/White image on matrix
#####

image.binary <- function( img.matrix ){
  
  image( t(apply(   img.matrix   , 2, rev)), useRaster = F, col = gray.colors(n = 2, start = 0, end = 1) )
  
}


####################################################################################
## Reconstruction of 3D object from TIFF imgs. Path is a folder with all .tiff files
####################################################################################

tiff.stack.into.3D.array <- function( path ){
  
  if("tiff" %in% rownames(installed.packages()) == FALSE) {install.packages("tiff")}
  
  library(tiff)
  
  names.of.all.files.in.dir <- sort( list.files(path = path, full.names = T, pattern = '\\.tif') )
  
  current.picture <- readTIFF(names.of.all.files.in.dir[2])
  pic.dimensions <- dim(current.picture)
  object.3D.dimensions <- c( pic.dimensions, length(names.of.all.files.in.dir ) )
  
  object.3D <- array( integer(0), object.3D.dimensions )
  
  gc()
  for (pic.number in 1:length(names.of.all.files.in.dir)) {
    print(pic.number)
    current.picture <- readTIFF( names.of.all.files.in.dir[pic.number] )
    current.picture.int <- as.integer( current.picture )
    current.picture.intarr <- array( current.picture.int, dim = pic.dimensions )
    object.3D[,,pic.number] <- current.picture.intarr
    if( ! pic.number %% 100 ) { gc() }
  }
  gc()
  
  return(object.3D)
  
}

##############################################################################################################
#####
## Specific Surface Area calculation
#####

specific.surface.area <- function( object.3D ){
  
  object.3D.dimensions <- dim(object.3D)
  
  number.of.layers.initval <- 2
  number.of.layers.lastval <- object.3D.dimensions[3]
  
  VdivS.vect <- rep( 0, number.of.layers.lastval )
  solid.surface.area.vect <- rep( 0, number.of.layers.lastval )
  
  solid.surface.area <- 0
  
  is.it.one <- T
  for ( layer.number.along.z in seq(from = number.of.layers.initval, to = number.of.layers.lastval, by = 1 ) ) {
    
    print(layer.number.along.z)
    
    object.3D.part.dimensions <- c( object.3D.dimensions[1:2], 2 )
    #gc()
    object.3D.part <- object.3D[ , , (layer.number.along.z-1):(layer.number.along.z) ]; 
    #gc()
    
    
    if ( is.it.one ){
      
      #gc(); 
      solid.voxels.number.part <- as.numeric( sum( object.3D.part == as.integer(1) ) )
      
      #gc(); 
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ 1:(object.3D.part.dimensions[1]-1), , ] - object.3D.part[ 2:object.3D.part.dimensions[1], , ]) )
      #gc(); 
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ , 1:(object.3D.part.dimensions[2]-1), ] - object.3D.part[ , 2:object.3D.part.dimensions[2], ]) )
      #gc(); 
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ , , 1:(object.3D.part.dimensions[3]-1)] - object.3D.part[ , , 2:object.3D.part.dimensions[3]]) )
      
      is.it.one <- F
      
      VdivS.vect[layer.number.along.z-1] <- solid.voxels.number.part/solid.surface.area
      solid.surface.area.vect[layer.number.along.z-1] <- solid.surface.area
      
    } else {
      
      gc(); solid.voxels.number.part <- solid.voxels.number.part + length(object.3D.part[,,2][ object.3D.part[,,2] == as.integer(1) ])
      
      #gc(); 
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ 1:(object.3D.part.dimensions[1]-1), , 2] - object.3D.part[ 2:object.3D.part.dimensions[1], , 2]) )
      #gc(); 
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ , 1:(object.3D.part.dimensions[2]-1), 2] - object.3D.part[ , 2:object.3D.part.dimensions[2], 2]) )
      #gc();
      solid.surface.area <- solid.surface.area + sum(abs(object.3D.part[ , , 1:(object.3D.part.dimensions[3]-1)] - object.3D.part[ , , 2:object.3D.part.dimensions[3]]) )
      
      VdivS.vect[layer.number.along.z-1] <- solid.voxels.number.part/solid.surface.area
      solid.surface.area.vect[layer.number.along.z-1] <- solid.surface.area
      
    }
    
    #gc() 
  }
  
  
  # plot( VdivS.vect[(number.of.layers.initval-1):(number.of.layers.lastval-1)] ); grid()
  # boxplot.stats( VdivS.vect[(number.of.layers.initval-1):(number.of.layers.lastval-1)] )
  # mean( VdivS.vect[(number.of.layers.initval-1):(number.of.layers.lastval-1)] )
  
  #return( solid.surface.area/prod(dim(object.3D)) )
  return( solid.surface.area/solid.voxels.number.part )
  
}



##############################################################################################################
#####
## Specific Surface Area calculation (over all object in memory)
#####

specific.surface.area.bulk <- function(object.3D.part, voxsizeW=1., voxsizeH=1., voxsizeD=1. ){
  
  
  
  solid.surface.area <- 0
  
  object.3D.part.dimensions <- dim(object.3D.part)
  
  gc(); solid.voxels.number.part <- length(object.3D.part[ object.3D.part == as.integer(1) ])
  
  gc(); solid.surface.area <- 
    solid.surface.area + 
    (voxsizeW*voxsizeD)*sum(abs(object.3D.part[ 1:(object.3D.part.dimensions[1]-1), , ] - 
                                  object.3D.part[ 2:object.3D.part.dimensions[1], , ]) ) #/ voxsizeD
  
  gc(); solid.surface.area <- 
    solid.surface.area + 
    (voxsizeH*voxsizeD)*sum(abs(object.3D.part[ , 1:(object.3D.part.dimensions[2]-1), ] - 
                                  object.3D.part[ , 2:object.3D.part.dimensions[2], ]) ) #/ voxsizeW
  
  gc(); solid.surface.area <- 
    solid.surface.area + 
    (voxsizeW*voxsizeH)*sum(abs(object.3D.part[ , , 1:(object.3D.part.dimensions[3]-1)] - 
                                  object.3D.part[ , , 2:object.3D.part.dimensions[3]]) ) #/ voxsizeH
  
  gc()
  
  
  ssa <- solid.surface.area/(solid.voxels.number.part*(voxsizeW*voxsizeD*voxsizeH))
  #ssa <- solid.surface.area/prod(dim(object.3D.part))
  
  return( ssa )
  #return( solid.surface.area/solid.voxels.number.part )
  
}




##############################################################################################################
#####
## Plot 3D
#####


plot.3d.object <- function(object.3D){
  
  if("rgl" %in% rownames(installed.packages()) == FALSE) {install.packages("rgl")}
  
  library("rgl")
  
  solid.coord <- which( object.3D > 0, arr.ind = T)
  
  gc()
  
  colnames(solid.coord) <- c("x", "y", "z")
  
  # Plot voxels
  # plot3d(solid.coord, col = rainbow(1000) ) # consumes much RAM
  plot3d(solid.coord )
  
}



##############################################################################################################
#####
## Dilate solid phase by one voxel (with diagonal elements)
#####

dilate.solid.phase.by.one.voxel.size <- function(object.3D){
  
  object.3D.dimensions <- dim(object.3D)
  img.init <- object.3D[,,1]
  img.dims <- dim(object.3D[,,1])
  
  for( k in 1:object.3D.dimensions[3]){
    
    if( ! k %% 50 ) { print( paste0( c('Image', k, 'of', object.3D.dimensions[3] ), collapse = ' ' ) ) }
    
    img.init <- object.3D[,,k]
    
    
    img.new <- img.init
    # image( t(apply(   img.new   , 2, rev)), useRaster = F, col = gray.colors(n = 2, start = 0, end = 1) )
    
    ## along X
    mask <- array( as.integer(0), dim = img.dims )
    
    mask[1:img.dims[1]-1, ] <- mask[1:img.dims[1]-1, ] + img.new[2:img.dims[1],] - img.new[1:img.dims[1]-1,]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    # image( t(apply(   img.init+mask/2   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    
    
    
    mask[2:img.dims[1], ] <- mask[2:img.dims[1], ] + img.new[1:img.dims[1]-1,] - img.new[2:img.dims[1],]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    
    mask[mask > as.integer(0)] <- as.integer(1)
    # image( t(apply(   img.init+mask/2   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    
    
    ## applying the mask
    img.new <- img.new + mask
    # image( t(apply(   img.new   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    ##
    
    mask.X <- mask
    # along Y
    mask <- array( as.integer(0), dim = img.dims )
    
    mask[, 1:img.dims[2]-1] <- mask[, 1:img.dims[2]-1] + img.new[,2:img.dims[2]] - img.new[, 1:img.dims[2]-1]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    # image( t(apply(   img.new+mask/2   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    
    
    mask[ , 2:img.dims[2]] <- mask[ , 2:img.dims[2]] + img.new[ , 1:img.dims[2]-1] - img.new[ , 2:img.dims[2]]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    mask[mask > as.integer(0)] <- as.integer(1)
    
    # image( t(apply(   img.new+mask/2   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    ##
    
    mask.Y <- mask
    
    mask <- mask.X + mask.Y
    mask[mask>as.integer(0)] <- as.integer(1)
    # image( t(apply(   img.init+mask/2   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    # image( t(apply(   img.init   , 2, rev)), useRaster = F, col = gray.colors(n = 3, start = 0, end = 1) )
    object.3D[,,k] <- img.init + mask
    if( ! k %% 10 ) { gc() }
  }
  
  #
  
  img.dims <- dim(object.3D[ 1, , ])
  for( i in 1:object.3D.dimensions[1] ) {
    if( ! i %% 50 ) { print( paste0( c('Image', i, 'of', object.3D.dimensions[1] ), collapse = ' ' ) ) }
    
    img.init <- object.3D[ i, , ]
    
    img.new <- img.init
    
    mask <- array( as.integer(0), dim = img.dims )
    
    mask[, 1:img.dims[2]-1] <- mask[, 1:img.dims[2]-1] + img.new[,2:img.dims[2]] - img.new[, 1:img.dims[2]-1]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    
    
    
    mask[ , 2:img.dims[2]] <- mask[ , 2:img.dims[2]] + img.new[ , 1:img.dims[2]-1] - img.new[ , 2:img.dims[2]]
    
    mask[ mask == as.integer(-1) ] <- as.integer(0)
    
    mask[mask > as.integer(0)] <- as.integer(1)
    
    object.3D[ i, , ] <- img.init + mask
    
    if( ! i %% 100 ) { gc() }
  }
  
  return( object.3D )
  
}

##############################################################################################################
#####
## Write 3D array into .dis with mirrored conditions
#####

write.dis.mirrored <- function( object.3D, dest.bin.file.path, binorascii ) {
  
  ascii.shift <- 0
  
  if( tolower(binorascii) %in% c('ascii', 'asci', 'acsi', 'acsii', 'a' ) ){
    ascii.shift <- 48
  } else if( tolower(binorascii) %in% c('bin', 'binary', 'binar', 'b' ) ) {
    ascii.shift <- 0
  } else{
    stop( "Input parameter 'binorascii' is not correct." )
  }
  
  object.3D.dimensions <- dim( object.3D )
  
  to.writebin = file( dest.bin.file.path, "wb" )
  
  layers.number <- -1
  
  for( i in 1:object.3D.dimensions[3]){
    print( paste0( 'Write bin ', i, ' (Layer ', layers.number, ')' ) )
    if( i %% 100 == 0 ){gc()}
    tmp.arr <- object.3D[, , i]
    tmp.arr.mirrored <- rbind( cbind( tmp.arr, tmp.arr[ 1:dim(tmp.arr)[1], dim(tmp.arr)[2]:1])
                               , cbind( tmp.arr[ dim(tmp.arr)[1]:1, 1:dim(tmp.arr)[2]], tmp.arr[ dim(tmp.arr)[1]:1, dim(tmp.arr)[2]:1])
    )
    
    tmp.raw.mirrored <- as.raw(tmp.arr.mirrored+ascii.shift)
    writeBin(tmp.raw.mirrored, to.writebin)
  }
  
  for( i in object.3D.dimensions[3]:1){
    print( paste0( 'Write bin ', i, ' (Layer ', layers.number, ')' ) )
    if( i %% 100 == 0 ){gc()}
    tmp.arr <- object.3D[, , i]
    tmp.arr.mirrored <- rbind( cbind( tmp.arr, tmp.arr[ 1:dim(tmp.arr)[1], dim(tmp.arr)[2]:1])
                               , cbind( tmp.arr[ dim(tmp.arr)[1]:1, 1:dim(tmp.arr)[2]], tmp.arr[ dim(tmp.arr)[1]:1, dim(tmp.arr)[2]:1])
    )
    
    tmp.raw.mirrored <- as.raw(tmp.arr.mirrored+ascii.shift)
    writeBin(tmp.raw.mirrored, to.writebin)
  }
  
  close(to.writebin)
  
}


##############################################################################################################
#####
## Write 3D array into .dis
#####

write.dis <- function( object.3D, dest.bin.file.path ) {
  
  to.writebin = file( dest.bin.file, "wb" )
  
  tmp.raw <- as.raw(object.3D+48)
  writeBin(tmp.raw, to.writebin)
  
  close(to.writebin)
  
}


##############################################################################################################
#####
## Read .dis and write into 3D array
#####

read.dis <- function( path, xyz.object.size.vector ){
  
  to.read = file( path, "rb")
  
  object.3D = readBin(to.read, raw(), n=prod(xyz.object.size.vector), size = 1); gc()
  
  close(to.read)
  
  object.3D <- as.integer( object.3D ); gc()
  
  object.3D <- array( object.3D, dim = xyz.object.size.vector ); gc()
  object.3D <- object.3D - as.integer(48); gc()
  
  return( object.3D )
  
}


##############################################################################################################
#####
## Create 3D array with cylindrical hole
#####

cylindrical.pore <- function(  ){
  
  # cyl.biggest.radius <- 250
  # 
  # #######################
  # # part 1
  # #######################
  # cyl.radius <- 250
  # 
  # cyl.length <- 50
  # 
  # cyl.img.dim <- c(2*cyl.biggest.radius+3, 2*cyl.biggest.radius+3)
  # cyl <- array( as.integer(1), dim = cyl.img.dim )
  # cyl.center <- c( floor(cyl.img.dim[1]/2)+1, floor(cyl.img.dim[2]/2)+1)
  # 
  # 
  # 
  # cyl[cyl.center[1], cyl.center[2]] <- 0
  # 
  # all.cyl.indexes <- which(cyl > -1, arr.ind = T)
  # 
  # for( i in 1:nrow(all.cyl.indexes) ){
  #   if( (all.cyl.indexes[i, 1]-cyl.center[1])^2 +  (all.cyl.indexes[i, 2]-cyl.center[1])^2 <= cyl.radius^2 ){
  #     cyl[  all.cyl.indexes[i, 1]  ,  all.cyl.indexes[i, 2]  ] <- 0
  #   }
  # }
  # 
  # cyl.part.1 <- array( rep( cyl, times = cyl.length ), dim = c(cyl.img.dim, cyl.length ) )
  # 
  # #######################
  # # part 2
  # #######################
  # 
  # cyl.radius <- 25
  # cyl.img.dim <- c(2*cyl.biggest.radius+3, 2*cyl.biggest.radius+3)
  # cyl <- array( as.integer(1), dim = cyl.img.dim )
  # cyl.center <- c( floor(cyl.img.dim[1]/2)+1, floor(cyl.img.dim[2]/2)+1)
  # 
  # cyl.length <- 450
  # 
  # cyl[cyl.center[1], cyl.center[2]] <- 0
  # 
  # all.cyl.indexes <- which(cyl > -1, arr.ind = T)
  # 
  # for( i in 1:nrow(all.cyl.indexes) ){
  #   if( (all.cyl.indexes[i, 1]-cyl.center[1])^2 +  (all.cyl.indexes[i, 2]-cyl.center[1])^2 <= cyl.radius^2 ){
  #     cyl[  all.cyl.indexes[i, 1]  ,  all.cyl.indexes[i, 2]  ] <- 0
  #   }
  # }
  # 
  # cyl.part.2 <- array( rep( cyl, times = cyl.length ), dim = c(cyl.img.dim, cyl.length ) )
  # 
  # #######################
  # # Write RDS
  # #######################
  # 
  # cyl <- array( c(cyl.part.1, cyl.part.2), dim = c(dim(cyl), dim(cyl.part.1)[3]+dim(cyl.part.2)[3] )  )
  # 
  # image( t(apply(   cyl[,,51]    , 2, rev)), useRaster = F, col = gray.colors(n = 2, start = 0, end = 1) )
  # 
  # tmp.cyl.part <- cyl[,,( dim(cyl)[3] - dim(cyl.part.2)[3]/2 +1 ):( dim(cyl)[3] )] 
  # 
  # cyl[,, (dim(cyl)[3]/2-dim(cyl.part.1)[3]/2 +1):(dim(cyl)[3]/2+dim(cyl.part.1)[3]/2) ] <- cyl[,,1:dim(cyl.part.1)[3]]; gc()
  # cyl[,,1:(dim(cyl)[3]/2-dim(cyl.part.1)[3]/2)] <- tmp.cyl.part
  # 
  # # grid(nx = cyl.img.dim[1], ny = cyl.img.dim[1])
  # 
  # #cyl[ , cyl.img.dim[2]]
  # 
  # #cyl[ , 1]
  # 
  # #cyl <- array( rep( cyl, times = cyl.length ), dim = c(cyl.img.dim, cyl.length ) )
  # 
  # saveRDS( cyl, file = 'd:/1_Work/Programming/R/PROJECTS/2016/Mesoporous monoliths (from Daniela, for P. Monson)/Cylinder-like_pores/cylinder-likeH500L50h50l450_extra_voxel0.rds' )
  # 
  # 
  # 
  # fake.porosity <- array(0, dim = c(dim(cyl)[3], 2))
  # 
  # for( i in 1:dim(cyl)[3]){
  #   
  #   fake.porosity[i,1] <- i
  #   fake.porosity[i,2] <- length(cyl[,,i][cyl[,,i]==0])/prod(dim(cyl)[1:2])
  #   
  # }
  # 
  # plot( fake.porosity, type = 'l' ); grid()
  
}


##############################################################################################################
#####
## Invert array values: 0 -> 1; 1 -> 0 of 3D object.
#####

invert.array.values.0.1 <- function(object.3D){
  
  for( k in 1:dim(object.3D)[3] ){
    
    print( paste0( 'Invert pixels; Layer = ', k  )  )
    object.3D[,,k] <- object.3D[,,k] - as.integer(1)
    object.3D[,,k][object.3D[,,k]==-1] <- as.integer(1)
    if( ! k %% 80 ) { gc() }
  }
  return(object.3D)
}


##############################################################################################################
#####
## Save 3D array as TIFF
#####

obj3d2tiff <- function( object.3d
                        , dest.dir
                        , img.prefix = 'Img_'
                        , num.digits = 4
                        , start.imgnum = 0){
  if( !dir.exists(dest.dir) ){
    dir.create(path = dest.dir, recursive = T)
  }
  obj.dims <- dim(object.3d)
  for(  imnum in 1:obj.dims[3] ){
    img.name <- 
      paste0( 'Img_'
              , sprintf( paste0('%0', num.digits, 'd'), imnum-1+start.imgnum)
              , '.tif' )
    writeTIFF( what = array( as.numeric(object.3d[,,imnum])
                             , dim = c(obj.dims[1:2]) )
               , where = paste(dest.dir
                               , img.name
                               ,  sep = '/')
               , compression = 'none' )
  }
}



