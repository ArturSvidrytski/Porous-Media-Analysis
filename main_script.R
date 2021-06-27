library(tidyverse)

path <- 'D:/1_Work/PROJECTS/2018/5_Ns/Images_ini'
img.paths <- dir(path, full.names = T)
# Porosity values ---------------------------------------------------------
tib.porosity <- tibble(str_name = character(0)
                       , porosity = numeric(0) )

for( i in 1:length(img.paths) ){
  cur.img.path <- img.paths[i]
  obj3d <- tiff.stack.into.3D.array(cur.img.path)
  tib.porosity[i, ] <- tibble( basename(cur.img.path), porosity.nslice(obj3d) )
}


# Dilated images preparation ----------------------------------------------
dil.path <- 'D:/1_Work/PROJECTS/2018/5_Ns/Dilated_images'
max.dil.voxels <- 35

for( i in 1:length(img.paths) ){
  cur.img.path <- img.paths[i]
  obj3d <- tiff.stack.into.3D.array(cur.img.path)
  
  for( cur.dil.vox in 0:max.dil.voxels ){
    extvoxnum <- sprintf( paste0('%0', 3, 'd'), cur.dil.vox)
    cur.dil.path <- paste( dil.path
                           , basename(cur.img.path)
                           , paste0( basename(cur.img.path)
                                     , '_'
                                     , 'extravox'
                                     , '_'
                                     , extvoxnum)
                           , sep='/')
    
    if( dir.exists(cur.dil.path) ){
      unlink(cur.dil.path, recursive = T)
    }

    dir.create( cur.dil.path, recursive = T )
    
    if( cur.dil.vox > 0 ){
      obj3dold <- obj3d
      obj3d <- dilate.solid.phase.by.one.voxel.size(object.3D = obj3dold)
      obj3d2tiff(object.3d = obj3d
                 , dest.dir = cur.dil.path  )
    } else {
      obj3d2tiff(object.3d = obj3d
                 , dest.dir = cur.dil.path  )
    }
    
  }
}



# Porosities for dilated sturctures ---------------------------------------

tib.dil.porosity <- tibble(str_name = character(0)
                           , extravoxnum = integer(0)
                           , porosity = numeric(0) )

dil.obj.paths <- dir( dil.path, full.names = T )

for( cur.dil.obj.path  in dil.obj.paths ){
  
  dil.img.paths <- dir(cur.dil.obj.path, full.names = T)
  
  for( i in 1:length(dil.img.paths) ){
    cur.dil.img.path <- dil.img.paths[i]
    obj3d <- tiff.stack.into.3D.array(cur.dil.img.path)
    img.dir.name <- basename(cur.dil.img.path)
    extvoxnum <- 
      as.integer( 
        substr( x = img.dir.name
                , start = nchar(img.dir.name)-2 
                , stop = nchar(img.dir.name) )
      )
        
    tib.dil.porosity[nrow(tib.dil.porosity)+1, ] <- 
      tibble( basename(dirname(cur.dil.img.path))
              , extvoxnum
              , porosity.nslice(obj3d) )
  }
}


# Plot graphs -------------------------------------------------------------

library(ggplot2)
library(tidyverse)

tib.dil.porosity$lambda <- tib.dil.porosity$extravoxnum
tib.dil.porosity$lambda[tib.dil.porosity$str_name=='Nucleosil10'] <- tib.dil.porosity$lambda[tib.dil.porosity$str_name=='Nucleosil10']/16

ggplot( tib.dil.porosity, aes(x = lambda, y = porosity) ) +
  geom_line(aes(colour = str_name)) +
  geom_point(aes(colour = str_name)) +
  xlab('Amount of extra voxels') +
  ylab( 'Porosity, %')


tib.dil.porosity25 <- tib.dil.porosity %>%
  filter( extravoxnum <= 25 )


ggplot( tib.dil.porosity25, aes(x = extravoxnum, y = porosity) ) +
  geom_line(aes(colour = str_name)) +
  geom_point(aes(colour = str_name)) +
  xlab('Amount of extra voxels') +
  ylab( 'Porosity, %')


# Save data ---------------------------------------------------------------
library(xlsx)

write.xlsx(x = tib.dil.porosity25
           , file = 'porosity_extravoxels.xlsx')


# Prepare data for simulations --------------------------------------------

dis.path <- 'd:/1_Work/PROJECTS/2018/5_Ns/Dis_files'
dil.img.paths <- dir( dir( dil.path, full.names = T )[2], full.names = T )
dis.dir.paths <- paste( sep = '/'
                        , dis.path
                        , basename( dirname(dil.img.paths) )
                        , basename(dil.img.paths) )


for( cur.dil.obj.num in 1:length(dil.img.paths) ){
  cur.dil.img.path <- dil.img.paths[cur.dil.obj.num]
  cur.dis.dir.path <- dis.dir.paths[cur.dil.obj.num]
  cur.dis.file.path <- paste(sep = '/', cur.dis.dir.path, 'input.dis')
  
  if( dir.exists(cur.dis.dir.path) ){
    unlink(cur.dis.dir.path, recursive = T)
  }
  dir.create(cur.dis.dir.path, recursive = T)

  
  obj3d <- tiff.stack.into.3D.array(cur.dil.img.path)
  write.dis.mirrored(object.3D = obj3d
                     , dest.bin.file.path = cur.dis.file.path
                     , binorascii = 'a')
}


# Coreshell9 (dis files without bubbles) ----------------------------------

dis.path <- 'd:/1_Work/PROJECTS/2018/5_Ns/Dis_files'
dil.img.paths <- dir( dir( dil.path, full.names = T )[1], full.names = T )
dis.dir.paths <- paste( sep = '/'
                        , dis.path
                        , basename( dirname(dil.img.paths) )
                        , basename(dil.img.paths) )


for( cur.dil.obj.num in 1:length(dil.img.paths) ){
  cur.dil.img.path <- dil.img.paths[cur.dil.obj.num]
  cur.dis.dir.path <- dis.dir.paths[cur.dil.obj.num]
  cur.dis.file.path <- paste(sep = '/', cur.dis.dir.path, 'input.dis')
  
  if( dir.exists(cur.dis.dir.path) ){
    unlink(cur.dis.dir.path, recursive = T)
  }
  dir.create(cur.dis.dir.path, recursive = T)
  
  
  obj3d <- tiff.stack.into.3D.array(cur.dil.img.path)
  write.dis.mirrored(object.3D = obj3d
                     , dest.bin.file.path = cur.dis.file.path
                     , binorascii = 'a')
}


# Testing section ---------------------------------------------------------



  