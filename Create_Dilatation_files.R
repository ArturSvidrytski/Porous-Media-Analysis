path <- 'd:/1_Work/PROJECTS/2019/02_KIT6s02_SBA15s02'

fname.kit6s02 <- 'KIT6_s02'
fname.sba15s02 <- 'SBA15_s02'

path.kit6s02 <- file.path(path, 'Reconstructed_Images', fname.kit6s02 )
path.sba15s02 <- file.path(path, 'Reconstructed_Images', fname.sba15s02)

dest.path.kit6s02 <- file.path(path, 'Dilatation', fname.kit6s02 )
dest.path.sba15s02 <- file.path(path, 'Dilatation', fname.sba15s02)

if( !dir.exists(dest.path.kit6s02) ){
  dir.create(dest.path.kit6s02, recursive = T)
}

if( !dir.exists(dest.path.sba15s02) ){
  dir.create(dest.path.sba15s02, recursive = T)
}


obj.3d <- tiff.stack.into.3D.array(path.kit6s02)
obj3d2tiff(object.3d = obj.3d
           , dest.dir = paste( dest.path.kit6s02, 'KIT6s02_extravox000', sep='/')  )

obj.3d.affected <- obj.3d
for( i in 1:8 ){
  obj.3d.affected <- dilate.solid.phase.by.one.voxel.size(obj.3d.affected)
  extvoxnum <- sprintf( paste0('%0', 3, 'd'), i)
  cur.path <- paste( dest.path.kit6s02, paste0('KIT6s02_extravox', extvoxnum), sep='/')
  obj3d2tiff(object.3d = obj.3d.affected
             , dest.dir = cur.path  )
}


obj.3d <- tiff.stack.into.3D.array(path.sba15s02)
obj3d2tiff(object.3d = obj.3d
           , dest.dir = paste( dest.path.sba15s02, 'SBA15s02_extravox000', sep='/')  )

obj.3d.affected <- obj.3d
for( i in 1:8 ){
  obj.3d.affected <- dilate.solid.phase.by.one.voxel.size(obj.3d.affected)
  extvoxnum <- sprintf( paste0('%0', 3, 'd'), i)
  cur.path <- paste( dest.path.sba15s02, paste0('SBA15s02_extravox', extvoxnum), sep='/')
  obj3d2tiff(object.3d = obj.3d.affected
             , dest.dir = cur.path  )
}
