src.path.core16nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/Dilatation/Coreshell_16nm/'
src.path.core9nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/Dilatation/Coreshell_9nm/'

dest.path.core16nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/DIS_files/Coreshell_16nm/'
dest.path.core9nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/DIS_files/Coreshell_9nm/'

if( !dir.exists(dest.path.core16nm) ){
  dir.create(dest.path.core16nm, recursive = T)
}

if( !dir.exists(dest.path.core9nm) ){
  dir.create(dest.path.core9nm, recursive = T)
}

img.paths.core16nm <- dir(src.path.core16nm, full.names = T)
img.paths.core9nm <- dir(src.path.core9nm, full.names = T)

for( i in 1:length(img.paths.core16nm)){
  cur.path <- img.paths.core16nm[i]
  cur.obj <- tiff.stack.into.3D.array(path = cur.path)
  cur.dest <- paste( dest.path.core16nm, basename(cur.path), sep = '/')
  #cur.dest.nm <- paste0(cur.dest, 'nm.dis')
  cur.dest <- paste0(cur.dest, '.dis')
  
  write.dis.mirrored(object.3D = cur.obj
                     , dest.bin.file.path = cur.dest
                     , binorascii = 'ascii')
 # write.dis(object.3D = cur.obj
  #                   , dest.bin.file.path = cur.dest.nm )
}

for( i in 1:length(img.paths.core9nm)){
  cur.path <- img.paths.core9nm[i]
  cur.obj <- tiff.stack.into.3D.array(path = cur.path)
  cur.dest <- paste( dest.path.core9nm, basename(cur.path), sep = '/')
  cur.dest <- paste0(cur.dest, '.dis')
  write.dis.mirrored(object.3D = cur.obj
                     , dest.bin.file.path = cur.dest
                     , binorascii = 'ascii')
}

any.dis.into.input.dis(dest.path.core16nm)
any.dis.into.input.dis(dest.path.core9nm)
