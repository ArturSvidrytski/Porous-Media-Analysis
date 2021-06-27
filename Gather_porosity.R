library(ggplot2)

src.path.core16nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/Dilatation/Coreshell_16nm/'
src.path.core9nm <- 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/Reconstructed_Images/Dilatation/Coreshell_9nm/'

img.paths.core16nm <- dir(src.path.core16nm, full.names = T)
img.paths.core9nm <- dir(src.path.core9nm, full.names = T)

df.porosity <- data.frame( 
  extravox = integer(length(img.paths.core16nm))
  , core16nm = numeric(length(img.paths.core16nm))
  , core9nm = numeric(length(img.paths.core16nm))
)

i <- 1
for( i in 1:length(img.paths.core16nm)){
  cur.path <- img.paths.core16nm[i]
  cur.obj <- tiff.stack.into.3D.array(path = cur.path)
  cur.name <- basename(cur.path)
  df.porosity$extravox[i] <- 
    as.integer(substr(  x = cur.name, start = nchar(cur.name)-2, stop = nchar(cur.name)  ))
  df.porosity$core16nm[i] <- porosity.nslice(cur.obj)
}

for( i in 1:length(img.paths.core9nm)){
  cur.path <- img.paths.core9nm[i]
  cur.obj <- tiff.stack.into.3D.array(path = cur.path)
  cur.name <- basename(cur.path)
  #df.porosity$extravox[i] <- 
  #  as.integer(substr(  x = cur.name, start = nchar(cur.name)-2, stop = nchar(cur.name)  ))
  df.porosity$core9nm[i] <- porosity.nslice(cur.obj)
}

df.porosity$extravox <- as.integer(df.porosity$extravox)

df.porosity$d_tracer <- df.porosity$extravox*0.46*2
# df.porosity$lambda <- df.porosity$extravox*0.46*2/9

write.table(x = df.porosity
            , file = 'd:/1_Work/PROJECTS/2017/10_Coreshell_particles/accessible_porosity.csv'
            , row.names = F )

ggplot(data = df.porosity) +
  geom_point(mapping = aes(x = extravox, y = core16nm), colour = 2) +
  geom_point(mapping = aes(x = extravox, y = core9nm), colour = 3)
