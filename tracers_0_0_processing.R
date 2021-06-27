
# Functions ----------------------------------------------------------------
get.diff.vect <- function(data, col.num.to.diff){
  
  if( length(col.num.to.diff)==1 ){
    res.diff.vect <- diff( data )
  } else if( length(col.num.to.diff)>1 ){
    res.diff.vect <- numeric(nrow(data)-1)
    for(i in col.num.to.diff){
      res.diff.vect <- res.diff.vect + diff( data[,i] )
    }
    res.diff.vect <- res.diff.vect/length(col.num.to.diff)
  }
  
  return(res.diff.vect)
  
}

normalize.diff.vect <- function( data
                                 , iter.step
                                 , time.step
                                 , filtering.step = 1
                                 , mol.diff.val=0.0281 ){
  norm.data <- data/iter.step/time.step/filtering.step/mol.diff.val/2
  return( norm.data)
}

diff.vals.from.tracers <- 
  function( data
            , diff.direction = c('3D', 'vertical', 'horizontal', 'longitudinal' )
            , range.to.avg
            , iter.step
            , time.step
            , filtering.step = 1
            , mol.diff.val=0.0281){
    if( length( range.to.avg ) == 2 ){
      start.avg.pos <- range.to.avg[1]
      stop.avg.pos <- range.to.avg[2]
    } else if( length( range.to.avg ) == 1 ) {
      
      if( is.data.frame(data) ){
        vect.lng <- nrow(data)
      } else if( is.vector(data) ){
        vect.lng <- length(data)
      }
      # position is shifted to the left as it diff function reduces
      # vector length by 1
      start.avg.pos <- vect.lng-range.to.avg
      stop.avg.pos <- vect.lng - 1
    }
    
    diff.vals.lst <- list()
    if('3D' %in% diff.direction ) {
      avg.diff.vect <- get.diff.vect( data = data, col.num.to.diff = c(1,2,3) )
      preproc.data <- avg.diff.vect[start.avg.pos:stop.avg.pos]
      preproc.data <- normalize.diff.vect(preproc.data, iter.step, time.step)
      diff.val <- mean(preproc.data)
      diff.vals.lst$all_directions <- diff.val
    } 
    if('longitudinal' %in% diff.direction ) {
      avg.diff.vect <- get.diff.vect( data = data[, 1], col.num.to.diff = 1 )
      preproc.data <- avg.diff.vect[start.avg.pos:stop.avg.pos]
      preproc.data <- normalize.diff.vect(preproc.data, iter.step, time.step)
      diff.val <- mean(preproc.data)
      diff.vals.lst$longitudinal <- diff.val
    } 
    if('horizontal' %in% diff.direction ) {
      avg.diff.vect <- get.diff.vect( data = data[, 2], col.num.to.diff = 1 )
      preproc.data <- avg.diff.vect[start.avg.pos:stop.avg.pos]
      preproc.data <- normalize.diff.vect(preproc.data, iter.step, time.step)
      diff.val <- mean(preproc.data)
      diff.vals.lst$horizontal <- diff.val
    } 
    if('vertical' %in% diff.direction ) {
      avg.diff.vect <- get.diff.vect( data = data[, 3], col.num.to.diff = 1 )
      preproc.data <- avg.diff.vect[start.avg.pos:stop.avg.pos]
      preproc.data <- normalize.diff.vect(preproc.data, iter.step, time.step)
      diff.val <- mean(preproc.data)
      diff.vals.lst$vertical <- diff.val
    }
    
    return(diff.vals.lst)
    
  }



# Main --------------------------------------------------------------------
library(tidyverse)

path.tracers <- 'd:/1_Work/PROJECTS/2018/5_Ns/Dis_files'
diff.files <- dir(path.tracers
                  , pattern = 'tracers_'
                  , full.names = T
                  , recursive = T )[1:3]

small.file.idx <- which.min( file.size(diff.files) )

cur.data <- read.table(diff.files[small.file.idx])[ ,5:7]

norm.diff.data <- normalize.diff.vect( data = get.diff.vect(cur.data, c(1,2,3)), iter.step = 20, time.step = 0.5 )
#plot(norm.diff.data)
mean(norm.diff.data[ (length(norm.diff.data)-4000):(length(norm.diff.data)-3000)] )
vals.to.avg <- 300
start.avg.pos <- length(norm.diff.data) - vals.to.avg + 1
stop.avg.pos <- length(norm.diff.data)

itstep <- 20
dt <- 0.5;



rm(tib.diff.data)
for( i in 1:length(diff.files) ){
  
  cur.path <- diff.files[i]
  print( paste0( 'File ', i, ' of ', length(diff.files) ) )
  cur.data <- read.table(cur.path)[ ,5:7]
  
  cur.obj.name <- basename(dirname( dirname(cur.path) ))
  full.obj.name <- basename( dirname(cur.path) ) 
  cur.extravox.num <- 
    as.integer( 
      substr( x = full.obj.name
              , start = nchar(full.obj.name)-2
              , stop = nchar(full.obj.name) ) 
      )
  rm(full.obj.name)
  
  diff.vals.lst <- 
    diff.vals.from.tracers( data = cur.data
                            , diff.direction = c('3D', 'vertical', 'horizontal', 'longitudinal' )
                            , range.to.avg = c(start.avg.pos, stop.avg.pos)
                            , iter.step = itstep
                            , time.step = dt )
  info.lst <- list( id = i
                    , obj_name = cur.obj.name
                    , extravox_num = cur.extravox.num  )
  
  if( i > 1 ){
    tib.diff.data[ nrow(tib.diff.data)+1, ] <- as.tibble( c( info.lst , diff.vals.lst) ) 
  } else {
    tib.diff.data <- as.tibble( c( info.lst , diff.vals.lst) ) 
  }
  
}

tib.diff.data

# Diffusion Visualization -------------------------------------------------

ggplot( tib.diff.data ) +
  geom_point(aes( x=extravox_num, y=all_directions, colour=obj_name)) +
  geom_line(aes( x=extravox_num, y=all_directions, colour=obj_name))



tib.diff.data <- 
rbind(
  tib.diff.data
, tib.diff.data.bkp[tib.diff.data.bkp$obj_name == 'Nucleosil30_old', ]
)
# Save diff. data ---------------------------------------------------------

library( xlsx )

write.xlsx(x = tib.diff.data, file = 'Nucleosil_diff_data.xlsx')


# Save visualizations -----------------------------------------------------
library(ggplot2)

path.tracers <- 'd:/1_Work/PROJECTS/2018/5_Ns/Dis_files'
diff.files <- dir(path.tracers, pattern = 'tracers_', full.names = T, recursive = T )[1:2]


for( i in 1:length(diff.files) ){
  #for( i in 10:11 ){
  
  cur.path <- diff.files[i]
  if(!file.exists(cur.path)){
    next
  }
  
  print( paste0( 'File ', i, ' of ', length(diff.files) ) )
  cur.data <- read.table(cur.path)[ ,5:7]
  
  cur.obj.name <- basename(dirname( dirname(cur.path) ))
  full.obj.name <- basename( dirname(cur.path) ) 
  cur.extravox.num <- 
    as.integer( 
      substr( x = full.obj.name
              , start = nchar(full.obj.name)-2
              , stop = nchar(full.obj.name) ) 
    )
  # rm(full.obj.name)
  
  
  diff.vect <- get.diff.vect( data = cur.data, col.num.to.diff = c(1,2,3))
  diff.df <- data.frame( x = 1:length(diff.vect)
                         , y = diff.vect )
  
  
  x.start <- sort( length(diff.vect) - seq(from = 1000, to = 450000, by = 1000) )
  x.stop <- sort( length(diff.vect) - seq(from = 1000, to = 450000, by = 1000) ) + 1000
  y.vals <- sapply( X = 1:length(x.start), function(i){
    mean( diff.vect[x.start[i]:x.stop[i]] )
  } )
  
  tib.segments <- tibble( start = x.start, end = x.stop, y = y.vals )
  
  diff.val <- mean( diff.vect[ (length(diff.vect)-5000):length(diff.vect)] )
  
  cur.ggplot <- ggplot() +
    geom_line(data = diff.df, mapping = aes( x, y )) +
    ggplot2::ylim( c(min(y.vals) - 45*sd(y.vals), max(y.vals) + 45*sd(y.vals) ) ) +
    geom_hline( yintercept = diff.val, colour = 'blue'  ) +
    geom_segment( data = tib.segments, aes(x = start, y = y, xend = end, yend = y), colour = 'red', size = 0.5 )
    
    ggsave(filename = paste0(full.obj.name, '.tif')
           , plot = cur.ggplot
           , device = 'tiff'
           , path = 'G:/graph_figs'
           , dpi = 320 )
  
}

# Test --------------------------------------------------------------------
library('pastecs')

tmp.rnorm <- diff.vect[ (length(diff.vect)-1000):(length(diff.vect)-900)]
tmp.minmax <- range(tmp.rnorm)
tmp.breaks <- seq(tmp.minmax[1], tmp.minmax[2], length.out = 5)    
tmp.cuts <- cut(tmp.rnorm, tmp.breaks, right=FALSE )
tmp.freq <- table(tmp.cuts)
tmp.thprob <- diff( pnorm( q = tmp.breaks, mean = diff.val, sd = sd(diff.vect[ (length(diff.vect)-500):length(diff.vect)]) ) ) 
chisq.test( cbind( tmp.freq, tmp.thprob*sum(tmp.freq) ) )
qchisq(0.05, df = 6)






x.start <- sort( length(diff.vect) - seq(from = 500, to = 150000, by = 500) )
x.stop <- sort( length(diff.vect) - seq(from = 500, to = 150000, by = 500) ) + 500
y.vals <- sapply( X = 1:length(x.start), function(i){
  mean( diff.vect[x.start[i]:x.stop[i]] )
} )

tib.segments <- tibble( start = x.start, end = x.stop )

tmp.list <- list()

for( i in 1:nrow(tib.segments)){
  tmp.list[[i]] <- t.test( diff.vect[ (length(diff.vect)-500):length(diff.vect)]
                           , diff.vect[ tib.segments$start[i]:tib.segments$end[i] ]
  )
}



tmp.ttestres <- tmp.list[[length(tmp.list)-190]]
tmp.ttestres$p.value


by(data = tmp.rnorm, rep('dumb', length(tmp.rnorm)) )

stat.desc(tmp.rnorm, basic = F, norm = T)
stat.desc(diff.vect[ (length(diff.vect)-1000):(length(diff.vect)-900)], basic = F, norm = T)




