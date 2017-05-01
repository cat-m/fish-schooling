# CA Project 
# Module 14.10 - 'Fish Schooling'
# Version 1.1
# counts own direction in mean
# because all neighbors influence each other
# so I think this will be more realistic

# SOUTH/EAST bias in behavior, does not form homogenous clusters
# rather involved, written-out round thing for mean

# ignore cohesion rule
# obey separation rule and alignment rule
# separation takes precedence

# this version's interpretation of separation rule:
# "don't move into the same space on the grid"

# wrap edges

# this version is probably gonna have the 'herky jerky' movement

# possible cell states (by 'cell' I mean 'pair of coords')
EMPTY <- c(0,0)   # unoccupied cell                    #white
#NW <- list(1,-1)     # fish with direction of Northwest   #purple
#NORTH <- list(1,0)   # fish with direction of North       #indigo
#NE <- list(1,1)      # fish with direction of Northeast   #blue
#WEST <- list(0,-1)   # fish with direction of West        #red
#EAST <- list(0,1)    # fish with direction of East        #dark green
#SW <- list(-1,-1)    # fish with direction of Southwest   #orange
#SOUTH <- list(-1,0)  # fish with direction of South       #yellow
#SE <- list(-1,1)     # fish with direction of Southeast   #green

# minimum gap between fish
#gap.size <- 1 # number of empty cells between two occupied cells


# dimensions
size <- 20
gens <- 50 # slices

probability <- 0.4 # to make sure some cells are empty (for initialization)


# FUNCTIONS

# test whether a pair of coords is (0,0)
empty <- function(coords) {
  return(sum(coords==EMPTY)==2)
}

# copy edges to ghost margins
wrap.edges <- function(slice) {
  # wrap top and bottom edges:
  for(i in 1:2) {
    loaf[[slice]][[i]] <- loaf[[slice]][[size+i]]
    loaf[[slice]][[size+i+2]] <- loaf[[slice]][[i+2]]
  }
  # wrap left and right edges:
  for(i in 1:2) {
    for(row in 3:(size+2)) {
      loaf[[slice]][[row]][[i]] <- loaf[[slice]][[row]][[size+i]]
      loaf[[slice]][[row]][[size+2+i]] <- loaf[[slice]][[row]][[i+2]]
    }
  }
  # set corners:
  for(i in 1:2) {
    for(j in 1:2) {
      # top corners
      loaf[[slice]][[i]][[j]] <- loaf[[slice]][[size+i]][[size+j]]
      loaf[[slice]][[i]][[size+j+2]] <- loaf[[slice]][[size+i]][[j+2]]
      # bottom corners
      loaf[[slice]][[size+i+2]][[j]] <- loaf[[slice]][[i+2]][[size+j]]
      loaf[[slice]][[size+i+2]][[size+j+2]] <- loaf[[slice]][[i+2]][[j+2]]
    }
  }
  return(loaf[[slice]])
}

# get mean direction of all neighbors 
# (Moore's neighborhood,  dist 1 )
get.mean.dir <- function(slice,this.r,this.c) {
  #if(empty(loaf[[slice-1]][[this.r]][[this.c]])) { return(EMPTY) }
  dist <- 1
  num.neighbors <- 0
  neighbor.directions <- EMPTY
  for(row in -dist:dist) {
    for(col in -dist:dist) {
      bro.dir <- unlist(loaf[[slice-1]][[this.r+row]][[this.c+col]])
      #if(!(row==0 && col==0) && !empty(bro.dir)) {
      if(!empty(bro.dir)) {
        num.neighbors <- num.neighbors + 1
        neighbor.directions <- neighbor.directions + bro.dir
      }
    }
  }
  
  #mean <- round(neighbor.directions/num.neighbors)
  mean <- neighbor.directions/num.neighbors
  for(i in 1:2) {
    remainder <- mean[i]%%1
    #cat(remainder)
    if((mean[i]>0 && remainder>=.5) || (mean[i]<0 && remainder<.5)) { 
      mean[i] <- ceiling(mean[i]) 
    } else { mean[i] <- floor(mean[i]) }
  }
  if(num.neighbors==0 || empty(mean)) {
    return(unlist(loaf[[slice-1]][[this.r]][[this.c]]))
  } else { return(mean) }
}

# should keep two fish from moving into the same empty cell
# and keep fish from moving into occupied cell.
okay.to.move <- function(slice,row,col,dest) {
  if(!empty(unlist(loaf[[slice-1]][[dest[1]]][[dest[2]]]))) { return(FALSE) }
  for(r in -1:1) {
    for(c in -1:1) {
      bro.row <- dest[1]+r
      bro.col <- dest[2]+c
      bro.dir <- unlist(loaf[[slice-1]][[bro.row]][[bro.col]])
      if((!(bro.row==row && bro.col==col) && !empty(bro.dir)) && empty(bro.dir+c(r,c))) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

# Plot the grid for the generation specified.
print <- function(slice) {
  FLIPPED.Y.AXIS <- 1:(size+4)#:1 # rows reversed for plotting
  # it would be much more efficient to use a matrix here,
  # but this makes the code easier to read at a glance
  nw.x <- vector() 
  nw.y <- vector()
  north.x <- vector()
  north.y <- vector()
  ne.x <- vector()
  ne.y <- vector()
  west.x <- vector()
  west.y <- vector()
  east.x <- vector()
  east.y <- vector()
  sw.x <- vector()
  sw.y <- vector()
  south.x <- vector()
  south.y <- vector()
  se.x <- vector()
  se.y <- vector()
  
  for (row in 3:(size+2)) {
    for (col in 3:(size+2)) {
      direction <- unlist(loaf[[slice]][[row]][[col]])
      if(direction[1]==1) {
        if(direction[2]==-1) {
          nw.x <- c(nw.x,col)
          nw.y <- c(nw.y,FLIPPED.Y.AXIS[row])
        } else if(direction[2]==0) {
          north.x <- c(north.x,col)
          north.y <- c(north.y,FLIPPED.Y.AXIS[row])
        } else {
          ne.x <- c(ne.x,col)
          ne.y <- c(ne.y,FLIPPED.Y.AXIS[row])
        }
      } else if(direction[1]==0) {
        if(direction[2]==-1) {
          west.x <- c(west.x,col)
          west.y <- c(west.y,FLIPPED.Y.AXIS[row])
          #} else if(direction[2]==0) {
          #  empty.x <- c(empty.x,col)
          #  empty.y <- c(empty.y,FLIPPED.Y.AXIS[row])
        } else if(direction[2]==1) {
          east.x <- c(east.x,col)
          east.y <- c(east.y,FLIPPED.Y.AXIS[row])
        }
      } else {
        if(direction[2]==-1) {
          sw.x <- c(sw.x,col)
          sw.y <- c(sw.y,FLIPPED.Y.AXIS[row])
        } else if(direction[2]==0) {
          south.x <- c(south.x,col)
          south.y <- c(south.y,FLIPPED.Y.AXIS[row])
        } else {
          se.x <- c(se.x,col)
          se.y <- c(se.y,FLIPPED.Y.AXIS[row])
        }
      }
    }
  }
  
  # NW is PURPLE
  plot(x=nw.x,y=nw.y,ylim=c(3,size+2),
       xlim=c(3,size+2),pch=60,col="purple",
       main=c("Fish Generation",slice),
       yaxt="n",xaxt="n",
       xlab="NW: purple, North: dark blue, NE: blue,\nWest: red,                East: dark green,\nSW: orange, South: yellow, SE: green",
       ylab="NW at top left") 
  # North is DARK BLUE 
  points(x=north.x,y=north.y,pch=2,col="dark blue")
  # NE is BLUE
  points(x=ne.x,y=ne.y,pch=62,col="blue")
  # WEST is RED
  points(x=west.x,y=west.y,pch=171,col="red")
  # EAST is DARK GREEN
  points(x=east.x,y=east.y,pch=187,col="dark green")
  # SW is ORANGE
  points(x=sw.x,y=sw.y,pch=60,col="orange")
  # SOUTH is YELLOW
  points(x=south.x,y=south.y,pch=6,col="yellow")
  # SE is GREEN
  points(x=se.x,y=se.y,pch=62,col="green")
}


# set up empty loaf
# don't forget to initialize 'ghost' borders (2 cell margin)
loaf <- list()
for(slice in 1:gens) {
  loaf[[slice]] <- list()
  for(row in 1:(size+4)) {
    loaf[[slice]][[row]] <- list()
    for(col in 1:(size+4)) {
      loaf[[slice]][[row]][[col]] <- c(0,0)
    }
  }
}

# set up initial random configuration slice (generation 1)
for(row in 3:(size+2)) {
  for(col in 3:(size+2)) {
    if(rbinom(1,1,probability)) {
      loaf[[1]][[row]][[col]] <- floor(runif(2,-1,2))
    }
  }
}
loaf[[1]] <- wrap.edges(1)
print(1)

# FOR LOOP
for(slice in 2:gens) {
  for(row in 3:(size+2)) {
    for(col in 3:(size+2)) {
      direction <- unlist(loaf[[slice-1]][[row]][[col]])
      if(!empty(direction)) {
        new.direction <- get.mean.dir(slice,row,col)
        loaf[[slice]][[row]][[col]] <- new.direction
        destination <- c(row,col)+direction
        if(okay.to.move(slice,row,col,destination)) {
          #loaf[[slice]][[destination[1]]][[destination[2]]] <- direction
          
          for(i in 1:2) {
            if(destination[i]<3) {
              destination[i] <- destination[i]+size
            } else if(destination[i]>(size+2)) {
              destination[i] <- destination[i]-size
            }
          }
          loaf[[slice]][[destination[1]]][[destination[2]]] <- direction
          loaf[[slice]][[row]][[col]] <- EMPTY
        }
      } #else { loaf[[slice]][[row]][[col]] <- EMPTY }
    }
  }
  loaf[[slice]] <- wrap.edges(slice)
  print(slice)
}
