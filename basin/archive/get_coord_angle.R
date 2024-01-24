# get all coordinates of grid when source location, L, and angle are known. 
# Eventually, angle will be decided by altitude difference. 

# Date: 3 Oct 2022

pacman::p_load(data.table, tidyverse, sf, mapview,magrittr,elevatr,leaflet)

# Functions
# 1. coord.at.d
# 2. get.mesh
# 3. get.edge
# 4. get.edge.source
# 5. get.elevation
# 6. get.best.angle
# 7. plot.best.angle

#-----------------
# Get coordinate at d from source (d and heading always fixed, and not given)
coord.at.d <- function(dat,d=1,a=45) {
  # dat contains origin longitude/latitude/heading (0 is north)/distance d (km), and whatever else
  dat %<>% cbind(d,a)
  names(dat) <- c("lon","lat","dist_km","heading")
  if (dat$lat[1] > 90) stop("Longitude and Latitude backwards?")
  if (ncol(dat)>4) names(dat)[5:ncol(dat)] <- paste0("V",5:ncol(dat))
  pts <- dat %>% 
    st_as_sf(coords = c("lon","lat")) %>%
    st_set_crs(4326) %>%
    st_transform(3095) # st_transform(32632) # UTM zone for EUROPE -> Japan
  pts$utm_n <- st_coordinates(pts)[,1]
  pts$utm_e <- st_coordinates(pts)[,2]
  deg2rad <- function(deg) {(deg * pi) / (180)}
  pts %<>% mutate(newp_e=utm_e + (dist_km*1000*cos(deg2rad(heading))), newp_n=utm_n + (dist_km*1000*sin(deg2rad(heading)))) %>% 
    data.table %>% 
    st_as_sf(coords = c("newp_n", "newp_e")) %>%
    st_set_crs(3095) %>% # st_set_crs(32632)
    st_transform(4326) %>%
    st_geometry %>% 
    unlist %>% 
    matrix(ncol=2, byrow=T) %>% 
    as.data.frame
  dat$lon.d <- pts %>% select(V1) %>% unlist 
  dat$lat.d <- pts %>% select(V2) %>% unlist 
  return(dat %>% as.data.frame) # %>% select(-dist_km, -heading))
}

#-----------------
# Get coordinates around center or from corner
get.mesh <- function(dat, noX=15, noY=15, loc="center") {
  # use coord.at.d first, then determine coordinates for all within the mesh
  # loc = corner or center: source location

  # Warnings, Errors 
  if (loc=="center" & (noX%%2==0 | noY%%2==0)) stop("noX or noY is even. Change to odd number.")
  if (loc=="center" & (noX<7 | noY<7)) warning("noX or noY should be 7 or greater for best results.")
  if (loc=="corner" & (noX<3 | noY<3)) warning("noX or noY should be 3 or greater.")
  if (loc=="corner" & (noX>16 | noY>16)) stop("noX or noY is too large. Maximum 16.")
  
  
  dat %<>% slice(rep(1:n(), each = noX*noY)) %>% cbind(expand.grid(0:(noX-1),0:(noY-1)))
  dat$x.i <- dat$Var1 %>% divide_by(noX-1)
  dat$y.j <- dat$Var2 %>% divide_by(noY-1)
  dat$oldVar1 <- dat$Var1
  dat$oldVar2 <- dat$Var2
  if (loc=="center") { # shift cell points to center, shift heading, change d?
    dat$Var1 %<>% subtract((noX-1)/2)
    dat$Var2 %<>% subtract((noY-1)/2)
    dat$x.i %<>% subtract((noX-1)/2/(noX-1))
    dat$y.j %<>% subtract((noY-1)/2/(noY-1))
  }
  
  # make everything positive (temporarily)
  # lat.sign <- ifelse(dat$lat[1]>0, 1, -1); lon.sign <- ifelse(dat$lon[1]>0, 1, -1)
  # dat$lat.d %<>% abs; dat$lon.d %<>% abs; dat$lat %<>% abs; dat$lon %<>% abs
  
  dat$lat.d <- dat$lat+dat$y.j*(dat$lat.d-dat$lat)
  dat$lon.d <- dat$lon+dat$x.i*(dat$lon.d-dat$lon)
  dat$r <- sqrt(dat$x.i^2+dat$y.j^2)/sqrt(2)
  dat$a <- atan((dat$x.i)/(dat$y.j)) %>% multiply_by(180) %>% divide_by(pi)
  dat$a[dat$r==0] <- 0
  dat$a[dat$y.j<0] <- dat$a[dat$y.j<0] + 180 # adjust angles, if y<0
  dat$a[dat$x.i<0 & dat$y.j>=0] <- dat$a[dat$x.i<0 & dat$y.j>=0] + 360 # adjust angles, if x<0, y>0
  dat$a[dat$a>180] <- dat$a[dat$a>180] - 360 # adjust angles to match those of at123
  
  # change sign back 
  # dat$lon %<>% multiply_by(lon.sign)
  # dat$lat %<>% multiply_by(lat.sign)
  # dat$lon.d %<>% multiply_by(lon.sign)
  # dat$lat.d %<>% multiply_by(lat.sign)
  
  dat %<>% mutate(dist_km=r, heading=a) %>% select(-r,-a,-x.i,-y.j) 
  return(dat)
}

#-----------------
# Grab outer coordinates along mesh (for getting altitude there) + source
get.edge <- function(dat) return(dat %>% filter(Var1 %in% (dat$Var1 %>% range) | Var2 %in% (dat$Var2 %>% range)))
get.edge.source <- function(dat) {
  out <- bind_rows(dat %>% filter(Var1==0, Var2==0),
                   dat %>% filter(Var1 %in% (dat$Var1 %>% range) | Var2 %in% (dat$Var2 %>% range)))
  return(out)
}

#----------------------------
# Get elevation of each point 
get.elevation <- function(dat,z=14) {
  dat$elevation <- dat %>% select(lon.d,lat.d) %>% get_elev_point(prj = "EPSG:4326", src="aws",z=z) %>% as.data.frame %>% select(elevation) %>% unlist %>% suppressMessages()
  return(dat)
}


#----------------------------------------
# Get "best" angle from source to edge, based on edges of grid 
get.best.angle <- function(dat0) { #}, map=F) {
  out <- list()
  elev0 <- dat0$elevation[1]
  elev <- dat0$elevation
  
  i <- which((elev - elev0) == min(elev - elev0))
  if (length(i)==1 & 1 %in% i) warning("Source is lowest elevation. Angle may not be accurate.") # stop("Source is lowest elevation. No angle estimated.")
  s <- dat0[i,]
  out$dat0 <- dat0
  out$n.same.elevation.drop <- nrow(s)
  out$n.elevation.all <- nrow(dat0)
  out$p.same.elevation.drop <- 100*nrow(s)/nrow(dat0)
  
  the.mean <- dat0[i,] %>% apply(2,mean) %>% t %>%  as.data.frame
  the.mean$heading <- atan(the.mean$Var1/the.mean$Var2) %>% multiply_by(180) %>% divide_by(pi)
  the.mean$heading[the.mean$Var2<0] <- the.mean$heading[the.mean$Var2<0] + 180 # adjust angles, if y<0
  the.mean$heading[the.mean$heading>180] <- the.mean$heading[the.mean$heading>180] -360 # adjust angles to match those of at123
  out$source.elevation <- elev0
  out$elevation.difference.max <- abs(min(elev - elev0))
  out$mean <- the.mean
  return(out) # return values and do not plot
}

#----------------------------------------
# Plot on map "best" angle from source to edge, based on edges of grid 
plot.best.angle <- function(dat0) {
  dat0 <- dat0$dat0
  elev0 <- dat0$elevation[1]
  elev <- dat0$elevation
  i <- which((elev - elev0) == min(elev - elev0))
  if (length(i)==1 & 1 %in% i) stop("Source is lowest elevation. No angle estimated.")
  s <- dat0[i,]
  the.mean <- dat0[i,] %>% apply(2,mean) %>% t %>%  as.data.frame
  the.mean$heading <- atan(the.mean$Var1/the.mean$Var2) %>% multiply_by(180) %>% divide_by(pi)
  the.mean$heading[the.mean$Var2<0] <- the.mean$heading[the.mean$Var2<0] + 180 # adjust angles, if y<0
  the.mean$heading[the.mean$heading>180] <- the.mean$heading[the.mean$heading>180] -360 # adjust angles to match those of at123
  dat0 %>% leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.EPSG3857"))) %>% addTiles() %>%
    addCircleMarkers(lng = ~lon[1], lat = ~lat[1], col="black", opacity=1, radius=3) %>% # source
    addCircleMarkers(lng = ~lon.d, lat = ~lat.d, col="blue") %>% # points 
    addCircleMarkers(lng = ~lon.d[i], lat = ~lat.d[i], col="red",opacity = 1) %>% # all possibilities
    addCircleMarkers(lng = the.mean$lon.d, lat = the.mean$lat.d, col="green",opacity = 1) # mean
}


# Example usage

# Make data set of coordinates
dat <- data.table(c(135.47962531686105, # by mountain
                    135.5897576591972, # by river
                    138.72754031015168, # Mt Fuji
                    136.235941185112), # random
                  c(34.839310698220366,
                    34.78644090606354,
                    35.36142754740843,
                    35.9999)) 

dat %>% coord.at.d

# Make zoom level a parameter in UI or set as 10
dat %>% slice(4) %>% coord.at.d %>% get.mesh %>% get.edge.source %>% get.elevation(z=9) %>% get.best.angle %>% plot.best.angle
# or
dat %>% slice(4) %>% coord.at.d %>% get.mesh %>% get.elevation(z=9) %>% get.best.angle %>% plot.best.angle
 
# For slice 2, z=14, it gets an angle that goes opposite of river. Is this right? 
# for z=5, it goes a different way. 
# Need to find the best z. 
out <- data.frame(heading=NA, elev.diff.max=NA, source.elev=NA,p=NA)
for ( i in 1:14) {
tmp <- dat %>% slice(3) %>% coord.at.d %>% get.mesh %>% get.edge.source %>% get.elevation(z=i) %>% get.best.angle 
out[i,] <- c(tmp$mean$heading,tmp$elevation.difference.max,tmp$source.elevation,tmp$p.same.elevation.drop)
}
out_edge <- out

# What if we don't look at edges and instead use all points 
out <- data.frame(heading=NA, elev.diff.max=NA, source.elev=NA,p=NA)
for ( i in 1:14) {
  tmp <- dat %>% slice(3) %>% coord.at.d %>% get.mesh %>% get.elevation(z=i) %>% get.best.angle 
  out[i,] <- c(tmp$mean$heading,tmp$elevation.difference.max,tmp$source.elevation,tmp$p.same.elevation.drop)
}
out_all <- out

# out_edge
# out_all

#z= for slice 4, z=11-14
#z= for slice 3 # source is lowest elevation at 
#z=12 for slice 1
#z=12 for slice 2, on edge

# After testing, z=14 is best, faster to do on all mesh (not just edges)

tmp <- dat %>% slice(4) %>% coord.at.d %>% get.mesh %>% get.elevation(z=14) %>% get.best.angle 

# origin
tmp %>% unlist %>% t %>% data.frame %>% select(mean.lon,mean.lat)

# hydraulic gradient is unitless -> Give warning when flat area: max is 0 for more than half points 
tmp %>% unlist %>% t %>% data.frame %>% select(p.same.elevation.drop) 
# angle of heading based on altitude
tmp %>% unlist %>% t %>% data.frame %>% select(mean.heading) 


# Convert indices to lon/lat
tmp <- dat %>% slice(4) %>% coord.at.d(d=sqrt(530^2+530^2)/1000,a=-81.87) %>% get.mesh(noX=16,noY=16,loc="corner") 
tmp$oldVar1 %<>% add(1)
tmp$oldVar2 %<>% add(1)
lab1 <- paste0(tmp$oldVar1,",",tmp$oldVar2)
cnt <- read.csv("TMDU_CONTOUR.csv") %>% as_tibble
lab2 <- paste0(cnt$LONGITUDE,",",cnt$LATITUDE)
j <- NULL
for (i in 1:nrow(cnt)) j[i] <- which(lab2[i]==lab1)
cnt$LONGITUDE <- tmp$lng.d[j]
cnt$LATITUDE <- tmp$lat.d[j]


# JUST FIX TWO

# Now that we have all that we need to calculate at123, input them into the function and run 
# make sure HGRAD is the right unit

## make source the centre of mesh 
## get altitude of given set of locations 
## get direction based on altitude of all edge points of grid from center point. 
## take difference from center
## if all differences are negative, make difference zero (if source is lowest) -> source is center and it spreads out
## if one difference is positive and largest, choose that as direction 
# recalculate coordinates of rotated mesh? or rotate mesh x degrees and adjust all grid points accordingly (is there a function?) 
# run at123 

# for quick solution, choose largest difference comparing center and 4 corners. Then take the quadrant. Get angle. Get at123

# Archive code
# iii <- 1
# dat0 <- dat %>%   
#   slice(iii) %>%
#   coord.at.d(2) %>% 
#   get.mesh
# 
# # see on resulting mesh on leaflet map and outer edge
# dat1 <- dat0 %>% get.edge
# dat0 %>% leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.EPSG3857"))) %>% addTiles() %>%
#   addCircleMarkers(lng = dat1$lon.d, lat = dat1$lat.d, col="blue", opacity=1) %>%
#   addCircleMarkers(lng = ~lon.d, lat = ~lat.d, col="red") %>%
#   addMarkers(lng = ~lon, lat = ~lat)
# 
# # Plot them grid and angles
# par(pty="s")
# dat0[,c("lon.d","lat.d")] %>% plot(pch=16,col=3)
# abline(h=dat0$lat.d,col=2); abline(v=dat0$lon.d,col=2)
# dat0 %>% get.edge.source %>% select(c("lon.d","lat.d")) %>% points(pch=15,cex=3,col="black")
# dat0 %>% filter(Var1==0, Var2==0) %>% select(c("lon.d","lat.d")) %>% points(col="orange", pch=16, cex=2)
# text(dat0$lon.d,dat0$lat.d, dat0$heading %>% round, cex=0.8,col="magenta")
# 
# 
# # Example of usage 
# iii <- 1
# ex <- dat %>%   
#   slice(iii) %>% 
#   coord.at.d(2) %>% # whatever sqrt(2)*L is... so if L=1000m, d=1.4
#   get.mesh %>% 
#   get.edge.source %>% 
#   get.elevation %>% 
#   get.best.angle
# ex %>% 
#   plot.best.angle
# ex %>% 
#   print


# Don't need 
# rotate.mesh <- function (xy, angle,orig) {
#   xy <- as.matrix(xy)
#   xy[,1] %<>% subtract(orig[,1]) # ???
#   xy[,2] %<>% subtract(orig[,2])
#   angle %<>% multiply_by(pi) %>% divide_by(180)
#   theta <- angle
#   # angle[angle==pi] <- (-pi)
#   theta[angle>0 & angle<=(pi/2)] <- pi/2-angle[angle>0 & angle<=(pi/2)]
#   theta[angle>(pi/2) & angle<pi] <- 5*pi/2-angle[angle>(pi/2) & angle<pi]
#   theta[angle>=(-pi) & angle<(-pi/2)] <- pi+abs(angle[angle>=(-pi) & angle<(-pi/2)])
#   theta[angle>=(-pi/2) & angle<0] <- pi/2+abs(angle[angle>=(-pi) & angle<0])
#   sin.angle <- sin(theta)
#   cos.angle <- cos(theta)
#   xy.rot <- xy %*% t(matrix(c(cos.angle, -sin.angle, sin.angle, cos.angle), 2, 2))
#   xy.rot[,1] %<>% add(orig[,1])
#   xy.rot[,2] %<>% add(orig[,2])
#   return(xy.rot)
# }
# 
# p <- dat %>%   
#   slice(3) %>% 
#   coord.at.d(2) %>% # whatever sqrt(2)*L is... so if L=1000m, d=1.4
#   get.mesh(7,7) 
# 
# par(mfrow=c(1,1), pty="s")
# # 90 -> good
# plot(p %>% select(lon.d,lat.d), xlim=c(138.7,138.75),ylim=c(35.35,35.4), cex=2); r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=90); points(r,pch=16, col=2, cex=1)
# # 89 -> good
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=91); points(r,pch=16, col=3, cex=1)
# # 91 -> good
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=89); points(r,pch=16, col=4, cex=1)
# 
# 
# # 180
# plot(p %>% select(lon.d,lat.d), xlim=c(138.7,138.75),ylim=c(35.35,35.4), cex=2); r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=180); points(r,pch=16, col=2, cex=1)
# # 179 -> wrong
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=179); points(r,pch=16, col=3, cex=1)
# # 181 -> good
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=181); points(r,pch=16, col=4, cex=1)
# 
# 
# # -90 -> good
# plot(p %>% select(lon.d,lat.d), xlim=c(138.7,138.75),ylim=c(35.35,35.4), cex=2); r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=-90); points(r,pch=16, col=2, cex=1)
# # -89 -> good
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=-89); points(r,pch=16, col=3, cex=1)
# # -91 -> wrong
# r <- rotate.mesh(xy = p %>% select(lon.d,lat.d), orig = p %>% select(lon,lat), angle=-91); points(r,pch=16, col=4, cex=1)
