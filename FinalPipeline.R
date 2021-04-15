## R Script for creating a datasheet based on annotations
## Project: Kadaster Zeehonden 2021
## Created by Anne Grundlehner (anne.grundlehner@wur.nl)

# final pipeline to create datasheet


#### Install packages ####
library(raster)
library(readr)
library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(spdep)
library(plotrix)
library(spatstat)
library(simukde)
library(maptools)

#### Import annotations ####

#Richel, images:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Richel_annotaties_7/2019_136500_589500_rgb_hrl")
image1 = readOGR("annotations_19_136500_589500.geojson")
#filename1 = "2019_136500_589500_rgb_hrl"
coastline1=readOGR("coastline_19_136500_589500.geojson")
#region = "richel"
#resolution = "7.5 cm"
#species = "pv"

#### Basic info + Prepare ####
# 1) Merge all files from a particular region
annotations = image1 # + image2 + image3 + image4
plot(annotations, axes=T) # check
nf <- spTransform(annotations, CRS("+init=epsg:27700")) #set proper crs
coastline <- spTransform(coastline1, CRS("+init=epsg:27700"))

#### RANDOMISATIE ####
# (1) Kernel density plot
extent.nf = extent(coastline) ## better use extent of coastline
W = owin(c(extent.nf@xmin, extent.nf@xmax), c(extent.nf@ymin, extent.nf@ymax)) # set window
centroids.nf = (coordinates(nf)) # create ppp object using dataframe of seal center points
ppp.nf = as.ppp(centroids.nf, W=W)
#Bandwidth
#bw.weighedavg = (3.555339*n.anno.noorderhaaks+3.884186*n.anno.goedereede+2.95668*n.anno.dollard+2.192761*n.anno.renesse+2.691878*n.anno.simon+3.106699*n.anno.rotoog + 3.453626*n.anno.rotplaat+3.453626*n.anno.richel +1.110375*n.anno.rif + 2.682958*n.anno.engelhoek)/(n.anno.noorderhaaks+n.anno.goedereede+n.anno.dollard+n.anno.renesse+n.anno.simon+n.anno.rotoog +n.anno.rotplaat+n.anno.richel +n.anno.rif +n.anno.engelhoek)
bw.weighedavg = 3.060091
bandwidth.x = bw.weighedavg
bandwidth.y = bw.weighedavg
k.density = spatstat.core::densityfun(ppp.nf, kernel="gaussian", sigma=bw.weighedavg) #you can add sigma=...
# plot
plot(k.density, axes=T, cex.axis=0.8, las = 1)
plot(coastline, add=T, col="white")

# (1b) Create plot with smaller window (bbox)
# extent = bbox extent + 10 m
bbox.nf2 = extent(bbox(nf)) + 10
extent.nf.b = extent(bbox.nf2)
W.box = owin(c(extent.nf.b@xmin, extent.nf.b@xmax), c(extent.nf.b@ymin, extent.nf.b@ymax))
ppp.nf.bbox = as.ppp(centroids.nf, W=W.box)
k.density.bbox = density(ppp.nf.bbox, sigma=bw.weighedavg)
# plot
plot(k.density.bbox, axes=T, cex.axis=0.8, las = 1, main="Kernel density bw=5")
plot(coastline, add=T, col="white")

#### Kernel density sampling ####
# x en y seperately samplen, both from the region around the same datapoint
plot(centroids.nf) #original datapoints
N = length(nf) #number of simulations
centroids.x = centroids.nf[,1] #subset x
centroids.y = centroids.nf[,2] #subset y
#hist(centroids.x, prob=T, breaks = 50)
#lines(density(centroids.x), col="blue") #plot density x
#hist(centroids.y, prob=T, breaks=50)
#lines(density(centroids.y), col="blue") #plot density y


#### SAMPLING  KDE ####
## draw new point in the original point it's OWN KERNEL
# SD = BW (use result bw.diggle, use 1 value for all regions), normal distr. 
x = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.x = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  x[i,] <- centroids.x[i] # randomly sample one of the centroids
  sam.x[i,] <- rnorm(1, x[i,], sd=bandwidth.x) #pick a random location on it's kernel
}
y = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.y = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  y[i,] <- centroids.y[i] # randomly sample one of the centroids
  sam.y[i,] <- rnorm(1, y[i,], sd=bandwidth.y) #pick a random location on it's kernel
}
#plot the sampled points
plot(centroids.nf, main="bw=bw")
points(sam.x, sam.y, col="blue")

#new points
rpoints.kde = cbind(sam.x, sam.y)

#### Move Polgyons ####
#basic loop for shifting:
#polygons.shifted = nf[0,]
#for (i in 1:length(nf)){
#      polygons.shifted = bind(polygons.shifted, elide(nf[i,], shift=c(dx[i], dy[i])))
#}
#plot(polygons.shifted)
#plot(nf, add=T, border="grey")

# Creating test poly's
#test1 = st_as_sf(poly1) #transform objects you have so far to sf
#test2 = st_as_sf(poly2)
#test1.c = st_set_crs(test1, value=st_crs(test2))

# Create function for rotation (WORKS)
degrees = c(seq(10,360, by=10))
# if...intersect
repeat.rot.fun <- function(poly1, test2, degrees) {
      poly = poly1 #start with original poly1
      counter=0
      repeat {
        #do_something(); rotate
        for (j in 1:length(degrees)) {
          poly1.moved = elide(poly, shift=c(0,0), rotate = degrees[j],
                              center=coordinates(poly)) ## LET OP
          test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
          test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2))
          poly = poly1.moved #new round should be done with the moved poly
          # exit if the condition is met or if all 360 degrees were attempted (it will automatically after for-loop ends)
          if (st_intersects(test1.c.moved, test2, sparse=F) == FALSE) break}
        if (st_intersects(test1.c.moved, test2, sparse=F) == FALSE) break # nog een break om uit de REPEAT te komen
     }
    final.rot.poly = poly
    return(final.rot.poly)
}
    
# Create function for movement with 10cm steps until no intersection (WORKS)
dir = c(-1, 1) #pick constant direction (independent for each polygon)
degrees = c(seq(10,360, by=10))
dirx = sample(dir,size=1, replace=T) # set a fixed direction + or - for x
diry = sample(dir,size=1, replace=T) # set direction +- for y
repeat.move.fun <- function(poly1, test2, dirx, diry) {
      poly = poly1 #start with original poly1
      counter=0
      repeat {
        #poly = poly1.moved #new round should be done with the moved poly
        #do_something(); MOVE with steps of 10 cm
        poly1.moved = elide(poly, shift=c(dirx*0.1, diry*0.1), rotate = 0,
                            center=coordinates(poly)) ## LET OP
        test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
        test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2))
        poly = poly1.moved #new round should be done with the moved poly
        # exit if the condition is met:
        if (st_intersects(test1.c.moved, test2, sparse=F) == FALSE) break}
      final.poly = poly
      return(final.poly)
}
    

#### final movement loop ####
# Before looping set parameters
centroids.nf = coordinates(nf) #center points of our annoated polygons
dx =  coordinates(rpoints.kde)[,1] - centroids.nf[,1]
dy =  coordinates(rpoints.kde)[,2] - centroids.nf[,2]
polygons.shifted = nf[0,]
dir = c(-1, 1) #pick constant direction

## THIS LOOP NEEDS TO BE FIXED:
for (i in 1:length(nf)){
  focal.poly = elide(nf[i,], shift=c(dx[i], dy[i])) #move next poly
  #create variables for checking/avoiding intersection:
  for (k in 1:length(polygons.shifted)){ #intersection should be tested with each already moved polygon seperately
  poly1 = polygons.shifted[k,] #maybe [,] or [k-1] ? #already fixed polygons: polygons.shifted[-i,]
  poly2 = focal.poly[i] #polygons.shifted[i,]
  test1 = st_as_sf(poly1)
  test2 = st_as_sf(poly2)
  test2.c = st_set_crs(test2, value=st_crs(test1))  #used to be test1.c=st_set_crs(test1, value=st_crs(test2)) and all test2.c (below) set to test2 and all test1 set to test1.c, switch again if you get this error: "st_crs(x) == st_crs(y) is not TRUE"
  #move through if() loops
    if(st_intersects(test1, test2.c, sparse=F) == TRUE){
      focal.poly = repeat.rot.fun(poly1, test2.c, degrees) #returns only moved/focal object
    }
    if (st_intersects(test1, test2.c, sparse=F) == TRUE){
      dirx = sample(dir, size=1, replace=T) # set a fixed direction + or - for x
      diry = sample(dir, size=1, replace=T) # set direction +- for y
      focal.poly = repeat.rot.fun(poly1, test2.c, dirx, diry) #returns only the moved/focal object
    }}
  #bind new polygon to the others:
  polygons.shifted = bind(polygons.shifted, focal.poly) #bind old & new poly's
}




## ----------------------------------------
#### (3) Shift polygons ####
    #basic loop that works:
#polygons.shifted = nf[0,]
#for (i in 1:length(nf)){
#      polygons.shifted = bind(polygons.shifted, elide(nf[i,], shift=c(dx[i], dy[i])))
#}

#### Rotate & shift untill no intersect ####
# similar loop, but preventing overlap ##draft!!
  # Check intersection
  # rotate & check, rotate & check, .. until correct
  # check
  # move & check, move & check, until correct
#polygons.shifted = nf[0,]
    
# Zo zou het er ongeveer uit moeten zien in grove lijnen:
#for (i in 1:length(nf)){
     # polygons.shifted = bind(polygons.shifted, elide(nf[i,], shift=c(dx[i], dy[i]))).
    #  if(intersect(polygons.shifted) == T) #intersect? eerst roteren, niet verplaatsen
    #  polygons.shifted = bind(polygons.shifted, elide(nf[i,], shift=c(0,0), theta =180 ))
    #  if() #nog steeds intersect? random verplaatsen tussen -x en x, uniforme 
    #  polygons.shifted = bind(polygons.shifted, elide(nf[i,], shift=c(runif(1, min = -10, max = 10), runif(1, min = -10, max = 10))))
} ## voorhet roteren en moven "until intersect=F" kunnen we repeat functies schrijven


# ----------------------------------------
#Create functions to repeat rotation and movement until no interesection
#insert these functions into the "shift polygons" for-loop 


#### TEST STUKJES CODE DIE MOETEN WERKEN, GEBRUIK 2 TEST POLY's ####

#create 2 simple test poly's
poly.test = nf[4:5,]
plot(poly.test, axes = T)
## 1 Test rotation loop, rotate around centerpoint - WORKS
poly.test.rotate=poly.test
poly.test.rotate = elide(poly.test[1,], shift=c(0,0), rotate = 90,
                        center=coordinates(poly.test[1,])) ## LET OP 
plot(poly.test, axes=T)
plot(poly.test.rotate, add=T, border="red")

## loop ROTATE 90 degr "IF INTERSECT" - WORKS
test1 = st_as_sf(poly1) #transform objects you have so far to sf to be able to check if they intersect
test2 = st_as_sf(poly2)
test1.c = st_set_crs(test1, value=st_crs(test2))

#st_intersects(test1.c, test2, sparse=F) #line that checks if there is an intersection

if(st_intersects(test1.c, test2, sparse=F) == TRUE)
  {  #do_something(); #rotate 
  poly1.rot = elide(poly1, shift=c(0,0), rotate = 90, # als ze overlappen, 1x kwartslag draaien
        center=coordinates(poly1)) ## LET OP
}
#check if it worked:
plot(poly1.rot, axes=T) #WORKS

## move 10 cm if intersects WORKS
if(st_intersects(test1.c, test2, sparse=F) == TRUE)
{  #do_something(); #rotate 
  poly1.move = elide(poly1, shift=c(0.1, 0.1), rotate = 0,
                    center=coordinates(poly1)) ## LET OP
}

# 2 LOOP move if with 10 cm IF intersects WORKS --> we need to fix choice between -x / x, -y, y
dir = c(-1, 1) #create vector with directions
if(st_intersects(test1.c, test2, sparse=F) == TRUE)
{  #do_something(); #MOVE 
  dirx = sample(dir,size=1, replace=T) #  direction +- for x
  diry = sample(dir,size=1, replace=T) # set direction +- for y
  # alternetively to runif, you could just do dx/dy * 10 cm to take constant steps
  poly1.move = elide(poly1, shift=c(dirx*runif(1, min=0, max=0.1),diry*runif(1, min=0, max=0.1)), rotate = 0,
                     center=coordinates(poly1)) ## LET OP
}

# plot to check the result
plot(poly1, axes=T)
plot(poly1, axes=T)
plot(poly1.move, add=T, border="red")

# ------->>> HERE FOLLOWS THE PROBLEM
#### REPEAT function for moving if intersect - move #### 
## Rotate if intersect TEST
## create 2 intersecting polys:
#gDistance(poly.test[1,], poly.test[2,]) #check distance
overlap.test.polys = poly.test
overlap.test.poly1 = elide(poly.test[1,], shift=c(5.5, 0)) #shift 1 poly so that it overlaps the other
poly1 = elide(poly.test[1,], shift=c(5.5, 0), center=coordinates(poly.test[1,]))
poly2 = poly.test[2,]
#check if you have 2 overlapping polys now for the test
plot(poly1, axes=T)
plot(poly2, add=T, border="red")

test1 = st_as_sf(poly1) #transform objects you have so far to sf, to be able to check intersection
test2 = st_as_sf(poly2)
test1.c = st_set_crs(test1, value=st_crs(test2))
st_intersects(test1.c, test2, sparse=F) #test if they intersect

# .. if(st_intersects(test1.c, test2, sparse=F) == TRUE) .. 
dir = c(-1, 1) #pick constant direction
dirx = sample(dir,size=1, replace=T) # set a fixed direction + or - for x
diry = sample(dir,size=1, replace=T) # set direction +- for y

# put in loop with if intersect test1.c test2 ...
test2 = st_as_sf(poly2)
repeat.move.loop <- function(poly1, test2, dirx, diry) {
  poly = poly1 #start with original poly1
  repeat {
    poly = poly1.moved #new round should be done with the moved poly
    #do_something(); MOVE with steps of 10 cm
      poly1.moved = elide(poly, shift=c(dirx*0.1, diry*0.1), rotate = 0,
                         center=coordinates(poly)) ## LET OP
      test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
      test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2)) 
      poly = poly1.moved #new round should be done with the moved poly
      # exit if the condition is met:
    if (st_intersects(test1.c.moved, test2, sparse=F) == FALSE) break
  final.poly = poly
  return(final.poly)
  }
}
poly = repeat.move.loop(poly1, test2, dirx, diry)

#CHECK:
plot(poly, border="red") #moved polygon
plot(poly1, add=T) #original location
plot(poly2, add=T) ## polygon intersected - Nog steeds intersection??


#### REPEAT LOOP 2 - rotate NEEDS EDITS ####
degrees = c(seq(10,360, by=10))
repeat.rot.loop <- function(x) {
  repeat {
    #do something: rotate 
    for (j in 1:length(degrees)){
    poly1.rot = elide(poly1, shift=c(0,0), rotate = degrees[j],
                        center=coordinates(poly1))
    }
    # exit if the condition is met:
    if (st_intersects(poly1.rot, poly2, sparse=F) == FALSE) break
    if (j == length(degrees)) break
  }
  return(poly.rot)
}



# draft codes & tests / fragments of the code ---------------------------------------------------------
poly1.moved = elide(poly1, shift=c(dirx*runif(1, min=0, max=0.1),diry*runif(1, min=0, max=0.1)), rotate = 0,
                    center=coordinates(poly1)) ## LET OP
test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
test2 = st_as_sf(poly2)
test1.c = st_set_crs(test1.moved, value=st_crs(test2))
plot(poly1.moved)

# ===> use this created function in loop:
if(st_intersects(poly.sf[i,], poly.sf[-i,], sparse=F) == TRUE) #if they intersect
{  #do_something(); #move randomly with max 10 cm steps
  repeat.move.loop(poly.sf[i])
}



# -------------


#### (4) Calculate NNDs ####
#dist mat
nnd.kde = nnd.kde
#### (5) add to datasheet ####
datasheet5 = cbind(datasheet4, nnd.kde, nnd.chull, nnd.unif)
    
#### (6) Histogram  ####
# can also be done in statistics script
# to compare random vs real data


### removed script
#### To do ####
# Radius uitschrijven
# correctie voor pups verwerken
# ?? radius info voor rndm points?
## plaatjes saven
# histogrammen maken -- OF in statistiek scriptje

#Rottumeroog, images:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Rottumeroog_annotaties_7/2019_236250_617250_rgb_hrl/2019_236250_617250_rgb_hrl")
image1 = readOGR("annotations_19_236250_617250.geojson")
filename1= "2019_236250_617250_rgb_hrl"
coastline1= readOGR("coastline1.geojson")
region = "Rottumeroog"
resolution = "7.5 cm"
species = "pv"
#date = "image_date"

#Rif(10), images:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Rif_annotaties_10/2019_199500_609750_rgb_hrl")
image1 = readOGR("annotations_19_199500_609750.geojson")
filename1="2019_199500_609750_rgb_hrl"
coastline1= readOGR("coastline1.geojson")
region = "Rif"
resolution = "10 cm"
species = "species"
#date = "image_date"

#Rottumerplaat, images:2, coastline files:1
image1=
filename1=
image2=
filename2=
image2 = 
coastline1=
region = "rottumerplaat"
resolution = "7.5 cm"
species = "pv"
#date = "image_date"
#Simonszand, images:3, coastline files:1
#region = "simonszand"
#resolution = "7.5 cm"
#species = "pv"
#date = "image_date"
#Renesse, images:2, coastline files:1
#region = "renesse"
#resolution = "7.5 cm"
#species = "species"
#date = "image_date"
#Goedereede(10), images:4, coastline files:1
#region = "goedereede"
#resolution = "10 cm"
#species = "species"
#date = "image_date"
#Engelschehoek, images:4, coastline files:1
#region = "engelsche_hoek"
#resolution = "10 cm"
#species = "hg"
#date = "image_date"
#Eemsdollard (10), images:5, coastline files:1
#region = "eemsdollard"
#resolution = "10 cm"
#species = "pv"
#date = "image_date"
#Noorderhaaks(10), images:8, coastline files: 1
#region = "noorderhaaks"
#resolution = "10 cm"
#species = "species"
#date = "image_date"

# Basic info !!!! EDIT MANUALLY WHEN RUNNING NEXT REGION
year = "2019"

indiv.in.region = length(annotations)
sealID.per.image = rbind(
  (0:(length(image1)-1)) ## seal 1 has ID 0 (because this matches codes in NND)
  #,(0:(length(image2)-1))
  #,(0:(length(image3)-1))
)
image.of.region = rbind(
  rep("image1", times =length(image1))
  #,rep("image2", times=length(image2))
  #,rep("image3", times=length(image3))
  ## ADD MORE IF NECESSARY
)
image.filename = rbind(
  rep(filename1, times =length(image1))
  #,rep("filename2", times=length(image2))
  #,rep("filename3", times=length(image3))
  ## ADD MORE IF NECESSARY
)
category = rbind(  ### CHOOSE: Coastline, SGroups, LGroups, Dispersed
  rep("category1", times =length(image1))
  #,rep("category2", times=length(image2))
  #,rep("category", times=length(image3))
  ## ADD MORE IF NECESSARY
)

# respective sizes of the individuals
seal.surface = raster::area(annotations)
hist(seal.surface, main="Seal sizes (m2)", breaks=20)

#transpose and transform some objects to fit dataframe
#date.v = rep("date", times=length(annotations))
sealID.per.image=as.data.frame(t(sealID.per.image))
image.filename = as.data.frame(t(image.filename))
image.of.region=as.data.frame(t(image.of.region))
region.v = as.data.frame(rep(region, times=length(annotations)))
region.v = as.data.frame(rep(region, times=length(annotations)))
resolution.v = as.data.frame(rep(resolution, times=length(annotations)))
year.v = as.data.frame(rep(year, times=length(annotations)))
species.v = as.data.frame(rep(species, times=length(annotations)))
seal.surface = as.data.frame(seal.surface)
category = as.data.frame(category)
#combine in datasheet
datasheet.base = cbind(sealID.per.image, seal.surface, region.v, species.v, image.filename, resolution.v, year.v, image.of.region, category )
region.info = cbind(region, resolution, indiv.in.region)
colnames(datasheet.base) <- c("seal ID (per region)","seal surface area (m2)", "region", "species", "image filename", "resolution", "year", "imageID (per region)", "category" )

#### Distance matrix ####
# once per region
dist.mat <- gDistance(nf, byid = TRUE)
mat=dist.mat #copy
diag(mat) <- NA #set self-self distances to NA
#mat[mat==0] <- NA #remove self-self distances (0) by setting those to NA
nnd.result <- t(sapply(seq(nrow(mat)), function(i) {
  j <- which.min(mat[i,])
  c(paste(rownames(mat)[i], colnames(mat)[j], sep='/'), mat[i,j])
}))  # #sapply loop obtains the minimum values of each row and stores in

# store and add to datasheet
nnd.couple = as.data.frame(nnd.result[,1])
colnames(nnd.couple) = "nnd.couple"
nnd = as.data.frame(nnd.result[,2]) #nearest neighbour distances
colnames(nnd) = "nnd_m2"
datasheet2 = cbind(datasheet.base, nnd, nnd.couple)
colnames(datasheet.incl.nnd$V1) = "NND couple"
colnames(datasheet.incl.nnd$V2) = "NND (m)"

#### Correct for pups in cases necessary ####
## ...


#### Distance to sea ####
coastline = spTransform(coastline1, CRS("+init=epsg:27700")) # transform crs
plot(coastline, axes=T, col="lightseagreen", border="lightseagreen")
plot(nf, add=T) ## Check
dist.to.sea = gDistance(coastline, nf, byid=T) # !! use annotations with correct CRS == nf
colnames(dist.to.sea) = "distance to sea (m)"
datasheet3 = cbind(datasheet2, dist.to.sea) #add to datasheet

#### Radius ####
# Number of individuals within a radius of c(x,x,x) m around focal individual
# Optional: Occupied surface area (fraction) idem dito

r1 = 10  # Determine radius size
r2 = 25
r3 = 100
mat.radius1 = dist.mat #make copy of matrix to work with
mat.radius2 = dist.mat 
mat.radius3 = dist.mat
#(r1)
mat.radius1[mat.radius1 >= r1] <- NA #remove all values above 2r m become NA
mat.radius1[mat.radius1 <= r1] <- 1 # set all values to 1
n.seals.in.radius1 = as.data.frame(rowSums(mat.radius1, na.rm=T))
#(r2)
mat.radius2[mat.radius2 >= r2] <- NA
mat.radius2[mat.radius2 <= r2] <- 1 # set all values to 1
n.seals.in.radius2 = as.data.frame(rowSums(mat.radius2, na.rm=T))
#(r3)
mat.radius3[mat.radius3 >= r3] <- NA
mat.radius3[mat.radius3 <= r3] <- 1 # set all values to 1
n.seals.in.radius3 = as.data.frame(rowSums(mat.radius3, na.rm=T))
#collect
radius.n = cbind(n.seals.in.radius1, n.seals.in.radius2, n.seals.in.radius3)

#occupied fraction
# r1
m2.seals.within1 = (mat.radius1 * (raster::area(nf))) #sizes = m2 occupied per indiv
m2.in.radius1 = as.data.frame(rowSums(m2.seals.within1, na.rm=T)) #take the sum of each row
areafraction.of.radius1 = (m2.in.radius1) / (pi*(r1^2)) #as fraction of total area
# r2
m2.seals.within2 = (mat.radius2 * (raster::area(nf))) #sizes = m2 occupied per indiv
m2.in.radius2 = as.data.frame(rowSums(m2.seals.within2, na.rm=T)) #take the sum of each row
areafraction.of.radius2 = (m2.in.radius2) / (pi*(r2^2)) #as fraction of total area
# r3
m2.seals.within3 = (mat.radius3 * (raster::area(nf))) #sizes = m2 occupied per indiv
m2.in.radius3 = as.data.frame(rowSums(m2.seals.within3, na.rm=T)) #take the sum of each row
areafraction.of.radius3 = (m2.in.radius3) / (pi*(r3^2)) #as fraction of total area
#collect
radius.m2fraction = cbind(areafraction.of.radius1, areafraction.of.radius2, areafraction.of.radius3)
colnames(radius.n) = c("number of seals within 10m", "number of seals within 25m", "number of seals within 100m")
colnames(radius.m2fraction) = c("fraction occupied surface area within 10m", "fraction occupied surface area within 25m", "fraction occupied surface area within 100m")
radius.info = cbind(radius.n, radius.m2fraction)
datasheet4 = cbind(datasheet3, radius.info) #add to datasheet

#### 2b = random punten binnen chull ####
chull <- gConvexHull(nf)
rpoints.chull <-spsample(chull,n=length(nf),type="random") #generate random points
plot(rpoints.chull, pch=19, axes=T, cex=0.3)
    ## Plot and check if coastline overlaps with sampling space:
    plot(nf, axes=T, main="sampling space Rottumeroog")
    plot(chull, axes=T, add=TRUE, border="coral")
    plot(coastline1, add=TRUE, border="blue", col="blue")



#### RANDOMISATIE ####
# (1) Kernel density plot (large window)
extent.nf = extent(coastline) ## better use extent of coastline
W = owin(c(extent.nf@xmin, extent.nf@xmax), c(extent.nf@ymin, extent.nf@ymax)) # set window
centroids.nf = (coordinates(nf)) # create ppp object using dataframe of seal center points
ppp.nf = as.ppp(centroids.nf, W=W)
k.density = spatstat.core::densityfun(ppp.nf, kernel="gaussian", sigma=2) #you can add sigma=...
# plot
plot(k.density, axes=T, cex.axis=0.8, las = 1)
plot(coastline, add=T, col="white")

# (1b) plot with smaller window (bbox)
# extent = bbox extent + 10 m
bbox.nf2 = extent(bbox(nf)) + 10
extent.nf.b = extent(bbox.nf2)
W.box = owin(c(extent.nf.b@xmin, extent.nf.b@xmax), c(extent.nf.b@ymin, extent.nf.b@ymax))
ppp.nf.bbox = as.ppp(centroids.nf, W=W.box)
k.density.bbox = density(ppp.nf.bbox, sigma=5)
# plot
plot(k.density.bbox, axes=T, cex.axis=0.8, las = 1, main="Kernel density bw=5")
plot(coastline, add=T, col="white")


#### Kernel density sampling ####
# x en y seperately samplen, both from the region around the same datapoint

plot(centroids.nf) #original datapoints
N = length(nf) #number of simulations
centroids.x = centroids.nf[,1] #subset x
centroids.y = centroids.nf[,2] #subset y

hist(centroids.x, prob=T, breaks = 50)
lines(density(centroids.x), col="blue") #plot density x
hist(centroids.y, prob=T, breaks=50)
lines(density(centroids.y), col="blue") #plot density y
# store bandwidth >>> We need to take a constant bandwidth for all regions..
  #bandwidth.x = density(centroids.x)$bw # 4.7 
  #bandwidth.y = density(centroids.y)$bw # 5.59
# --> we need to use constant bandwidths!

# corssvalidate bandwidth -- x+y similar,  ppp.nf = ppp of centroids.nf
  # bw.diggle(ppp.nf, correction="good", hmax=NULL, nr=512, warn=TRUE)
  # sigma 3.453626 (Isotropic crossvalidated BW)
bandwidth.x = bw.diggle(ppp.nf, correction="good", hmax=NULL, nr=512, warn=TRUE)
bandwidth.y = bw.diggle(ppp.nf, correction="good", hmax=NULL, nr=512, warn=TRUE)

## Strictly speaking, given that the mixture's components are equally weighted, 
# you could avoid the sampling with replacement part and simply draw a sample a size 𝑀
#from each components of the mixture:
M = length(nf)
hist(rnorm(N * M, mean = centroids.x, sd = bandwidth.x)) # sd=bw, we want to use a constant
hist(rnorm(N * M, mean = centroids.y, sd = bandwidth.y))

#### USED SAMPLING METHOD KDE ####
## draw new point in the original point it's OWN KERNEL
# SD = BW (use result bw.diggle, use 1 value for all regions), normal distr. 
x = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.x = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  x[i,] <- centroids.x[i] # randomly sample one of the centroids
  sam.x[i,] <- rnorm(1, x[i,], sd=bandwidth.x) #pick a random location on it's kernel
}
y = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.y = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  y[i,] <- centroids.y[i] # randomly sample one of the centroids
  sam.y[i,] <- rnorm(1, y[i,], sd=bandwidth.y) #pick a random location on it's kernel
}

plot(centroids.nf, main="bw=bw")
points(sam.x, sam.y, col="blue")
    
