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
library(writexl)
library(tidyr)

#### INDEX ####
# 1. *import annotations: 
    #Only run the block belonging to 1 region, continue to step 2, run next region when finished
    #When you run a new year, correct the parameters in the block of the focal region (e.g. year, date, species etc.)
# 2. Prepare data and create a dataframe containing the basic information of the focal region
# 3. Distance matrix: NND's
# 4. Distance to sea
# 5. Radius information
# 6. Randomisation
    # 6 Create kernel density
    # 6A = overlap avoided by moving away from initial rndm location with steps of 10cm
    # 6B = overlap avoided by sampling new point on same kernel
# 7 (A & B) Calculate NND's of the randomised distribution
# 8. *Create a dataframe with all info, per region
# 9. *Merge the dataframes of all regions
# Parts marked (*) need attention / edits when running next region / shutting lines on or off
# all other parts do not need to be edited when you run next region

# How to properly run this script:

# Import data (1) with caution and properly set parameters
# Run parts 2 tm 7 (takes quite a while, especially when you have 200+ seal annotations)
# Run part 8 with caution - you only have to pick 1 line!
    # If you run the wrong line there, you might have to run a full region again .. 
# Run part 9 when you did 1 tm 8 for all the regions of interest


#### (1) Import annotations ####
## Choose 1 site, run only that block and continue to (2)

#Rottumeroog, images:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Rottumeroog_annotaties_7")
image1 = readOGR("annotations_19_236250_617250.geojson")
filename1= "2019_236250_617250_rgb_hrl"
coastline1= readOGR("coastline_rottumeroog_1.geojson")
region = "Rottumeroog"
resolution = "7.5 cm"
species = "pv"
date = "30-3-2019"
year = "2019"
# Do not change, only run:
annotations = image1
n.anno.rotoog = length(annotations)
image.filenames = data.frame(rep(filename1, times =length(image1)))

#Rif(10), images:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Rif_annotaties_10/2019_199500_609750_rgb_hrl")
image1 = readOGR("annotations_19_199500_609750.geojson")
filename1="2019_199500_609750_rgb_hrl"
coastline1= readOGR("coastline1.geojson")
region = "rif"
resolution = "10 cm"
species = "pv"
date = "30-3-2019"
year = "2019"
annotations = image1
n.anno.rif = length(annotations)
image.filenames = data.frame(rep(filename1, times =length(image1)))

#Richel, images:2
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Richel_annotaties_7")
image1 = readOGR("annotations_19_136500_589500.geojson")
image2 = readOGR("annotations_19_135750_589500.geojson")
filename1 = "2019_136500_589500_rgb_hrl"
filename2 = "2019_135750_589500_rgb_hrl"
coastline1=readOGR("coastline_richel_12.geojson")
resolution = "7.5 cm"
species = "pv"
date="24-3-2019"
year = "2019"
# Do not change, only run:
region = "richel"
annotations = image1+image2
n.anno.richel = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename" 
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename" 
image.filenames = rbind(im1, im2)

#Rottumerplaat, images:2, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Rottumerplaat_annotaties_7")
image1= readOGR("annotations19_230250_616500.geojson")
filename1= "2019_230250_616500_rgb_hrl"
image2= readOGR("annotations_19_230250_617250.geojson")
filename2= "2019_230250_617250_rgb_hrl"
# Add more if necessary
coastline1= readOGR("coastline_rottumerplaat_1_2.geojson")
resolution = "7.5 cm"
species = "pv"
year = "2019"
date = "30-3-2019"
annotations = image1+image2 # add more if necessary
# Do not change, only run:
region = "rottumerplaat"
n.anno.rotplaat = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename"
im2 = data.frame(rep(filename2, times=length(image2)))
colnames(im2) = "filename"
image.filenames = rbind(im1, im2)

#Simonszand, images:3, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Simonszand_annotaties_7")
region = "simonszand"
resolution = "7.5 cm"
species = "pv"
year = "2019"
date = "30-3-2019"
image1 = readOGR("annotations_19_222750_615000.geojson")
image2 = readOGR("annotations_19_223500_615000.geojson")
image3 = readOGR("annotations_19_223500_615750.geojson")
filename1="2019_222750_615000_rgb_hrl"
filename2 = "2019_223500_615000_rgb_hrl."
filename3 = "2019_223500_615750_rgb_hrl"
coastline1 = readOGR("coastline_simonszand_1_2_3.geojson")
annotations = image1+image2+image3
n.anno.simon = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename"
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename"
im3 = data.frame(rep(filename3, times =length(image3)))
colnames(im3) = "filename"
image.filenames = rbind(im1, im2, im3)

#Renesse, images:2, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Renesse_annotaties_7")
image1 = readOGR("annotations_19_042000_419250.geojson")
filename1 = "2019_042000_419250_rgb_hrl"
image2 = readOGR("annotations_19_042750_419250.geojson")
filename2 = "2019_042750_419250_rgb_hrl"
coastline1 = readOGR("coastline_Renesse_1_2.geojson")
region = "renesse"
resolution = "7.5 cm"
species = "species"
date = "24-2-2019"
annotations = image1+image2
n.anno.renesse = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename"
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename"
image.filenames = rbind(im1, im2)

#Goedereede(10), images:4, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Goedereede_annotaties_10")
image1 = readOGR("annotations_19_059000_435000.geojson")
image2 = readOGR("annotations_19_059000_437000.geojson")
image3 = readOGR("annotations_19_060000_435000.geojson")
image4 = readOGR("annotations_19_060000_436000.geojson")
coastline1 = readOGR("coastline_goed.geojson")
filename1 = "2019_059000_435000_rgb_hrl"
filename2 = "2019_059000_437000_rgb_hrl"
filename3 = "2019_060000_435000_rgb_hrl"
filename4 = "2019_060000_436000_rgb_hrl"
region = "goedereede"
resolution = "10 cm"
species = "pv"
date = "5-4-2019"
year = "2019"
annotations = image1+image2+image3+image4
n.anno.goedereede = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename"
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename"
im3 = data.frame(rep(filename3, times =length(image3)))
colnames(im3) = "filename"
im4 = data.frame(rep(filename4, times =length(image4)))
colnames(im4) = "filename"
image.filenames = rbind(im1, im2, im3, im4)

#Engelschehoek, images:3, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/EngelscheHoek_annotaties_7")
image1 = readOGR("annotations_19_137250_595500.geojson")
image2 = readOGR("annotations_19_137250_596250.geojson")
image3 = readOGR("annotations_19_138000_596250.geojson")
coastline1 = readOGR("coastline_engelschehoek_1234.geojson")
region = "engelsche_hoek"
resolution = "10 cm"
species = "hg"
date = "30-3-2019"
year="2019"
annotations = image1 + image2 + image3
n.anno.engelhoek = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename"
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename"
im3 = data.frame(rep(filename3, times =(length(image3)+2)))
colnames(im3) = "filename"
image.filenames = rbind(im1, im2, im3)

#Eemsdollard (10), images:5, coastline files:1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Eemsdollard_annotaties_10")
image1 = readOGR("annotations_19_268000_591000.geojson")
image2 = readOGR("annotations_19_268000_592000.geojson")
image3 = readOGR("annotations_19_269000_590000.geojson")
image4 = readOGR("annotations_19_269000_591000.geojson")
image5 = readOGR("annotations_19_269000_592000.geojson")
coastline1=readOGR("coastline_eemsdollard_12345.geojson")
filename1 = "2019_268000_591000_rgb_hrl"
filename2 = "2019_268000_592000_rgb_hrl"
filename3 = "2019_269000_590000_rgb_hrl"
filename4 = "2019_269000_591000_rgb_hrl"
filename5 = "2019_269000_592000_rgb_hrl"
region = "eemsdollard"
resolution = "10 cm"
species = "pv"
date = "23-6-2019"
year = "2019"
annotations = image1+image2+image3+image4+image5
n.anno.dollard = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
im2 = data.frame(rep(filename2, times =length(image2)))
im3 = data.frame(rep(filename3, times =length(image3)))
im4 = data.frame(rep(filename4, times =length(image4)))
im5 = data.frame(rep(filename5, times =length(image5)))

      ## if you want to remove pups, run this block: (if not continue to line 221)
      #adults = subset(nf, raster::area(nf) >= 0.2)
      #annotations = adults
      image1a = subset(image1, raster::area(image1) >= 0.2)
      image2a = subset(image2, raster::area(image2) >= 0.2)
      image3a = subset(image3, raster::area(image3) >= 0.2)
      image4a = subset(image4, raster::area(image4) >= 0.2)
      image5a = subset(image5, raster::area(image5) >= 0.2)
      annotations = image1a+image2a+image3a+image4a+image5a
      im1 = data.frame(rep(filename1, times =length(image1a)))
      im2 = data.frame(rep(filename2, times =length(image2a)))
      im3 = data.frame(rep(filename3, times =length(image3a)))
      im4 = data.frame(rep(filename4, times =length(image4a)))
      im5 = data.frame(rep(filename5, times =length(image5a)))

# continue here for dollard, in all cases (adults/pups);
colnames(im1) = "filename"
colnames(im2) = "filename"
colnames(im3) = "filename"
colnames(im4) = "filename"
colnames(im5) = "filename"
image.filenames = rbind(im1, im2, im3, im4, im5)


#Noorderhaaks(10), images:8, coastline files: 1
setwd("~/Documents/Internship WMR zeehonden/Data Analyses/Noorderhaaks_annotaties_10")
coastline1 = readOGR("coastline_noorderhaaks_1tm8.geojson")
region = "noorderhaaks"
resolution = "10 cm"
species = "species"
species2="hg"
date = "24-3-2019"
year = "2019"
image1 = readOGR("annotations_19_106000_554000.geojson")
image2 = readOGR("annotations_19_106000_555000.geojson")
image3 = readOGR("annotations_19_106000_556000.geojson")
image4 = readOGR("annotations_19_107000_553000.geojson")
image5 = readOGR("annotations_19_107000_554000.geojson")
image6 = readOGR("annotations_19_107000_556000.geojson")
image7 = readOGR("annotations_19_108000_555000.geojson")
image8 = readOGR("annotations_19_109000_555000.geojson")
filename1 = "2019_106000_554000_rgb_hrl"
filename2 = "2019_106000_555000_rgb_hrl"
filename3 = "2019_106000_556000_rgb_hrl"
filename4 = "2019_107000_553000_rgb_hrl"
filename5 = "2019_107000_554000_rgb_hrl"
filename6 = "2019_107000_556000_rgb_hrl"
filename7 = "2019_108000_555000_rgb_hrl"
filename8 = "2019_109000_555000_rgb_hrl"
annotations = image1+image2+image3+image4+image5+image6+image7+image8
n.anno.noorderhaaks = length(annotations)
im1 = data.frame(rep(filename1, times =length(image1)))
colnames(im1) = "filename" # do NOT change
im2 = data.frame(rep(filename2, times =length(image2)))
colnames(im2) = "filename"
im3 = data.frame(rep(filename3, times =length(image3)))
colnames(im3) = "filename"
im4 = data.frame(rep(filename4, times =length(image4)))
colnames(im4) = "filename"
im5 = data.frame(rep(filename5, times =length(image5)))
colnames(im5) = "filename"
im6 = data.frame(rep(filename6, times =length(image6)))
colnames(im6) = "filename"
im7 = data.frame(rep(filename7, times =length(image7)))
colnames(im7) = "filename"
im8 = data.frame(rep(filename8, times =length(image8)))
colnames(im8) = "filename"
image.filenames = rbind(im1, im2, im3, im4, im5, im6, im7, im8)




#### (2) Basic info + Prepare ####
# Continue here when correct files were imported and dates, species etc. were corrected
# Merge all info from a particular region into dataframe

# annotations =   image1  + image3 +image2 + image4 #+ image5 + image6 + image7+ image8
plot(annotations, axes=T) # check
plot(coastline1, add=T, col="lightseagreen") 
# check if the coastline and annotations match

# prepare annotations by setting correct CRS
nf <- spTransform(annotations, CRS("+init=epsg:27700")) #set proper crs to work with for annotations
coastline <- spTransform(coastline1, CRS("+init=epsg:27700")) #set proper crs to work with for coastline

# Rep Basic information:
# DO NOT CHANGE!!
indiv.in.region = length(annotations)
seal.surface = raster::area(nf) #respective sizes of the individuals within the focal region
hist(seal.surface, main="Seal sizes (m2)", breaks=20)

# transpose and transform some objects to fit dataframe:
sealID.per.site=as.data.frame((c(seq(from=1, to=length(annotations), by=1))))
image.filename = as.data.frame((image.filenames))
region.v = as.data.frame(rep(region, times=length(annotations)))
resolution.v = as.data.frame(rep(resolution, times=length(annotations)))
year.v = as.data.frame(rep(year, times=length(annotations)))
species.v = as.data.frame(rep(species, times=length(annotations)))
date.v = as.data.frame(rep(date, times=length(annotations)))
seal.surface = as.data.frame(seal.surface)
groupsize = length(annotations)

#Create basis datasheet containing all starting information
datasheet.base = cbind(sealID.per.site, seal.surface, region.v, species.v, image.filename, resolution.v, year.v, date.v)
region.info = cbind(region, resolution, year, indiv.in.region)
colnames(datasheet.base) <- c("seal ID (per site)","seal surface area (m2)", "haul-out site", "species", "image filename", "resolution", "year", "date image")




#### (3) Distance matrix ####
# once per region
dist.mat <- gDistance(nf, byid = TRUE)
mat=dist.mat #copy
diag(mat) <- NA #set self-self distances (to NA) remove diagonal
nnd.result <- t(sapply(seq(nrow(mat)), function(i) {
  j <- which.min(mat[i,])
  c(paste(rownames(mat)[i], colnames(mat)[j], sep='/'), mat[i,j])
}))  # #sapply loop obtains the minimum values of each row and stores in
# store and add to datasheet
nnd.couple = as.data.frame(nnd.result[,1])
colnames(nnd.couple) = "nnd couple"
nnd = as.data.frame(as.numeric(nnd.result[,2])) #nearest neighbour distances
colnames(nnd) = "nnd (m)"
#combine with basis datasheet
datasheet2 = cbind(datasheet.base, nnd, nnd.couple)




#### (4) Distance to sea ####
coastline = spTransform(coastline1, CRS("+init=epsg:27700")) # transform crs
plot(coastline, axes=T, col="lightseagreen", border="lightseagreen", main=region)
plot(nf, add=T) ## Check
dist.to.sea = gDistance(coastline, nf, byid=T) # !! use annotations with correct CRS == nf
colnames(dist.to.sea) = "distance to sea (m)"
datasheet3 = cbind(datasheet2, dist.to.sea) #add to datasheet


#### (5) Radius ####
# Number of individuals within a radius of c(x,x,x) m around focal individual
# Optional: Occupied surface area (fraction) idem dito

r1 = 1  # Determine radius size
r2 = 5
r3 = 10
mat.radius1 = dist.mat #make copy of matrix to work with
mat.radius2 = dist.mat 
mat.radius3 = dist.mat
#(r1)
mat.radius1[mat.radius1 >= r1] <- NA #remove all values above r m become NA
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
colnames(radius.n) = c("number of seals within 1m distance", "number of seals within 5m distance", "number of seals within 10m distance")
colnames(radius.m2fraction) = c("fraction occupied surface area within 1m radius", "fraction occupied surface area within 5m radius", "fraction occupied surface area within 10m radius")
radius.info = cbind(radius.n, radius.m2fraction)

#add to datasheet
datasheet4 = cbind(datasheet3, radius.info) 




#### (6) RANDOMISATIE ####

# (1a) Kernel density plot
extent.nf = extent(coastline) ## better use extent of coastline
W = owin(c(extent.nf@xmin, extent.nf@xmax), c(extent.nf@ymin, extent.nf@ymax)) # set window
centroids.nf = (coordinates(nf)) # create ppp object using dataframe of seal center points
ppp.nf = as.ppp(centroids.nf, W=W)
# Bandwidth; take the weighed avg --> we need to use constant bandwidths!
#bw.diggle() used to calculate crossvalidated bw per region
#bw.weighedavg = (3.555339*n.anno.noorderhaaks+3.884186*n.anno.goedereede+2.95668*n.anno.dollard+2.192761*n.anno.renesse+2.691878*n.anno.simon+3.106699*n.anno.rotoog + 3.453626*n.anno.rotplaat+3.453626*n.anno.richel +1.110375*n.anno.rif + 2.682958*n.anno.engelhoek)/(n.anno.noorderhaaks+n.anno.goedereede+n.anno.dollard+n.anno.renesse+n.anno.simon+n.anno.rotoog +n.anno.rotplaat+n.anno.richel +n.anno.rif +n.anno.engelhoek)
bw.weighedavg = 3.060091

bandwidth.x = bw.weighedavg
bandwidth.y = bw.weighedavg
k.density = spatstat.core::densityfun(ppp.nf, kernel="gaussian", sigma=bw.weighedavg) #you can add sigma=...

# plot the kernel density
plot(k.density, axes=T, cex.axis=0.8, las = 1)
plot(coastline, add=T, col="white")

# (1b) plot with smaller window (bbox)
# extent = bbox extent + 10 m
bbox.nf2 = extent(bbox(nf)) + 10
extent.nf.b = extent(bbox.nf2)
W.box = owin(c(extent.nf.b@xmin, extent.nf.b@xmax), c(extent.nf.b@ymin, extent.nf.b@ymax))
ppp.nf.bbox = as.ppp(centroids.nf, W=W.box)
k.density.bbox = density(ppp.nf.bbox, sigma=bw.weighedavg)

# plot
plot(k.density.bbox, axes=T, cex.axis=0.8, las = 1, main=region)
plot(coastline, add=T, border="white", lty=2)


#### Kernel density sampling ####
# x en y seperately samplen, both from the region around the same datapoint

# original datapoints (centre points of the seals) => (centroids.nf)
N = length(nf) #number of simulations
centroids.x = centroids.nf[,1] #subset x
centroids.y = centroids.nf[,2] #subset y
hist(centroids.x, prob=T, breaks = 50)
lines(density(centroids.x), col="blue") #plot density x
hist(centroids.y, prob=T, breaks=50)
lines(density(centroids.y), col="blue") #plot density y


#### Kernel Density Sampling ####
## draw new point in the original point it's OWN KERNEL
# SD = BW (use result bw.diggle, use 1 value for all regions), normal distr. 
# Here we sample x any y coords for the (initial) new locations of the polys

x = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.x = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  x[i,] <- centroids.x[i] #sample one of the centroids
  sam.x[i,] <- rnorm(1, x[i,], sd=bandwidth.x) #pick a random location on it's kernel
}
y = matrix(ncol=1, nrow=length(nf)) #empty vector to store sampled data-point
sam.y = matrix(ncol=1, nrow=length(nf)) #empty vector to store new coords
for(i in 1:length(nf)){
  y[i,] <- centroids.y[i] # randomly sample one of the centroids
  sam.y[i,] <- rnorm(1, y[i,], sd=bandwidth.y) #pick a random location on it's kernel
}

#plot the sampled points and old points
plot(centroids.nf, main="kde sampled points")
points(sam.x, sam.y, col="blue")

#store new points (KDE)
rpoints.kde = cbind(sam.x, sam.y)
plot(sam.x, sam.y, cex=0.5)
points(centroids.x, centroids.y, cex=0.5, add=T, col="red") 


# =======================================================================================


#### RANDOM A ####
# Use this code if you want to avoid overlap by moving with steps of 10
# Use 3B method instead if you wish to pick new point on kernel when polygons overlap


#### (6A) Shift polygons ####

# Create function for rotation until either overlap is non-existent or 0 to 360 degrees was attempted
degrees = c(seq(10,360, by=10))
repeat.rot.fun <- function(poly1, test2, degrees) {
  poly = poly1 #start with original poly1
  counter=0
  for (j in 1:length(degrees)) {
    poly1.moved = elide(poly, shift=c(0,0), rotate = degrees[j],
                        center=coordinates(poly)) ## LET OP
    test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
    test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2))
    poly = poly1.moved #new round should be done with the moved poly
    if (sum(st_intersects(test1.c.moved, test2, sparse=F)) == 0) break} 
  final.rot.poly = poly
  return(final.rot.poly)
}


# Create function for moving with steps of 10cm until either overlap is non-existent
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
    if (sum(st_intersects(test1.c.moved, test2, sparse=F))==0) break}
  final.poly = poly
  return(final.poly)
}


# Before looping set parameters
centroids.nf = coordinates(nf) #center points of our annoated polygons
dx =  coordinates(rpoints.kde)[,1] - centroids.nf[,1]
dy =  coordinates(rpoints.kde)[,2] - centroids.nf[,2]
dir = c(-1, 1) # direction for x and y should be either 1 or -1

#FINAL LOOP: move polygons to initial new location, check for overlap, if overlap exists put into rot.fun and move.fun
polygons.shifted = nf[0,]
for (i in 1:length(nf)){
  print(i)
  # Create first shifted polygon, no existing ones, so no intersect
  if (i==1) focal.poly = elide(nf[i,], shift=c(dx[i], dy[i])) 
  # For all others, shift polygon and test for intersction
  if (i!=1) {
    # Move polgyon  
    focal.poly = elide(nf[i,], shift=c(dx[i], dy[i])) 
    # Check if overlap with ALL existing polygons
    # Create st objects with same crs
    test1 = st_as_sf(focal.poly)
    test2 = st_as_sf(polygons.shifted)
    test2.c = st_set_crs(test2, value=st_crs(test1))  #used to be test1.c=st_set_crs(test1, value=st_crs(test2)) and all test2.c (below) set to test2 and all test1 set to test1.c, switch again if you get this error: "st_crs(x) == st_crs(y) is not TRUE"
    # Check if it overlaps with any of the existing polygons, if yes rotate loop  
    if(sum(st_intersects(test1, test2.c, sparse=F)) >=1){
      focal.poly = repeat.rot.fun(focal.poly, test2.c, degrees) #returns only moved/focal object
    }
    # Check if still overlaps, if yes move.  
    if (sum(st_intersects(test1, test2.c, sparse=F)) >=1){
      dirx = sample(dir, size=1, replace=T) # set a fixed direction + or - for x
      diry = sample(dir, size=1, replace=T) # set direction +- for y
      focal.poly = repeat.move.fun(focal.poly, test2.c, dirx, diry) #returns only the moved/focal object
    }
  }
  # bind new polygon to the others:
  polygons.shifted = bind(polygons.shifted, focal.poly) #bind old & new poly's
}

#Check and plot the result (lightgrey = original data, black = randomized distribution)
w.bbox = bbox(nf)
plot(nf, axes=T, border="lightgrey", xlim = c((w.bbox[1,1]-3), (w.bbox[1,2]+3)), ylim = c((w.bbox[2,1]-5), (w.bbox[2,2]+5)), 
     las=1, cex.axis=0.8, main=region)
plot(polygons.shifted, add=T)
plot(coastline, add=TRUE, border="lightseagreen")

#### (7A) Calculate rndm NNDs ####
##Distance matrix RNDM DATA 
mat2 <- gDistance(polygons.shifted, byid = TRUE)
diag(mat2) <- NA #set self-self distances (to NA) remove diagonal
result.nnd.kde <- t(sapply(seq(nrow(mat2)), function(i) {
  j <- which.min(mat2[i,])
  c(paste(rownames(mat2)[i], colnames(mat2)[j], sep='/'), mat2[i,j])
}))  # #sapply loop obtains the minimum values of each row and stores in

# store
nnd.couple.kde = as.data.frame(result.nnd.kde[,1])
colnames(nnd.couple.kde) = "NND rndm KDE couple"
nnd.kde = as.data.frame(as.numeric(result.nnd.kde[,2])) #nearest neighbour distances
colnames(nnd.kde) = "NND rndm KDE (m)"
datasheet.rndm.a = cbind(nnd.kde, nnd.couple.kde)


## ===============================================

#### (6B) Shift polygons ####

## move loop with new points instead of steps of 10 cm 
# sample again from the same  kernel, try rotations if there is overlap, if overlap persists, sample new point from kernel .. repeat until location without overlap is found
degrees = c(seq(10,360, by=10))
repeat.move.fun2 <- function(poly1, test2, x, y, bw.weighedavg, degrees) {
  poly = poly1 #start with original poly1
  counter=0
  repeat {
    sam.y <- rnorm(1, y[i,], sd=bw.weighedavg) # Sample again from the kernel
    sam.x<- rnorm(1, x[i,], sd=bw.weighedavg) #pick a random location on it's kernel
    dx =  sam.x - coordinates(poly)[,1] # calculate dx and dy (work with coords of the poly that is moved to new location)
    dy =  sam.y - coordinates(poly)[,2]
    poly1.moved = elide(poly, shift=c(dx, dy), rotate = 0,
                        center=coordinates(poly)) ## LET OP
    test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
    test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2))
    
    #Similarly as after first movement, try all rotations if there is intersection
    if(sum(st_intersects(test1.moved, test2.c, sparse=F)) >=1){
      poly1.moved = repeat.rot.fun(poly1.moved, test2, degrees) #returns only moved/focal object
    } 
    
    poly = poly1.moved #Store the newly moved poly to go for another round or exit
    # exit if the condition is met:
    if (sum(st_intersects(test1.c.moved, test2, sparse=F))==0) break}
  final.poly = poly
  return(final.poly)
}

# Create function for rotation (this is the same function as for method 3A)
degrees = c(seq(10,360, by=10))
repeat.rot.fun <- function(poly1, test2, degrees) {
  poly = poly1 #start with original poly1
  counter=0
  for (j in 1:length(degrees)) {
    poly1.moved = elide(poly, shift=c(0,0), rotate = degrees[j],
                        center=coordinates(poly)) ## LET OP
    test1.moved = st_as_sf(poly1.moved) #transform objects you have so far to sf
    test1.c.moved = st_set_crs(test1.moved, value=st_crs(test2))
    poly = poly1.moved #new round should be done with the moved poly
    if (sum(st_intersects(test1.c.moved, test2, sparse=F)) == 0) break} 
  final.rot.poly = poly
  return(final.rot.poly)
}

# Before looping set parameters dx and dy for the initial movement
centroids.nf = coordinates(nf) #center points of our annoated polygons
dx =  coordinates(rpoints.kde)[,1] - centroids.nf[,1]
dy =  coordinates(rpoints.kde)[,2] - centroids.nf[,2]

#FINAL LOOP B
polygons.shifted = nf[0,]
for (i in 1:length(nf)){
  print(i)
  # Create first shifted polygon, no existing ones, so no intersect
  if (i==1) focal.poly = elide(nf[i,], shift=c(dx[i], dy[i])) 
  # For all others, shift polygon and test for intersction
  if (i!=1) {
    # Move polgyon  
    focal.poly = elide(nf[i,], shift=c(dx[i], dy[i])) 
    # Check if overlap with ALL existing polygons and try rotations
    # Create st objects with same crs
    test1 = st_as_sf(focal.poly)
    test2 = st_as_sf(polygons.shifted)
    test2.c = st_set_crs(test2, value=st_crs(test1))  #used to be test1.c=st_set_crs(test1, value=st_crs(test2)) and all test2.c (below) set to test2 and all test1 set to test1.c, switch again if you get this error: "st_crs(x) == st_crs(y) is not TRUE"
    # Check if it overlaps with any of the existing polygons, if yes rotate loop  
    if(sum(st_intersects(test1, test2.c, sparse=F)) >=1){
      focal.poly = repeat.rot.fun(focal.poly, test2.c, degrees) #returns only moved/focal object
    }
    # Check if still overlaps, if yes move.  
    if (sum(st_intersects(test1, test2.c, sparse=F)) >=1){
      dirx = sample(dir, size=1, replace=T) # set a fixed direction + or - for x
      diry = sample(dir, size=1, replace=T) # set direction +- for y
      focal.poly = repeat.move.fun2(focal.poly, test2.c, x, y, bw.weighedavg, degrees) #returns only the moved/focal object
    }
  }
  # bind new polygon to the others:
  polygons.shifted = bind(polygons.shifted, focal.poly) #bind old & new poly's
}

#Check and plot method B
w.bbox = bbox(nf)
plot(nf, axes=T, border="lightgrey", xlim = c((w.bbox[1,1]-3), (w.bbox[1,2]+3)), ylim = c((w.bbox[2,1]-5), (w.bbox[2,2]+5)), 
     las=1, cex.axis=0.8, main=region)
plot(polygons.shifted, add=T)
plot(coastline, add=TRUE, border="lightseagreen")

#### (7B) Calculate rndm NNDs ####
##Distance matrix RNDM DATA for method B
# B stands for rndm method B (= new point from kernel instead of 10cm steps movement)

mat2b <- gDistance(polygons.shifted, byid = TRUE)
diag(mat2b) <- NA #set self-self distances (to NA) remove diagonal
result.nnd.kde.b <- t(sapply(seq(nrow(mat2b)), function(i) {
  j <- which.min(mat2b[i,])
  c(paste(rownames(mat2b)[i], colnames(mat2b)[j], sep='/'), mat2b[i,j])
}))  # #sapply loop obtains the minimum values of each row and stores in

# store and add to datasheet (method B)
nnd.couple.kde.b = as.data.frame(result.nnd.kde.b[,1])
colnames(nnd.couple.kde.b) = "nnd couple kde.b"
nnd.kde.b = as.data.frame(as.numeric(result.nnd.kde.b[,2])) #nearest neighbour distances
colnames(nnd.kde.b) = "nnd rndm kde.b (m)"
datasheet.rndm.b = cbind(nnd.kde.b, nnd.couple.kde.b)


#### (8) Store data per region ####

# run 1 of the lines below matching the region you are running
# !!! if you  have additional regions, make sure to add properly

## WATCH OUT WITH DATA WITH / WITHOUT PUPS! (nf and datasheet.base etc are continuously overwritten)

#noorderhaaks.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#rottumeroog.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#richel.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#rif.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#rottumerplaat.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#simonszand.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#renesse.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#goedereede.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#engelschehoek.finaldata = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#eemsdollard.finaldata.adults = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)
#eemsdollard.finaldata.inclpups = cbind(datasheet4, datasheet.rndm.a, datasheet.rndm.b)



#### (9) Store all reigons in Final datasheet ####

# RUN ONLY AFTER RUNNING ALL REGIONS OF INTEREST
# CBIND(..) CONTENT MAY NEED ATTENTION DEPENDING ON WHAT REGIONS YOU RAN THROUGH THE SCRIPT
# !!! if you do not have all regions or have additional regions, make sure to add them to the cbind(..) lines

# all data:
final.data = rbind(rottumeroog.finaldata, rif.finaldata, richel.finaldata, rottumerplaat.finaldata, simonszand.finaldata, renesse.finaldata, goedereede.finaldata, eemsdollard.finaldata.inclpups, noorderhaaks.finaldata, engelschehoek.finaldata)
#write_xlsx(final.data, "/Users/Anne/Documents/Internship WMR zeehonden/Data Analyses/finaldata_adults.xlsx")

## Datasheet Corrected for pups:
# new "final" datasheet, adults only
final.data.adults = rbind(rottumeroog.finaldata, rif.finaldata, richel.finaldata, rottumerplaat.finaldata, simonszand.finaldata, renesse.finaldata, goedereede.finaldata, eemsdollard.finaldata.adults, noorderhaaks.finaldata, engelschehoek.finaldata)
#write_xlsx(final.data.adults, "/Users/Anne/Documents/Internship WMR zeehonden/Data Analyses/finaldata_adults.xlsx")



#### Correction for pups methodology ####
# sites: Dollard
# 1. determine threshold size
#seal.surface = raster::area(nf)
hist(seal.surface, main=paste(region, "seal sizes (m2)", species), breaks=100, freq=F, xlim=c(0,1),ylim=c(0,6), xlab="surface area (m2)")
lines(density(seal.surface), col="blue", lwd=1.5)
#simonszand en rottumerplaat, 9 mnd NA puppen --> ligt tussen 0.18 en 0.85, meeste tussen 0.3 en 0.7
hist(simonszand.finaldata$`seal surface area (m2)`, breaks = 50, freq=F, xlim=c(0,1))
hist(rottumerplaat.finaldata$`seal surface area (m2)`, breaks = 50, freq=F, xlim=c(0,1))
# Alles kleiner dan 0.2 zien we als pup
# we nemen deze WEL mee voor de KDE en rndm sampling, maar NIET bij het maken van de dist.mat en nnd bepaling
# Only run for dollard



