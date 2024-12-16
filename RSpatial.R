# Introduction----
name <- LETTERS[1:10]
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5,
               -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9,
              36.2, 39, 41.6, 36.9)
stations <- cbind(longitude, latitude)

# Simulated data rainfall
set.seed(0)
precip <- round((runif(lenght(latitude))*10)^3)

psize <- 1 + precip/500
plot(stations, cex=psize, pch=20, col='red', main='Precipitation')
# add names to plot
text(stations, name, pos=4)
# add a legend
breaks <- c(100, 250, 500, 1000)
legend.psize <- 1+breaks/500
legend("topright", legend=breaks, pch=20, pt.cex=legend.psize, col='red', bg='grey')

library(ggplot2)
p <- ggplot(aes())


lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
x <- cbind(lon, lat)
plot(stations, main='Precipitation')
polygon(x, col='blue', border='light blue')
lines(stations, lwd=3, col='red')
points(x, cex=2, pch=20)
points(stations, cex=psize, pch=20, col='red', main='Precipitation')


wst <- data.frame(longitude, latitude, name, precip)
wst


# Vector Data ----
longitude <- c(-116.7, -120.4, -116.7, -113.5, -115.5, -120.8, -119.5, -113.7, -113.7, -110.7)
latitude <- c(45.3, 42.6, 38.9, 42.1, 35.7, 38.9, 36.2, 39, 41.6, 36.9)
lonlat <- cbind(longitude, latitude)

library(terra)

pts <- vect(lonlat) # creates a SpatVector file
class(pts)
pts
geom(pts)

crdref <- "+proj=longlat +datum=WGS84"
pts <- vect(lonlat, crs=crdref) # create a SpatVector and assign coordinates

crs(pts) # check the crs

# generate random precipitation values
precipvalue <- runif(nrow(lonlat), min=0, max=100)
df <- data.frame(ID=1:nrow(lonlat), precip=precipvalue)

ptv <- vect(lonlat, atts = df, crs = crdref) # assign the precipitation dataframe to the SpatVector
ptv


lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id = 1, part = 1, lon, lat)

lns <- vect(lonlat, type = "lines", crs = crdref) # here we create a line as SpatVector
lns

pols <- vect(lonlat, type = "polygons", crs = crdref) # for a polygon

plot(pols, las = 1)
plot(pols, border = "blue", col = "yellow", lwd = 3, add = T)
points(pts, col = "red", pch = 20, cex = 3)


# Raster Data ----
r <- rast(ncol = 10, nrow = 10,
          xmin = -150, xmax = -80,
          ymin = 20, ymax = 60) # creates a SpatRaster
r

values(r) <- runif(ncell(r))  # we assign some random values to the raster cells
r

plot(r)
# Let's add some points and polygons to the raster plot
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id=1, part=1, lon, lat)
pts <- vect(lonlat)
pols <- vect(lonlat, type="polygons", crs="+proj=longlat +datum=WGS84")
points(pts, col="red", pch=20, cex=3)
lines(pols, col="blue", lwd=2)

r2 <- r * r
r3 <- sqrt(r)
s <- c(r, r2, r3) # with c we create a multilayer SpatRaster
s

plot(s)

# Reading and Writing Spatial Data----

# Shapefile is the most common vector data format, it is a set of 3 or 4 files with same name but different extensions: x.shp, x.shx, x.dbf, x.prj

library(terra)

filename <- system.file("ex/lux.shp", package = "terra")
basename(filename)

s <- vect(filename)
s # NOTE: this SpatVector is DIFFERENT from the original shapefile. It is a "SpatVector of polygons in R".

outfile <- "shp_test.shp"
writeVector(s, outfile, overwrite = T) # writes a new Vector Shapefile file

ff <- list.files(pattern = "^shp_test")
file.remove(ff) # remove the file we just created


f <- system.file("ex/logo.tif", package = 'terra')
basename(f)

r <- rast(f)
r # SpatRaster of three layers (bands)
r2 <- r[[2]] # subset to get only the second layer
r2

x <- writeRaster(r, "test_output.tif", overwrite = T) # creates a tif file
x

# Coordinate Reference Systems----
library(terra)

f <- system.file("ex/lux.shp", package = 'terra')
p <- vect(f)
p

crs(p) # to inspect the crs

pp <- p
crs(pp) <- "" # this removes the current file crs
crs(pp)

crs(pp) <- "+proj=longlat +datum=WGS84" # crs assignment. NOTE: we are only changing the label here, not the crs itself!
crs(pp)


newcrs <- "+proj=robin +datum=WGS84"

rob <- terra::project(p, newcrs) # this function can transform the crs
rob

p2 <- terra::project(rob, "+proj=longlat +datum=WGS84") # backtransform to longitude/latitude


r <- rast(xmin=-110, xmax=-90, ymin=40, ymax=60, ncols=40, nrows=40)
values(r) <- 1:ncell(r)
r
plot(r)

newcrs
pr1 <- terra::project(r, newcrs) # we change the raster crs
crs(pr1)
plot(pr1)

x <- rast(pr1)
res(x) <- 200000

pr3 <- terra::project(r, x)
pr3 # note the change in coordinates
plot(pr3)

# Vector Data Manipulation----
library(terra)

f <- system.file("ex/lux.shp", package='terra')
p <- vect(f)
p

plot(p, "NAME_2")


d <- as.data.frame(p) # to extract the attributes (aka dataframe) from a SpacVector
head(d) 

g <- geom(p) # to extract vector geometry in form of a matrix
g

g <- geom(p, wkt=T)
substr(g, 1, 50)


p$NAME_2 # we can extract variables as we would do with a data frame

p[, "NAME_2"] # to subset one or more variables from a SpatVector

set.seed(0)
p$lets <- sample(letters, nrow(p)) # adding a new variable to a SpacVector, similar to dataframes
p 


perim(p) # returns the length of the SpatVector spatial objects

p$lets <- sample(LETTERS, nrow(p)) # assigning new values to an existing variable
head(p)

p$lets <- NULL # gets rid of variable

# Here we assign an attribute table (dataframe) to a SpatVector with merge() 
dfr <- data.frame(District = p$NAME_1, Canton = p$NAME_2, Value = round(runif(length(p), 100, 1000)))
dfr <- dfr[order(dfr$Canton), ]
pm <- merge(p, dfr, by.x=c('NAME_1', 'NAME_2'), by.y=c('District', 'Canton'))
pm

i <- which(p$NAME_1 == 'Grevenmacher')
g <- p[i,] # selecting rows, so the records
g


z <- rast(p)
dim(z) <- c(2,2)
values(z) <- 1:4
names(z) <- 'Zone'
z <- as.polygons(z) # corce SpatRaster to SpatVector polygons
z

z2 <- z[2,]
plot(p)
plot(z, add=T, border='blue', lwd=5)
plot(z2, add=T, border='red', lwd=2, col='red')


b <- rbind(p, z) # to append SpatVector objects
head(b) 
tail(b)


pa <- aggregate(p, by='NAME_1') # aggregare/dissolve polygons that have the same value for an attribute of interest
za <- aggregate(z)
plot(za, col='light gray', border='light gray', lwd=5)
plot(pa, add=T, col=rainbow(3), lwd=3, border='white')

zag <- aggregate(z, dissolve=F) # to aggregate polygons without dissolving the borders
zag
plot(zag, col='light gray')


zd <- disagg(zag) # to split polyogons into their parts

e <- erase(p, z2) # erase a part of a SpatVector
plot(e)

i <- intersect(p, z2) # Intersect SpatVectors
plot(i)

e <- ext(6, 6.4, 49.7, 50)
pe <- crop(p, e) # difference with respect to intersect is that with crop the grometry of the second argument is not retained
plot(p)
plot(e, add = T, lwd=3, col='red')
plot(pe, col='light blue', add=T)
plot(e, add=T, lwd=3, border='blue')


u <- union(p, z) # get the union of two SpatVector
u 

set.seed(5)
plot(u, col=sample(rainbow(length(u))))


cov <- cover(p, z[c(1,4),]) # combination of intersect and union.
cov
plot(cov)


dif <- symdif(z,p) # computes symmetrical difference of two SpatVectors
plot(dif, col=rainbow(length(dif))) 


pts <- matrix(c(6, 6.1, 5.9, 5.7, 6.4, 50, 49.9, 49.8, 49.7, 49.5), ncol=2)
spts <- vect(pts, crs=crs(p))
plot(z, col='light blue', lwd=2)
points(spts, col='light gray', pch=20, cex=6)
text(spts, 1:nrow(pts), col='red', font=2, cex=1.5)
lines(p, col='blue', lwd=2)

extract(spts, p) # used for queries between SpatVector and SpatRaster objects
extract(spts, z)
