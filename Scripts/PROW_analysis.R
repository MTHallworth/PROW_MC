## ----eval = FALSE--------------------------------------------------------
## # Check to make sure the required packages are installed on your machine
## # If not, they will be installed
## 
## reqPackages <- c("devtools","raster","sp","maptools","rgeos","MASS")
## get.packages <- reqPackages[!(reqPackages %in% installed.packages()[,"Package"])]
## if(length(get.packages)>0) install.packages(get.packages)
## 
## # Install necessary packages from Github using the devtools library #
## library(devtools)
## install_github("SWotherspoon/SGAT")
## install_github("SWotherspoon/BAStag")

## ---- warning = FALSE, message = FALSE-----------------------------------
library(raster)
library(sp)
library(rgeos)
library(geosphere)
library(TwGeos)
library(SGAT)
library(MASS)
library(maptools)

## ------------------------------------------------------------------------
# read in a simple world map from the maptools package #
Americas<-raster::shapefile("Spatial_Layers/Americas.shp")
ProwDist<-raster::shapefile("Spatial_Layers/PROWdist.shp")

## ----echo = FALSE--------------------------------------------------------
plot(ProwDist)
plot(Americas, col = "lightgray", border="gray88",add=TRUE)
plot(subset(ProwDist,SEASONAL==3),col="orange",add=TRUE,border="orange")
plot(subset(ProwDist,SEASONAL==2),col="lightblue",add=TRUE,border="lightblue")
plot(Americas, add=TRUE)

# Set the capture coordinates for each bird #

CapLocs<-array(NA,c(23,2))

# Louisiana Capture locations
CapLocs[1:9,1]<- -91.10717
CapLocs[1:9,2]<- 30.37029

# South Carolina Capture Locations
CapLocs[10,]<- cbind(-80.34655,33.21930)
 
# Virginia Capture locations
CapLocs[11:14,1]<- -77.26448
CapLocs[11:14,2]<- 37.36484

# Arkansas Capture locations
CapLocs[15,]<- cbind(-91.11775, 34.24716) # U784
CapLocs[16,]<- cbind(-91.10993, 34.24632) # U786
CapLocs[17,]<- cbind(-91.11480, 34.24332) # U852
CapLocs[18,]<- cbind(-91.11848, 34.24839) # U856
CapLocs[19,]<- cbind(-91.11649, 34.24251) # U859
CapLocs[20,]<- cbind(-91.10819, 34.24635) # U861
CapLocs[21,]<- cbind(-91.11650, 34.24252) # U862 

# Ohio Capture Locations
CapLocs[22,]<- cbind(-82.91729,39.89847)
CapLocs[23,]<- cbind(-82.91729,39.89847)

points(CapLocs,pch=19,cex=1.25)

## ------------------------------------------------------------------------
# Read in the data from LUX files 
PROW_LA<-list.files("Data/Louisiana",pattern = ".lux", full.names = TRUE)

PROW_SC<-list.files("Data/SouthCarolina", pattern = ".lux", full.names = TRUE)

PROW_VA<-list.files("Data/Virginia", pattern = ".lux", full.names = TRUE)

PROW_AR<-list.files("Data/Arkansas", pattern = ".lux", full.names = TRUE)

PROW_OH<-c(list.files("Data/Ohio", pattern = ".lig", full.names = TRUE),
           list.files("Data/Ohio", pattern = ".lux", full.names = TRUE))

# Combined the different locations into a single file #
PROWFiles<-c(PROW_LA,PROW_SC,PROW_VA,PROW_AR,PROW_OH)


# Read just the file names for Bird ID
PROW_LA_names<- list.files("Data/Louisiana", pattern = ".lux", full.names = FALSE)
PROW_SC_names<- list.files("Data/SouthCarolina", pattern = ".lux", full.names = FALSE)
PROW_VA_names<- list.files("Data/Virginia", pattern = ".lux", full.names = FALSE)
PROW_AR_names<- list.files("Data/Arkansas", pattern = ".lux", full.names = FALSE)
PROW_OH_names<- c(list.files("Data/Ohio", pattern = ".lig", full.names = FALSE),
                  list.files("Data/Ohio", pattern = ".lux", full.names = FALSE))

# Combined the different locations into a single file #
BirdId<-c(PROW_LA_names,PROW_SC_names,PROW_VA_names,PROW_AR_names,PROW_OH_names)

# Determine the number of birds
nBirds<-length(BirdId)

# Read in the lux file
PROWdata<-vector('list',nBirds)

# Loop through all the files and read them in as LUX files #
for(i in c(1:21,24)){ # all birds but OH are .lux files 
PROWdata[[i]] <- read.table(PROWFiles[i], skip=21,col.names=c("Date","Light"), sep = "\t")  

# format the date so it can be used later in the analysis
PROWdata[[i]]$Date <- as.POSIXct(strptime(PROWdata[[i]]$Date,format="%d/%m/%Y %H:%M:%S",tz="GMT"))

# Set the lowest light value to 1 
PROWdata[[i]][PROWdata[[i]]$Light==1.14] <- 1

# Take the log of light data # 
PROWdata[[i]]$Light <- log(PROWdata[[i]]$Light)
}
# Read in the Lig Files
PROWdata[[22]]<-readLig(PROWFiles[22],skip=1)
PROWdata[[23]]<-readLig(PROWFiles[23],skip=1)

# Re-arrange the LigFiles
PROWdata[[22]]<-PROWdata[[22]][,c(2,4)] # save just date and light
PROWdata[[23]]<-PROWdata[[23]][,c(2,4)] # save just date and light

## ----echo = FALSE--------------------------------------------------------
head(PROWdata[[1]])

## ------------------------------------------------------------------------
# Set the capture coordinates for each bird #

CapLocs<-array(NA,c(nBirds,2))

# Louisiana Capture locations
CapLocs[1:9,1]<- -91.10717
CapLocs[1:9,2]<- 30.37029

# South Carolina Capture Locations
CapLocs[10,]<- cbind(-80.34655,33.21930)

# Virginia Capture locations
CapLocs[11:14,1]<- -77.26448
CapLocs[11:14,2]<- 37.36484

# Arkansas Capture locations
CapLocs[15,]<- cbind(-91.11775, 34.24716) # U784
CapLocs[16,]<- cbind(-91.10993, 34.24632) # U786
CapLocs[17,]<- cbind(-91.11480, 34.24332) # U852
CapLocs[18,]<- cbind(-91.11848, 34.24839) # U856
CapLocs[19,]<- cbind(-91.11649, 34.24251) # U859
CapLocs[20,]<- cbind(-91.10819, 34.24635) # U861
CapLocs[21,]<- cbind(-91.11650, 34.24252) # U862 

# Ohio Capture Locations
CapLocs[22,]<- cbind(-82.91729,39.89847)
CapLocs[23,]<- cbind(-82.91729,39.89847)
CapLocs[24,]<- cbind(-82.87994,40.21280)


## ------------------------------------------------------------------------
tm<-rise<-vector('list',nBirds)

for(i in 1:nBirds){
  tm[[i]] <- seq(from = PROWdata[[i]][1,1], 
                 to = PROWdata[[i]][nrow(PROWdata[[i]]),1], 
                 by = "day")
  
  rise[[i]] <- rep(c(TRUE, FALSE), length(tm[[i]]))
}

# making predicted twilight times given location and zenith #
cal.dat<-vector('list',nBirds)

for(i in 1:nBirds){
  cal.dat[[i]] <- data.frame(Twilight = twilight(rep(tm[[i]], each = 2),
                                        lon = CapLocs[i,1], 
                                        lat = CapLocs[i,2], 
                                        rise = rise[[i]], zenith = 94),
                             Rise = rise[[i]]) 
}

## ------------------------------------------------------------------------
for(i in 1:nBirds){
lightImage(PROWdata[[i]],
           offset = 19, 
           zlim = c(0,4), 
           main = BirdId[[i]]) 
  
tsimagePoints(cal.dat[[i]]$Twilight, 
              offset = 19, 
              pch = 16, cex = 0.5, 
              col = ifelse(cal.dat[[i]]$Rise, "blue", "red"))

# adds line at two equinoxes for reference. Change the dates if necessary (can vary by year) #
eqnx<-as.POSIXct(c("2014-09-23", "2015-03-20"), tz = "GMT") 
abline(v = eqnx, lwd=3, lty=3, col="purple")
}


## ---- eval = TRUE--------------------------------------------------------
twl <- vector('list',nBirds)

seed <- c(rep("2014-11-01 04:00:00",14), rep("2015-11-01 04:00:00",10))
seed[17] <- "2015-10-01 04:00:00"

for(i in 1:nBirds){
i = 22
twl[[i]] <- findTwilights(tagdata = PROWdata[[i]],
                         threshold = 0.8,
                         include = seed[i],
                         dark.min = 360) # 6 hours minimum dark period
}

## ----eval = FALSE--------------------------------------------------------
## twlEdit <- vector('list',nBirds)
## 
## for(i in 1:nBirds){
## twlEdit[[i]] <- twilightEdit(twilights = twl[[i]],
##                     window = 4,           # two days before and two days after
##                     outlier.mins = 45,    # difference in mins
##                     stationary.mins = 25, # are the other surrounding twilights within 25 mins of one another
##                     plot = TRUE)
## }
## 
## timediff <- rep(300,25)
## timediff[c(22,23)] <- 120
## 
## for(i in 1:nBirds){
##   twl[[i]]<-twilightAdjust(twilights=twlEdit[[i]], interval=timediff[i])
## }
## 

## ----eval = FALSE--------------------------------------------------------
## for(i in 1:nBirds){
##   saveRDS(twl[[i]],paste0("Data/twilight/",BirdId[[i]],"twlEdit.rds")) # Saves rds file with the BirdId.rds as the name
## }

## ----echo = FALSE--------------------------------------------------------
twl <- vector('list',nBirds)

for(i in 1:nBirds){
twl[[i]]<-readRDS(paste0("Data/twilight/",BirdId[[i]],"twlEdit.rds"))
}

## ------------------------------------------------------------------------
# Create a vector with the dates known to be at deployment #
calib.dates <- vector('list',nBirds)

for(i in 1:14){ # LA and VA birds
calib.dates[[i]] <- c(strptime(twl[[i]][1,1],format="%Y-%m-%d"),as.POSIXct("2014-07-31"))
}
for(i in 15:nBirds){ # AR and OH birds
calib.dates[[i]] <- c(strptime(twl[[i]][1,1],format="%Y-%m-%d"),as.POSIXct("2015-07-31"))
}

## ------------------------------------------------------------------------
calibration.data<-vector('list',nBirds)

for(i in 1:nBirds){
calibration.data[[i]]<-subset(twl[[i]],twl[[i]]$Twilight>=calib.dates[[i]][1] & twl[[i]]$Twilight<=calib.dates[[i]][2])
}

## ------------------------------------------------------------------------
# Generate empty lists to store data 
sun<-z<-twl_t<-twl_deviation<-fitml<-alpha<-vector('list',nBirds)

# Determine the sun elevation angle - here called the Zenith angle #

for(i in 1:nBirds){
  
# Calculate solar time from calibration data 
sun[[i]]  <- solar(calibration.data[[i]][,1])

# Adjust the solar zenith angle for atmospheric refraction
z[[i]] <- refracted( zenith(sun = sun[[i]],
                            lon = CapLocs[i,1], 
                            lat = CapLocs[i,2]))

twl_t[[i]] <- twilight(tm = calibration.data[[i]][,1],
                       lon = CapLocs[i,1], 
                       lat = CapLocs[i,2], 
                       rise = calibration.data[[i]][,2],
                       zenith = quantile(z[[i]],probs=0.5))

# Determine the difference in minutes from when the sun rose and the geolocator said it rose 
twl_deviation[[i]] <- ifelse(calibration.data[[i]]$Rise, as.numeric(difftime(calibration.data[[i]][,1], twl_t[[i]], units = "mins")),
                  as.numeric(difftime(twl_t[[i]], calibration.data[[i]][,1], units = "mins")))

# Throw out values less than 0 - These values are not valid 
twl_deviation[[i]]<-subset(twl_deviation[[i]], subset=twl_deviation[[i]]>=0)

# Describe the distribution of the error 
fitml[[i]] <- fitdistr(twl_deviation[[i]], "log-Normal")

# save the Twilight model parameters
alpha[[i]]<- c(fitml[[i]]$estimate[1], fitml[[i]]$estimate[2]) 
}

## ----echo = FALSE--------------------------------------------------------
b<-unlist(twl_deviation)
cols<-c(rep("red",length(PROW_LA)),
        rep("purple",length(PROW_SC)),
        rep("blue",length(PROW_VA)),
        rep("green",length(PROW_AR)),
        rep("orange",length(PROW_OH)))

seq <- seq(0,60, length = 100)
par(mfrow=c(1,2),mar=c(4,4,0,0))
hist(b, freq = F,
     yaxt="n",
     ylim = c(0, 0.15),
     xlim = c(0, 60),
     breaks=15,
     col="gray",
     main = "",
     xlab = "Twilight error (mins)")
axis(2,las=2)
for(i in 1:nBirds){
lines(seq, dlnorm(seq, alpha[[i]][1], alpha[[i]][2]), col = cols[i], lwd = 3, lty = 2)
}

#Zenith angle plot
par(bty="l")
plot(median(z[[1]],na.rm=TRUE),xlim=c(1,nBirds),ylim=c(80,100),pch=19,ylab="Zenith Angle",xlab="PROW",col=cols[1])
segments(1,quantile(z[[1]],probs=0.025),1,quantile(z[[1]],probs=0.975),col=cols[1])
for(i in 2:nBirds){
  par(new = TRUE)
  plot(median(z[[i]],na.rm=TRUE)~i,xlim=c(1,nBirds),ylim=c(80,100),pch=19,yaxt="n",xaxt="n",ylab="",xlab="",col=cols[i])
  segments(i,quantile(z[[i]],probs=0.025),i,quantile(z[[i]],probs=0.975),col=cols[i])
}

## ------------------------------------------------------------------------
# Create empty vectors to store objects #
d.twl<-path<-vector('list',nBirds)

zenith0<-zenith1<-rep(NA,nBirds)

# loop through the birds #
for(i in 1:nBirds){
  # Store the zenith (sun-elevation angle)
  zenith0[i] <-quantile(z[[i]],prob=0.5)
  zenith1[i]<-quantile(z[[i]],prob=0.95)
}

## ------------------------------------------------------------------------
WinterZenith_S <- mean(c(93.4724,92.4592))
WinterZenith_SL <- mean(c(93.9496,94.4907,93.8249,93.4077,93.6820))

# subset the twilight file for dates after the first calibration date (presumably the deployment date)  
# and exclude points that were deleted  
# note we didn't delete any transitions here

twl <- lapply(twl, subset, !Deleted)

# Manual edits #

twl[[2]] <- twl[[2]][3:(nrow(twl[[2]])-1),]

twl[[4]] <- twl[[4]][19:(nrow(twl[[4]])-2),]
twl[[5]] <- twl[[5]][1:(nrow(twl[[5]])-2),]
twl[[6]] <- twl[[6]][2:nrow(twl[[6]]),]
twl[[7]] <- twl[[7]][3:nrow(twl[[7]]),]
twl[[8]] <- twl[[8]][6:(nrow(twl[[8]])-2),]

twl[[10]] <- twl[[10]][17:(nrow(twl[[10]])-2),]
twl[[11]] <- twl[[11]][15:(nrow(twl[[11]])-2),]
twl[[12]] <- twl[[12]][30:nrow(twl[[12]]),]
twl[[13]] <- twl[[13]][20:nrow(twl[[13]]),]

twl[[15]] <- twl[[15]][5:(nrow(twl[[15]])-2),]
twl[[16]] <- twl[[16]][3:(nrow(twl[[16]])-3),]

twl[[18]] <- twl[[18]][8:nrow(twl[[18]]),]
twl[[19]] <- twl[[19]][30:nrow(twl[[19]]),]
twl[[20]] <- twl[[20]][3:nrow(twl[[20]]),]

twl[[22]] <- twl[[22]][10:(nrow(twl[[22]])-5),]
twl[[23]] <- twl[[23]][4:nrow(twl[[23]]),]
twl[[24]] <- twl[[24]][30:(nrow(twl[[24]])-3),]

# Set tol values #
tolvalues <- array(0,c(nBirds,2))
tolvalues[1,1] <- 0.36
tolvalues[2,1] <- 0.25
tolvalues[3,1] <- 0.22
tolvalues[4,1] <- 0.34
tolvalues[5,1] <- 0.25
tolvalues[6,1] <- 0.185
tolvalues[7,1] <- 0.19
tolvalues[8,1] <- 0.3
tolvalues[9,1] <- 0.25
tolvalues[10,1] <- 0.225
tolvalues[11,1] <- 0.224
tolvalues[12,1] <- 0.27
tolvalues[13,1] <- 0.225
tolvalues[14,1] <- 0.32
tolvalues[15,1] <- 0.255
tolvalues[16,1] <- 0.185
tolvalues[18,1] <- 0.26
tolvalues[19,1] <- 0.26
tolvalues[20,1] <- 0.285
tolvalues[21,1] <- 0.26
tolvalues[22,1] <- 0.178
tolvalues[23,1] <- 0.18
tolvalues[24,1] <- 0.145

tolvalues[14,2] <- 0.085
tolvalues[15,2] <- 0.08
tolvalues[16,2] <- 0.12
tolvalues[17,2] <- 0.05
tolvalues[18,2] <- 0.2
tolvalues[19,2] <- 0.162
tolvalues[20,2] <- 0.167
tolvalues[21,2] <- 0.1
tolvalues[22,2] <- 0.14
tolvalues[23,2] <- 0.19
tolvalues[24,2] <- 0.2

WinterZenith_S <- mean(c(93.4724,92.4592))
## ---- warning = FALSE, message = FALSE-----------------------------------
Zeniths <- vector('list',nBirds)

for(i in 1:nBirds){  
  Zeniths[[i]]<-rep(NA,length(twl[[i]]$Twilight))
  Zeniths[[i]][which(twl[[i]]$Twilight<as.POSIXlt("2014-11-01",format="%Y-%m-%d"))]<-zenith0[i]
  Zeniths[[i]][is.na(Zeniths[[i]])]<-mean(zenith0)
  
  path[[i]] <- suppressWarnings(thresholdPath(twilight = twl[[i]]$Twilight,
                             rise = twl[[i]]$Rise,
                             zenith = Zeniths[[i]],
                             tol = tolvalues[i,]))
}


## ----echo = FALSE--------------------------------------------------------
 for(i in 1:nBirds){
  print(BirdId[[i]])
  layout(matrix(c(1,3,
                  2,3), 2, 2, byrow = TRUE))
  par(mar=c(2,4,2,0))
  plot(path[[i]]$time, path[[i]]$x[, 2], type = "b", pch = 16, cex = 0.5, ylab = "Lat", xlab = '',xaxt="n")
  abline(h = CapLocs[i,2])
  abline(v = as.POSIXct("2014-09-23"),col="red",lty=2,lwd=1.5)
  abline(v = as.POSIXct("2015-03-20"),col="red",lty=2,lwd=1.5)
  par(mar=c(2,4,2,0))
  plot(path[[i]]$time, path[[i]]$x[, 1], type = "b", pch = 16, cex = 0.5, ylab = "Lat", xlab = '')
  abline(h = CapLocs[i,1])
  abline(v = as.POSIXct("2014-09-23"),col="red",lty=2,lwd=1.5)
  abline(v = as.POSIXct("2015-03-20"),col="red",lty=2,lwd=1.5)
  plot(Americas, col = "grey95",xlim = c(-120,-60),ylim=c(0,40))
  box()
  lines(path[[i]]$x, col = "blue")
  points(path[[i]]$x, pch = 16, cex = 0.5, col = "blue")
Sys.sleep(2)
}

## ------------------------------------------------------------------------
x0 <- z0 <- vector('list',nBirds)

for(i in 1:nBirds){
  # Take the location estimates created above
x0[[i]]<- path[[i]]$x

  # the model also needs the mid-points - generate those here
z0[[i]]<- trackMidpts(x0[[i]])
}

## ------------------------------------------------------------------------
beta <- c(0.7, 0.08)

## ------------------------------------------------------------------------
fixedx <- vector('list',nBirds)

for(i in 1:nBirds){
fixedx[[i]]<- rep(FALSE, nrow(x0[[i]]))

fixedx[[i]][1:10] <- TRUE

x0[[i]][fixedx[[i]], 1] <- CapLocs[i,1]
x0[[i]][fixedx[[i]], 2] <- CapLocs[i,2]

z0[[i]] <- trackMidpts(x0[[i]]) # update z0 positions
}

## ------------------------------------------------------------------------
# set xlim and ylim values need to span the range of your dataset
xlim <- c(-115, -60)
ylim <- c(0, 50)

## ------------------------------------------------------------------------
## Function to construct a land/sea mask
distribution.mask <- function(xlim, ylim, n = 4, land = TRUE, shape) {
    r <- raster(nrows = n * diff(ylim), ncols = n * diff(xlim), xmn = xlim[1], 
        xmx = xlim[2], ymn = ylim[1], ymx = ylim[2], crs = proj4string(shape))
    r <- cover(rasterize(shape, shift = c(-360, 0), r, 1, silent = TRUE), 
        rasterize(shape, r, 1, silent = TRUE), rasterize(elide(shape, 
            shift = c(360, 0)), r, 1, silent = TRUE))
    r <- as.matrix(is.na(r))[nrow(r):1, ]
    if (land) 
        r <- !r
    xbin <- seq(xlim[1], xlim[2], length = ncol(r) + 1)
    ybin <- seq(ylim[1], ylim[2], length = nrow(r) + 1)

    function(p) {
        r[cbind(.bincode(p[, 2], ybin), .bincode(p[, 1], xbin))]
    }
}

## ------------------------------------------------------------------------
WGS84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(Americas)<-WGS84
crs(ProwDist)<-WGS84

## ------------------------------------------------------------------------
## Define mask for Ovenbird distribution
is.dist <- distribution.mask(shape=Americas,
                             xlim = xlim,
                             ylim = ylim,
                             n = 10,
                             land = TRUE)

## ------------------------------------------------------------------------
# Define the log prior for x and z
log.prior <- function(p) {
    f <- is.dist(p)
    ifelse(f | is.na(f), 0, -10)
}

## ------------------------------------------------------------------------
# Define the threshold model - slimilar to above #
model <-  vector('list', nBirds)

for(i in 1:nBirds){
model[[i]]<- thresholdModel(twilight = twl[[i]]$Twilight,
                            rise = twl[[i]]$Rise,
                            twilight.model = "ModifiedLogNormal",
                            alpha = alpha[[i]],
                            beta = beta,
                             # Here is where we set the constraints for land
                            logp.x = log.prior, logp.z = log.prior, 
                            x0 = x0[[i]],
                            z0 = z0[[i]],
                            zenith = Zeniths[[i]],
                            fixedx = fixedx[[i]])
}


## ------------------------------------------------------------------------
# This defines the error distribution around each location #
proposal.x <- proposal.z <- vector('list',nBirds)

for(i in 1:nBirds){
proposal.x[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(x0[[i]]))
proposal.z[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(z0[[i]]))
}

## ----message=FALSE-------------------------------------------------------
fit <- zsum <- xsum <- vector('list', nBirds)

for(i in 1:nBirds){
  
fit[[i]] <- estelleMetropolis(model = model[[i]],
                         proposal.x = proposal.x[[i]],
                         proposal.z = proposal.z[[i]],
                         iters = 5000, # This value sets the number of iterations to run
                         thin = 2,
                         chains = 3)


zsum[[i]] <- locationSummary(fit[[i]]$z)
xsum[[i]] <- locationSummary(fit[[i]]$x)

proposal.x[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(cbind(xsum[[i]]$'Lon.50%',xsum[[i]]$'Lat.50%')))
proposal.z[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(cbind(zsum[[i]]$'Lon.50%',zsum[[i]]$'Lat.50%')))

fit[[i]] <- estelleMetropolis(model = model[[i]],
                              proposal.x = proposal.x[[i]],
                              proposal.z = proposal.z[[i]],
                              x0 = cbind(xsum[[i]]$'Lon.50%',xsum[[i]]$'Lat.50%'),
                              z0 = cbind(zsum[[i]]$'Lon.50%',zsum[[i]]$'Lat.50%'),
                              iters=5000, # This value sets the number of iterations to run
                              thin=2,
                              chains=3)

proposal.x[[i]] <- mvnorm(chainCov(fit[[i]]$x),s=0.1)
proposal.z[[i]] <- mvnorm(chainCov(fit[[i]]$z),s=0.1)

zsum[[i]] <- locationSummary(fit[[i]]$z)
xsum[[i]] <- locationSummary(fit[[i]]$x)

# Note the increase in number of interations - this takes a bit longer to run
fit[[i]] <- estelleMetropolis(model = model[[i]],
                              proposal.x = proposal.x[[i]],
                              proposal.z = proposal.z[[i]],
                              x0 = cbind(xsum[[i]]$'Lon.50%',xsum[[i]]$'Lat.50%'),
                              z0 = cbind(zsum[[i]]$'Lon.50%',zsum[[i]]$'Lat.50%'),
                              iters=5000, # This value sets the number of iterations to run
                              thin=10,
                              chains=3)

}

saveRDS(fit,paste0("PROW_",format(Sys.Date(),"%b_%d_%Y"),".rds"))
## ------------------------------------------------------------------------
# This step makes an empty raster #
r <- raster(res = c(0.25,0.25),xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2])

## ------------------------------------------------------------------------
S <- vector('list',nBirds)

for(i in 1:nBirds){
S[[i]] <- slices(type="intermediate",
                 breaks="day",
                 mcmc=fit[[i]],
                 grid=r)
}

## ----echo = FALSE--------------------------------------------------------

DATES <-  tm <- sk <- vector('list',nBirds)
Jul31 <- Nov01  <- Mar01 <- rep(NA,nBirds)

EndBreedDate<-c(rep("2014-07-31",14),rep("2015-07-31",10))
StartWinterDate<-c(rep("2014-11-01",14),rep("2015-11-01",10))

EndWinterDate<-c(rep("2015-02-15",14),rep("2016-02-15",10))

# Create a vector of the Dates in the file - here we set rise=true because the data has both rise and sunset

for(i in 1:nBirds){ 
DATES[[i]] <- S[[i]]$mcmc[[1]]$time[ which( S[[i]]$mcmc[[1]]$rise==TRUE) ]


# Here we specify our dates of interest. 
ReleaseDay<-1
Jul31[i]<-which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(EndBreedDate[i]))


# Breeding 
# Get time when on the breeding grounds 
tm[[i]]<-sliceInterval(S[[i]],k=c(ReleaseDay:Jul31[i]))


# "Slice" the data and save all dates between Release date and July 31 2011, and June 1 2012 until capture.
sk[[i]]<-slice(S[[i]],k=c(ReleaseDay:Jul31[i]))

print(BirdId[i])

  plot(Americas,col="gray74",
       ylim=ylim, xlim=xlim)
  
  plot(sk[[i]],useRaster=TRUE,
       ylim=ylim, xlim=xlim,
       axes=FALSE, add=TRUE,
       legend=FALSE,
       col=rev(bpy.colors(50)),
       cex.axis=0.7)
  
  plot(Americas,border="gray",add=TRUE)
  
# Non-breeding 

# Non-breeding 

Nov01[i] <- ifelse(i==17,length(DATES[[i]]),
                        which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(StartWinterDate[i])))
                   
Mar01[i] <- ifelse(i>15, which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(EndWinterDate[i])),
                         length(DATES[[i]]))
Mar01[17] <-length(DATES[[17]])

if(i != 17){

  tm[[i]]<- sliceInterval(S[[i]],k=c(Nov01[i]:Mar01[i]))
  
# "Slice" data and merge between Nov 01 2014 and Feb 31 2015.
  sk[[i]]<- slice(S[[i]],k=c(Nov01[i]:Mar01[i]))
  
  plot(sk[[i]],useRaster=TRUE,add=TRUE,
       #ylim=c(15,55),xlim=c(-100,-60),
       axes=FALSE,
       legend=FALSE,
       col=rev(bpy.colors(50)),
       cex.axis=0.7)
}
  plot(Americas,border="black",add=TRUE)
  plot(SpatialPoints(cbind(CapLocs[i,1],CapLocs[i,2])),add=TRUE,pch=19,cex=1.5)
 box()
 } 

## ------------------------------------------------------------------------
Col_S<-readLig("Data/Calibration/stalk geo_000.lig", skip = 1)
Col_SLess<-readLig("Data/Calibration/no stalk geo_000.lig", skip=1)

# Save only the columns named Date and Light
Col_S<-Col_S[,c("Date","Light")]
Col_SLess<-Col_SLess[,c("Date","Light")]

## ------------------------------------------------------------------------
Salamanca_S<-Col_S[which(Col_S$Date > as.POSIXlt("2016-01-05",format = "%Y-%m-%d") 
                         & Col_S$Date < as.POSIXlt("2016-01-22",format = "%Y-%m-%d")),]

Flamencos_S<-Col_S[which(Col_S$Date > as.POSIXlt("2016-01-22",format = "%Y-%m-%d") 
                         & Col_S$Date < as.POSIXlt("2016-01-26",format = "%Y-%m-%d")),]

## ------------------------------------------------------------------------
Salamanca_SL<-Col_SLess[which(Col_SLess$Date > as.POSIXlt("2016-01-05",format = "%Y-%m-%d") 
                         & Col_SLess$Date < as.POSIXlt("2016-01-10",format = "%Y-%m-%d")),]


Flamencos_SL<-Col_SLess[which(Col_SLess$Date > as.POSIXlt("2016-01-22",format = "%Y-%m-%d") 
                         & Col_SLess$Date < as.POSIXlt("2016-01-26",format = "%Y-%m-%d")),]

Marimonda_SL<-Col_SLess[which(Col_SLess$Date > as.POSIXlt("2016-01-11",format = "%Y-%m-%d") 
                         & Col_SLess$Date < as.POSIXlt("2016-01-14",format = "%Y-%m-%d")),]

Bocas_SL<-Col_SLess[which(Col_SLess$Date > as.POSIXlt("2016-01-13",format = "%Y-%m-%d") 
                         & Col_SLess$Date < as.POSIXlt("2016-01-17",format = "%Y-%m-%d")),]

Cispata_SL<-Col_SLess[which(Col_SLess$Date > as.POSIXlt("2016-01-17",format = "%Y-%m-%d") 
                         & Col_SLess$Date < as.POSIXlt("2016-01-20",format = "%Y-%m-%d")),]

## ----eval = FALSE--------------------------------------------------------
## twl_Flamencos_S <- preprocessLight(tagdata = Flamencos_S,
##                               dark.min=360,
##                               threshold = 0.8,
##                               offset = 19,
##                               lmax=5,
##                               zlim = c(0, 64))
## 
## 
## twl_Flamencos_S<-twilightAdjust(twilights=twl_Flamencos_S, interval=150)

## ----eval = FALSE--------------------------------------------------------
## # Calculate solar time from calibration data
##   sun  <- solar(twl_Flamencos_S[,1])
## 
##   # Adjust the solar zenith angle for atmospheric refraction
##   z <- refracted( zenith(sun = sun,
##                               lon = -73.10123,
##                               lat = 11.42013))
## 
## 
##   median(z)
## [1] 92.45916

## ----echo = FALSE--------------------------------------------------------
plot(subset(wrld_simpl,NAME=="Colombia"),col="lightgray")
  plot(wrld_simpl,add=TRUE)
plot(SpatialPoints(cbind(c(-74.686,-73.101,-76.817,-76.837,-75.784),
                         c(11.006,11.420,08.569,08.089,09.393))),add=TRUE,pch=19,col = (c("orange","red","yellow","blue","green")))

## ------------------------------------------------------------------------
WinterZenith_S <- mean(c(93.4724,92.4592))
WinterZenith_SL <- mean(c(93.9496,94.4907,93.8249,93.4077,93.6820))

## ---- warning = FALSE, message = FALSE-----------------------------------
Zeniths <- vector('list',nBirds)

for(i in 1:nBirds){  
  d.twl[[i]]<-subset(twl[[i]],twl[[i]]$Twilight>=calib.dates[[i]][1] & !Deleted)

  Zeniths[[i]]<-rep(NA,length(d.twl[[i]]$Twilight))
  Zeniths[[i]][which(d.twl[[i]]$Twilight<as.POSIXlt("2014-11-01",format="%Y-%m-%d"))]<-zenith0[i]
  Zeniths[[i]][is.na(Zeniths[[i]])]<-WinterZenith_S

  path[[i]] <- thresholdPath(twilight = d.twl[[i]]$Twilight,
                             rise = d.twl[[i]]$Rise,
                             zenith = Zeniths[[i]],
                             tol = c(0,0.1))
}

## ------------------------------------------------------------------------
# Define the threshold model - slimilar to above #
model <-  vector('list', nBirds)

for(i in 1:nBirds){
model[[i]]<- thresholdModel(twilight = d.twl[[i]]$Twilight,
                            rise = d.twl[[i]]$Rise,
                            twilight.model = "ModifiedLogNormal",
                            alpha = alpha[[i]],
                            beta = beta,
                             # Here is where we set the constraints for land
                            logp.x = log.prior, logp.z = log.prior, 
                            x0 = x0[[i]],
                            z0 = z0[[i]],
                            zenith = Zeniths[[i]],
                            fixedx = fixedx[[i]])
}

## ------------------------------------------------------------------------
for(i in 1:nBirds){
model[[i]]$fixedx<-c(model[[i]]$fixedx,rep(FALSE,(dim(model[[i]]$x0)[1]-length(model[[i]]$fixedx))))
}

## ------------------------------------------------------------------------
# This defines the error distribution around each location #
proposal.x <- proposal.z <- vector('list',nBirds)

for(i in 1:nBirds){
proposal.x[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(x0[[i]]))
proposal.z[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(z0[[i]]))
}

## ---- message=FALSE------------------------------------------------------
fit <- vector('list', nBirds)

for(i in 1:nBirds){

zsum[[i]] <- locationSummary(fit[[i]]$z)
xsum[[i]] <- locationSummary(fit[[i]]$x)

x0[[i]]<-cbind(xsum[[i]]$'Lon.50%',xsum$'Lat.50%')
z0[[i]]<-cbind(zsum[[i]]$'Lon.50%',zsum$'Lat.50%')

fit[[i]] <- estelleMetropolis(model = model[[i]],
                         proposal.x = proposal.x[[i]],
                         proposal.z = proposal.z[[i]],
                         x0 = x0[[i]],
                         z0 = z0[[i]],
                         iters = 2000, # This value sets the number of iterations to run
                         thin = 10,
                         chains = 3)

}


## ------------------------------------------------------------------------
for(i in 1:nBirds){
proposal.x[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(x0[[i]]))
proposal.z[[i]] <- mvnorm(S=diag(c(0.0025,0.0025)),n=nlocation(z0[[i]]))

zsum[[i]] <- locationSummary(fit[[i]]$z)
xsum[[i]] <- locationSummary(fit[[i]]$x)

x0[[i]]<-cbind(xsum[[i]]$'Lon.50%',xsum$'Lat.50%')
z0[[i]]<-cbind(zsum[[i]]$'Lon.50%',zsum$'Lat.50%')

fit[[i]] <- estelleMetropolis(model = model[[i]],
                              proposal.x = proposal.x[[i]],
                              proposal.z = proposal.z[[i]],
                              x0 = x0[[i]],
                              z0 = z0[[i]],
                              iters=1000, # This value sets the number of iterations to run
                              thin=1,
                              chains=3)

# Final Run

proposal.x[[i]] <- mvnorm(chainCov(fit[[i]]$x),s=0.1)
proposal.z[[i]] <- mvnorm(chainCov(fit[[i]]$z),s=0.1)

# Note the increase in number of interations - this takes a bit longer to run
fit[[i]] <- estelleMetropolis(model = model[[i]],
                              proposal.x = proposal.x[[i]],
                              proposal.z = proposal.z[[i]],
                              x0=chainLast(fit[[i]]$x),
                              z0=chainLast(fit[[i]]$z),
                              iters=5000,  # This value sets the number of iterations to run
                              thin=10,
                              chains=3)
}

## ---- echo=FALSE---------------------------------------------------------
for(i in 1:nBirds){
  saveRDS(fit[[i]],paste0("Data/Fit/",BirdId[i],"_fit_twlEdit.rds"))
#fit[[i]]<-readRDS(paste0("Data/Fit/",BirdId[i],"_fit.rds"))
}

birdfit<-list.files("Data/Fit/",pattern = ".rds")

birdfit<-birdfit[order(match(birdfit,paste0(BirdId,"_fit.rds")))]

fit <- lapply(paste0("Data/Fit/",birdfit),readRDS)

## ------------------------------------------------------------------------
# This step makes an empty raster #
r <- raster(nrows=10*diff(ylim),ncols=4*diff(xlim),xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2])

## ------------------------------------------------------------------------
S <- vector('list',nBirds)

for(i in 1:nBirds){
S[[i]] <- slices(type="intermediate",
                 breaks="day",
                 mcmc=fit[[i]],
                 grid=r)
}

## ----echo = FALSE,message=FALSE,warning=FALSE----------------------------
DATES <-  tm <- sk <- sk_winter <- vector('list',nBirds)
Jul31 <- Nov01 <- Mar01 <- rep(NA,nBirds)

EndBreedDate<-c(rep("2014-07-31",14),rep("2015-07-31",10))
StartWinterDate<-c(rep("2014-11-01",14),rep("2015-11-01",10))
EndWinterDate<-c(rep("2015-02-15",14),rep("2016-02-15",10))

# Create a vector of the Dates in the file - here we set rise=true because the data has both rise and sunset

for(i in 1:nBirds){ 
DATES[[i]] <- S[[i]]$mcmc[[1]]$time[ which( S[[i]]$mcmc[[1]]$rise==TRUE) ]


# Here we specify our dates of interest. 
ReleaseDay<-1
Jul31[i]<-which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(EndBreedDate[i]))


# Breeding 
# Get time when on the breeding grounds 
tm[[i]]<-sliceInterval(S[[i]],k=c(ReleaseDay:Jul31[i]))


# "Slice" the data and save all dates between Release date and July 31.
sk[[i]]<-slice(S[[i]],k=c(ReleaseDay:Jul31[i]))

print(BirdId[i])

  plot(Americas,col="gray74",
       ylim=ylim, xlim=xlim)
  
  plot(sk[[i]],useRaster=TRUE,
       ylim=ylim, xlim=xlim,
       axes=FALSE, add=TRUE,
       legend=FALSE,
       col=rev(bpy.colors(50)),
       cex.axis=0.7)
  
  plot(Americas,border="gray",add=TRUE)
  
# Non-breeding 

Nov01[i] <- ifelse(i==17,length(DATES[[i]]),
                        which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(StartWinterDate[i])))
                   
Mar01[i] <- ifelse(i>15, which(strptime(DATES[[i]],format="%Y-%m-%d")==as.POSIXct(EndWinterDate[i])),
                         length(DATES[[i]]))
Mar01[17] <-length(DATES[[17]])

if(i != 17){
  tm[[i]]<- sliceInterval(S[[i]],k=c(Nov01[i]:Mar01[i]))
  
# "Slice" data and merge between Nov 01 2014 and Feb 31 2015.
  sk_winter[[i]]<- slice(S[[i]],k=c(Nov01[i]:Mar01[i]))
plot(sk_winter[[i]],useRaster=TRUE,add=TRUE,
#ylim=c(15,55),xlim=c(-100,-60),
 axes=FALSE,
legend=FALSE,
col=rev(bpy.colors(50)),
cex.axis=0.7)
}
plot(Americas,border="black",add=TRUE)
plot(SpatialPoints(cbind(CapLocs[i,1],CapLocs[i,2])),add=TRUE,pch=19,cex=1.5)
box()
 } 

## ----echo = FALSE, message=FALSE,warning=FALSE---------------------------
zm<-GeoLight.back<-GeoLight.twl<-vector('list',nBirds)

for(i in 1:nBirds){
  zm[[i]] <- locationSummary(fit[[i]]$z,
                             time=fit[[i]]$model$time,
                             collapse=T) 
  
  GeoLight.back[[i]] <- data.frame(Twilight=twilight(fit[[i]]$model$time[-length(fit[[i]]$model$time)],
                                                lon= zm[[i]][,5],
                                                lat=zm[[i]][,10],
                                                fit[[i]]$model$rise[-length(fit[[i]]$model$time)],
                                                zenith=94, iters=3),
                              Rise=fit[[i]]$model$rise[-length(fit[[i]]$model$time)])
  
  GeoLight.twl[[i]] <- data.frame(tFirst=GeoLight.back[[i]][-nrow(GeoLight.back[[i]]),1],
                            tSecond=GeoLight.back[[i]][-1,1],
                            type=ifelse(GeoLight.back[[i]][,2],1,2)[-nrow(GeoLight.back[[i]])])
}

library(GeoLight)

changeLightAnalysis<-vector('list',nBirds)

for(i in 1:nBirds){
  print(BirdId[i])
  changeLightAnalysis[[i]] <- changeLight(twl = GeoLight.twl[[i]][complete.cases(GeoLight.twl[[i]]),],
                                          quantile = 0.95, rise.prob=0.075,set.prob=0.075,
                                          days = 0.5,
                                          plot = FALSE, 
                                          summary = FALSE)
}

Departures<-cbind(BirdId, DepartureDate= sapply(changeLightAnalysis, function(x){format(x$migTable[1,3])}))

print(Departures)

## ----message=FALSE,echo=FALSE,warning=FALSE------------------------------
for(i in 1:nBirds){
changeLightAnalysis[[i]] <- changeLight(twl = GeoLight.twl[[i]][complete.cases(GeoLight.twl[[i]]),],
                                          quantile = 0.95, rise.prob=0.025,set.prob=0.025,
                                          days = 1,
                                          plot = FALSE, 
                                          summary = TRUE)

  siteMap(cbind(zm[[i]][,5], zm[[i]][,10]),
          map.range="America",
          site=changeLightAnalysis[[i]]$site,
          xlim=xlim,ylim=c(0,ylim[2]),
          type='cross',
          hull=F,
          legend=FALSE)
}

## ----echo=FALSE----------------------------------------------------------
sk_winter95<-vector('list',nBirds)
for(i in c(1:16,18:nBirds)){ 
  sk_winter[[i]][sk_winter[[i]]>120]<-NA
  sk_winter[[i]]<-sk_winter[[i]]/cellStats(sk_winter[[i]],max,na.rm=TRUE)
  sk_winter[[i]][sk_winter[[i]]>quantile(sk_winter[[i]],probs=0.95)]<-1
  sk_winter[[i]][sk_winter[[i]]<1]<-0
}
stack(sk_winter[c(1:16,18:nBirds)])

PROW_MC<-sum(stack(sk_winter[c(1:16,18:nBirds)]),na.rm=TRUE)
par(mar=c(0,0,0,0),bty="n")
plot(mask(PROW_MC,Americas),col=c("transparent",rev(bpy.colors(23))),axes=FALSE,legend=FALSE)
plot(States, add=TRUE,border="gray88")
plot(Americas, col = "lightgray", border="gray88",add=TRUE)
plot(mask(PROW_MC,Americas),col=c("transparent",rev(bpy.colors(23))),axes=FALSE,legend=FALSE,add=TRUE)
plot(wrld_simpl, add=TRUE)
ras<-raster(nrow=1,ncol=25)
ras[]<-0:24
plot(ras,legend.only=TRUE,horizontal=TRUE,col=(c("transparent",rev(bpy.colors(23)))),
     smallplot=c(0.1,0.4,0.1,0.12),
     legend.width=0.25,
     legend.shrink=0.5,
     axis.args=list(at=c(1,5,10,15,20,24),
                    labels=c(1,5,10,15,20,25),
                    cex.axis=1,
                    mgp=c(5,0.3,0)),
     legend.args=list(text="# of PROW", side=3, font=2, line=0.5, cex=1),add=TRUE)

points(CapLocs,pch=19)

tiff("PROW_MC.tiff", res = 500, width = 3000, height = 3000)
plot(subset(Americas,NAME=="Colombia"))
plot(PROW_MC,add=TRUE, col = c("transparent",rev(bpy.colors(23))),horizontal = TRUE)
plot(Americas, add = TRUE)
#plot(shapefile("Spatial_Layers/COL_water_lines_dcw.shp"),add=TRUE,lwd = 0.1, col = "blue")
#plot(shapefile("Spatial_Layers/COL_roads.shp"),add=TRUE,lwd = 0.1,col = "gray", alpha = 0.5)
plot(raster("Spatial_Layers/COL_msk_cov.grd"),add = TRUE, legend = FALSE,alpha = 0.5)
raster::scalebar(500,below = "km",lonlat = TRUE)
dev.off()



