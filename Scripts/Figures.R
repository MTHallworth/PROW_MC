library(raster)
library(sp)
library(rgeos)
library(geosphere)
library(SGAT)
library(BAStag)
library(MASS)
library(maptools)


Americas<-raster::shapefile("Spatial_Layers/Americas.shp")
ProwDist<-raster::shapefile("Spatial_Layers/PROWdist.shp")
States<-raster::shapefile("Spatial_Layers/st99_d00.shp")
#Mangroves<-raster::shapefile("Spatial_Layers/MANGROVES.shp")


WGS84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(Americas)<-WGS84
crs(ProwDist)<-WGS84
crs(Mangroves)<-WGS84
crs(States)<-WGS84

prow<-readRDS("M761_29Jun15_141610.lux_fit.rds")
str(prow)

xlim <- c(-115, -60)
ylim <- c(0, 50)

r <- raster::raster(nrows=20*diff(ylim),ncols=20*diff(xlim),xmn=xlim[1],xmx=xlim[2],ymn=ylim[1],ymx=ylim[2])

  S <- slices(type="intermediate",
                   breaks="day",
                   mcmc=prow,
                   grid=r)
  
  

  library(GeoLight)

zm<- locationSummary(prow$z,
                     time=prow$model$time,
                           collapse=T) 

twl.back<- data.frame(Twilight=twilight(prow$model$time[-length(prow$model$time)],
                                    lon= zm[,5],
                                    lat=zm[,10],
                                    prow$model$rise[-length(prow$model$time)],
                                    zenith=94, iters=3),
                            Rise=prow$model$rise[-length(prow$model$time)])

twl.gl<- data.frame(tFirst=twl.back[-nrow(twl.back),1],
                          tSecond=twl.back[-1,1],
                          type=ifelse(twl.back[,2],1,2)[-nrow(twl.back)])


cL<- changeLight(twl = twl.gl[complete.cases(twl.gl),],
                         quantile = 0.95,
                         days = 1,
                         plot = TRUE, 
                         summary = TRUE)

siteMap(cbind(zm[,5], zm[,10]),map.range="America",site=cL$site,xlim=xlim,ylim=c(-30,ylim[2]),type='cross',hull=F,legend=FALSE)



  # Create a vector of the Dates in the file - here we set rise=true because the data has both rise and sunset
  
    DATES <- S$mcmc[[1]]$time[ which( S$mcmc[[1]]$rise==TRUE) ]
    
stops<-nrow(cL$migTable)
AnnualCycle<-vector("list",stops)

for(t in 1:stops){
if(t<stops){
AnnualCycle[[t]]<-slice(S,k=c(which(strptime(DATES,format="%Y-%m-%d")==as.POSIXct(strptime(cL$migTable[t,2],format="%Y-%m-%d"))):
                                         which(strptime(DATES,format="%Y-%m-%d")==as.POSIXct(strptime(cL$migTable[t,3],format="%Y-%m-%d")))))
} else{
AnnualCycle[[t]]<-slice(S,k=c(which(strptime(DATES,format="%Y-%m-%d")==as.POSIXct(strptime(cL$migTable[t,2],format="%Y-%m-%d"))):
                                        length(DATES)))
}
AnnualCycle[[t]][AnnualCycle[[t]]>50]<-NA

AnnualCycle[[t]]<-AnnualCycle[[t]]/maxValue(AnnualCycle[[t]])
AnnualCycle[[t]][AnnualCycle[[t]]<quantile(AnnualCycle[[t]],probs=0.95)]<-NA
  } # t

plot(Americas,xlim=xlim,ylim=ylim)
for(t in 1:stops){
plot(AnnualCycle[[t]],add=TRUE)
}
plot(SpatialLines(list(Lines(Line(cbind(zm[,"Lon.50%"],zm[,"Lat.50%"])),ID="a"))),add=TRUE)


    
    # Here we specify our dates of interest. 
    ReleaseDay<-1
    Jul31<-which(strptime(DATES,format="%Y-%m-%d")==as.POSIXct("2014-07-31"))
    
    
    # Breeding 
    # Get time when on the breeding grounds 
    tm<-sliceInterval(S,k=c(ReleaseDay:Jul31))
    
    
    # "Slice" the data and save all dates between Release date and July 31 2011, and June 1 2012 until capture.
    sk<-slice(S,k=c(ReleaseDay:Jul31))
    crs(sk)<-WGS84
    sk[sk>50]<-50
    sk[sk<quantile(sk,probs=0.95)]<-NA
    plot(sk)
    

    Nov01 <- which(strptime(DATES,format="%Y-%m-%d")==as.POSIXct("2014-11-01"))
    
    tmW<- sliceInterval(S,k=c(Nov01:length(DATES)))
    
    # "Slice" data and merge between Nov 01 2014 and Feb 31 2015.
    skW<- slice(S,k=c(Nov01:length(DATES)))
    crs(skW)<-WGS84
    skW[skW<quantile(skW,probs=0.95)]<-NA
    
    
    #plot(SpatialPoints(cbind(CapLocs[i,1],CapLocs[i,2])),add=TRUE,pch=19,cex=1.5)
    box()
    
    plot(Americas,col="gray74",
         ylim=ylim, xlim=xlim)
    
    plot(sk,useRaster=TRUE,
         ylim=ylim, xlim=xlim,
         axes=FALSE, add=TRUE,
         legend=FALSE,
         col=rev(bpy.colors(50)),
         cex.axis=0.7)
    plot(skW,useRaster=TRUE,add=TRUE,
         #ylim=c(15,55),xlim=c(-100,-60),
         axes=FALSE,
         legend=FALSE,
         col=rev(bpy.colors(50)),
         cex.axis=0.7)
    plot(Americas,border="black",add=TRUE)
    plot(Americas,border="gray",add=TRUE)
   CapLocs<-SpatialPoints(cbind(-91.107176,30.370293))
   crs(CapLocs)<-WGS84

polyconic<-"+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
Americas_proj<-spTransform(Americas,CRS=polyconic)
ProwDist_proj<-spTransform(ProwDist,CRS=polyconic)
Mangroves_proj<-spTransform(Mangroves,CRS=polyconic)
States_proj<-spTransform(States,CRS=polyconic)

sk_proj<-projectRaster(sk,crs=polyconic)
skW_proj<-projectRaster(skW,crs=polyconic)
CapLocs_proj<-spTransform(CapLocs,CRS=polyconic)

par(bty="n")
plot(sk_proj,axes=FALSE,legend=FALSE)
plot(Americas_proj,add=TRUE)
plot(skW_proj,add=TRUE,legend=FALSE)
par(fig=c(0.5,1,0.5,1), new=TRUE)

tiff(filename = "BreedingLocation.tiff")
plot(Americas_proj,ylim=c(3201184,3561902),xlim=c(641579,1175977),axes=FALSE,col="gray88")
scalebar(100000,xy=c(718607.5,3197217),type="bar",div=2,label=c(0,50,100),below="km")
plot(sk_proj,legend=FALSE,add=TRUE)
plot(Americas_proj,add=TRUE)
plot(States_proj,add=TRUE)
plot(CapLocs_proj,add=TRUE,pch=19)
dev.off()
system("open BreedingLocation.tiff")

tiff(filename = "NonBreedingLocation.tiff")
plot(ProwDist_proj,col="gray",xlim=c(2403795,3611995),ylim=c(695391.5,1389464),border="gray")
plot(skW_proj,xlim=c(2403795,3611995),ylim=c(695391.5,1389464),col=rev(bpy.colors(100)),add=TRUE,legend=FALSE)
scalebar(200000,xy=c(2997984, 559619.5),type="bar",div=2,label=c(0,100,200),below="km")
plot(Americas_proj,add=TRUE)
dev.off()
system("open NonBreedingLocation.tiff")


tiff(filename = "Breed_NonBreed.tiff")
par(mar=c(0,0,0,0))
plot(Americas_proj,
     xlim=c(-305364.3,3866107),
     ylim=c(710345.3,3902428),
     col="gray")
#plot(subset(ProwDist_proj,SEASONAL==3),col="orange",add=TRUE,border="orange")
#plot(subset(ProwDist_proj,SEASONAL==2),col="lightblue",add=TRUE,border="lightblue")
plot(sk_proj,legend=FALSE,add=TRUE)
plot(skW_proj,legend=FALSE,add=TRUE,col=rev(bpy.colors(100)))
plot(States_proj,add=TRUE)
scalebar(1000000,xy=c(198192,744319),type="bar",label=c(0,500,1000),below="km")
dev.off()

