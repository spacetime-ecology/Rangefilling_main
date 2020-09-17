###### Code used in preparation to running random.range.2.R function
# rland <- readOGR('Data\\Base\\ne_110m_land\\ne_110m_land_BJSMOD3.shp')
# rlakes <- readOGR('Data\\Base\\ne_110m_lakes\\ne_110m_lakes.shp')
# rland <- crop(rland, bbox)
# rland <- gBuffer(rland, width=.2)
# 
# # rbase is a simplified version of base for the randomized range simulations. Allows for simulation to reach islands.
# if(res!=20){
#   baseagg <- aggregate(base, 1/(res/20))
#   rbase <- rasterize(rland, baseagg)
#   rbase <- rbase*0
#   rbase <- mask(rbase, rlakes, inverse=T)
#   rbase <- disaggregate(rbase, 20/res)}
# if(res==20){
#   rbase <- rasterize(rland, base)
#   rbase <- rbase*0
#   rbase <- mask(rbase, rlakes, inverse=T)}
# 
# shp <- as(rbase, 'SpatialPolygonsDataFrame')
# colnames(shp@data) <- 'ID'
# shp_box <- poly_findInBoxGEOS(shp, as_points = T)
# nb <- poly2nb(shp, queen=F, foundInBox =shp_box)
# 
# # model random ranges
# ranges.random <- list()
# i <- 1
# for(i in i:length(rasterlist)){print(names(rasterlist)[i])
#   ranges.random[[i]] <- random.range.2(rasterlist[[i]], nb, rbase)}
# names(ranges.random) <- names(ranges.crop)
#######

random.range.2 <- function(rrange, nb, rbase){
  require(rangemodelR)
  require(raster)
  
  var=NULL
  first=F
  
  rpoint <- SpatialPoints(t(data.frame(colMeans(xyFromCell(rrange, which(rrange[]==1))))), proj4string = crs(rrange))
  rcent <- rasterize(rpoint, rrange*0)
  rcent[which(is.na(values(rcent)))] <- 0
  rcent <- rbase+rcent
  
  if(sum(values(rcent), na.rm=T)==0){
    r2 <- rcent
    r2[which.min(replace(distanceFromPoints(rbase, rpoint), is.na(rrange*0), NA))] <- 1
    rcent <- r2
  }
  
  spmat <- as.data.frame(na.omit(values(rrange)))
  spmat[spmat>0] <- 1
  spmat <- as.matrix(spmat)
  keep <- which(colSums(spmat) > 0)
  spmat <- as.matrix(spmat[,keep])
  range.size <- colSums(spmat)
  
  mat.temp <- which(values(rbase)==0)
  uid <- c(1:length(nb))
  orig <- uid*0
  orig[[which(na.omit(values(rcent))==1)]] <- 1
  
  sel.vec <- sample(uid, 1, prob = orig)
  sel.nb <- unlist(nb[sel.vec])
  
  for (i in 2:range.size){
    sel.vec[i] <- sample(sel.nb, 1)
    sel.nb <- unique(c(sel.nb, unlist(nb[sel.vec[i]])))
    sel.nb <- sel.nb[!sel.nb %in% sel.vec]
  }
  
  rtemp <- rbase
  rtemp[mat.temp[sel.vec]] <- 1
  rout <- rtemp+(rrange*0)
  
  while(sum(values(rtemp),na.rm=T)!=sum(values(rout),na.rm=T)){
    one.down <- ((which(values(rout)==1))-1)[which(rout[(which(values(rout)==1))-1]==0)]
    one.up <- ((which(values(rout)==1))+1)[which(rout[(which(values(rout)==1))+1]==0)]
    one.all <- c(one.down, one.up)
    nsamp <- (sum(values(rtemp),na.rm=T)-sum(values(rout),na.rm=T))
    if(nsamp>length(one.all)){nsamp <- length(one.all)}
    one.sel <- sample(one.all, nsamp)
    rout[one.sel] <- 1
  }
  
  return(rout)
  
}