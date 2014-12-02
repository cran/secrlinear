############################################################################################
## package 'secrlinear'
## clipmask.R
## last changed 2014-08-29 2014-09-09
############################################################################################

clipmask <- function (mask, traps, buffer = 100, clipvert = FALSE) {
    tmp <- networkdistance (mask, traps, geometry = mask)
    inrange <- apply(tmp,1,min) < buffer
    mask <- subset(mask, inrange)
    if (clipvert) {
        SPDF <- SpatialPoints(coords = as.matrix(mask))
        SP <- gBuffer(spgeom = SPDF, width = attr(mask, "spacing")/2)
        SLDF <- attr(mask, "SLDF")
        newvert <- gIntersection (SLDF, SP)
        ldf <-  data.frame(ID = 1:length(newvert), rownames=1)
        newvert <- SpatialLinesDataFrame(newvert, data = ldf)
        attr(mask, "SLDF") <- newvert 
    }
    mask
}

clippopn <- function (popn, mask, buffer = 100) {
    tmp <- networkdistance (mask, popn, geometry = mask)
    inrange <- apply(tmp,2,min) < buffer
    popn <- subset(popn, inrange)
    popn
}

