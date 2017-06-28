dyn.load("distance.dylib") #for Mac OS
#dyn.load("distance.so") #for Linux

# tangent distance
tangentdistance <- function(imageOne,imageTwo) {
  out <- .C("distance",
    img1=as.double(imageOne),
    img2=as.double(imageTwo),
    dist=as.double(0))
  return(out$dist)
}
