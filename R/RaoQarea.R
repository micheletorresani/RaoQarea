#'RaoQarea
#'
#'This package returns the Rao's Q values (using an Euclidean distance) for a considered area (raster cropeed by a shapefile) without making use of the moving windows approach
#'@param x A Rasterlayer
#'@param y A shapefile where the Raster has to be cropped
#'@return The packege returns the Rao's Q values of the whole cropped area, without making use of the moving window approach
#'@export

RaoQarea <- function(x, y ){
  crop1<-crop(x, y )
  mat_s<-values(crop1)
  n_s<-length(mat_s)
  n2_s<-n_s^2
  distm_s<-as.matrix(dist(mat_s))
  rao<-(sum(distm_s))/n2_s
  return(rao)}
