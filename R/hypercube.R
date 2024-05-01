#' Estimate a hypercube that contains all ellipsoids
#' @description Build a hypervolume that contains the union of all ellipsoids
#' in a raster_stack.
#' @param ellipsoids An object of class ellipsoid_metada or ellipsoid_stack.
#' @param n Number of points to create a random sample inside the hypercube.
#' @details  Given the union of all ellipspoids, it generates a dataset X
#' constituted by the environmental information of a random sample of
#' \emph{n} points inside the hypervolume.
#' @examples
#' \dontrun{
#' set.seed(111)
#' mat <- matrix(rnorm(33),ncol=3)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <- overllip::ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' rgl::plot3d(hypercube(emdat,10000),col="red")
#' emdat2 <- overllip::ellipsoid_data(centroid=centroid+0.5,sigma =sig,cf=0.99)
#' aa <- stack(emdat,emdat2)
#' rgl::plot3d(hypercube(aa,10000),col="gray70")
#' ¨}
#' @export
hypercube <- function(ellipsoids,n){
  ss <- ellipsoids
  if(!class(ss) %in% c('ellipsoid_metadata','ellipsoid_stack'))
    stop("ellipsoids should be of class 'ellipsoid_metadata' or 'ellipsoid_stack'")
  #if(length(ss@ellipsoids)<2)
  #  stop("At least two ellipsoids are needed")
  if(class(ss) == 'ellipsoid_stack'){
    ellipsoid_axis <- do.call(rbind,lapply(ss@ellipsoids,function(x){
      axxes <- do.call(rbind,x@axes_coordinates)
      return(axxes)
    }))
  } else{
    ellipsoid_axis <- do.call(rbind,ss@axes_coordinates)
  }

  min_env <- apply(ellipsoid_axis,MARGIN=2,min)
  max_env <- apply(ellipsoid_axis,MARGIN=2,max)
  data_rand <- matrix(nrow = n,
                      ncol = length(max_env))
  for(i in 1:length(min_env)){
    rand_var <- stats::runif(n = n,
                             min = min_env[i],
                             max = max_env[i])
    data_rand[,i] <- rand_var
  }
  return(data_rand)
}
