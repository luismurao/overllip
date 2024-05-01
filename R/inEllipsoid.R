#' inEllipsoid: Determine if a point is inside or outside an ellipsoid
#' @description Determine if a point is inside or outside an ellipsoid.
#' @param centroid A vector with coordinates of niche-centroid.
#' @param sigma A square matrix
#' @param cf Confidence level
#' @param env_data A data frame with the environmental training data.
#' @param suitability Logical. If TRUE niche-centroid distances are normalized to
#' suitability values.
#' @return A matrix with 2 columns. The first "in_Ellipsoid" binary response
#' with values 1 (inside the ellipsoid) and zeros (outside the ellipsoid);
#'  the second "mh_dist" distance to niche-centroid. If suitability=TRUE,
#'  the distance can be normalized to suitability values ranging from 0 to 1,
#'  where a value of zero means outside the ellipsoid.
#' @details The function can be call by using indexing operations implemented in
#' ellipsoid_metadata class. See second example
#' @examples
#' \dontrun{
#' # ---------------------------------------------------------------------------
#' # First example:
#' # Create and ellipsoid_metadata object
#' # Then, extract a random sample from the hypervolume
#' # that encloses the ellipsoid
#' # Finally, determine which points from the random
#' # sample are inside the ellipsoid
#' # ---------------------------------------------------------------------------
#' set.seed(111)
#' mat <- matrix(rnorm(33),ncol=3)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <- overllip::ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' hc <- overllip::hypercube(emdat,100000)
#' in_ellip_a <- overllip::inEllipsoid(centroid = emdat@centroid,
#'                                     sigma = emdat@sigma,
#'                                     cf= emdat@cf,
#'                                     env_data= hc)
#' rgl::plot3d(hc[in_ellip_a[,1],],
#'             col=hsv(0.85*in_ellip_a[,2][in_ellip_a[,1]],0.88,0.8),alpha=0.5)
#' plot(emdat,semiaxes=TRUE)
#' rgl::aspect3d("iso")
#' rgl::plot3d(hc[!in_ellip_a[,1],],col="gray70",add=T)

#' # ---------------------------------------------------------------------------
#' # Second example:
#' # Extract a random sample from the hypervolume
#' # that encloses the ellipsoid using indexing.
#' # ---------------------------------------------------------------------------
#' in_ellip_b <- emdat[hc]
#' suits <- as.numeric(names(in_ellip_b))
#' rgl::plot3d(hc[in_ellip_b,],
#'             col=hsv(0.85*suits[in_ellip_b],0.88,0.8),alpha=0.5)
#' rgl::plot3d(hc[!emdat[hc],],col="gray70",add=T)
#'
#' #----------------------------------------------------------------------------
#' # Last example
#' # Ellipsoids in 2-D
#' #----------------------------------------------------------------------------
#' set.seed(111)
#' mat <- matrix(rnorm(20),ncol=2)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <- overllip::ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' hc <- overllip::hypercube(emdat,5000)
#' in_ellip_a <- emdat[hc]
#' overllip::plot(emdat,semiaxes=TRUE,asp=1,xlab="dim1",ylab="dim2")
#' points(hc[in_ellip_a,],pch=".")
#' }
#' @export

inEllipsoid <- function(centroid,sigma,cf,env_data,suitability=TRUE){
  mh_dist <- stats::mahalanobis(env_data,
                                center = centroid,
                                cov = sigma)
  in_Ellipsoid <- mh_dist <= stats::qchisq(cf,
                                           length(centroid))
  if(suitability) mh_dist <- exp(-0.5*mh_dist)

  in_Ellipsoid_mh <- data.frame(in_Ellipsoid,mh_dist)
  return(in_Ellipsoid_mh)
}
