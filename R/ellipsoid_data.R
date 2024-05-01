#' Function to compute ellipsoid metadata.
#' @description Function to compute ellipsoid metadata. It computes volume,
#' semi-axes length and coordinates.
#' @param centroid A vector with coordinates of niche-centroid.
#' @param sigma A square matrix
#' @param cf Confidence level
#' @examples
#' set.seed(111)
#' mat <- matrix(rnorm(33),ncol=3)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <-ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' @export
ellipsoid_data <- function(centroid,sigma,cf=0.95){
  nr <- nrow(sigma)
  nc <- ncol(sigma)
  if(nr != nc)
    stop("sigma is not a square matrix")
  #if(sum(sigma == t(sigma) ) == (nr ^ 2))
  #  stop("sigma is not a symmetric matrix")

  # Ellipsoid volume
  ellip_vol <- function(n, axis_length) {
    term1 <- 2 * pi^(n/2)
    term2 <- n * gamma(n/2)
    term3 <- prod(axis_length)
    term4 <- (term1/term2) * term3
    return(term4)
  }
  r1 <- stats::qchisq(cf, df = ncol(sigma))
  sigmaI <- solve(sigma)/r1
  sigIEigenSys <- eigen(sigmaI)
  sigIEval <- sigIEigenSys$values
  sigIEvec <- sigIEigenSys$vectors
  stds <- 1/sqrt(sigIEval)
  axis_length <- NULL
  for (i in 1:dim(sigmaI)[1]) {
    axis_length[i] <- stds[i] * 2
  }
  names(axis_length) <- letters[1:nr]
  n <- nr

  vol2 <- ellip_vol(n, axis_length/2)
  axis_coordinates <- list()
  for (i in 1:dim(sigma)[1]) {
    assign(paste0("l", i, "_inf"),
           centroid - sigIEvec[,i] * stds[i])
    assign(paste0("l", i, "_sup"),
           centroid + sigIEvec[,i] * stds[i])
    coord_matrix <- matrix(c(eval(parse(text = paste0("l",
                                                      i, "_sup"))),
                             eval(parse(text = paste0("l", i, "_inf")))),
                           byrow = T, nrow = 2)
    colnames(coord_matrix) <- names(centroid)
    rownames(coord_matrix) <- paste0("vec_", 1:2)
    axis_coordinates[[i]] <- coord_matrix
  }
  results <- ellipsoid_metadata(centroid = centroid,
                                sigma = sigma,
                                axes_length = axis_length,
                                axes_coordinates = axis_coordinates,
                                volume = vol2,
                                cf = cf,
                                radius=sqrt(r1))
  return(results)
}
