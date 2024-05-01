#' niche_metrics: Estimate ellipsoid overlap
#' @description Estimate ellipsoid overlap and other metrics, such as the volume
#' of the intersection.
#' @param ellipsoid_sp1 An object of class ellipsoid_metadata.
#' @param ellipsoid_sp2 An object of class ellipsoid_metadata.
#' @param env_points A numeric matrix with environmental data
#' @examples
#' \dontrun{
#' library(overllip)
#' set.seed(111)
#' mat <- matrix(rnorm(33),ncol=3)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' ellipsoid_sp1 <- overllip::ellipsoid_data(centroid=centroid,
#'                                           sigma = sig, cf=0.95)
#' ellipsoid_sp2 <- overllip::ellipsoid_data(centroid=centroid-1,
#'                                           sigma = sig, cf=0.95)
#' env_points <- overllip::hypercube(stack(ellipsoid_sp1,
#'                                         ellipsoid_sp2),100000)
#'
#' n_overlap <- overllip::niche_overlap(ellipsoid_sp1 = ellipsoid_sp1,
#'                                      ellipsoid_sp2 = ellipsoid_sp2,
#'                                      env_points = env_points)
#'
#' }
#' @export
niche_overlap <- function(ellipsoid_sp1,ellipsoid_sp2,env_points){

  in_ellip_sp1 <- ellipsoid_sp1[env_points]
  in_ellip_sp2 <- ellipsoid_sp2[env_points]
  return(.niche_overlap(ellipsoid_sp1 = ellipsoid_sp1,
                        ellipsoid_sp2 = ellipsoid_sp2,
                        in_ellip_sp1 = in_ellip_sp1,
                        in_ellip_sp2 = in_ellip_sp2))

}

#' .niche_overlap: Estimate ellipsoid overlap
#' @description Estimate ellipsoid overlap and other metrics, such as the volume
#' of the intersection.
#' @param ellipsoid_sp1 An object of class ellipsoid_metadata.
#' @param ellipsoid_sp2 An object of class ellipsoid_metadata.
#' @param in_ellip_sp1 A Logical vector
#' @param in_ellip_sp2 A Logical vector
.niche_overlap <- function(ellipsoid_sp1,ellipsoid_sp2,
                           in_ellip_sp1,in_ellip_sp2){
  res_vec <- paste0(in_ellip_sp1,"_",in_ellip_sp2)
  nmetrics <- table(res_vec)
  cnames <- names(nmetrics)
  notinellipsoids <- if("FALSE_FALSE" %in% cnames){
    nmetrics[["FALSE_FALSE"]]
  } else 0
  just_insp1 <- if("TRUE_FALSE" %in% cnames) {
    nmetrics[["TRUE_FALSE"]]
  } else 0
  just_insp2 <- if("FALSE_TRUE" %in% cnames) {
    nmetrics[["FALSE_TRUE"]]
  } else 0
  eintersec <- if("TRUE_TRUE" %in% cnames) {
    nmetrics[["TRUE_TRUE"]]
  } else 0

  np_in_sp1 <- just_insp1 + eintersec
  np_in_sp2 <- just_insp2 + eintersec
  jackard <- eintersec/(np_in_sp1+np_in_sp2 - eintersec)
  inter_vol <- ellipsoid_sp1@volume * jackard
  res <- methods::new("overlap_metrics")
  res@n_ellipsoid_1 <- np_in_sp1
  res@n_ellipsoid_2 <- np_in_sp2
  res@comparison <- overllip::stack(ellipsoid_sp1,ellipsoid_sp2)
  res@n_hypercube <- length(res_vec)
  res@n_union <- (np_in_sp1+np_in_sp2 - eintersec)
  res@n_intersection <- eintersec
  res@Jackard_index <- jackard
  res@intersection_volume <- inter_vol
  res@intersection_ids <- which(res_vec == "TRUE_TRUE")
  return(res)
}

