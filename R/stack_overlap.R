#' stack_overlpa: Estimate ellipsoid overlap for an object of class ellipsoid_stack
#' @description Estimate ellipsoid overlap and other metrics, such as the volume
#' of the intersection for an ellipsoid_stack.
#' @param ellipsoid_stack An object of class ellipsoid_stack.
#' @param env_data A numeric matrix with environmental data
#' @param suitability_differences Logical. Show differences in suitabilities
#' (only if env_data is of class RasterStack). See details
#' @param parallel Run process in parallel
#' @param ncores Number of cores for parallel process
#' @details If env_data is of class RasterStack and if suitability_differences = TRUE.
#' The geographic projection of intersections will show the differences between
#' the suitabilities of the ellipsoids being compared. Positive values mean closer
#' places to the centroid of ellipsoid A and negative values are values closer to the
#' centroid of ellipsoid B.
#' @importFrom magrittr %>%
#' @import future furrr
#' @examples
#' \dontrun{
#' bionames <- c("bio5","bio6","bio12")
#' de_centroid <- c(314.4205,141.8564,1767.3282)
#' names(de_centroid) <- bionames
#' de_sigma <- matrix(c(913.3171,621.1225,-1834.9222,
#'                      621.1225,1693.1855,6825.4082,
#'                      -1834.9222,6825.4082,802340.5206),
#'                    ncol=3,dimnames = list(bionames,bionames))
#' dr_centroid <- c(309.829,156.0725,1759.1617)
#' names(dr_centroid) <- bionames
#' dr_sigma <- matrix(c(1379.9633,795.3904,-3377.1604,
#'                      795.3904,2481.8104,20088.3235,
#'                      -3377.1604,20088.3235,812031.9645),
#'                    ncol=3,dimnames = list(bionames,bionames))
#' dy_centroid <- c(328.197,191.2879,1791.8939)
#' dy_sigma <- matrix(c(286.6837,52.6501,695.4674,
#'                      52.6501,775.8082,5174.0618,
#'                      695.4674,5174.0618,351406.8655),
#'                    ncol=3,dimnames = list(bionames,bionames))
#' sp_names <- c("D. ecaudata","D. rotundus","D. youngi")
#' de_edata <- overllip::ellipsoid_data(centroid = de_centroid,
#'                                      sigma = de_sigma,cf = 0.95)
#' dr_edata <- overllip::ellipsoid_data(centroid = dr_centroid,
#'                                      sigma = dr_sigma,cf = 0.95)
#' dy_edata <- overllip::ellipsoid_data(centroid = dy_centroid,
#'                                      sigma = dy_sigma,cf = 0.95)
#' ellipsoid_stack <- overllip::stack(de_edata,dr_edata,dy_edata,
#'                                    ellipsoid_names = sp_names)
#'
#' env_data <- overllip::hypercube(ellipsoids = ellipsoid_stack,
#'                                 n = 10000000)

#' rmat <- overllip::stack_overlap(ellipsoid_stack = ellipsoid_stack,
#'                                 env_data = env_data,
#'                                 parallel = T,ncores = 4)
#' overllip::plot(rmat)
#' #----------------------------------------------------------------------------
#' # Example with raster data
#' #----------------------------------------------------------------------------
#' env_rdata <- raster::getData("worldclim",var='bio', res=10,path="~/Desktop")
#' env_rdata <- env_rdata[[c("bio5","bio6","bio12")]]
#' rras <- overllip::stack_overlap(ellipsoid_stack = ellipsoid_stack,
#'                                 env_data = env_rdata,
#'                                 suitability_differences =TRUE,
#'                                 parallel = F)
#' overllip::plot(rras,semiaxes=TRUE)
#' raster::plot(rras@geographic_intersection)
#' }
#' @export
stack_overlap <- function(ellipsoid_stack,env_data,suitability_differences=FALSE,parallel=FALSE,
                          ncores){

  if(class(env_data)[1] %in% c("RasterBrick","RasterStack")){
    ras_inter <- env_data[[1]]
    nonas <- which(!is.na(ras_inter[]))
    env_data <- env_data[]
    env_data <- env_data[nonas,]
    ras_inter  <-  ras_inter *0
    row.names(env_data) <- nonas

  }

  n_ellip <- length(ellipsoid_stack@ellipsoids)
  enames <- names(ellipsoid_stack@ellipsoids)
  e_compa <- utils::combn(enames,2)
  n_combi <- ncol(e_compa)
  if(parallel){
    if(missing(ncores)) ncores <- future::availableCores()-1

    if(ncores > n_ellip)
      ncores <-n_ellip
    plan(multisession, workers = ncores)

    parops <- furrr::furrr_options(globals = c("env_data",
                                               "ellipsoid_stack"),
                                   packages = c("overllip"))

    st_in <- 1:n_ellip %>% furrr::future_map_dfc(function(x,edata=env_data){
      ee <- ellipsoid_stack@ellipsoids[[x]]
      in_el <- ee[edata]
      d00 <- data.frame(in_el)
      names(d00) <- names(ellipsoid_stack@ellipsoids[x])
      return(d00)
    },.progress = TRUE,.options =  parops )
    future::plan(future::sequential)
  } else{
    st_in <- 1:n_ellip %>% purrr::map_dfc(function(x,edata=env_data){
      in_el <- ellipsoid_stack@ellipsoids[[x]][edata]
      d00 <- data.frame(in_el)
      names(d00) <- names(ellipsoid_stack@ellipsoids[x])
      return(d00)
    })
  }

  overlap_s <- 1:n_combi %>% purrr::map(function(x){
    .niche_overlap(ellipsoid_sp1 = ellipsoid_stack@ellipsoids[[ e_compa[1,x]]],
                   in_ellip_sp1 = st_in[, e_compa[1,x]],
                   ellipsoid_sp2 = ellipsoid_stack@ellipsoids[[ e_compa[2,x]]],
                   in_ellip_sp2 = st_in[,e_compa[2,x]])
  })

  if(n_ellip>=3){
    global_intersectionID <- which(rowSums(st_in) ==n_ellip)

  } else{
    global_intersectionID <- overlap_s[[1]]@intersection_ids
  }
  inter_e <- methods::new("stack_metrics")
  inter_e@env_data <- env_data
  for(i in 1:n_combi){
    inter_e@n_hypervolume <- c(nrow(env_data))
    inter_e@n_unions[i] <- overlap_s[[i]]@n_union
    inter_e@n_intersections[i] <- overlap_s[[i]]@n_intersection
    inter_e@Jackard_indices[i] <- overlap_s[[i]]@Jackard_index
    inter_e@intersection_volumes[i] <-  overlap_s[[i]]@intersection_volume
    inter_e@intersection_ids[[i]] <- overlap_s[[i]]@intersection_ids
  }
  inter_e@ellipsoids_metadata <- ellipsoid_stack
  cnames <- paste0(e_compa[1,1:n_combi],
                   "_vs_",e_compa[2,1:n_combi])
  names(inter_e@intersection_ids) <- cnames
  names(inter_e@n_unions) <- cnames
  names(inter_e@n_intersections) <- cnames
  names(inter_e@Jackard_indices) <- cnames
  names(inter_e@intersection_volumes) <- cnames
  nellipsoids <- lapply(seq_along(overlap_s),function(s){
    v1 <- c(overlap_s[[s]]@n_ellipsoid_1,
            overlap_s[[s]]@n_ellipsoid_2)
    names(v1) <- c(paste0("ellipsoid_",e_compa[1,s]),
                   paste0("ellipsoid_",e_compa[2,s]))
    return(v1)
  }) %>% unlist()
  inter_e@n_ellipsoids <- unique(nellipsoids)
  names(inter_e@n_ellipsoids) <- unique(names(nellipsoids))
  inter_e@global_intersection_volume <- (length(global_intersectionID)/
                                           inter_e@n_ellipsoids[[1]])*ellipsoid_stack@ellipsoids[[1]]@volume
  inter_e@global_intersection_ids <-  global_intersectionID

  if(exists("ras_inter")){
    rvals <- rep(NA,raster::ncell(ras_inter))
    if(!suitability_differences){
      rvals[nonas] <- 0
      sinter <- 1

      #ras_inter <- r
    }

    geo_intersect <-seq_along(overlap_s) %>% purrr::map(function(s){
      compare_name <- names(inter_e@intersection_ids[s])
      inter_ids <- inter_e@intersection_ids[[compare_name]]
      env_combi <- env_data[inter_ids,]
      inter_ids <- as.numeric(row.names(env_combi))

      if(suitability_differences){
        e1_suit <- overlap_s[[s]]@comparison@ellipsoids[[1]][env_combi] %>%
          names() %>% as.numeric()

        e2_suit <- overlap_s[[s]]@comparison@ellipsoids[[2]][env_combi] %>%
          names() %>% as.numeric()
        sinter <- e1_suit - e2_suit
      }

      rvals[inter_ids] <- sinter
      ras_inter[] <- rvals
      names(ras_inter) <- compare_name
      return(ras_inter)
    }) %>% raster::stack()
    global_ids <- inter_e@global_intersection_ids
    global_ids <- as.numeric(row.names(env_data)[global_ids])
    rvals[nonas] <- 0
    rvals[global_ids] <- 1
    ras_inter[] <- rvals
    names(ras_inter) <- "Global_intersection"
    inter_e@geographic_intersection <- raster::stack(ras_inter,geo_intersect)
  }

  return(inter_e)
}

