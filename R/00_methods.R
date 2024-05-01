if (!isGeneric("stack")) {
  methods::setGeneric("stack", function(x, ...)
    standardGeneric("stack"))
}
if (!isGeneric("plot")) {setGeneric("plot", function(x,y,...)standardGeneric("plot"))}

#' Stack method for class missing of the \pkg{overllip} package.
#' @importFrom methods new
#' @param x An object of class ellipsoid_metadata
#' @param ... Additional objects of class ellipsoid_metadata
#' @rdname stack

methods::setMethod("stack", signature(x='missing'),
          function(x,...) {
            return(methods::new('ellipsoid_stack'))
          }
)




#' Stack method for class ellipsoid_stack of the \pkg{overllip} package.
#' @importFrom methods new
#' @param x An object of class ellipsoid_metadata
#' @param ellipsoid_names A character vector with names for each ellipsoid
#' @param ... Additional objects of class ellipsoid_metadata
#' @method stack overllip
#' @rdname stack
#' @export

methods::setMethod("stack",signature = c(x="ellipsoid_metadata"),
                   function(x,...,ellipsoid_names = NULL) {
                     rlist <- list(x, ...)
                     nellipsoids <- seq_along(rlist)
                     ss <- methods::new("ellipsoid_stack")
                     if(length(rlist) ==1){
                       if(inherits(x,"ellipsoid_metadata")){
                         ss@ellipsoids[[1]] <- x
                       }
                     } else{

                       for(i in nellipsoids){
                         if(inherits(rlist[[i]],'ellipsoid_metadata')){
                           ss@ellipsoids[[i]] <- rlist[[i]]
                         }
                         else{
                           stop(paste("Elements should be of class",
                                      "ellipsoid_metada"))
                         }
                       }
                     }
                     if(is.null(ellipsoid_names) || length(ellipsoid_names) != length(nellipsoids)){
                       names(ss@ellipsoids) <- paste0("ellipsoid_",nellipsoids)
                     }
                     else{
                       names(ss@ellipsoids) <- ellipsoid_names
                     }

                     return(ss)
                   }
)



#' Check if an environmental combination is inside an ellipsoid
#'
#' Askif an environmental combination is inside
#' an ellipsoid. This method applies to ellipsoid_metadata object
#' \code{`[`} operator.
#'
#' @param x A numerical matrix with environmental combinations to test.
#' @param i Not used
#' @param j Not used.
#' @param ... Additional parameters for class ellipsoid_metadata
#' @param suitability Logical. Show suitability values.
#' @param drop Not used.
#'
#' @return A named-logical vector. Names are suitability values
#' @examples
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Ellipsoids in 2-D
#' #----------------------------------------------------------------------------
#' set.seed(111)
#' mat <- matrix(rnorm(20),ncol=2)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <- overllip::ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' hc <- overllip::hypercube(emdat,5000)
#' in_ellip_a <- emdat[hc]
#' plot(emdat,semiaxes=TRUE,asp=1,xlab="dim1",ylab="dim2")
#' points(hc[in_ellip_a,],pch=".")
#' }
#' @name in-ellipsoid
NULL

#' @rdname in-ellipsoid
#' @export

methods::setMethod("[",
                   signature(x = "ellipsoid_metadata", i = "missing",
                             j = "missing", drop = "missing"),
                   definition = function(x, i, j, ..., drop = FALSE) x)


#' @rdname in-ellipsoid
#' @export
methods::setMethod('[',signature(x="ellipsoid_metadata"),
                   function(x, i, j,...,suitability=TRUE,drop=TRUE){
                     if(ncol(i) != length(x@centroid)){
                       mssg <- paste("Environmental data matrix should have the same number",
                                     "of colums as number of dimensions of the ellipsoid object")
                       stop(mssg)

                     }

                     mh_dist <- stats::mahalanobis(i,
                                                   center = x@centroid,
                                                   cov = x@sigma)
                     in_Ellipsoid <- mh_dist <= stats::qchisq(x@cf,
                                                              length(x@centroid))
                     if(suitability) mh_dist <- round(exp(-0.5*mh_dist),8)
                     names(in_Ellipsoid) <- mh_dist
                     return(in_Ellipsoid)
                   })

#' @rdname in-ellipsoid
#' @export
methods::setMethod('[',signature(x="ellipsoid_metadata",i="RasterStack"),
                   function(x, i, j,...,suitability=TRUE,drop=TRUE){
                     if(raster::nlayers(i) != length(x@centroid)){
                       mssg <- paste("RasterStack should have the same number",
                                     "of layers as number of dimensions of the ellipsoid object")
                       stop(mssg)

                     }
                     ivals <- raster::getValues(i)
                     rvals <- ivals[,1]
                     nona_vals <- which(!is.na(ivals[,1]))

                     mh_dist <- stats::mahalanobis(ivals[nona_vals,],
                                                   center = x@centroid,
                                                   cov = x@sigma)
                     in_Ellipsoid <- mh_dist <= stats::qchisq(x@cf,
                                                              length(x@centroid))
                     if(suitability) {
                       mh_dist <- round(exp(-0.5*mh_dist),8)
                       rvals[nona_vals] <- mh_dist
                     } else{
                       rvals[nona_vals] <- in_Ellipsoid
                     }

                     rellip <- i[[1]]
                     rellip[] <- rvals
                     names(rellip) <- if(suitability) {"suitability"} else "in_ellipsoid"
                     return(rellip)
                   })





#' Plot method (S4) for ellipsoid_metadata-class object
#'
#' plot is a generic function for plotting of R objects.
#' The function invokes particular [methods] which depend on the [class] of the first argument.
#'
#' @param x An object of class ellipsoid_metadata.
#' @param y Not used
#' @param col Plot color
#' @param semiaxes Logical. Show semi-axes
#' @param lwd_axes Line width for ellipsoid semi-axes
#' @param lty_axes Line type for ellipsoid semi-axes
#' @param add Add plot.
#' @param ... Arguments to pass to base::plot, rgl::plot3d,rgl::wire3d or to
#' @return A 2-Dimensional or 3-Dimensional plot
#' rgl::segments3d
#' @examples
#' \dontrun{
#' set.seed(111)
#' mat <- matrix(rnorm(20),ncol=2)
#' centroid <- colMeans(mat)
#' sig <- cov(mat)
#' emdat <- overllip::ellipsoid_data(centroid=centroid,sigma =sig, cf=0.95)
#' plot(emdat,semiaxes=TRUE,asp=1,xlab="dim1",ylab="dim2")
#' }
#' @name plot-ellipsoid
NULL
#' @rdname plot-ellipsoid
#' @export
methods::setMethod(f = "plot",
                   signature = c(x="ellipsoid_metadata",y="missing"),
                   function(x,y=NA,col=NULL,lwd_axes=2,lty_axes=2,semiaxes=FALSE,add=FALSE,...){
                     # Color palette taken from the RColorBrewer package
                     dark2 <- c("#1B9E77","#D95F02","#7570B3",
                                "#E7298A","#66A61E","#E6AB02",
                                "#A6761D","#666666","#ff7f00")
                     ndim <- length(x@centroid)
                     if(missing(col)) col <- sample(dark2,1)
                     if(ndim==2){

                       radius <- x@radius
                       r <- x@sigma[1, 2]
                       theta <- seq(0,2*pi,by=0.005)
                       scale <- sqrt(c(x@sigma[1, 1],
                                       x@sigma[2,2]))
                       if (scale[1] > 0) r <- r/scale[1]
                       if (scale[2] > 0) r <- r/scale[2]
                       r <- min(max(r,-1),1)
                       d <- acos(r)
                       xx <- radius * scale[1] * cos(theta + d/2) + x@centroid[1]
                       yy <- radius * scale[2] * cos(theta - d/2) + x@centroid[2]
                       if(add){
                         graphics::lines(xx,yy,lwd=3,col=col,...)
                       }
                       else{
                         plot(xx,yy,type="l",lwd=3,col=col,...)
                       }


                       if(semiaxes){

                         graphics::segments(x0 = x@axes_coordinates[[1]][1,1],
                                            y0 = x@axes_coordinates[[1]][1,2],
                                            x1 = x@axes_coordinates[[1]][2,1],
                                            y1 = x@axes_coordinates[[1]][2,2],
                                            col="gray70",lwd=lwd_axes,lty_axes,...)
                         graphics::segments(x0 = x@axes_coordinates[[2]][1,1],
                                            y0 = x@axes_coordinates[[2]][1,2],
                                            x1 = x@axes_coordinates[[2]][2,1],
                                            y1 = x@axes_coordinates[[2]][2,2],
                                            col="gray70",lwd=lwd_axes,
                                            lty_axes,...)
                       }
                     }
                     if(ndim ==3){
                       ellips_E <- rgl::ellipse3d(x@sigma, centre = x@centroid,
                                                  level = x@cf)
                       rgl::wire3d(ellips_E, col=col,lit = FALSE,...)

                       if(semiaxes){

                         rgl::segments3d(x = c(x@axes_coordinates[[1]][1,1],
                                               x@axes_coordinates[[1]][2,1]),
                                         y = c(x@axes_coordinates[[1]][1,2],
                                               x@axes_coordinates[[1]][2,2]),
                                         z = c(x@axes_coordinates[[1]][1,3],
                                               x@axes_coordinates[[1]][2,3]),
                                         lwd=3,...)

                         rgl::segments3d(x = c(x@axes_coordinates[[2]][1,1],
                                               x@axes_coordinates[[2]][2,1]),
                                         y = c(x@axes_coordinates[[2]][1,2],
                                               x@axes_coordinates[[2]][2,2]),
                                         z = c(x@axes_coordinates[[2]][1,3],
                                               x@axes_coordinates[[2]][2,3]),
                                         lwd=3,...)


                         rgl::segments3d(x = c(x@axes_coordinates[[3]][1,1],
                                               x@axes_coordinates[[3]][2,1]),
                                         y = c(x@axes_coordinates[[3]][1,2],
                                               x@axes_coordinates[[3]][2,2]),
                                         z = c(x@axes_coordinates[[3]][1,3],
                                               x@axes_coordinates[[3]][2,3]),
                                         lwd=3,...)
                       }

                     }

                   })

#' @rdname plot-ellipsoid
#' @param which_overlaps Plot all overlaps or just the global intersection
#' @param new_window Open a new plot device
#' @export
methods::setMethod(f = "plot",
                   signature = c(x="stack_metrics",y="missing"),
                   function(x,y=NA,col=NULL,which_overlaps="all",new_window=TRUE,lwd_axes=2,lty_axes=2,semiaxes=FALSE,...){
                     if(is.null(col)){
                       col <-  c("#1B9E77","#D95F02","#7570B3",
                                 "#E7298A","#66A61E","#E6AB02",
                                 "#A6761D","#666666","#ff7f00")

                     }

                     # Pair to pair comparisons
                     #comparisons <-names(x@geographic_intersection)[-1]
                     enames <- names(x@ellipsoids_metadata@ellipsoids)
                     comparisons <- utils::combn(enames,2)
                     colorss <- utils::combn(col[1:length(x@ellipsoids_metadata@ellipsoids)],2)

                     if(ncol(x@env_data) == 2){
                       ee <- x@ellipsoids_metadata@ellipsoids
                       xy <- seq_along(ee) %>% purrr::map_df(function(y){
                         radius <- ee[[y]]@radius
                         r <- ee[[y]]@sigma[1, 2]
                         theta <- seq(0,2*pi,by=0.005)
                         scale <- sqrt(c(ee[[y]]@sigma[1, 1],
                                         ee[[y]]@sigma[2,2]))
                         if (scale[1] > 0) r <- r/scale[1]
                         if (scale[2] > 0) r <- r/scale[2]
                         r <- min(max(r,-1),1)
                         d <- acos(r)
                         xx <- radius * scale[1] * cos(theta + d/2) + ee[[y]]@centroid[1]
                         yy <- radius * scale[2] * cos(theta - d/2) + ee[[y]]@centroid[2]
                         data.frame(x=xx,y=yy)
                       })
                       grDevices::dev.new()
                       plot(x@env_data[x@global_intersection_ids,],
                            col="#3c99dc",xlim=range(xy$x),
                            ylim=c(min(xy$y),max(xy$y)),
                            main="Global intersection",...)
                       lapply(seq_along(x@ellipsoids_metadata@ellipsoids), function(y){
                         plot(x@ellipsoids_metadata@ellipsoids[[y]],col=col[y],
                              semiaxes=semiaxes,add=TRUE)
                         return(invisible())
                       })

                       bs_plots <- lapply(seq_along(x@intersection_ids), function(z){
                         grDevices::dev.new()
                         graphics::par(xpd=TRUE)
                         plot(x@env_data[x@intersection_ids[[z]],],
                                     col="#3c99dc",xlim=range(xy$x),
                              ylim=c(min(xy$y),max(xy$y)),...)
                         plot(x@ellipsoids_metadata@ellipsoids[[comparisons[1,z]]],
                              col=colorss[1,z],semiaxes=semiaxes,add=TRUE,...)
                         plot(x@ellipsoids_metadata@ellipsoids[[comparisons[2,z]]],
                              col=colorss[2,z],semiaxes=semiaxes,add=TRUE,...)
                         graphics::legend("topright", legend = c(comparisons[1,z],
                                                              comparisons[2,z]),
                                          inset=c(0.2,-0.1),lwd=2.5,
                                          lty = 1, col = c(colorss[1,z],colorss[2,z]),
                                          cex=1.5,horiz = T,bty = "n")
                         #grDevices::dev.off()
                         return(invisible())
                       })

                     }

                     if(ncol(x@env_data) == 3){
                       varnames <- names(x@ellipsoids_metadata@ellipsoids[[1]]@centroid)
                       if(which_overlaps %in% c("all",1)){
                         # Global intersection
                         if(new_window) {
                           rgl::open3d()
                           rgl::par3d(windowRect = c(20, 30, 800, 800))
                         }
                         rgl::plot3d(x@env_data[x@global_intersection_ids,],
                                     xlab=varnames[1],
                                     ylab=varnames[2],
                                     zlab=varnames[3],
                                     col="#3c99dc",...)


                         lapply(seq_along(x@ellipsoids_metadata@ellipsoids), function(y){
                           plot(x@ellipsoids_metadata@ellipsoids[[y]],col=col[y],
                                semiaxes=semiaxes,...)
                           return(invisible())
                         })
                         #rgl::bgplot3d({
                          # graphics::plot.new()
                           #title(main = 'This is the main title', line = 3,font.main=3,cex.main=2)
                           #graphics::mtext(side = 1, 'Global Intersection', line = 4,cex=4)
                           # use here any other way you fancy to write your title
                         #})

                         rgl::legend3d("topright", legend = unique(as.vector(comparisons)),
                                       pch = 20, col = col,
                                       cex=1.5, inset=c(0.02),...)
                       }
                       if(which_overlaps != 1){
                         rgl_plots <- lapply(seq_along(x@intersection_ids), function(z){
                           rgl::open3d()
                           rgl::par3d(windowRect = c(20, 30, 800, 800))
                           rgl::plot3d(x@env_data[x@intersection_ids[[z]],],
                                       xlab=varnames[1],
                                       ylab=varnames[2],
                                       zlab=varnames[3],
                                       col="#3c99dc",...)
                           plot(x@ellipsoids_metadata@ellipsoids[[comparisons[1,z]]],
                                col=colorss[1,z],semiaxes=semiaxes,...)
                           plot(x@ellipsoids_metadata@ellipsoids[[comparisons[2,z]]],
                                col=colorss[2,z],semiaxes=semiaxes,...)
                           rgl::legend3d("topright", legend = c(comparisons[1,z],
                                                                comparisons[2,z]),
                                         pch = 20, col = c(colorss[1,z],colorss[2,z]),
                                         cex=1.5, inset=c(0.02),...)

                           return(invisible())
                         })
                       }

                     }

                   })
