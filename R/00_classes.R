# R classes for overllip
# March 2021


#' S4 classes to organize ellipsoid data objects
#' @aliases emeta
#' @importFrom methods new
#' @slot centroid Centroid of the ellipsoid
#' @slot sigma A square matrix
#' @slot axes_length Length of ellipsoid axes
#' @slot axes_coordinates Axes coordinates
#' @slot volume Elliposoid volume
#' @slot cf Confidence level
#' @slot radius Radius of the ellipsoid.
#' @exportClass ellipsoid_metadata

ellipsoid_metadata <- methods::setClass(Class = "ellipsoid_metadata",
                                        slots = c(centroid="numeric",
                                                  sigma = "matrix",
                                                  axes_length = "numeric",
                                                  axes_coordinates = "list",
                                                  volume = "numeric",
                                                  cf = "numeric",
                                                  radius ="numeric"))

#' S4 classes to organize ellipsoid data objects
#' @aliases stack
#' @importFrom methods new
#' @slot ellipsoids A list with ellipsoid metadata
#' @exportClass ellipsoid_stack

ellipsoid_stack <- methods::setClass(Class = "ellipsoid_stack",
                                     slots = c(ellipsoids="list"),
                                     validity = function(object){
                                       lapply(object, function(x){
                                         if(!inherits(x,"ellipsoid_metadata")){
                                           stop(paste("Elements should be of class",
                                                      "ellipsoid_metada"))

                                         }
                                       })
                                     })



#' S4 classes to organize ellipsoid overlap data
#' @importFrom methods new
#' @slot comparison A stack with metadata of the ellipsoids that are compared
#' @slot n_hypercube Number of points in the hypercube that contains the
#' ellipsoids being compared.
#' @slot n_ellipsoid_1 Number of points in ellipsoid 1
#' @slot n_ellipsoid_2 Number of points in ellipsoid 2
#' @slot n_union Number of points in the union of ellipsoids
#' @slot n_intersection Number of points in the intersection of both ellipsoids
#' @slot Jackard_index Jackard index
#' @slot intersection_volume Approximated volume of the intersection. The approximation is
#' estimated from Mote Carlo simulation.
#' @slot intersection_ids Indexes of points in the intersection.
#' @exportClass overlap_metrics

overlap_metrics <- methods::setClass(Class = "overlap_metrics",
                                   slots = c(comparison = "ellipsoid_stack",
                                             n_hypercube = "numeric",
                                             n_union = "numeric",
                                             n_ellipsoid_1 = "numeric",
                                             n_ellipsoid_2 = "numeric",
                                             n_intersection = "numeric",
                                             #p_intersection = "numeric",
                                             Jackard_index = "numeric",
                                             intersection_volume = "numeric",
                                             intersection_ids = "integer"))




#' S4 classes to organize ellipsoid overlap data
#' @importFrom methods new
#' @importClassesFrom raster RasterStack
#' @slot ellipsoids_metadata A stack with metadata of the ellipsoids that are compared
#' @slot env_data A matrix with environmental data values
#' @slot n_hypervolume Number of points in the hypervolume that contains the
#' ellipsoids being compared.
#' @slot n_unions Number of points in the union of ellipsoids
#' @slot n_ellipsoids Number of points in each ellipsoid
#' @slot n_intersections Number of points in the intersection of both ellipsoids
#' @slot Jackard_indices Jackard indeces
#' @slot intersection_volumes Approximated volume of the intersection. The approximation is
#' estimated from Mote Carlo simulation.
#' @slot geographic_intersection A raster stack of intersections.
#' @slot intersection_ids Indexes of points in the intersection.
#' @slot global_intersection_volume Volume of the global intersection.
#' @slot global_intersection_ids Record ids of the global intersection.
#' @exportClass stack_metrics

stack_metrics <- methods::setClass(Class = "stack_metrics",
                                      slots = c(ellipsoids_metadata = "ellipsoid_stack",
                                                env_data = "matrix",
                                                n_hypervolume = "numeric",
                                                n_unions = "numeric",
                                                n_ellipsoids = "numeric",
                                                n_intersections = "numeric",
                                                #p_intersection = "numeric",
                                                Jackard_indices = "numeric",
                                                intersection_volumes = "numeric",
                                                geographic_intersection = "RasterStack",
                                                intersection_ids = "list",
                                                global_intersection_volume ="numeric",
                                                global_intersection_ids ="numeric"))

