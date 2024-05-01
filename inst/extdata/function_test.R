## Not run:
bionames <- c("bio5","bio6","bio12")
de_centroid <- c(314.4205,141.8564,1767.3282)
names(de_centroid) <- bionames
de_sigma <- matrix(c(913.3171,621.1225,-1834.9222,
                     621.1225,1693.1855,6825.4082,
                     -1834.9222,6825.4082,802340.5206),
                   ncol=3,dimnames = list(bionames,bionames))
dr_centroid <- c(309.829,156.0725,1759.1617)
names(dr_centroid) <- bionames
dr_sigma <- matrix(c(1379.9633,795.3904,-3377.1604,
                     795.3904,2481.8104,20088.3235,
                     -3377.1604,20088.3235,812031.9645),
                   ncol=3,dimnames = list(bionames,bionames))
dy_centroid <- c(328.197,191.2879,1791.8939)
dy_sigma <- matrix(c(286.6837,52.6501,695.4674,
                     52.6501,775.8082,5174.0618,
                     695.4674,5174.0618,351406.8655),
                   ncol=3,dimnames = list(bionames,bionames))
sp_names <- c("D. ecaudata","D. rotundus","D. youngi")
de_edata <- overllip::ellipsoid_data(centroid = de_centroid[1:2],
                                     sigma = de_sigma[1:2,1:2],cf = 0.95)
dr_edata <- overllip::ellipsoid_data(centroid = dr_centroid[1:2],
                                     sigma = dr_sigma[1:2,1:2],cf = 0.95)
dy_edata <- overllip::ellipsoid_data(centroid = dy_centroid[1:2],
                                     sigma = dy_sigma[1:2,1:2],cf = 0.95)
ellipsoid_stack <- overllip::stack(de_edata,dr_edata,dy_edata,
                                   ellipsoid_names = sp_names)

env_rdata <- raster::getData("worldclim",var='bio', res=2.5,path="~/Desktop")
env_rdata <- env_rdata[[c("bio5","bio6")]]
rras <- overllip::stack_overlap(ellipsoid_stack = ellipsoid_stack,
                                env_data = env_rdata,
                                suitability_differences =TRUE,
                                parallel = F)
x11()
overllip::plot(rras,semiaxes=FALSE,pch=20)
raster::plot(rras@geographic_intersection)

## Not run:
bionames <- c("bio5","bio6","bio12")
de_centroid <- c(314.4205,141.8564,1767.3282)
names(de_centroid) <- bionames
de_sigma <- matrix(c(913.3171,621.1225,-1834.9222,
                     621.1225,1693.1855,6825.4082,
                     -1834.9222,6825.4082,802340.5206),
                   ncol=3,dimnames = list(bionames,bionames))
dr_centroid <- c(309.829,156.0725,1759.1617)
names(dr_centroid) <- bionames
dr_sigma <- matrix(c(1379.9633,795.3904,-3377.1604,
                     795.3904,2481.8104,20088.3235,
                     -3377.1604,20088.3235,812031.9645),
                   ncol=3,dimnames = list(bionames,bionames))
dy_centroid <- c(328.197,191.2879,1791.8939)
dy_sigma <- matrix(c(286.6837,52.6501,695.4674,
                     52.6501,775.8082,5174.0618,
                     695.4674,5174.0618,351406.8655),
                   ncol=3,dimnames = list(bionames,bionames))
sp_names <- c("D. ecaudata","D. rotundus","D. youngi")
de_edata <- overllip::ellipsoid_data(centroid = de_centroid,
                                     sigma = de_sigma,cf = 0.95)
dr_edata <- overllip::ellipsoid_data(centroid = dr_centroid,
                                     sigma = dr_sigma,cf = 0.95)
dy_edata <- overllip::ellipsoid_data(centroid = dy_centroid,
                                     sigma = dy_sigma,cf = 0.95)
ellipsoid_stack <- overllip::stack(de_edata,dr_edata,dy_edata,
                                   ellipsoid_names = sp_names)

env_data <- overllip::hypercube(ellipsoids = ellipsoid_stack,
                                n = 10000000)
rmat <- overllip::stack_overlap(ellipsoid_stack = ellipsoid_stack,
                                env_data = env_data,
                                parallel = T,ncores = 4)
overllip::plot(rmat)
#----------------------------------------------------------------------------
# Example with raster data
#----------------------------------------------------------------------------
env_rdata <- raster::getData("worldclim",var='bio', res=10,path="~/Desktop")
env_rdata <- env_rdata[[c("bio5","bio6","bio12")]]
env_rdata <- raster::stack(list.files("~/Dropbox/chelsa_/bios_2010_2016/",
                           pattern = ".tif$",full.names = T))
env_rdata <- env_rdata[[c("bio_05","bio_06","bio_12")]]
tt <- system.time(
  rras <- overllip::stack_overlap(ellipsoid_stack = ellipsoid_stack,
                                  env_data = env_rdata,
                                  suitability_differences =TRUE,
                                  parallel = TRUE)
)
overllip::plot(rras,semiaxes=TRUE)
x11()
raster::plot(rras@geographic_intersection)
raster::writeRaster(rras@geographic_intersection,filename = "~/Desktop/sobrelape_mur_",bylayer=T,format="GTiff")
## End(Not run)
