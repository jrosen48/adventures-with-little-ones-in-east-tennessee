# from: https://www.tylermw.com/adding-open-street-map-data-to-rayshader-maps-in-r/

library(elevatr)
library(raster)
library(sf)
library(rayshader)

convert_coords = function(lat,long, from = CRS("+init=epsg:4326"), to) {
    data = data.frame(long=long, lat=lat)
    coordinates(data) <- ~ long+lat
    proj4string(data) = from
    #Convert to coordinate system specified by EPSG code
    xy = data.frame(sp::spTransform(data, to))
    colnames(xy) = c("x","y")
    return(unlist(xy))
}

lat_range = c(35.577226, 37.629084)
long_range = c(-83.823413, -83.971876)

utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs("+init=epsg:4326"))
utm_bbox

get_elev_raster()

#bryce = raster("/Users/joshuarosenberg/little-kids-big-adventures/data/Bryce_Canyon_GeoTIFF/Bryce_Canyon.tif")
#bryce_mat = raster_to_matrix(bryce)
# bryce_small <- resize_matrix(bryce_mat, 0.25)

mt_carrigain <- data.frame(x = -83.9357147, y = 35.6123093)
mt_carrigain <- st_as_sf(mt_carrigain, coords = c("x", "y"), crs = 4326)
elev <- get_elev_raster(mt_carrigain, z = 14)
nh_elmat <- raster_to_matrix(elev)
bryce_small = resize_matrix(nh_elmat,0.25)

base_map <- bryce_small %>% 
    height_shade() %>% 
    add_overlay(sphere_shade(bryce_small,
                             zscale=4, colorintensity = 5), alphalayer=0.5) %>%
    add_shadow(lamb_shade(bryce_small,zscale=6), 0) %>%
    add_shadow(ambient_shade(bryce_small), 0) %>%
    add_shadow(texture_shade(bryce_small,detail=8/10,contrast=9,brightness = 11), 0.1) %>%
    plot_map()

lat_range = c(37.614998, 37.629084)
long_range = c(-112.174228, -112.156230)

utm_bbox = convert_coords(lat = lat_range, long=long_range, to = crs(bryce))
utm_bbox


extent_zoomed = extent(utm_bbox[1], utm_bbox[2], utm_bbox[3], utm_bbox[4])
bryce_zoom = crop(bryce, extent_zoomed)
bryce_zoom_mat = raster_to_matrix(bryce_zoom)

base_map = bryce_zoom_mat %>% 
    height_shade() %>%
    add_overlay(sphere_shade(bryce_zoom_mat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
    add_shadow(lamb_shade(bryce_zoom_mat), 0) %>%
    add_shadow(ambient_shade(bryce_zoom_mat),0) %>% 
    add_shadow(texture_shade(bryce_zoom_mat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)