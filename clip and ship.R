library(sf)
library(readr)
library(readxl)
library(arcpullr)

clip_and_ship <- function(user_data, service_url, output_name, long = 'x', lat = 'y', out_spatial_file = FALSE) {
  # Determine the file extension of the user data
  file_ext <- tools::file_ext(user_data)
  
  # Initialize variables
  ud <- NULL
  ud_sf <- NULL
  sr <- 4326
  
  # Read user data
  if (file_ext == 'csv') {
    ud <- read_csv(user_data)  # Read CSV file
    ud_sf <- st_as_sf(ud, coords = c(long, lat), crs = sr)  # Create sf object with specified CRS
    
  } else if (file_ext %in% c('xls', 'xlsx', 'xlsm', 'xltx', 'xltm')) {
    ud <- read_excel(user_data)  # Read Excel file
    ud_sf <- st_as_sf(ud, coords = c(long, lat), crs = sr)  # Create sf object with specified CRS
    
  } else if (file_ext %in% c('shp', 'gpkg')) {
    ud_sf <- st_read(user_data) %>% # Read shapefile or GeoPackage into an sf object
      st_transform(., sr)  # Transform coordinate system
    
  } else {
    stop("Only .csv, .xls, .xlsx, .xlsm, .xlt, .xltm, .shp, and .gpkg files are supported")
  }
  
  url_final <- paste0(service_url, '/0')
  service_sf <- get_layer_by_point(url = url_final, geometry = ud_sf, sp_rel = 'intersects')
  
  # Spatial join the user's data with the service data
  sj <- st_join(ud_sf, service_sf, join = st_intersects)
  
  # Output the result to the specified file format
  if (!out_spatial_file) {
    out_name <- paste0(output_name, '.csv')
    write_csv(st_drop_geometry(sj), out_name)  # Save as CSV
  } else {
    out_name <- paste0(output_name, '.gpkg')
    st_write(sj, out_name, driver = 'GPKG')  # Save as GeoPackage
  }
}

### EXAMPLE

ud <- 'Test input data/RR_point.shp'
service_url <- 'https://services.arcgis.com/8df8p0NlLFEShl0r/arcgis/rest/services/County_homeownership_inequity/FeatureServer'
clip_and_ship(ud, service_url, 'output', out_spatial_file = TRUE)
