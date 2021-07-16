

# Setup
install.packages("pacman") # This package will install amd prepare packages in the lines below:


library(pacman)
pacman::p_load(sf, tidyverse, rgdal, mapview)
# I use sf and tidyverse for data manipulation.


# Loading data -----------------------------------------------------------------

# Use rds instead of shapefiles
# We immediately convert the csr:
plots_geoms <- 
  read_rds("./data-clean/10.1-sarah-request-clean/plot-geom-api-meta.rds") %>% 
  st_transform(st_crs(3857))


# No need to transform because it is transformed:
plots_ugods_geoms <- 
  read_rds("./data-clean/10.1-sarah-request-clean/plot-geom-ugod-areas.rds") 

st_crs(plots_ugods_geoms) == st_crs(plots_geoms)


# Analysis -----------------------------------------------------------------

# Showing data without geometries:
plots_geoms %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  glimpse


# Plotting data with colours by rayons. 
# May take some time:
plots_geoms %>% 
  filter(rayon_rus %in% c("Kegenskij")) %>% 
  ggplot() + 
  aes(fill = rayon_rus) + 
  geom_sf()



# Getting summary about ugodias types and plots---------------------------

plots_ugods_geoms %>% 
  select(cadastre_id, plot_area, contains("ugod"))


# Interactive map
# Takes time too
plots_ugods_geoms %>% 
  mapview(zcol = "rayon_rus")

