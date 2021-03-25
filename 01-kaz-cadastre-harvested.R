# kad data scrapper




# Setups ------------------------------------------------------------------

remotes::install_github("yonghah/esri2sf")
pacman::p_load(sf, here, fs, tidyverse, esri2sf, jsonlite)


# Harvesting trys:

# Root map:
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/



# # Layers list with data about layers:
# # http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/layers?f=pjson
# our_layers <- jsonlite::fromJSON("http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/layers?f=pjson")
# write_rds(our_layers, here("data-raw", "01-layers.rds"), compress = "gz")

our_layers <-
  read_rds(here("data-raw", "01-layers.rds")) %>% 
  magrittr::extract2("layers") %>% 
  flatten() %>% 
  as_tibble() %>% 
  select(currentVersion, id, name, type, contains("extent"))



# # # Legend json
# # # http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/legend?f=pjson
# # our_legend <- jsonlite::fromJSON("http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/legend?f=pjson")
# # write_rds(our_legend, here("data-raw", "01-legend.rds"), compress = "gz")
# our_legend <-
#   read_rds(here("data-raw", "01-legend.rds")) %>% 
#   magrittr::extract2("layers") %>% 
#   flatten() %>% 
#   as_tibble() 


# Tryiong one layer 
base_url <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/"
one_layer <- our_layers %>% slice(12)


# Sys.setlocale(, "ru_RU")


# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkTopo2/MapServer/34/query?f=json&where=RAION%20%3D%20%2706092%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outSR=3857

# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkTopo2/MapServer/34/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outSR=3857


# Valid call:
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkTopo2/MapServer/34/query?f=json&where=RAION%20%3D%20%2706095%27&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outSR=3857


"http://www.aisgzk.kz/aisgzk/Proxy/aisgzkTopo2/MapServer/34/query?f=json&where=RAION = '06095'&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outSR=3857" %>% 

one_layer_sf <- 
  esri2sf(url = 
            "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkTopo2/MapServer/34/query?f=json&where=RAION = '06095'&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outSR=3857")


# layerInfo <-
#   jsonlite::fromJSON(content(POST(
#     url,
#     query = list(f = "json",
#                  token = token),
#     encode = "form",
#     config = config(ssl_verifypeer = FALSE), encoding = "uncode"
#   ),
#   as = "text"))  
  
  
  
  