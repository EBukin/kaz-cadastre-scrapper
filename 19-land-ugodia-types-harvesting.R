# Harvesting the land types.


library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")


# Example link: 


# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkUgL/MapServer/find?searchText=7900&contains=true&searchFields=OBJECTID_1&sr=&layers=0&layerDefs=&returnGeometry=true&maxAllowableOffset=&geometryPrecision=&dynamicLayers=&returnZ=false&returnM=false&gdbVersion=&f=html


# Idea:

# 1. sample a random point on a polygon of one rayon
# 2. subtract returned geometry.
# 3. repeat until is done.

# Data =====================================

target_ray <- 
  here("data-clean",
       "02-rayon-shapes",
       "kaz-rayons-shapes_2021-03-31-13-24-01.rds") %>% 
  read_rds() %>% 
  filter(rayon_id %in% c("323","044","050")) 


# Call Function =======================================

find_call_generic <- 
  str_c(
    "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkUgL/MapServer/identify",
    "?f=json",
    "&tolerance=1",
    "&returnGeometry=true",
    "&returnFieldName=false",
    "&returnUnformattedValues=false",
    "&imageDisplay=359,878,96",
    "&geometry={'x':8679286.03776061,'y':5354094.948797265}",
    "&geometryType=esriGeometryPoint",
    "&sr=3857",
    "&mapExtent=8675235.749785392,5344736.823757751,8681098.868265646,5359076.149734529",
    "&layers=all:0,1,2,3"
  )


# Try harvesting one: request ==============================================

hsrvest_save_interm_poly <- function(url, interm_fldr = "~/kaz-cad-raw/land-types/", reduced = TRUE) {
  
  one_poly_geo <- 
    get_res_geo_attrs(url) %>%
    pull(response_geo_attrs) %>%
    bind_rows() 
  
  one_poly_geo %>% 
    select(layerId, cad_code_land = value, USL, USL_3, land_poly_id = OBJECTID) %>% 
    mutate(id2 = land_poly_id) %>% 
    group_by(id2) %>% 
    nest() %>% 
    pwalk(~{
      interm_file <- str_c(.x, "=", Sys.time() %>% str_replace_all("[^[0-9]]", "-"), ".rds") 
      write_rds(.y, file.path("~/kaz-cad-raw/land-types-raw/", interm_file), compress = "gz")
    })
  
  return(one_poly_geo)
  
  # interm_file <-
  #   one_poly_geo$response_geo_attrs[[1]] %>%
  #   st_drop_geometry() %>%
  #   as_tibble() %>%
  #   janitor::clean_names() %>%
  #   distinct(kadastrovyj_nomer, usl)
  # 
  # interm_file <- 
  #   str_c(
  #     str_c(unique(interm_file$kadastrovyj_nomer), collapse = "-"),
  #     "_",
  #     str_c(unique(interm_file$usl), collapse = "-"),
  #     "_",
  #     Sys.time() %>% str_replace_all("[^[0-9]]", "-"),
  #     ".rds"
  #   ) 
  # 
  # write_rds(one_poly_geo, file.path(interm_fldr, interm_file), compress = "gz")
  
  # if (!reduced) {
  #   return(one_poly_geo)
  # } else {
  #   one_poly_geo$response_geo_attrs %>%
  #     bind_rows() %>%
  #     # select(geometry) %>%
  #     # st_combine() %>%
  #     st_make_valid()
  # }
}

interm_fldr <- "~/kaz-cad-raw/land-types/"

# one_poly_geo <- get_res_geo_attrs(find_call_generic)

# interm_file <-
#   one_poly_geo$response_geo_attrs[[1]] %>%
#   st_drop_geometry() %>%
#   as_tibble() %>%
#   janitor::clean_names() %>%
#   distinct(kadastrovyj_nomer, usl) %>%
#   slice(1) %>%
#   mutate(file = str_c(kadastrovyj_nomer, "-", usl, ".rds")) %>%
#   pull(file)
# 
# write_rds(one_poly_geo, file.path(interm_fldr, interm_file), compress = "gz")

# 
# 
# # one_poly_geo$response_geo_attrs[[1]] %>% 
# #   slice(1) %>% 
# #   ggplot() + 
# #   geom_sf()
# 
# # one_poly_geo$response_geo_attrs[[1]] %>% 
# #   select(contains("USL"))
# #   ggplot() + 
# #   geom_sf() + 
# #   facet_wrap(.~layerId)
#   
#   
# one_poly_geo$response_geo_attrs[[1]] %>% 
#   filter(layerId == 2) %>% 
#   select(-layerId) %>%
#   janitor::clean_names()
#   all_equal(
#     
#     one_poly_geo$response_geo_attrs[[1]] %>% 
#       filter(layerId == 3) %>% 
#       select(-layerId) 
#   )



# Parameters of the call ===================================
point_coords <- c(8679286.03776061, 5354094.948797265)
extend_coords <- c(8675235.749785392,5344736.823757751,8681098.868265646,5359076.149734529)

ext_from_pnt <- function(pnt = c(0,0)) {
  point_coords <- c(8679286.03776061, 5354094.948797265)
  extend_coords <- c(8675235.749785392,5344736.823757751,8681098.868265646,5359076.149734529)
  c(
    pnt[1] + extend_coords[1] - point_coords[1],
    pnt[2] + extend_coords[2] - point_coords[2],
    pnt[1] + extend_coords[3] - point_coords[1],
    pnt[2] + extend_coords[4] - point_coords[2]
  ) %>% 
    str_c( collapse = ",")
}
ext_from_pnt()


find_call_str <- 
  str_c(
    "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkUgL/MapServer/identify",
    "?f=json",
    "&tolerance=1",
    "&returnGeometry=true",
    "&returnFieldName=false",
    "&returnUnformattedValues=false",
    "&imageDisplay=359,878,96",
    '&geometry={"x":_{point_xy[[1]]}_,"y":_{point_xy[[2]]}_}',
    "&geometryType=esriGeometryPoint",
    "&sr=3857",
    "&mapExtent={_{ext_from_pnt(point_xy)}_}",
    # "&layers=all:0,1,2,3"
    "&layers=all:3"
  )

# point_xy = c(8679286.03776061, 5354094.948797265)
# glue(find_call_str,  .open = "_{", .close = "}_")


# new_poly_test <- 
#   glue(find_call_str,  .open = "_{", .close = "}_") %>% 
#   hsrvest_save_interm_poly(reduced = FALSE)

# new_poly_test$response_geo_attrs



# Trying to get a new point ------------------------------------------
# Methods of getting a new point by intersection did not work well somehow.
# This is because data contains so many flows and is extremely complex to process.
# We end up:
# - Simplifying data before spatial operations
# - setting precision to 1 meter or more
# - extracting only polygons and discarding all the linestrings etc. 



# 
# # Resaving some intermid results into clear structure.
# list.files("~/kaz-cad-raw/land-types-old/", full.names = T, pattern = "rds") %>%
#   # sample(10) %>%
#   map( ~ {
#     # browser()
# 
#     # current_type <-
#       read_rds(.x) %>%
#       pull(response_geo_attrs) %>%
#       bind_rows() %>%
#       # select(layerId, cad_code_land = value, USL, USL_3, land_poly_id = OBJECTID) %>%
#       mutate(id2 = OBJECTID) %>%
#       group_by(id2) %>%
#       nest() %>%
#       pwalk(~{
#         interm_file <- str_c(.x, "=", Sys.time() %>% str_replace_all("[^[0-9]]", "-"), ".rds")
#         write_rds(.y, file.path("~/kaz-cad-raw/land-types-raw/", interm_file), compress = "gz")
#       })
#   })


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# # Harvesting Ugodias setup ------------------------------------------------------
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# 
# interm_fldr <- "~/kaz-cad-raw/land-types-raw/"
# 
# harv_in_geom <-
#   target_ray %>%
#   # slice(1) %>%
#   # rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
#   st_set_precision(1) %>%
#   st_collection_extract("POLYGON") %>%
#   st_make_valid() %>% 
#   st_buffer(1) %>% 
#   st_union() 
# 
# ggplot(st_geometry(harv_in_geom)) + geom_sf()
# 
# # # We try using metric CSR
# # # Pulkovo 1942 / CS63 zone K4
# 
# # simpify_geom <-
# #   . %>%
# #   st_collection_extract("POLYGON") %>%
# #   st_set_precision(1) %>%
# #   # rmapshaper::ms_simplify(keep = 0.2, keep_shapes = T) %>%
# #   st_make_valid()
# 
# # clean_simpify_raw_geom <-
# #   . %>%
# #   st_union() %>%
# #   select(layerId, cad_code_land = value, USL, USL_3, land_poly_id = OBJECTID) %>%
# #   # group_by(cad_code_land, USL, land_poly_id ) %>%
# #   # filter(row_number() == 1) %>%
# #   # ungroup() %>%
# #   simpify_geom()
# 
# # # Already harvested polygons
# # harvested_poly <-
# #   list.files(interm_fldr, full.names = T, pattern = "rds") %>%
# #   map_dfr( ~ read_rds(.x)) %>%
# #   st_make_valid() %>%
# #   st_collection_extract("POLYGON") %>%
# #   st_union() %>%
# #   # clean_simpify_raw_geom
# #   st_make_valid()
# # 
# # # write_rds(harvested_poly, "./data-temp/harvested-ugodia-raw-union.rds", compress = "gz")
# harvested_poly <- read_rds("./data-temp/harvested-ugodia-raw-union.rds")
# 
# # # Plotting Harvested polygons in rayons
# # harvested_poly %>%
# #   ggplot() +
# #   geom_sf() +
# #   geom_sf(data = harv_in_geom, colour = "blue", alpha = 0.2)
# 
# # Clipping polygons in one rayon of interest
# # harvested_poly_in_ray <-
# #   harvested_poly %>%
# #   st_intersection(st_make_valid(st_union(harv_in_geom)))%>%
# #   st_collection_extract("POLYGON") %>%
# #   st_make_valid()
# # 
# # # Checking area of this polygon that is harvested
# # sum(st_area(harvested_poly_in_ray)) / sum(st_area(harv_in_geom))
# # 
# # # # Plotting Harvested polygons in rayons
# # harvested_poly_in_ray %>%
# #   ggplot() +
# #   geom_sf(aes(fill = USL_3), colour = NA) +
# #   geom_sf(data = harv_in_geom, colour = "blue", alpha = 0.2)
# 
# # # Uniting polygons for clipping them out later
# # object.size(harvested_poly_in_ray) / 1000000
# # harvested_poly_united_simplified <-
# #   harvested_poly_in_ray %>%
# #   st_union()
# # object.size(harvested_poly_united_simplified) / 1000000
# 
# 
# 
# ## ## ## ## ## ## ## ## ## ## #
# ## CHANGE HERE TO GET NEW RAYON
# ## CHANGE slice()
# area_remained <-
#   harv_in_geom %>%
#   st_difference(harvested_poly) %>% 
#   st_make_valid()
# # plot(st_geometry(area_remained))
# # write_rds(area_remained, "./data-temp/harvested-ugodia-area_remained.rds", compress = "gz")
# area_remained <- read_rds("./data-temp/harvested-ugodia-area_remained.rds")
# 
# area_remained_buffered <-
#   area_remained %>%
#   st_buffer(dist = -50) %>% 
#   st_make_valid()
# 
# plot(st_geometry(area_remained_buffered))
# as.numeric(st_area(area_remained_buffered)) / sum(as.numeric(st_area(target_ray)))
# 
# 
# 
# ### CODE BLOEW HAS TO BE REWRITTEN!!!
# ### ATTENTION
# ### IT WAS USED TO DO THE HARVESTING
# ### THIS IS INSUFFICIENT NOW, HOWEVER
# area_remained_buffered %>%
#   ggplot() +
#   geom_sf( fill = "red", alpha = 0.5)
# 
# area_remain_polys <-
#   area_remained_buffered %>%
#   st_cast("POLYGON") %>%
#   st_as_sf() %>%
#   mutate(area = as.numeric(st_area(.))) %>%
#   arrange(desc(area)) %>%
#   slice(1:50)
# 
# area_remain_polys2 %>%
#   ggplot() +
#   geom_sf( fill = "red", alpha = 0.5)
# 
# 
# ## ## ## ## ## ## ##
# # Regular harvesting approach
# harvested_poly_in_ray_check <- harvested_poly
# 
# 
# i <- 1
# while (i < 15) {
# 
#   cat("Iteration ", i, "\n")
# 
# # Sampling random point -----------------------------
# is_not_in_harvested <- FALSE
# 
# while (!is_not_in_harvested) {
#   cat("new random it\n")
#   point_spat <-
#     area_remain_polys %>%
#     st_collection_extract("POLYGON") %>%
#     st_cast("POLYGON") %>%
#     st_as_sf() %>%
#     mutate(area = st_area(.)) %>%
#     arrange(desc(area)) %>%
#     slice(1:5) %>%
#     st_sample(1)
# 
#   is_not_in_harvested <- st_intersects(point_spat, harvested_poly_in_ray_check)
#   is_not_in_harvested <- !isTRUE(is_not_in_harvested[[1]] == 1)
# }
# 
# point_xy <-
#   point_spat %>%
#   as.character() %>%
#   sample(1) %>%
#   parse(text = .) %>%
#   eval()
# 
# # Getting polygon ------------------------------------
# 
# cat("Getting polygons\n")
# new_poly <-
#   glue(find_call_str,  .open = "_{", .close = "}_") %>%
#   hsrvest_save_interm_poly(reduced = T)
# 
# 
# # Simplifying poly -----------------------
# 
# cat("Siplifying polygons\n")
# # new_poly_extract <-
# #   new_poly %>%
# #   clean_simpify_raw_geom %>%
# #   filter(!land_poly_id %in% unique(harvested_poly_in_ray_check$land_poly_id))
# 
# if (nrow(new_poly) > 0) {
#   new_poly_extract <-
#     new_poly %>%
#     st_intersection(area_remain_polys) #%>%
#     # simpify_geom
# 
#   # harvested_poly_in_ray_check <- bind_rows(harvested_poly_in_ray_check, new_poly_extract)
#   # area_of_remained <- st_area(area_remain_polys)
#   # sum(st_area(new_poly_extract)) / area_of_remained
#   # Cutting poly out of harvested  -----------------------
#   cat("Area remain calcs \n")
#   area_remain_polys <-
#     area_remain_polys %>%
#     # st_buffer(dist = -500) %>%
#     st_collection_extract("POLYGON") %>%
#     st_union() %>%
#     st_set_precision(1) %>%
#     st_make_valid() %>%
#     st_difference(
#       new_poly_extract %>%
#         st_union() %>%
#         st_set_precision(1) %>%
#         st_make_valid())
# 
# }
# 
# 
# i <- i + 1
# }
# 
# ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# ## End of the harvesting loop -
# 
# area_remain_polys %>%
#   # st_buffer(dist = -500) %>%
#   ggplot() +
#   geom_sf( fill = "red") +
#   geom_sf( data = new_poly_extract, fill = "green", alpha = 0.2)



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
## Cleaning re-saving harvested geometries -------------------------------------
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


# interm_fldr <- "~/kaz-cad-raw/land-types-raw/"
ugod_raw  <-
  list.files(interm_fldr, full.names = T, pattern = "rds") %>%
  map_dfr( ~ read_rds(.x)) %>%
  mutate(area = as.numeric(st_area(.))) %>%
  select(USL, USL_3, land_poly_id, OBJECTID, area) %>%
  mutate(land_poly_id  = ifelse(is.na(land_poly_id), OBJECTID, land_poly_id)) %>%
  group_by(USL, USL_3, land_poly_id) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(-OBJECTID) %>%
  st_make_valid()

sum(as.numeric(st_area(ugod_raw))) / sum(as.numeric(st_area(target_ray)))


# Clipping ugoia in the regions ==============================================

ugod_poly_clean <-
  ugod_raw %>%
  st_intersection(
    target_ray %>%
      select(obl_id, rayon_id, name, obl_rus, raj_rus) %>% 
      st_make_valid()) %>%
  st_make_valid() %>% 
  mutate(area_ha = as.numeric(st_area(.)) / 10000) %>%
  arrange(raj_rus, desc(area_ha)) %>% 
  st_cast("MULTIPOLYGON")

ggplot() + 
  aes(fill = raj_rus) + 
  geom_sf(data = target_ray, inherit.aes = FALSE, fill = "black") +
  geom_sf(data = ugod_poly_clean, color = NA) 


ugod_poly_clean %>%
  write_rds(
    here(
      "data-clean",
      "07-land-ugodia-types",
      "kaz-ugodia-geometries.rds"
    ),
    compress = "gz"
  )

# Saving all --------------------------------------------------------------

# ugod_poly_clean <-
#   here(
#     "data-clean",
#     "07-land-ugodia-types",
#     "kaz-ugodia-geometries.rds"
#   ) %>% 
#   read_rds()

write_rds(
  ugod_poly_clean,
  here(
    "data-clean",
    "10-sarah-request",
    "kaz-akmol-3-ray-ugodia-types.rds"
  ),
  compress = "gz"
)

write_rds(
  ugod_poly_clean,
  here(
    "data-clean",
    "10.1-sarah-request-clean",
    "ugodia-geom.rds"
  ),
  compress = "gz"
)


st_write(
  ugod_poly_clean,
  here(
    "data-clean",
    "10-sarah-request",
    "kaz-akmol-3-ray-ugodia-types-shp",
    "kaz-akmol-3-ray-ugodia-types.shp"
  ),
  delete_layer = TRUE
)


st_write(
  ugod_poly_clean,
  here(
    "data-clean",
    "10.1-sarah-request-clean",
    "ugodia-geom-shp",
    "ugodia-geom.shp"
  ),
  delete_layer = TRUE
)


# Plotting some aspects ---------------------------------------------------


ugod_poly_clean %>% 
  mutate(area_ha = as.numeric(st_area(.)) / 10000) %>% 
  st_drop_geometry() %>%
  as_tibble() %>%
  arrange(desc(area_ha)) %>% 
  mutate(
    USL_3 = USL_3 %>% as_factor() %>% fct_reorder2(area_ha, area_ha, .desc = T)
  ) %>% 
  ggplot() +
  aes(x = USL_3, y = area_ha, fill = USL_3 ) +
  geom_col() + 
  facet_wrap(. ~ raj_rus, scales = "free_x") 






# 
# # if (!st_is_valid(area_remained))
# #   area_remained <- area_remained %>% st_make_valid()
# 
# 
# ## ## ## ## ## ## ## ## 
# # Making a reducer
# 
# n_it <- 5
# 
# pb <- progress::progress_bar$new(
#   format = ":spin :elapsedfull [:bar] :percent eta: :eta",
#   total = n_it + 1)
# 
# 
# remaining_polygons <- 
#   seq(1, n_it, 1) %>% 
#   as.list() %>% 
#   prepend(
#     list(area_remained)
#   ) %>%   
#   reduce2(seq_along(seq(1, n_it, 1)), ~ {
#     
#     point_xy <- 
#       st_sample(.x, 1) %>% 
#       as.character() %>% 
#       parse(text = .) %>% 
#       eval()
#     
#     pb$tick()
#     
#     new_poly <- 
#       glue(find_call_str,  .open = "_{", .close = "}_") %>% 
#       hsrvest_save_interm_poly(reduced = T)
#     
#     out <- try({
#       .x %>%
#         st_union() %>% 
#         st_difference(st_union(new_poly))
#     })
#     
#     if ("try-error" %in% class(out)) {
#       .x <- .x %>%
#         st_union() %>% 
#         st_difference(st_union(new_poly) %>% 
#                         st_make_valid()) %>%
#         st_make_valid()
#     } else {
#       .x <- out
#     }
#     
#     # if (!st_is_valid(.x)) .x <- .x %>% st_make_valid()
#     .x
#   })
# 
# 
# 
# 
# poly_list <- 
#   harvested_poly %>% 
#   bind_rows() %>% 
#   mutate(rowid = row_number() %/% 50) %>% 
#   group_by(rowid) %>% 
#   nest() %>% 
#   pull(data)
# 
# 
# # test_dif <- 
# #   target_ray_space %>% 
# #   sf::st_difference(poly_list[1200:1250] %>% bind_rows())
# 
# 
# library(progress)
# pb <- progress::progress_bar$new(
#   format = ":spin :elapsedfull [:bar] :percent eta: :eta",
#   total = length(poly_list) + 3)
# 
# 
# remaining_poly <-
#   target_ray_space %>% 
#   # st_set_precision(0.000001) %>% 
#   list(.) %>%
#   append(poly_list) %>%
#   reduce2(seq_along(poly_list), ~ {
#     # if (..3 %% 5 == 0)
#     #   .x <- .x %>% st_make_valid()
#     # browser()
#     pb$tick()
#     # .y <-.y %>% #st_set_precision(0.0001) %>%
#     #   st_buffer(0.00001)
#     sf::st_difference(.x, .y) %>% st_make_valid()
#   })
# 
# 
# remaining_poly %>%
#   ggplot() +
#   geom_sf()
# 
# 1:25 %>% 
#   as.list() %>% 
#   prepend(list(remaining_poly)) %>% 
#   reduce(~{
#     browser()
#     
#     
#   })
# 
# point_xy <- 
#   st_sample(.x, 1) %>% 
#   as.character() %>% 
#   parse(text = .) %>% 
#   eval()
# 
# new_poly <- 
#   glue(find_call_str,  .open = "_{", .close = "}_") %>% 
#   hsrvest_save_interm_poly()
# 
# remaining_poly2 <- 
#   .x %>% 
#   sf::st_difference(new_poly)
# 
# remaining_poly2 %>%
#   ggplot() +
#   geom_sf()
# 
# 
# new_poly %>%
#   ggplot() +
#   geom_sf()
# 
# 
# # 
# 
# # target_ray %>% 
# #   sf::st_difference(harvested) %>% 
# #   ggplot() + 
# #   
# #   geom_sf() 
# # %>% 
# #   ggplot() + 
# #   
# #   geom_sf() 
# # 
# # 
# # 
# # harvested_poly %>% 
# #   select(geometry ) %>% 
# #   st_combine() %>% 
# #   ggplot() + 
# #   geom_sf() 
# # 
# # 
# # 
# # 
# # harvested_poly %>% 
# #   select(geometry ) %>% 
# #   st_combine() %>% 
# #   st_make_valid() %>% 
# #   ggplot() + 
# #   geom_sf() 
# #   st_is_valid()




