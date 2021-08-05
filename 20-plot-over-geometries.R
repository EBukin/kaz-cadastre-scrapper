# Overlaying polygons with the land types.

library(pacman)
pacman::p_load(sf, here, fs, tidyverse, rgdal, glue, rmapshaper)

source("R/find-plots-calls-processors.R")


# Loading data ----------------------------------------------------------------

# Rayon geometries
ray_geom <-
  read_rds("data-clean/02-rayon-shapes/kaz-rayons-shapes_2021-03-31-13-24-01.rds") %>% 
  filter(raj_rus %in% c("Enbeksikazahskij", "Kegenskij", "Rajymbekskij")) %>% 
  select(raj_rus )%>% 
  # st_transform(st_crs(3857)) %>%
  st_make_valid()


# Plots data
plot_geoms <- 
  read_rds("./data-clean/10.1-sarah-request-clean/plot-geom-api-meta.rds") %>%
  st_transform(st_crs(3857)) %>%
  st_make_valid()

    # filter(area_ha > 0.05)

  
# mapview::mapview(plot_geoms)
# plot_geoms %>% 
#   ggplot + 
#   geom_sf()

# Ugodia types and unharvested ugodia types ------------------------------------

ugod_types <-
  tribble(
    ~ USL, ~ ugodia,
    '71732000', "Cropland",
    '71723000', "Cropland", 
    '71722000', "Cropland",
    '71162000', "Cropland",
    '71721000', 'Marginal cropland',
    '71734000', "Hayfield",
    '71735000', "Pasture",
    '71736000', "Pasture on slopes",
    '72110000', "Rocky areas",
    '72120000', "Rocky areas",
    '72600000', "Rocky areas",
    '71111000', "Forests",
    '22520000', 'Permanent snow',
    '22422000', 'Permanent snow',
    '22160000', 'Permanent snow',
    '22171000', 'Permanent snow',
    '22121000', 'Permanent snow',
    '42100000', 'Buildup area',
    '41300000', 'Buildup area',
    '71727000', 'Buildup area'
  )

type_geoms <-
  read_rds("./data-clean/10.1-sarah-request-clean/ugodia-geom.rds") %>%
  select(USL) %>%
  st_transform(st_crs(3857)) %>%
  left_join(ugod_types) %>%
  mutate(ugodia = ifelse(is.na(ugodia), "Others", ugodia),
         ugodia = str_to_lower(ugodia) %>% str_replace_all("[^p[:alnum:]]", "_")) %>%
  group_by(ugodia) %>%
  summarise(n = sum(row_number())) %>%
  select(-n) %>%
  st_make_valid()
# 
# unharvested_types <- 
#   ray_geom %>% 
#   st_union %>%
#   st_difference(type_geoms %>% st_union) %>%
#   st_as_sf() %>%
#   mutate(ugodia = "others_nonharvested") %>% 
#   rename(geometry = x) 
# 
# ugodia_type_all <- 
#   type_geoms %>% 
#   bind_rows(unharvested_types)
# 
# unharvested_types %>% 
#     write_rds("./data-clean/10.1-sarah-request-clean/ugodia-geom-unharvested.rds",
#               compress = "gz")
# ugodia_type_all %>%
#   write_rds("./data-clean/10.1-sarah-request-clean/ugodia-geom-all-groups-with-unharvested.rds",
#             compress = "gz")

ugodia_type_all <- read_rds("./data-clean/10.1-sarah-request-clean/ugodia-geom-all-groups-with-unharvested.rds")
unharvested_types <- read_rds("./data-clean/10.1-sarah-request-clean/ugodia-geom-unharvested.rds")

# ugodia_type_all %>%
#   st_write(
#     here("data-clean", "10.1-sarah-request-clean",
#          "ugodia-geom-all-groups-with-unharvested-shp",
#          "ugodia-geom-all-groups-with-unharvested.shp"),
#     delete_layer = TRUE
#   )
# 
# unharvested_types %>%
#   st_write(
#     here("data-clean", "10.1-sarah-request-clean",
#          "ugodia-geom-unharvested-shp",
#          "ugodia-geom-unharvested.shp"),
#     delete_layer = TRUE
#   )


# # Checking how the all ugodia types add up to all land.
# # Small difference exists still. About 0.0033%
# ugodia_type_all %>% 
#   mutate(area = st_area(.)) %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   summarise(area = sum(area))
# 
# ray_geom %>% 
#   mutate(area = st_area(.)) %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   summarise(area = sum(area))


# Finding un-allocated land in rayons ---------------------------------------------

# library(rmapshaper)
# 
# unallocated_land <- 
#   seq(1, nrow(ray_geom)) %>% 
#   map_dfr(~{
#     targ <- ray_geom %>% slice(.x) 
#     targ %>%
#       ms_erase(plot_geoms %>%
#                  filter(rayon_rus == targ$raj_rus) %>%
#                  st_make_valid())
#   })
# unallocated_land
# unallocated_land %>% 
#   ggplot + 
#   aes(fill = raj_rus) + 
#   geom_sf()
# write_rds(unallocated_land, "data-raw/20-unallocated-land.rds", compress = "gz")

unallocated_land <- read_rds("data-raw/20-unallocated-land.rds")

# mapview::mapview(unallocated_land)




# Overlaying polygons with the geometries and saving this info. ---------------

# # # Takes about 5h
# polysubset <-
#   plot_geoms %>%
#   # slice(1:100) %>% 
#   select(cadastre_id, rayon_rus ) %>%
#   mutate(plot_area = st_area(geometry)) %>%
#   st_intersection(ugodia_type_all) %>%
#   mutate(ugod_area = st_area(.))
# 
# polysubset %>% 
#   write_rds("data-raw/20-plot-geoms-ugod-all-2021-07-20.rds", compress = "gz")


# Calculating other unharvested land":
polysubset <- read_rds("data-raw/20-plot-geoms-ugod-all-2021-07-20.rds")

# polysubset %>%
#   write_rds(
#     here("data-clean", "10.1-sarah-request-clean",
#          "plot-fractions-ugod-all-geom.rds"),
#     compress = "gz"
#   )
# polysubset %>%
#   st_write(
#     here("data-clean", "10.1-sarah-request-clean",
#          "plot-fractions-ugod-all-geom-shp",
#          "plot-fractions-ugod-all-geom.shp"),
#     delete_layer = TRUE
#   )


polysubset_wide <-
  polysubset %>%
  select(cadastre_id, rayon_rus, plot_area, ugodia, ugod_area ) %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  pivot_wider(names_from = ugodia, values_from = ugod_area, names_prefix = "ugod_") %>%
  arrange(desc(plot_area)) %>%
  mutate(all_harvested = dplyr::select(., 4:length(.)) %>% rowSums(na.rm = T)) %>%
  mutate(ugod_others_nonharvested = round(as.numeric(plot_area) - all_harvested, 3),
         ugod_others_nonharvested = ifelse(ugod_others_nonharvested < 0, 0, ugod_others_nonharvested)) %>%
  mutate(across(plot_area:ugod_others_nonharvested, ~as.numeric(.))) 

plot_geoms %>%
  left_join(polysubset_wide) %>%
  select(cadastre_id, rayon_rus, plot_area, contains("ugod_"), everything()) %>%
  # filter(is.na(plot_area)) %>%
  write_rds(
    here("data-clean", "10.1-sarah-request-clean", "plot-geom-ugod-areas.rds"),
    compress = "gz"
  )

plot_geoms %>%
  left_join(polysubset_wide) %>%
  select(cadastre_id, rayon_rus, plot_area, contains("ugod_"), everything()) %>%
  st_write(
    here("data-clean", "10.1-sarah-request-clean", "plot-geom-ugod-areas-shp",
         "plot-geom-ugod-areas.shp"),
    delete_layer = TRUE
  )

plot_geoms %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  left_join(polysubset_wide) %>%
  select(cadastre_id, rayon_rus, plot_area, contains("ugod_"), everything()) %>%
  # filter(is.na(plot_area)) %>%
  write_csv(
    here("data-clean",
         "10.1-sarah-request-clean",
         "plot-geom-ugod-areas.csv")
  )

# %>%
#   bind_rows(
#     plot_geoms %>%
#       select(cadastre_id, rayon_rus) %>%
#       filter(!cadastre_id  %in% polysubset_wide$cadastre_id) %>%
#       mutate(plot_area = as.numeric(st_area(geometry))) %>%
#       st_drop_geometry() %>%
#       as_tibble() %>%
#       mutate(ugod_others_nonharvested = as.numeric(plot_area))
#   )

polysubset %>%
    select(cadastre_id, rayon_rus, plot_area, ugodia, ugod_area ) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    pivot_wider(names_from = ugodia, values_from = ugod_area, names_prefix = "ugod_")



# %>%
# %>% 
#   bind_rows(
#     plot_geoms %>% 
#       filter(polysubset_wide$)
#   )

# 
# st_intersection(polysubset, type_geoms ) %>% 
#   mutate(ugod_area = st_area(.)) 



# 
# 
# # Getting Open street map city boundaties -------------------------------------
# 
# # Help here: https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html#5_Additional_Functionality
# # And here: https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
# # 
# # # # A bot difficult for now. 
# # # if(!require("osmdata")) install.packages("osmdata")
# # # if(!require("ggmap")) install.packages("ggmap")
# # # library(osmdata)
# # # library(ggmap)
# # # library(tmap)
# # 
# # available_features() 
# # available_tags("boundary")
# # 
# # boundaries <-
# #   opq(bbox = 'Almaty') %>%
# #   add_osm_feature(key = 'boundary', value = "postal_code") %>% 
# #   osmdata_sf %>%
# #   unique_osmdata
# # boundaries
# # 
# # tmap_mode('view')
# # qtm(boundaries$osm_lines )
# 
# # Finding allocated land in rayons ---------------------------------------------
# 
# # Step 1.0 Filterig out extremely small polygons ------------------------------
# 
# plot_geoms %>% 
#   st_drop_geometry() %>% 
#   as_tibble() %>% 
#   filter(area_ha > 0.05) %>% 
#   ggplot() + 
#   aes(x = area_ha ) +
#   geom_histogram(bins = 100) + 
#   scale_x_log10(labels = scales::label_comma(0.1)) +
#   annotation_logticks(sides = "b") +
#   theme_bw()
# 
# plot_geoms$area_ha %>% psych::describe()
# 
# # Step 1.1 Merging nearby polygons into one ------------------------------------
# 
# 
# # 1.1.1 Finding nearest distances between the polygons -------------------------
# 
# # # Example for one kvartal
# # one_kvartal <- 
# #   plot_geoms  %>% 
# #   # st_drop_geometry() %>% 
# #   # as_tibble() %>% 
# #   select(cadastre_id, rayon_rus , kvartal_id ) %>% 
# #   filter(rayon_rus == "Rajymbekskij", kvartal_id == "006")
# # 
# # one_kvartal_buffered <- 
# #   one_kvartal %>%
# #   group_by(kvartal_id ) %>% 
# #   ungroup() %>% 
# #   st_buffer(dist = 50)
# # 
# # 
# # # Creating all overlaps
# # inc <- st_intersects(one_kvartal_buffered, one_kvartal_buffered, sparse = F)
# # colnames(inc) <- one_kvartal_buffered$cadastre_id
# # rownames(inc) <- one_kvartal_buffered$cadastre_id
# # 
# # # Converting all overlaps into a graph
# # groups_members <- 
# #   # ifelse(inc, 1, 0) %>%
# #   inc %>% 
# #   graph_from_incidence_matrix() %>%
# #   components() %>% 
# #   igraph::groups()  %>% 
# #   imap_dfr(~tibble(cadastre_id = unique(.x), group = .y)) 
# # 
# # 
# # # Joining groups to main not-buffered data
# # multipol_grouped <- 
# #   one_kvartal %>% 
# #   left_join(groups_members) %>% 
# #   group_by(group) %>% 
# #   summarise(n = n()) 
# # 
# # # Closing any existing gaps
# # # https://github.com/r-spatial/sf/issues/547#issuecomment-343694597
# # 
# # multipol_grouped %>% plot() 
# # 
# # 
# # library(lwgeom)
# # 
# # multipol_grouped %>%
# #   st_snap(x = ., y = ., tolerance = 100) %>% 
# #   st_make_valid() %>% 
# #   st_union() %>% plot
# # 
# # 
# # multipol_grouped %>%
# #   mutate(area = st_area(.)) %>% 
# #   st_buffer(50) %>% 
# #   mutate(area2 = st_area(.)) %>%
# #   mutate(diff = area2 - area)
# #   st_difference(., .) %>%
# #   st_combine() %>%
# #   st_union(by_feature = T) %>%
# #   plot()
# 
# # 
# # st_union(multipol_grouped, by_feature = T) %>% plot
# # st_combine(multipol_grouped) %>% plot
# # 
# # one_kvartal_ids <- 
# #   one_kvartal %>% 
# #   st_drop_geometry() %>%
# #   as_tibble() %>%
# #   select(cadastre_id)
# #   
# # 
# # sparse_touch <- 
# #   one_kvartal %>% 
# #   st_relate(one_kvartal)
# # 
# # sparse_touch %>%  
# #   map2_df(seq_along(.), ~tibble(id = .y, group = str_c(.x, collapse = ","), len = length(.x))) %>% 
# #   arrange(desc(len)) %>% 
# #   group_by(group, len) %>% 
# #   count
# # 
# # sparse_touch_orer <-
# #   sparse_touch %>% 
# #   # set_names(seq_along(.)) %>% 
# #   map2_df(seq_along(.), ~tibble(len = length(.x), ele = list(id = .y, ele = .x))) %>% 
# #   arrange(desc(len)) 
# #   
# #   
# #   %>% 
# #     sort(sparse_touch_orer$len, decreasing = T)
# #   `[`(sort(.$len, decreasing = T))
# #   sort(decreasing = T)
# # 
# # sparse_touch[names(sparse_touch_orer) %>% as.numeric()] 
# # 
# # list(one_kvartal)
# # scapce_touch_matrix_to_reduce %>% 
# #   reduce()
# # 
# # 
# # 
# # spare_mtx_rook <- poly2nb(as(nc, "Spatial"), queen = FALSE)
# 
# # 1.1.1 Simplifyong polygons before findin empty spaces -----------------------
# 
# one_kvartal <-
#   plot_geoms  %>%
#   # st_drop_geometry() %>%
#   # as_tibble() %>%
#   select(cadastre_id, rayon_rus , kvartal_id ) %>%
#   filter(rayon_rus == "Rajymbekskij", kvartal_id == "001")
# 
# 
# one_kvartal_simple <-
#   one_kvartal %>% 
#   rmapshaper::ms_simplify(keep = 0.5, keep_shapes = T, snap_interval = 10)
# 
# 
# object.size(one_kvartal_simple) / object.size(one_kvartal)
# 
# plot(one_kvartal_simple)
# 
# # unallocated <-
#   ray_geom %>%
#   filter(raj_rus == "Rajymbekskij") %>% 
#   as.list()
#   # slice(1:100) %>% 
#   mutate(ray_area = st_area(.)) %>%
#   st_difference(one_kvartal_simple)
# 
# plot(unallocated)
# 
# 
# rmapshaper::ms_dissolve()
# 
# 
# 
# # THinking how to loop intersections
# 
# kvartals <-
#   plot_geoms  %>%
#   select(cadastre_id, rayon_rus , kvartal_id ) 
# 
# kvartals_simplified <-
#   kvartals %>% 
#   rmapshaper::ms_simplify(keep = 0.05, keep_shapes = T, snap_interval = 100) %>% 
#   group_by(kvartal_id) %>% 
#   nest() %>% 
#   pull(data) 
# 
# 
# simple_ray <- 
#   ray_geom %>%
#   filter(raj_rus == "Rajymbekskij") %>% 
#   list(.) %>% 
#   append(kvartals_simplified) %>% 
#   `[`(1:10)
# 
# library(rmapshaper)
# 
# unallocated_land <- 
#   ray_geom %>%
#   # filter(raj_rus == "Rajymbekskij") %>%
#   ms_erase(plot_geoms) 
# 
# unallocated_land %>% 
#   plot()
# 
# 
# 
#   geojson_sp() %>% # Convert to SpatialPolygonsDataFrame
#   plot(col = "blue")
# 
# 
# pb <- progress::progress_bar$new(total = length(simple_ray), 
#                                  format = ":current / :total in :elapsed [:bar] :percent ETA: :eta")
# unallocated_ray <- 
#   simple_ray %>% 
#   reduce(~{
#     out <- 
#       .x %>% 
#       st_difference(.y)
#     
#     browser()
#     
#     pb$tick()
#     out
#   })
# 
# # 1.1.2 Coining smalles polygons by distance -----------------------------------
# 
# 
# 
# library(rmapshaper)
# 
# unallocated_land <- 
#   ray_geom %>%
#   # filter(raj_rus == "Rajymbekskij") %>%
#   ms_erase(plot_geoms) 
# 
# unallocated_land_poly <-
#   unallocated_land %>% 
#   st_cast("POLYGON") %>% 
#   mutate(area = st_area(.) %>% as.numeric()) %>% 
#   filter(area > 0.5)
# 
# 
# library(mapview)
# 
# unallocated_land_poly %>% mapview::mapview()
# 
# 
# %>%
#   ggplot() + 
#   aes(x = area ) +
#   geom_histogram(bins = 100) + 
#   scale_x_log10(labels = scales::label_comma(0.1)) +
#   # annotation_logticks(sides = "b") +
#   theme_bw()
# 
# 
# # unallocated_land_poly %>% 
#   ggplot(ray_geom) + 
#   geom_sf(data = ray_geom, aes(fill = raj_rus), alpha = 0.1) + 
#   geom_sf(data = unallocated_land_poly) 
# 
# 
# unallocated <-
#   ray_geom %>%
#   slice(1:100) %>% 
#   mutate(ray_area = st_area(.)) %>%
#   st_difference(plot_geoms)


