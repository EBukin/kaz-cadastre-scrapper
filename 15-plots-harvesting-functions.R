# Plots harvester




library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE

source("R/find-plots-calls-processors.R")



# Data to start from -*-------------------------

four_level_digits_clean <-
  list.files("data-raw/03-kvartal-1-dig-plot-03", full.names = T) %>%
  map_dfr(read_rds) %>% 
  mutate(base_digit = str_c(base_digit, new_digit)) %>% 
  select(leyer_id : base_digit ) %>% 
  distinct() 

four_level_digits_clean %>% 
  glimpse()




# Trying with on response --------------------------

find_plots_call_generic <- 
  "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{rayon_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"


set.seed(12114)

find_plots_call <-
  four_level_digits_clean %>%
  sample_n(10) %>%
  mutate(new_digit = 0,
         one_call = glue(find_plots_call_generic)) %>% 
  pull(one_call)




# Actual harvesting =======================================================

interm_plot_fldr <- "data-raw/05-SR-request/04-kvartal-3-dig-plots-geometries"

harvested <- list.files(here(interm_plot_fldr))


four_level_digits_clean <-
  list.files("data-raw/03-kvartal-1-dig-plot-03", full.names = T) %>%
  map_dfr(read_rds) %>% 
  mutate(base_digit = str_c(base_digit, new_digit)) %>% 
  select(leyer_id : base_digit ) %>% 
  distinct() 

four_level_digits_clean %>% 
  glimpse()

lines_to_harvest <- 
  four_level_digits_clean %>% 
  mutate(
    file_to =
      str_c(obl_id, rayon_id, base_digit, sep = "-") %>%
      str_c(., ".rds")
  ) %>% 
  filter(!file_to %in% harvested)


geometry_call <- 
  "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{rayon_id}{base_digit}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={leyer_id}&returnGeometry=true&f=pjson"





parallel_harvest <- function(lines_to_harvest, interm_plot_fldr) {
  
  p <- progressor(steps = nrow(lines_to_harvest))
  
  # Non parallel harvest
  lines_to_harvest %>%
    future_pwalk(~ {
      one_line <- rlang::dots_list(...) %>% as_tibble()
      line_file <- here(interm_plot_fldr, one_line$file_to)
      line_file_incomplete <-
        here(interm_plot_fldr, "incomplete", one_line$file_to)
      line_geometries <-
        0:9 %>%
        map_dfr(~ {
          one_line %>%
            mutate(new_digit = .x) %>%
            mutate(url = glue(geometry_call)) %>%
            pull(url) %>%
            get_res_geo_attrs
        })
      
      p()
      
      if (all(
        line_geometries$response_n_elements ==
        line_geometries$response_n_elements_success
      )) {
        line_geometries %>%
          write_rds(line_file, compress = "gz")
      } else {
        write_rds(line_file_incomplete, compress = "gz")
        
      }
      
    })
  
}

# library(progressr)
# library(furrr)
# handlers(global = TRUE)
# 
# handlers(list(
#   handler_progress(
#     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
#     width    = 80,
#     complete = "+"
#   )
# ))
# plan(multisession)
# parallel_harvest(lines_to_harvest, interm_plot_fldr = interm_plot_fldr)



# Loading all harvested geometries and saving in one file -----------------

three_dig_harvest <- 
  list.files(interm_plot_fldr, pattern = "rds", full.names = T) %>% 
  map_dfr(read_rds)

# Checing those that have 0 observations (invalid digits)
# three_dig_harvest %>% 
#   filter(!response_success) %>% 
#   sample_n(10) %>% 
#   pull(response_url)

exceeded_responces <- 
  three_dig_harvest%>% 
  filter(response_success) %>% 
  filter(exceededTransferLimit) %>% 
  select(response_geo_attrs ) %>% 
  unnest(response_geo_attrs )


interm_plot_fldr_five <- "data-raw/05-SR-request/04-kvartal-4-dig-plots-geometries"
harvested_five <- list.files(here(interm_plot_fldr_five))


# # Uncomment this part to reharvest
# to_reharvest <-
#   exceeded_responces %>%
#   select(KAD_NOMER) %>%
#   mutate(
#     obl_id = str_sub(KAD_NOMER, 1,2),
#     rayon_id  = str_sub(KAD_NOMER, 3,5),
#     base_digit = str_sub(KAD_NOMER, 6,9),
#     extra_digit = str_sub(KAD_NOMER, 10,10)
#   ) %>%
#   select(-KAD_NOMER   ) %>%
#   distinct() %>%
#   left_join(four_level_digits_clean) %>%
#   mutate(base_digit = str_c(base_digit, extra_digit)) %>%
#   mutate(
#     file_to =
#       str_c(obl_id, rayon_id, base_digit, sep = "-") %>%
#       str_c(., ".rds")
#   ) %>%
#   filter(!file_to %in% harvested_five)

# library(progressr)
# library(furrr)
# handlers(global = TRUE)
# 
# handlers(list(
#   handler_progress(
#     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
#     width    = 80,
#     complete = "+"
#   )
# ))
# plan(multisession)
# parallel_harvest(to_reharvest, interm_plot_fldr = interm_plot_fldr_five)

# Checking another level if something was not harvested --------------------

four_dig_harvest <- 
  list.files(interm_plot_fldr_five, pattern = "rds", full.names = T) %>% 
  map_dfr(read_rds)

four_dig_harvest %>% filter(exceededTransferLimit)


interm_plot_fldr_six <- "data-raw/05-SR-request/04-kvartal-6-dig-plots-geometries"
harvested_six <- list.files(here(interm_plot_fldr_five))


# # Uncomment to re-harvest
# to_reharvest_again <-
#   four_dig_harvest%>% 
#   filter(response_success) %>% 
#   filter(exceededTransferLimit) %>% 
#   select(response_geo_attrs ) %>% 
#   unnest(response_geo_attrs ) %>%
#   select(KAD_NOMER) %>%
#   mutate(
#     obl_id = str_sub(KAD_NOMER, 1,2),
#     rayon_id  = str_sub(KAD_NOMER, 3,5),
#     base_digit = str_sub(KAD_NOMER, 6,9),
#     extra_digit = str_sub(KAD_NOMER, 10,11)
#   ) %>%
#   select(-KAD_NOMER   ) %>%
#   distinct() %>%
#   left_join(four_level_digits_clean) %>%
#   mutate(base_digit = str_c(base_digit, extra_digit)) %>%
#   mutate(
#     file_to =
#       str_c(obl_id, rayon_id, base_digit, sep = "-") %>%
#       str_c(., ".rds")
#   ) %>%
#   filter(!file_to %in% harvested_six)
# library(progressr)
# library(furrr)
# handlers(global = TRUE)
# 
# handlers(list(
#   handler_progress(
#     format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
#     width    = 80,
#     complete = "+"
#   )
# ))
# plan(multisession)
# parallel_harvest(to_reharvest_again,  interm_plot_fldr_six)



# Checking one more level of digits ----------------------------------------

five_dig_harvest <- 
  list.files(interm_plot_fldr_six, pattern = "rds", full.names = T) %>% 
  map_dfr(read_rds)

five_dig_harvest %>% filter(exceededTransferLimit)


# Combining raw_data ----------------------------------------------------

# list(
#   three_dig_harvest,
#   four_dig_harvest,
#   five_dig_harvest
# ) %>% 
#   bind_rows() %>% 
#   dplyr::filter(response_success, !exceededTransferLimit) %>% 
#   write_rds(
#     here('data-raw', "05-SR-request", "04-kvartal-3-4-5-dig-plots-geometries-raw.rds"), compress = "gz")



# Cleaning data -----------------------------------------------------------

drop_nonalphanum <- 
  . %>%
  stringi::stri_trans_general("Latin-ASCII" ) %>% 
  stringi::stri_trans_general("Any-Hex/Unicode" )  %>%
  str_replace_all("U\\+02B9", "") %>%
  stringi::stri_trans_general("Hex-Any/Unicode") %>%
  str_replace_all("[^[:alnum:]_]", "")


translit_kaz <- 
  . %>% 
  stringi::stri_trans_general(.x, "Kazakh-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  drop_nonalphanum


translit_rus <- 
  . %>% 
  stringi::stri_trans_general(.x, "Cyrillic-Latin/BGN") %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  drop_nonalphanum


raw_requests <-
  read_rds(
    here(
      'data-raw',
      "05-SR-request",
      "04-kvartal-3-4-5-dig-plots-geometries-raw.rds"
    )
  ) %>% 
  pull(response_geo_attrs ) %>% 
  bind_rows()


rayon_index <- 
  read_rds("data-clean/02-1-ray-shapes-clean-2021-03-28-07-45-13.rds") %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(leyer_id, obl_id , rayon_id, obl_name = obl_rus, ray_name = raj_rus)


clean_plots_requests <- 
  raw_requests %>% 
  mutate(obl_id = str_sub(KAD_NOMER, 1,2),
         rayon_id = str_sub(KAD_NOMER, 3,5),
         kvartal_id = str_sub(KAD_NOMER, 6,8)) %>% 
  left_join(rayon_index) %>% 
  select(KAD_NOMER, layerId, obl_id, rayon_id, kvartal_id, obl_name, ray_name, 
         server_plot_id = OBJECTID, 
         SRVGET, Shape_Length, Shape_Area, geometry ) %>% 
  mutate(Shape_Area = str_replace(Shape_Area, ",", ".") %>% as.numeric())


## Saving Shapes ---------------------------
clean_plots_requests %>% 
  write_rds(
    here("data-clean", "05-SR-request", "plots-geometry-3-rayons.rds"),
    compress = "gz"
  )

clean_plots_requests %>% 
  sf::write_sf(
    here("data-clean", "05-SR-request", "shpfl", "plots-geometry-3-rayons.shp")
  )



clean_plots_requests %>% 
  filter(Shape_Area > 100*100) %>% 
  sf::write_sf(
    here("data-clean", "05-SR-request", "shpfl_1ha", "plots-geometry-3-rayons.shp")
  )


clean_plots_requests  %>%
  st_drop_geometry() %>% 
  as_tibble() %>% 
  group_by(rayon_id) %>% 
  dplyr::count()

clean_plots_requests %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  filter(rayon_id  == "044") %>% 
  count(kvartal_id, sort = T)

clean_plots_requests %>% 
  filter(rayon_id  == "050") %>% 
  ggplot() + 
  geom_sf()

clean_plots_requests %>% 
  filter(rayon_id  == "323") %>% 
  ggplot() + 
  geom_sf()

clean_plots_requests %>% 
  filter(rayon_id  == "044", kvartal_id == "180") %>% 
  ggplot() + 
  geom_sf()


three_dig_harvest%>% 
  filter(!response_success) %>% 
  pull(response_url )
  # filter(exceededTransferLimit) %>% 
  select(response_geo_attrs ) %>% 
  unnest(response_geo_attrs )






