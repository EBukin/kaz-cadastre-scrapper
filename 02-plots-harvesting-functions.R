# Plots harvester




library(pacman)
pacman::p_load(sf, here, fs, tidyverse, stringi, jsonlite, 
               httr, RCurl, progress, rgdal, glue)
force_reharvest <- FALSE




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



# Function to get raw response behind URL
get_response_raw <- function(url) {
  one_call_raw <-
    try({
      RCurl::getURL(url)
    })
  
  if ("try-error" %in% class(one_call_raw)) {
    Sys.sleep(0.5 + runif(1, 0.1, 0.3))
    raw_get <- httr::GET(url,
                         encode = "form",
                         config = config(ssl_verifypeer = FALSE))
    one_call_raw <- content(raw_get, as = "text")
  }
  
  if ("try-error" %in% class(one_call_raw)) {
    Sys.sleep(1.5 + runif(1, 0.1, 0.3))
    raw_get <- httr::GET(url,
                         encode = "form",
                         config = config(ssl_verifypeer = FALSE))
    one_call_raw <- content(raw_get, as = "text")
  }
  
  one_call_raw
}

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# get_response_raw(cll)

# Function to transform response to a json
get_response_json <- function(one_call_raw) {
  
  out <- try({
    one_call_raw %>%
      jsonlite::fromJSON(simplifyDataFrame = F, simplifyVector = F)
  }, silent = T
  )
  
  if ("try-error" %in% class(out)) return(NULL) else return(out)
}

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw() %>%
#   get_response_json


# Function to convert rings to multipolygon
rings_to_multipoly <- 
  function(rings) {
    
    ring2matrix <- function(ring) do.call(rbind, lapply(ring, unlist))
    rings2multipoly <- function(rings)
      st_multipolygon(list(lapply(rings, ring2matrix)))
    
    getGeometry <- function(rings) {
      # browser()
      if (is.null(unlist(rings))) {
        st_multipolygon()
      } else rings2multipoly(rings)
    }
    
    getGeometry(rings = rings)
  }

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw %>%
#   get_response_json %>%
#   `[[`("results") %>%
#   `[[`(1) %>%
#   `[[`("geometry") %>% 
#   `[[`("rings") %>% 
#   rings_to_multipoly

# Extracting geometry and all other info from one element of the response
get_res_ele_geo_attrs <- function(res_ele) {
  res_ele_generic_info <-
    res_ele %>%
    keep(~ length(.x) == 1) %>%
    as_tibble()
  
  res_ele_attrs <-
    res_ele$attributes %>%
    as_tibble() %>%
    mutate(across(any_of(c(
      "Shape_Length", "Shape_Area "
    )),
    ~ str_replace_all(., ",", ".") %>%
      as.numeric()))
  
  resp_ele_geo_raw <- res_ele$geometry
  
  resp_ele_multipoly <-
    resp_ele_geo_raw$rings %>%
    rings_to_multipoly  %>%
    st_geometry %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    sf::st_set_crs(resp_ele_geo_raw$spatialReference$latestWkid)
  
  
  
  list(tibble(success_element = TRUE),
       res_ele_generic_info,
       res_ele_attrs,
       resp_ele_multipoly) %>%
    bind_cols() %>%
    st_as_sf()
}

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw %>%
#   get_response_json %>% 
#   `[[`("results") %>% 
#   `[[`(1) %>% 
#   get_res_ele_geo_attrs

# # Other URL with only one response
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411999&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw %>%
#   get_response_json %>%
#   `[[`("results") %>%
#   `[[`(1) %>%
#   get_res_ele_geo_attrs




# Function for extracting everythiong from one response
get_res_geo_attrs <- function(url) {
  
  response_raw <- 
    url %>% 
    get_response_raw
    
  response_json <- 
    response_raw %>% 
    get_response_json
  
  out_generic <- tibble(
    response_url = url,
    response_raw = response_raw,
    response_json = list(response_json),
    response_success = TRUE,
    response_n_elements = 0,
    response_n_elements_success = 0,
    exceededTransferLimit = FALSE
  )
  
  
  if(is.null(response_json$results) | length(response_json$results) < 1) {
    out_generic <- 
      out_generic %>% 
      mutate(response_success = FALSE)
    return(out_generic)
  }
  
  if (!is.null(response_json$exceededTransferLimit) &&  response_json$exceededTransferLimit) {
    out_generic <- 
      out_generic %>% 
      mutate(exceededTransferLimit = TRUE)
  }
  
  res_geo_attrs <- 
    response_json$results %>% 
    map_dfr(~{
      out <-
        try({.x %>% get_res_ele_geo_attrs
        }, silent = T)
      if ("try-error" %in% class(out)) {
        out <- tibble(success_element = FALSE)
      }
      out
    })
  
  out_generic %>% 
    mutate(
      response_n_elements = res_geo_attrs %>% nrow,
      response_n_elements_success = res_geo_attrs$success_element %>% sum(na.rm = T),
      response_geo_attrs = list(res_geo_attrs)
      )
  
}

# # Other URL with no responses
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304419999&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_res_geo_attrs
# 
# 
# # Other URL with one responses
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_res_geo_attrs

# # Another one example
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_res_geo_attrs

# # Testing for many calls
# manycals_out <- 
#   c(
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304423690&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304406140&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0305002520&contains=true&searchFields=KAD_NOMER&layers=54&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304415860&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304426570&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304412650&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304400750&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304416700&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425970&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson",
#   "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304423080&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# ) %>% 
#   map_dfr(get_res_geo_attrs)
# 
# manycals_out %>% 
#   unnest(response_geo_attrs) %>%
#   st_as_sf() %>% 
#   select(KAD_NOMER, everything()) %>% 
#   ggplot() + 
#   geom_sf()

# # Testing for call with more than one requested.
# "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425970&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson" %>% 
#   get_res_geo_attrs %>% 
#   unnest(response_geo_attrs) %>% 
#   st_as_sf() %>% 
#   ggplot() + 
#   geom_sf()


# Actual harvesting =======================================================

interm_plot_fldr <- "data-raw/04-kvartal-3-dig-plots-geometries"

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


library(progressr)
library(furrr)
handlers(global = TRUE)

handlers(list(
  handler_progress(
    format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
    width    = 80,
    complete = "+"
  )
))


parallel_harvest <- function(lines_to_harvest) {
  
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


plan(multisession)
parallel_harvest(lines_to_harvest)



