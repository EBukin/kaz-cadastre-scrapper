

# Function to get raw response behind URL -------------------------------------
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
      stringi::stri_trans_general("Latin-ASCII") %>% 
      str_replace_all('\\\\\"', "")  %>% 
      str_replace_all("\\\\", "")  %>% 
      # str_replace_all("\\\\", "")  %>% 
      # str_replace_all("\\\\u2116", "") %>% 
      # stringi::stri_trans_general("Kazakh-Latin/BGN") %>%
      # stringi::stri_trans_general("Latin-ASCII") %>%
      jsonlite::fromJSON(simplifyDataFrame = F, simplifyVector = F)
  }, silent = T
  )
  
  
  if ("try-error" %in% class(out)) {
    out <- try({
      one_call_raw %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        str_replace_all("\\n", "") %>%
        jsonlite::fromJSON(simplifyDataFrame = F, simplifyVector = F)
    }, silent = T)
    
  }
  
  if ("try-error" %in% class(out)) return(list( error = TRUE)) else return(out)
}

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304411050&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw() %>%
#   get_response_json

# # Problematic encoding:

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=037&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson"
# 
# cll %>%
#   get_response_raw %>%
#   get_response_json

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=036&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw %>%
#   get_response_json


# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304408131&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304408131&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304410994&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304412692&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304415667&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304422137&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425168&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425772&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304429540&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0305000207&contains=true&searchFields=KAD_NOMER&layers=54&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0305001778&contains=true&searchFields=KAD_NOMER&layers=54&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=03044115109&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson

# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304401569&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# cll %>%
#   get_response_raw %>%
#   get_response_json
# 
# 
# # iconv(resp, from = "1252", to = "unicode")
# #
# # resp %>%
# #   stri_trans_general("Any-Hex")
# #
# #
# 
# # # enc2utf8(resp)  %>%
# # 
# #   # str_replace_all("\\u2116+", "") %>%
# # 
# # 
# resp <-
#   cll %>%
#   get_response_raw
# 
# resp %>% 
#   stringi::stri_trans_general("Latin-ASCII") %>%
#   # stringi::stri_trans_general("ru-ru_Latn/BGN") %>%  
#   # str_replace_all('\\\\\"', "")  %>%
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\\\\\", "") %>%
#   # str_replace_all("\\\\\\\\\\\\", "") %>%
#   str_replace_all("\\n", "") %>%
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\", "") %>%
#     jsonlite::fromJSON()
# # 
# # aaa <-
# resp %>% 
#   # stringi::stri_trans_general("Latin-ASCII") %>% 
#   # stringi::stri_trans_general("ru-ru_Latn/BGN") %>% 
#   # str_replace_all('\\\\\"', "")  %>%
#   # str_replace_all("([\\u005C]).{1,3}", "") %>% 
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\\\\\", "") %>%
#   # str_replace_all("\\\\\\\\", "")  %>%
#   # str_replace_all("(\\\\){1,}", "") %>%
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\", "") %>%
#   # str_replace_all("\\\\", "") %>%
#   str_locate_all('\\n') %>%
#   `[[`(1) %>%
#   as_tibble() %>%
#   pmap(~{
#     resp %>%
#       str_sub(.x - 10, .x + 15) #%>% 
#       # str_replace_all('\\\\\"', "")  #%>%
#       # str_replace_all("([\\u005C])", "")
#   }) 
# 
# 
# 
# 
# aaa %>% 
#   str_replace_all("([\\u005C]).{1,3}",)
# aaa %>% 
#   stri_trans_general("Any-Hex")

# %>%
#   map(~.x %>% stringi::stri_trans_general("Latin-ASCII") %>%
#         str_replace_all("\\\\", ""))
# 
# resp %>%
#   stringi::stri_trans_general("Latin-ASCII") %>%
#   str_replace_all("\\\\", "")  %>%
#   jsonlite::fromJSON()
# 
# 
# resp %>%
#   str_locate_all('TSN_KAZ') %>%
#   `[[`(1) %>%
#   as_tibble() %>%
#   pmap(~{
#     resp %>%
#       str_sub(.x - 5, .x + 100)
#   }) 
# 
# resp %>%
#   str_locate_all('\\\\\"') %>%
#   `[[`(1) %>%
#   as_tibble() %>%
#   pmap(~{
#     resp %>%
#       str_sub(.x - 5, .x + 100)
#   }) 
# 
# 
# 
# 
# resp %>%
#   str_replace_all('\\\\\"', "")  %>% 
#   str_replace_all("\\\\", "")  %>% 
#   
#   stringi::stri_trans_general("Latin-ASCII") %>% 
#   jsonlite::fromJSON()
# 
# resp%>% jsonlite::fromJSON()
#   # get_response_raw() %>%
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


# Empty responces
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030507&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030508&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030509&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030507&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson"
# 
# resp <- 
#   cll %>%
#   get_response_raw 
# 
# resp %>%
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
    exceededTransferLimit = FALSE,
    response_geo_attrs = list(tibble(empty = TRUE))
  )
  
  # browser()
  
  if(!is.null(response_json$error) && isTRUE(response_json$error)) {
    out_generic <- 
      out_generic %>% 
      mutate(response_success = FALSE)
    return(out_generic)
  }
  
  if(!is.null(response_json$results) && length(response_json$results) == 0) {
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
    }) %>% 
    mutate(empty = FALSE)
  
  out_generic %>% 
    mutate(
      response_n_elements = res_geo_attrs %>% nrow,
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


# Example with empty responce
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030507&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson"
# 
# resp <-
#   cll %>%
#     get_res_geo_attrs


# # # Testing for many calls
# manycals_out <-
#   c(
#     "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=030507&contains=true&searchFields=KAD_NOMER&layers=75&returnGeometry=true&f=pjson",
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


# These return as unsuccessful
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304401569&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304408131&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304410994&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304412692&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304415667&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304422137&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425168&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304425772&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304429540&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0305000207&contains=true&searchFields=KAD_NOMER&layers=54&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0305001778&contains=true&searchFields=KAD_NOMER&layers=54&returnGeometry=true&f=pjson
# http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=03044115109&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson

# # Example with empty responce
# cll <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText=0304401569&contains=true&searchFields=KAD_NOMER&layers=50&returnGeometry=true&f=pjson"
# 
# resp <-
#   cll %>%
#     get_res_geo_attrs


# manycals_out %>%
#   unnest(response_geo_attrs) %>%
#   filter(empty)
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



# Function for getting one digit geometry on top of the call.
get_one_digit_geometries <- function(line, geom_call) {
  0:9 %>%
    map_dfr( ~ {
      # browser()
      inp <-
        line %>%
        mutate(new_digit = .x) %>%
        mutate(url = glue(geom_call))
      out <-
        inp$url %>%
        get_res_geo_attrs()
      bind_cols(inp, out)
    })
}

# local_call <- "http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/find?searchText={obl_id}{new_digit}&contains=true&searchFields=KAD_NOMER&layers={75}&returnGeometry=true&f=pjson"
# extra_digit <-
#   tibble(obl_id = 03050) %>%
#   get_one_digit_geometries(local_call)