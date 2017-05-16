# Load packages -----------------------------------------------------
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(datasets)
library(tibble)

# Set base URL ------------------------------------------------------
# Note: This is a copy of http://www.lq.com/en/findandbook/hotel-listings.html
base_url = "http://www2.stat.duke.edu/~cr173/lq/www.lq.com/en/findandbook/"

# Write a function that gets hotel URLs the base page ---------------
get_hotel_urls = function(base_url){
  url = paste0(base_url, "hotel-listings.html")
  page = read_html(url)
 
  hotel_pages = page %>% 
    html_nodes("#hotelListing a") %>% 
    html_attr("href") %>% 
    .[!is.na(.)] %>% 
    .[. != "#"]
  
  paste0(base_url, hotel_pages)
}

# Write a function that gets hotel details from the hotel URL -------
get_hotel_details = function(hotel_url){
  page = read_html(hotel_url)
  
  name = page %>% 
    html_nodes("h1") %>% 
    html_text() #%>%
    #data_frame(name = .)
  
  details = page %>% 
    html_nodes(".hotelDetailsBasicInfoTitle p") %>% 
    html_text() %>% 
    str_replace_all("\\s{2,}", "\n")

  city_state_zip = str_match(details,"\n([A-Za-z'. -]+), ([A-Z]{2,3}) ([0-9]{5}|[0-9]{5}[ -][0-9]{4})\n") %>%
    data.frame(stringsAsFactors=FALSE) %>%
    select(-1) %>%
    setNames(c("city","state","zip_code"))
  
  phone = str_match(details, "\nPhone: ([0-9-]{9,})") %>%
    data.frame(stringsAsFactors=FALSE) %>%
    select(-1) %>%
    setNames(c("phone_number"))
  
  map = page %>%
    html_nodes(".ppMap img") %>%
    html_attr("src")
  
  lat_long = str_match(map, "\\|(-?[0-9]+\\.[0-9]+),(-?[0-9]+\\.[0-9]+)") %>%
    data.frame(stringsAsFactors=FALSE) %>%
    select(-1) %>%
    setNames(c("latitude","longitude")) %>%
    mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))
  
  cbind(
    name,
    city_state_zip,
    phone,
    lat_long
  ) 
}

d = map_df(get_hotel_urls(base_url)[1:100], get_hotel_details)


# only US
d <- d %>%
  filter(!is.na(state), state %in% state.abb)
