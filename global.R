library(sf)
library(sp)
library(leaflet)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(shiny)

state_counties <- read.csv("counties.csv", header = TRUE)

vars10 <- c("P009002",
            "P009005",
            "P009006",
            "P009007",
            "P009008",
            "P009009"
            )

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

CRS_new <- CRS("+init=epsg:4326") 


get_P9 <- function(county, state) {

  P9 <- get_decennial(geography = "tract", year = 2010, 
                      variables = vars10, cache_table = TRUE,
                      county = county, state = state, 
                      geometry = TRUE, output = "wide")
  
  num_dots <- as.data.frame(P9) %>% 
    select(P009002:P009009) %>%
    rename(Hispanic = P009002, White = P009005, Black = P009006, 
           `American Indian` = P009007, Asian = P009008, `Pacific Islander` = P009009) %>%
    mutate_all(funs(. / 100)) %>% 
    mutate_all(random_round)
 
  
  census_dots <- map_dfr(names(num_dots), 
                        ~ st_sample(P9, size = num_dots[,.x], type = "random") %>%      # generate the points in each polygon
                          st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                          st_coordinates() %>%                                          # pull out coordinates into a matrix
                          as_tibble() %>%                                               # convert to tibble
                          setNames(c("lon","lat")) %>%                                  # set column names
                          mutate(race_ethnicity = .x)                                   # add categorical variable
                        )%>% 
    slice(sample(1:n()))
  
  census_dots_tm <- SpatialPointsDataFrame(
    coords = census_dots[,c("lon","lat")], data = census_dots)
  raster::projection(census_dots_tm)="+init=epsg:4269"
  census_dots_tm <- spTransform(census_dots_tm, CRS_new)
  
  factpal <- colorFactor("Set1", census_dots_tm$race_ethnicity, ordered = FALSE)
  
  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>% 
    addPolygons(data = P9, opacity = 0.1) %>%
    addCircles(data = census_dots_tm, color = ~factpal(census_dots_tm$race_ethnicity), radius = 1, weight = 2, opacity = 0.8) %>%
    addLegend(pal = factpal, values = census_dots_tm$race_ethnicity, opacity = 0.7, 
              title = "Race/Ethnicity" ,
              position = "bottomright")
  
}


