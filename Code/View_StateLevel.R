library(sf)
library(tidyverse)
library(ggspatial)


chn_lv1 <- read_sf("MapData/gadm41_CHN_1.shp")

chn_st <- maptools::thinnedSpatialPoly(
  as(chn_lv1, "Spatial"),tolerance = 0.1, minarea = 0.001, 
  topologyPreserve = TRUE)
chn <- st_as_sf(chn_st)

get_df <- function(geodf)
{
  pointdf <- data.frame()
  tmp <- geodf$geometry[[1]]
  pointdf <-lapply(1:length(tmp), FUN = function(x) data.frame(tmp[[x]][[1]], 1:nrow(tmp[[x]][[1]]), 
                                                               paste0(geodf$ID[i], x), geodf$name[i])) %>% 
              do.call(rbind,.) %>% "colnames<-"(c("long", "lat","order", "group", "district"))
  return(pointdf)
}

chn %>% transmute(ID = GID_1, points = geometry, name = NAME_1) %>% 
  split(.$ID) %>% purrr::map(function(x) get_df(x)) %>% 
  do.call(rbind, .) %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = district)) + geom_polygon() + 
  scale_y_continuous(n.breaks = 5)  + theme_bw() + theme(legend.position = "none")

for(i in 1:37)
  gg[[i]] %>%  dim() %>% print()
