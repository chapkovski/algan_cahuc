library(sp)
library(dplyr)
library(ggplot2) 
library(raster)
library(rgeos)
library(tibble)
library(maptools)
file.choose()
load('/Users/chapkovski/Downloads/rmaps/data/processed/region_mean_income2015.rda')
russian_map <- getData("GADM", country = "RUS", level = 1)
russia_1.simp <- gSimplify(russian_map, tol=.1, topologyPreserve=TRUE) %>%
  SpatialPolygonsDataFrame(data=russian_map@data)
mp1 = fortify(russia_1.simp)
mp2 <- mp1
mp2$long <- mp2$long + 360
mp2$group <- mp2$group + max(mp2$group) + 1
mp <- rbind(mp1, mp2)

# Clean map
# MAP MARAMS IN THEME OPNTS:
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

ggplot(aes(x = long, y = lat, group = group), data = mp)  + geom_polygon(
  data = mp1,
  aes(x = long, y = lat, group = group),
  fill = "lightblue",
  color = "grey20",
  # size = 0.15
) +  scale_x_continuous(limits = c(0, 200))+coord_map()+theme_opts

# Data binding to map
# Q1 for map

t %>% group_by(region, Q1) %>%summarise(n=n())%>%mutate(prop=n/sum(n))%>%
  subset(select=c("region","Q1","prop"))%>%  spread(Q1, prop) %>%subset(select=c('region','большинству людей можно доверять')) %>%rename(trust=`большинству людей можно доверять`)%>%arrange(trust)-> trust_data
trust_data %>% kable()%>% kable_styling()

# COOPERATION
drop.cols<-c('затрудняюсь ответить','Нет ответа')
t %>% group_by(region, Q5) %>%summarise(n=n())%>%mutate(prop=round(n / sum(n)*100,1))%>%
  subset(select=c("region","Q5","prop"))%>%  spread(Q5, prop) %>%
  mutate(cooperation=`безусловно к первым`+`скорее к первым`)%>% subset(select=c('region','cooperation')) -> cooperation

# TRUST MERGE
data_frame(id = rownames(russia_adm_map@data),
           region = russia_adm_map@data$NL_NAME_1) %>%
  left_join(trust_data, by = "region") %>% na.omit() -> names_and_numbers

final_map <- left_join(mp1, names_and_numbers, by = "id")

# COOPERATION MERGE
# 
#
data_frame(id = rownames(russia_adm_map@data),
           region = russia_adm_map@data$NL_NAME_1) %>%
  left_join(cooperation, by = "region") %>% na.omit() -> names_and_numbers

final_map <- left_join(mp1, names_and_numbers, by = "id")


ggplot(final_map) +
  theme_minimal() +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = cooperation
  ), ) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(25, 175)) + coord_map() +
  scale_fill_continuous(low = "green",
                        high = "blue",
                        guide = guide_legend(title = "Cooperation %")) +
  theme_opts + ggtitle('Cooperation (First cat.) - GeoRating (2011)')


library(maps)


