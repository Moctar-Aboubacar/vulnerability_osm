# This will eventually be titled: "Econometric analyses of poverty and food security in Nepal". There will be 4 parts: (1) Getting and visualizing OSM data (2) Cluster analysis of different types of palika (3) using PCA to get a vulnerability index, and (4) spatial regression model to predict counts of POIs in the country (ex. where OSM data might be missing).

# reset
rm(list = ls())

# load packages
require(tidyverse)
require(sf)
require(osmdata)
require(units)
require(gridExtra)

##getting and cleaning data ####
# getting polygon data ####
# define a bounding box
m <- matrix(c(79, 26, 88.5, 30.5), ncol = 2, byrow = TRUE)
row.names(m) <- c("x", "y")
names(m) <- c("min", "max")

# Nepal- country
q.nepal <- m %>% 
  opq() %>% 
  add_osm_feature(key = 'name:en', value = 'Nepal') %>% 
  osmdata_sf()

nepal <- q.nepal$osm_multipolygons

# get different administrative levels (Province and District)
# Province
q.province <- m %>% 
  opq() %>% 
  add_osm_feature(key = 'admin_level', value = '3') %>% 
  osmdata_sf()

province <- q.province$osm_multipolygons

# District
q.district <- m %>% 
  opq() %>% 
  add_osm_feature(key = 'admin_level', value = '6') %>% 
  osmdata_sf()

district <- q.district$osm_multipolygons


# getting roads and points of interest ####

# banks
q.banks <- m %>% 
  opq() %>% 
  add_osm_feature("amenity", "bank") %>% 
  osmdata_sf()
plot(banks)
banks <- q.banks$osm_points

# hospitals
q.hospitals <- m %>% 
  opq() %>% 
  add_osm_feature("amenity", "hospital") %>% 
  osmdata_sf()

hospitals <- q.hospitals$osm_points

# clinics
q.clinics <- m %>% 
  opq() %>% 
  add_osm_feature("amenity", "clinic") %>% 
  osmdata_sf()

clinics <- q.clinics$osm_points

# markets
q.markets <- m %>% 
  opq() %>% 
  add_osm_feature("amenity", "marketplace") %>% 
  osmdata_sf()

markets <- q.markets$osm_points

# roads (primary, secondary and trunk) 
# q.roads_prim <- m %>% # primary road coverage looks very spotty
#  opq(timeout = 50) %>%
#  add_osm_feature("highway", "primary") %>%
#  osmdata_sf()
#  
# roads_prim <- q.roads_prim$osm_lines

q.roads_trunk <- m %>% 
  opq(timeout = 50) %>% 
  add_osm_feature("highway", "trunk") %>% 
  osmdata_sf()

roads_trunk <- q.roads_trunk$osm_lines

q.roads_sec <- m %>% 
  opq(timeout = 50) %>% 
  add_osm_feature("highway", "secondary") %>% 
  osmdata_sf()

roads_sec <- q.roads_sec$osm_lines


## cleaning OSM data ####
# confirm that all objects are using the same projection (they are)
st_crs(nepal)
st_crs(district)
st_crs(SAE)
st_crs(banks)
st_crs(hospitals)
st_crs(clinics)
st_crs(markets)
st_crs(roads_trunk)
st_crs(roads_sec)

# keep only points of interest within Nepal
district <- st_intersection(x = district, y = nepal)
banks <- st_intersection(x = banks, y = nepal)
clinics <- st_intersection(x = clinics, y = nepal)
hospitals <- st_intersection(x = hospitals, y = nepal)
markets <- st_intersection(x = markets, y = nepal)
roads_prim <- st_intersection(x = roads_prim, y = nepal)
roads_trunk <- st_intersection(x = roads_trunk, y = nepal)
roads_sec <- st_intersection(x = roads_sec, y = nepal)

# import Small Area Estimates and combine files
SAE <- st_read(dsn = "C:/Users/moctar.aboubacar/Desktop/Nepal SAE~Palika/SAE_WFP_WB_2011", layer = "SAE_palika_WFP_WB_both_2011")

# remove protected forest/non-inhabitant areas
SAE <- SAE %>% 
  filter(SAE$Avg_P0WB > 0)



# counts of each point of interest by Palika (municipality)
SAE$banks <- lengths(st_covers(SAE, banks))
SAE$hospitals <- lengths(st_covers(SAE, hospitals))
SAE$clinics <- lengths(st_covers(SAE, clinics))
SAE$markets <- lengths(st_covers(SAE, markets))

# calculate area in square km of each palika
SAE$area <- set_units(st_area(SAE), 'km^2')

# save data (OSM calls, especially to large datasets, can be finnicky. As such it is best to save results)
save(list = c("nepal", "province", "district", "banks", "hospitals", "clinics", "markets", "roads_trunk","roads_prim", "roads_sec", "SAE"),
     file = "C:/Users/moctar.aboubacar/Desktop/nepalosm.RData")

# load saved data
load("C:/Users/moctar.aboubacar/Desktop/nepalosm.RData")

## Visualizing the data ####
# Provinces in Nepal
plot(province[1], col = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f"), 
     main = "Provinces of Nepal")
legend("right", legend = c("1", "2", "3", "4", "5", "6", "7"),
       fill = c("#b2df8a", "#fb9a99", "#33a02c", "#e31a1c", "#fdbf6f", "#a6cee3", "#1f78b4"))

# Plotting points of interest
par(mfrow = c(2, 2))
#par(mar =  c(5.1, 4.1, 4.1, 2.1))

# banks
plot(st_geometry(nepal), col = "grey80", border = NA, 
     main = "Banking services")
plot(province[1], col = "grey80", add = T)
plot(roads_trunk[1], col = "orchid4", lwd = 2.2, add = T)
plot(roads_sec[1], col = "orchid3", lwd = 1.5, add = T)
plot(banks[1], col = rgb(red = 0, green = 0.4, blue = 1, alpha = 0.6), pch = 19, cex = 1, add = T)

# clinics
plot(st_geometry(nepal), col = "grey80", border = NA, 
     main = "Clinics")
plot(province[1], col = "grey80", add = T)
plot(roads_trunk[1], col = "orchid4", lwd = 2.2, add = T)
plot(roads_sec[1], col = "orchid3", lwd = 1.5, add = T)
plot(clinics[1], col = rgb(red = 1, green = 0.4, blue = 0, alpha = 0.6), pch = 19, cex = 1, add = T)

# markets
plot(st_geometry(nepal), col = "grey80", border = NA, 
     main = "Markets")
plot(province[1], col = "grey80", add = T)
plot(roads_trunk[1], col = "orchid4", lwd = 2.2, add = T)
plot(roads_sec[1], col = "orchid3", lwd = 1.5, add = T)
plot(markets[1], col = rgb(red = 0.1, green = 0.8, blue = 0, alpha = 0.6), pch = 19, cex = 1, add = T)

# hospitals
plot(st_geometry(nepal), col = "grey80", border = NA, 
     main = "Hospitals")
plot(province[1], col = "grey80", add = T)
plot(roads_trunk[1], col = "orchid4", lwd = 2.2, add = T)
plot(roads_sec[1], col = "orchid3", lwd = 1.5, add = T)
plot(hospitals[1], col = rgb(red = 1, green = 0.2, blue = 0.4, alpha = 0.6), pch = 19, cex = 1, add = T)

par(mfrow = c(1, 1))

# SAE choropleth maps

plot.1 <- ggplot(SAE)+
  geom_sf(aes(fill = Avg_P0, size = NA))+
  scale_fill_viridis_c(name = "Food pov %", option = "C")+
  labs(title = "Food poverty prevalence")+
  theme_void()
    
plot.2 <- ggplot(SAE)+
  geom_sf(aes(fill = Avg_S2, size = NA))+
  scale_fill_viridis_c(name = "Stunting %", option = "C")+
  labs(title = "Stunting prevalence in children < 5")+
  theme_void()

plot.3 <- ggplot(SAE)+
  geom_sf(aes(fill = Avg_U2, size = NA))+
  scale_fill_viridis_c(name = "Underweight %", option = "C")+
  labs(title = "Underweight prevalence in children < 5")+
  theme_void()

plot.4 <- ggplot(SAE)+
  geom_sf(aes(fill = Avg_W2, size = NA))+
  scale_fill_viridis_c(name = "Wasting %", option = "C")+
  labs(title = "Wasting prevalence in children < 5",
       caption = "Source: Nepal Small Area Estimation for Food Security and Nutrition, 2014")+
  theme_void()

grid.arrange(plot.1, plot.2, plot.3, plot.4,
             nrow = 2, 
             ncol = 2)

plot.5pov <- ggplot(SAE)+
  geom_sf(aes(fill = Avg_P0WB, size = NA))+
  scale_fill_viridis_c(name = "Poverty %", option = "B")+
  labs(title = "Poverty Prevalence in Nepal",
       caption = "Source: World Bank Nepal Small Area Estimation of Poverty, 2014")+
  theme_void()


# POI histograms (colored by province)
# eliminate Kathmandu, Pokhara and surrounding districts
SAE.summ <- SAE %>% 
  filter(DISTRICT != "LALITPUR",
         DISTRICT != "BHAKTAPUR",
         DISTRICT != "KATHMANDU",
         DISTRICT != "KASKI")

plot.5 <- ggplot(SAE, aes(x = factor(STATE), y = banks))+
  geom_bar(stat = "identity")+
  xlab("")+
  theme_bw()

plot.6 <-ggplot(SAE, aes(y = markets, x = factor(STATE)))+
  geom_bar(stat = "identity")+
  xlab("")+
  theme_bw()

plot.7 <-ggplot(SAE, aes(y = clinics, x = factor(STATE)))+
  geom_bar(stat = "identity")+
  xlab("Province")+
  theme_bw()

plot.8 <-ggplot(SAE, aes(y = hospitals, x = factor(STATE)))+
  geom_bar(stat = "identity")+
  xlab("Province")+
  theme_bw()

grid.arrange(plot.5, plot.6, plot.7, plot.8,
             nrow = 2, 
             ncol = 2,
             top = "By-Province POI counts (not-including Kathmandu or Pokhara)")

# by-district counts for points of interest
SAE.Dist <- as.data.frame(SAE.summ %>% 
  group_by(DISTRICT, STATE) %>% 
  summarize(count.banks = sum(banks),
            count.markets = sum(markets),
            count.hospitals = sum(hospitals),
            count.clinics = sum(clinics)))
  

plot.9 <- ggplot(SAE.Dist, aes(x = reorder(DISTRICT, -count.banks), y = count.banks, fill = STATE))+
  geom_col()
  
plot.10 <- ggplot(SAE.Dist, aes(x = reorder(DISTRICT, -count.markets), y = count.markets, fill = STATE))+
  geom_col()

plot.11 <- ggplot(SAE.Dist, aes(x = reorder(DISTRICT, -count.hospitals), y = count.hospitals, fill = STATE))+
  geom_col()

plot.12 <- ggplot(SAE.Dist, aes(x = reorder(DISTRICT, -count.clinics), y = count.clinics, fill = STATE))+
  geom_col()

grid.arrange(plot.9, plot.10, plot.11, plot.12,
             nrow = 2, 
             ncol = 2,
             top = "By-District POI counts (not-including Kathmandu or Pokhara)")

## SAE analyses 

#kernel densities of pov, food pov, stunting, wasting
# transform from wide to long
SAE.long <- gather(SAE, key = "type", value = "prevalence", -c(1:9,11:15, 17:19, 21:23,  25:32))

#kernel densities
ggplot(SAE.long, aes(x = prevalence * 100, fill = type))+
  geom_density(alpha = 0.5)+
  labs(title = "Kernel density plot--key indicators",
       x = "Prevalence (%)",
       y = "density")+
  scale_fill_discrete(name = "Indicator", labels = c("Food Pov", "Poverty", "Stunting", "Wasting"))+
  theme_classic()

# Poverty against food security
graph.pov.foodpov <- ggplot(SAE, aes(x = Avg_P0, y = Avg_P0WB))+
  geom_point(color = "grey50")+
  geom_smooth(method = 'lm', color = 'black', se = F)+
  theme_classic()+
  labs(title = "Poverty vs. Food Poverty",
       x = "Food poverty prevalence",
       y = "Poverty prevalence")

summary(lm(SAE$Avg_P0WB ~ SAE$Avg_P0))$r.squared

# scatter poverty against stunting
graph.pov.stunt <- ggplot(SAE, aes(x = Avg_S2, y = Avg_P0WB))+
  geom_point(color = "grey50")+
  geom_smooth(method = 'lm', color = 'black', se = F)+
  theme_classic()+
  labs(title = "Stunting vs. Poverty",
       x = "Stunting prevalence",
       y = NULL)

grid.arrange(graph.pov.foodpov, graph.pov.stunt,
             nrow = 1,
             ncol = 2)

# but wasting is another dimention
# scatter poverty against wasting
ggplot(SAE, aes(x = Avg_P0WB, y = Avg_W2))+
  geom_point(color = "grey50")+
  geom_smooth(method = 'lm', color = 'black', se = F)+
  theme_classic()+
  labs(title = "Wasting vs. Poverty",
       x = "Poverty prevalence",
       y = "Wasting prevalence")

# rsquared
summary(lm(SAE$Avg_W2 ~ SAE$Avg_P0WB))$r.squared


# Provincewise comparison of SAE indicators
sae.prov.means <- SAE %>% 
  group_by(STATE) %>% 
  summarize(mean.pov = mean(Avg_P0WB),
            mean.foodpov = mean(Avg_P0),
            mean.stunt = mean(Avg_S2),
            mean.waste = mean(Avg_W2)) 


plot.13 <- ggplot(sae.prov.means, aes(x = STATE, y = (100 * mean.pov)))+
  geom_col()+
  labs(title = "Poverty by Province",
       x = "",
       y = NULL)+
  theme_classic()

plot.14 <- ggplot(sae.prov.means, aes(x = STATE, y = (100 * mean.foodpov)))+
  geom_col()+
  labs(title = "Food Poverty by Province",
       x = "",
       y = NULL)+
  theme_classic()

plot.15 <- ggplot(sae.prov.means, aes(x = STATE, y = (100 * mean.stunt)))+
  geom_col()+
  labs(title = "Stunting by Province",
       x = "Province",
       y = NULL)+
  theme_classic()

plot.16 <- ggplot(sae.prov.means, aes(x = STATE, y = (100 * mean.waste)))+
  geom_col()+
  labs(title = "Wasting by Province",
       x = "Province",
       y = NULL)+
  theme_classic()

grid.arrange(plot.13, plot.14, plot.15, plot.16,
             nrow = 2,
             ncol = 2,
             top = "Small Area Estimations by Province",
             left = "Prevalence (%)")