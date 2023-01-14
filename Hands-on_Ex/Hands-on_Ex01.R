# p_load function of pacman package is used to install and load sf and tidyverse packages
pacman::p_load(sf, tidyverse) 


# extract shape files
mpsz = st_read(dsn="Hands-on_Ex01/data/geospatial",
               layer="MP14_SUBZONE_WEB_PL")

cyclingpath = st_read(dsn="Hands-on_Ex01/data/geospatial",
                       layer = "CyclingPathGazette")

preschool = st_read(dsn = "Hands-on_Ex01/data/geospatial/preschools-location.kml")


# retrieve the geometry list-column
st_geometry(mpsz)

# see attributes in df
glimpse(mpsz)

head(mpsz, n=5) # top 5 rows


# PLOT
# multi-plot of all attributes
plot(mpsz)

# choose to plot only the geometry
plot(st_geometry(mpsz))

# choose the plot
plot(mpsz["PLN_AREA_N"])

# For high cartographic quality plot, other R package such as tmap should be used.


# assign the right epsg code
st_crs(mpsz) # correct EPSG code for svy21 should be 3414

mpsz3414 <- st_set_crs(mpsz, 3414)

st_crs(mpsz3414)


# transform projection from wgs84 to svy21
preschool3414 <- st_transform(preschool,
                              crs=3414)
st_geometry(preschool3414)



# import aspatial data
listings <- read_csv("Hands-on_Ex01/data/aspatial/listings.csv")

# check df
list(listings)

# convert df to feature df
listings_sf <- st_as_sf(listings,
                        coords = c("longitude", "latitude"),
                        crs=4326) %>%
st_transform(crs = 3414)
# %>% is used to nest st_transform() to transform the newly created simple feature data frame into svy21 projected coordinates system.
# latitude and longitude are in decimal degree format. Best guess,the data is in wgs84.


# longitude and latitude columns dropped and geometry is added
glimpse(listings_sf)



# BUFFERING
# create 5m buffer around cycling paths
buffer_cycling <- st_buffer(cyclingpath,
                            dist = 5,
                            nQuadSegs = 30)

# calculate area of buffers and create AREA column to buffer_cycling
buffer_cycling$AREA <- st_area(buffer_cycling)
sum(buffer_cycling$AREA)



# POINT-IN POLYGON COUNT
# identity preschools in each subzone with st_intersects()
# length() calculates no of presch
mpsz3414$`PreSch Count` <- lengths(st_intersects(mpsz3414, preschool3414))

summary(mpsz3414$`PreSch Count`)

# list subzone with most pre-school
top_n(mpsz3414, 1, `PreSch Count`)

# Calculate density
# calculate area of each zone
mpsz3414$Area <- mpsz3414 %>% st_area()

mpsz3414 <- mpsz3414 %>%
              mutate(`PreSch Density` = `PreSch Count`/Area*1000000)

# create histogram
hist(mpsz3414$`PreSch Density`)

ggplot(data = mpsz3414,
       aes(x = as.numeric(`PreSch Density`))) +
    geom_histogram(bins = 20,
                   color = "black",
                   fill = "light blue") +
    labs(title = "Are pre-school even distributed in Singapore?",
         subtitle = "There are many planning sub-zones with a single pre-school, on the other hand, \nthere are two planning sub-zones with at least 20 pre-schools",
         x = "Pre-school density (per km sq)",
         y = "Frequency")

# create scatterplot
ggplot(data = mpsz3414,
       aes(y = `PreSch Count`,
            x = as.numeric(`PreSch Density`))) +
    geom_point(color = "black",
               fill = "light blue") +
    xlim(0, 40) +
    ylim(0, 40) +
    labs(title = "Are pre-school even distributed in Singapore?",
         x = "Pre-school density (per km sq)",
         y = "Pre-school count")
