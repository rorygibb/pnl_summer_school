# check if required packages (deps) are installed and install if not
ip = installed.packages()
deps = c("terra", "dplyr", "magrittr", "sf", "ggplot2", "sf", "rstudioapi", "tidyr", "stringr")
for(d in deps){
  if(!d %in% rownames(ip)){
    print(paste("Installing:", d, sep=" "))
    install.packages(d)
  }
}
# load libraries dependencies
library(terra); library(dplyr); library(magrittr); library(ggplot2); library(sf)
library(rstudioapi); library(tidyr); library(stringr)
# automatically set working directory
# if this doesn't work,
# manually set your working directory to the folder "Week3-Measuring-Env-Effects")
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)




# the mosquito surveillance data are stored as a csv
# mosquito counts are stored in the "count" column
dd = read.csv("./data/vectabundance/italy_vectabundance_summercounts_monthly.csv")

# explore this dataset - you can use the "head" function to look at the top rows of the data
head(dd)

# and you can plot histograms of the numeric variables using hist()
# here is a histogram of counts
# remember, we use the $ operator to select a specific column from a data frame
hist(dd$count, 50)


# you can also generate quick tables of how much data fall in each group
table(dd$month)
table(dd$Region)

# what other variables does the dataset contain?
# plot some histograms and explore them


#italian municipalities
shp = sf::st_read("./data/shapefiles/italy_vectabundance_regions.shp")
# italian border (for mapping)
shp_ita = sf::st_read("./data/shapefiles/gadm41_ITA_0.shp")
# take a quick look at the shapefile
# what do you see? R tells you about what this is, and what it contains
# plot the map

map = ggplot() +
  geom_sf(data=shp_ita, color=NA, fill="grey60", alpha=0.5) +
  geom_sf(data=shp, color="grey50", aes(fill=Region), alpha=0.5) +
  geom_point(data = dd, aes(x, y), color='black', size=1)
map

# this map looks a bit basic
# let's make it look nicer by changing the background
# and giving it a title and a nicer colour scale
map = map +
  theme_bw() +
  MetBrewer::scale_fill_met_d(name="Archambault") +
  ggtitle("Survey locations") +
  theme(legend.position="right",
        plot.title=element_text(size=11, hjust=0.5)) +
  xlab("Longitude") + ylab("Latitude")
map

# 2. same plot but break down by region
# summarise by year and region
obs_by_year = dd %>%
  dplyr::group_by(year, Region) %>%
  dplyr::summarise(obs = length(count))
# use ggplot's "facet_wrap" function to break down into region subplots
ggplot(obs_by_year) +
  geom_bar(aes(year, obs), stat="identity", fill="skyblue4") +
  theme_classic() +
  facet_wrap(~Region) +
  xlab("Year") + ylab("Number of observations") +
  theme(strip.background = element_blank())

# subset to a random selection of locations
random_locs = dd %>%
  dplyr::filter(ID %in% c(6777, 8189, 7490, 14199))
# create a "date" column for easier plotting
random_locs$date = as.Date(paste(random_locs$year, random_locs$month, "01", sep="-"))
rl_plot =
  ggplot(random_locs) +
  geom_line(aes(date, count, group=year)) +
  geom_point(aes(date, count, group=year)) +
  theme_classic() +
  facet_wrap(~ID, ncol=1) +
  xlab("Month") + ylab("Ae. albopictus abundance") +
  ggtitle("Aedes albopictus count time-series for example locations") +
  theme(strip.background = element_blank(),
        plot.title=element_text(size=11, hjust=0.5))
rl_plot
