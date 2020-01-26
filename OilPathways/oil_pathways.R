

# https://finance.yahoo.com/news/what-us-iran-tensions-could-mean-for-oil-prices-193905568.html?guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAADp4Vt8w3NAGpqbj8B0Lj3oLzHjF-R9YqWXZKXiKiabBmEv_8GNYKJeLGytcLIHL1fLnn9Rv886i_Vr07iqAUu4fuZrpm-5goU7ulysC0QjUSEUxWfs1Ax_zinvN3DezuR3zAsWmwSClEXkSGXu9_d-wmCe22K4GKnRjQq7_1d3p


library(tabulizer)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)
library(here)

# This is the source location of the data in a pdf file
location <- str_c("https://www.eia.gov/beta/international/analysis_includes/",
                  "special_topics/World_Oil_Transit_Chokepoints/wotc.pdf")

# Use tabulizer to extract the relevant portion of the data table on page 2 of the pdf
extract <- extract_areas(file =location, pages = 2, resolution = 100)

# Set up the data as a data frame and then isolate just year 2016 for the graphic
df <- as_tibble(extract[[1]][2:nrow(extract[[1]]),])
names(df) <- extract[[1]][1,]

data <- df %>% 
    select("Location", Barrels = "2016") %>% 
    mutate(Barrels = as.numeric(Barrels))

coord <- tribble(
~Location, ~Lat, ~Lon,
"Strait of Hormuz", 26.5667, 56.2500,
"Strait of Malacca", 1.4300, 102.8900,
"Suez Canal and SUMED Pipeline", 30.4550, 32.3500,
"Bab el-Mandeb", 12.5833, 43.3333,
"Danish Straits", 56.0000, 11.0000,
"Turkish Straits", 40.7225, 28.2247,
"Panama Canal", 9.38743, -79.91863,
"Cape of Good Hope", -34.3548, 18.4698
)

data <- data %>% 
    left_join(coord, by = "Location")

data[data$Location =="Turkish Straits",1] <- "Bosporus"

write_csv(data, "oil_pathway_data.csv")

#=================================================

data <- read_csv("oil_pathway_data.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")


title <- "<b style='color:#FF0000'>IRAN</b> <b style='color:#FFFFFF'>BORDERS<br> KEY OIL PATHWAY</b>"


plot <- ggplot(data = world) +
    geom_sf(fill = "blue", color = "black", size = 0.2) +
    geom_sf(data = subset(world, geounit == "Iran"), fill = "red", color = "black", size = 0.2) +
    geom_point(data = data, aes(x =Lon, y=Lat, size = Barrels), 
               shape = 21, fill = "goldenrod2", color = "white", show.legend = F) +
    geom_text(data = data, aes(x =Lon, y=Lat, label = Location), color = "white", size = 3.5,
              vjust = -1, hjust = c(-0.3, 1.25, 1.1, 1.2, -0.15, -0.15, -0.15, 1.2), 
              fontface = "bold") +
    geom_text(data = data, aes(x =Lon, y=Lat, label = Barrels), color = "goldenrod2", size = 3.5,
              vjust = .5, hjust = c(-1.0, 2.7, 2.0, 2.0, -1.0, -1.0, -0.9, 2.1),
              fontface = "bold") +
    scale_size_continuous(range = c(2, 15)) +
    annotate("text", x = -60, y = 60, label = "MILLION BARRELS\nOF OIL MOVE PER\nDAY, 2016 DATA", size = 4.0,
             color = "goldenrod2", fontface= "bold") +
    annotate("text", x = -60, y = -60, label = "SOURCE U.S. ENERGY INFORMATION ADMINISTRATION", size = 2.3,
             color = "white")+
    geom_richtext(x = 20, y = 80, label = title, size = 9.0, fill = NA, label.color = NA) +
    geom_curve(aes(x = 73, y = 80, xend = 65, yend = 38), color = "red", arrow = arrow(type = "open",
              length = unit(0.15, "inches")), curvature = -0.75, angle = 100, ncp =10) + 
    coord_sf(xlim = c(-100.00, 120.00), ylim = c(-65, 95), expand = F) +
    theme(panel.background = element_rect(fill = "black"),
          panel.grid = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
          axis.text = element_blank(), aspect.ratio = 1) 

ggsave(here::here("oil_pathways.png"), plot, dpi = 320, width = 20, height = 20, units = "cm")