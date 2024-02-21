setwd("/Users/Gift/Library/CloudStorage/Box-Box/R21 Wildfire and Flash Floods - shared all/Results/Turner Lab Results/Combined datasets with MRA lab ID")
# Load the data====
longer <- read_xlsx("pfas_long.xlsx", sheet= "latlong", col_names = TRUE) 
long <- pivot_longer(longer, cols = c(6:28), names_to = "Parameter", values_to = "Concentration")
#chain <- pivot_longer(long, cols = c(6:7), names_to = "Chain Length", values_to = "PFAS Concentration")
#long <- pivot_longer(long, cols = c(6:9), names_to = "Functional Group", values_to = "Total Concentration")

long$Location <- factor(long$Location, levels = unique(long$Location))
long$Concentration <- as.numeric(long$Concentration)

site_totals <- long %>%
  group_by(Site) %>%
  summarise(total_concentration = max(`Total PFAS`))
long$Color <- "darkgreen"
# Assuming long and site_totals data frames are already defined
# Set up the colors for each Type
long$Color <- ifelse(long$Type == "Residential", "#ce806c", 
                     ifelse(long$Type == "Control", "darkgreen", "darkgreen"))

# Make sure site_totals also has the Color information
site_totals <- long %>%
  group_by(Site, Color) %>%
  summarise(total_concentration = max(`Total PFAS`), .groups = 'drop')

# Define colors for the scale
colors <- c("darkgreen", "#864E61")
site_types <- unique(long[c("Site", "Type")])
site_colors <- ifelse(site_types$Type == "Residential", "#ce806c", "darkgreen")

ggplot(long, aes(x = Site, y = Concentration, fill = Color)) +
  geom_col(position = "stack", width = 0.75) +
  geom_text(data = site_totals, aes(x = Site, y = total_concentration, 
                                    label = paste(round(total_concentration, 3)),
                                    color = Color),  
            size = 5, vjust = 0.5, hjust = -0.4) + 
  coord_flip(ylim = c(0, 105)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  labs(x = "Site", y = "Total PFAS Concentration (ug/kg)") +
  theme_minimal() +
  theme(legend.position = "none",  
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        plot.margin = unit(c(1,1,1,1), "lines"),
        panel.grid.major = element_blank(),  
        plot.background = element_rect(color = "darkgreen", fill = "white", size = 1),  # Corrected: plot.background and panel.background should be within theme()
        panel.background = element_rect(color = "darkgreen", fill = "white", size = 1)) +  
  scale_fill_manual(values = colors) +  # Apply the manual color scale with specific colors
  scale_color_manual(values = colors)  # Apply the manual color scale for text labels
dev.print(png, "individualconcentration.png", res=400, height=14, width=18, units="in")



kruskal.test(longer$`Total PFAS`, longer$Type)










# Define custom fill colors
custom_colors <- c("blue", "red", "green", "orange", "pink", "black", "darkblue", "purple",  "yellow", "magenta", "darkred", "darkgreen", "lightblue", "#FFD700", "#C0C0C0", "#4B0082", "#FF7F50", "#E6E6FA", "#808000", "#FFE5B4",  "#00FFFF", "#FF1493", "#B22222", "#483D8B", "#FF69B4" )  # Add more colors as needed

ggplot(long, aes(x = Site, y = Concentration/`Total PFAS`, fill = Parameter, shape = Location)) +
  geom_col(position = "fill", width = 0.5) +
  labs(x = "Site", y = "PFAS Contribution") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  scale_fill_manual(values = custom_colors) +  # Set custom fill colors
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14, face = "bold"),  
        axis.title = element_text(size = 14, face = "bold"),
        panel.grid.major = element_blank(),  
        legend.text = element_text(size = 10),  
        legend.title = element_text(size = 10, face = "bold"))


dev.print(png, "individualcongener.png", res=400, height=14, width=18, units="in")








functionalgroup <- pivot_longer(longer, cols = c(31:34), names_to = "Functional Group", values_to = "Total Concentration")
custom_colors <- c(    "#FF7F50","#FFE5B4",  "#4B0082",  "#808000", "darkred", "darkgreen",  "#00FFFF", "#FF1493", "#B22222", "#483D8B", "#FF69B4" )  # Add more colors as needed
ggplot(functionalgroup, aes(x = reorder(Site, -as.numeric(Location)), y =`Total Concentration` /`Total PFAS`, fill = `Functional Group`, shape = Location)) +
  geom_col(position = "fill", width = 0.5) +
  labs( x = "Site",
        y = "PFAS Functional Group") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  scale_fill_manual(values = custom_colors) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14, face = "bold"),  
        axis.title = element_text(size = 14, face = "bold"),  
        legend.text = element_text(size = 12),  
        panel.grid.major = element_blank(),  
        legend.title = element_text(size = 12, face = "bold"),
        plot.background = element_rect(color = "black", (fill = "white"),, size = 1),  # Extend frame to capture axis labels
        panel.background = element_rect(color = "black", fill = "white", size = 1))  # Extend frame to capture axis labels
dev.print(png, "functionalgroup.png", res=400, height=14, width=14, units="in")


chain <- pivot_longer(longer, cols = c(29:30), names_to = "Chain Length", values_to = "PFAS Concentration")
ggplot(chain, aes(x = reorder(Site, -as.numeric(Location)), y = `PFAS Concentration`/`Total PFAS`, fill = `Chain Length`, shape = Location)) +
  geom_col(position = "fill", width = 0.5) +
  labs(x = "Site",
       y = "PFAS Chain Length") +
  coord_flip(ylim = c(0, 1.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)))+
  scale_fill_manual(values = c("#1616FF", "#C24400")) + # Set custom colors for fill
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14, face = "bold"),  
        axis.title = element_text(size = 14, face = "bold"),  
        legend.text = element_text(size = 12),  
        panel.grid.major = element_blank(),  
        legend.title = element_text(size = 12, face = "bold"),
        plot.background = element_rect(color = "black", (fill = "white"),, size = 1),  # Extend frame to capture axis labels
        panel.background = element_rect(color = "black", fill = "white", size = 1))  # Extend frame to capture axis labels


dev.print(png, "length.png", res=400, height=14, width=14, units="in")





























#visualizations=======
ggplot(longer, aes(x=Type, y=log(`Short Chain`),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log Short Chain PFAS Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=12,  face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, face= "bold"),
        legend.position = "bottom")
dev.print(png, "shortchaincontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$`Short Chain`, longer$Type)

ggplot(longer, aes(x=Type, y=log(longer$`Long Chain`),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log Long Chain PFAS Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=12,  face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, face= "bold"),
        legend.position = "bottom")
dev.print(png, "longchaincontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$`Long Chain`, longer$Type)

ggplot(longer, aes(x=Type, y=log(longer$`Perfluoroalkane Sulfonamido Substances`),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log Perfluoroalkane Sulfonamido Substances Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=12,  face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12, face= "bold"),
        legend.position = "bottom")
dev.print(png, "perfluoroalkanesulfamidocontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$`Perfluoroalkane Sulfonamido Substances`, longer$Type)


ggplot(longer, aes(x=Type, y=log(PFCA),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log PFCA Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")
dev.print(png, "pfcacontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$PFCA, longer$Type)


ggplot(longer, aes(x=Type, y=log(PFSA),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log PFSA Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")
dev.print(png, "pfsacontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$PFSA, longer$Type)


ggplot(longer, aes(x=Type, y=log(longer$`Total PFAS`),  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "Residential Sample Sites") +
  ylab(label = "Log Total PFAS Concentration (ug/kg)") +
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2", "red"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")
dev.print(png, "sumpfascontrolsample.png", res=300, height=8, width=8, units="in")

kruskal.test(longer$`Total PFAS`, longer$Type)



ggplot(long, aes(x=Parameter, y=log_corrected,  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(coef = Inf)+
  xlab(label = "PFAS Compound in Residential and Control Sites") +
  ylab(label = "Log Concentration (ug/kg)") +
  labs(title = "PFAS concentration in Current Study")+
  coord_flip()+
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face= "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")


ggplot(long, aes(x=functionalgroup, y=log_corrected,  fill=Type)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot(coef = Inf)+
  xlab(label = "PFAS Compound in Residential and Control Sites") +
  ylab(label = "Log Concentration (ug/kg)") +
  labs(title = "PFAS concentration in Current Study")+
  coord_flip()+
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face= "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")
library(tidyverse)
library(ggspatial)
latlong<- readxl::read_excel("pfas_long.xlsx", sheet = "latlong")
library(leaflet)
library(leafem)
library(readxl)
library(tmap)


m <- leaflet(latlong) %>%
  addTiles() %>%
  addMarkers(~Longitude, ~Latitude, popup = ~Location)

north_arrow_html <- '<div style="position:absolute; top:10px; right:10px;"><img src="https://leafletjs.com/docs/images/compass-rose.png" alt="North" width="50" height="50"></div>'

m <- m %>% addControl(html = north_arrow_html, position = "topright")
m

north_arrow_html <- '<div style="position:absolute; top:10px; right:10px;"><img src="https://leafletjs.com/docs/images/compass-rose.png" alt="North" width="50" height="50"></div>'

m <- m %>% addControl(html = north_arrow_html, position = "topright")

# Create a tmap leaflet object
tmap_leaflet <- tm_shape(sf_data) +
  tm_dots(col = "red", size = 0.5) +
  tm_basemap_osm() +  # Use tm_basemap_osm for OpenStreetMap basemap
  tm_scale_bar(position = c("bottomleft"), text.size = 0.8)

# Combine the leaflet maps
m + tmap_leaflet

latlong$Latitude<- as.numeric(latlong$Latitude)
#latlong <- na.omit(latlong[, c("Latitude", "Longitude")])
spatial_data <- st_as_sf(latlong, coords = c("Longitude", "Latitude"), crs = 4326)




calculate_pie_segments <- function(x) {
  cum_prop <- cumsum(x) / sum(x)
  start <- c(0, head(cum_prop, -1))
  end <- cum_prop
  return(data.frame(start = start, end = end))
}


pie <- spatial_data%>%
  mutate(
    PieSegments_A = calculate_pie_segments(`long chain`),
    PieSegments_B = calculate_pie_segments(shortchain)
  )

# #pie2 <- latlong%>%
#  # mutate(
#     PieSegments_A = calculate_pie_segments(pfsa),
#     PieSegments_B = calculate_pie_segments(pfca),
#     PieSegments_C = calculate_pie_segments(`Fluorotelomer sulfonic acids`),
#     PieSegments_D = calculate_pie_segments(`Perfluoroalkane sulfonamido substances`)
#   )
print(pie2)

# Assuming pie2 contains the PieSegments columns and spatial coordinates

# Assuming pie2 contains the PieSegments columns and spatial coordinates

plot_data <- pie2 %>%
  pivot_longer(cols = starts_with("PieSegments"), names_to = "SegmentType", values_to = "PieSegments") %>%
  unnest(PieSegments)

# ggplot() +
#   geom_sf(data = pie2, aes(fill = Location), color = "white", size = 0.2) +
#   geom_segment(
#     data = plot_data %>% filter(SegmentType == "PieSegments_A"),
#     aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude,
#         size = 2, color = "red"),
#     arrow = arrow(length = unit(0.02, "npc")),
#     lineend = "round"
#   ) +
#   geom_segment(
#     data = plot_data %>% filter(SegmentType == "PieSegments_B"),
#     aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude,
#         size = 2, color = "blue"),
#     arrow = arrow(length = unit(0.02, "npc")),
#     lineend = "round"
#   ) +
#   geom_segment(
#     data = plot_data %>% filter(SegmentType == "PieSegments_C"),
#     aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude,
#         size = 2, color = "green"),
#     arrow = arrow(length = unit(0.02, "npc")),
#     lineend = "round"
#   ) +
#   scale_fill_manual(values = c("A" = "red", "B" = "blue", "C" = "green")) +
#   scale_size_manual(values = c("PieSegments_A" = 5, "PieSegments_B" = 5, "PieSegments_C" = 5)) +
#   scale_color_manual(values = c("red", "blue", "green"), guide = FALSE) +
#   labs(
#     title = "Spatial Map with Pie Chart Segments",
#     subtitle = "Concentration of Interest",
#     fill = "Location",
#     size = "SegmentType"
#   ) +
#   theme_minimal()


# Assuming pie2 contains the PieSegments columns and spatial coordinates

plot_data <- pie2 %>%
  pivot_longer(cols = starts_with("PieSegments"), names_to = "SegmentType", values_to = "PieSegments") %>%
  unnest(PieSegments)
pie2_sf <- st_as_sf(pie2, coords = c("Longitude", "Latitude"), crs = 4326)
plot_data <- plot_data %>% mutate(SegmentType = factor(SegmentType))

ggplot() +
  geom_sf(data = pie2_sf, aes(fill = Location), color = "white", size = 0.2) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_A"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_A", color = "red"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_B"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_B", color = "blue"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_C"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_C", color = "green"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  scale_fill_manual(values = c("A" = "red", "B" = "blue", "C" = "green")) +
  scale_size_manual(values = c("PieSegments_A" = 5, "PieSegments_B" = 5, "PieSegments_C" = 5)) +
  scale_color_manual(values = c("red", "blue", "green"), guide = FALSE) +
  labs(
    title = "Spatial Map with Pie Chart Segments",
    subtitle = "Concentration of Interest",
    fill = "Location",
    size = "SegmentType"
  ) +
  theme_minimal()



# Assuming pie2 contains the PieSegments columns and spatial coordinates

# Convert pie2 to sf object
pie2_sf <- st_as_sf(pie2, coords = c("Longitude", "Latitude"), crs = 4326)












# Assuming SegmentType is a factor, not a logical value
plot_data <- plot_data %>% mutate(SegmentType = factor(SegmentType))

ggplot() +
  geom_sf(data = pie2_sf, aes(fill = Location), color = "white", size = 0.2) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_A"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_A", color = "red"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_B"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_B", color = "blue"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data %>% filter(SegmentType == "PieSegments_C"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_C", color = "green"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  scale_fill_manual(values = c("A" = "red", "B" = "blue", "C" = "green")) +
  scale_size_manual(values = c("PieSegments_A" = 5, "PieSegments_B" = 5, "PieSegments_C" = 5)) +
  scale_color_manual(values = c("red", "blue", "green"), guide = "none") +
  labs(
    title = "Spatial Map with Pie Chart Segments",
    subtitle = "Concentration of Interest",
    fill = "Location",
    size = "SegmentType"
  ) +
  theme_minimal()


# Assuming plot_data contains PieSegments_A, PieSegments_B, PieSegments_C, Longitude, and Latitude columns

plot_data_filtered <- plot_data %>%
  filter(!is.na(SegmentType == "PieSegments_A"), !is.na(SegmentType == "PieSegments_B"), !is.na(SegmentType == "PieSegments_C"))

ggplot() +
  geom_sf(data = pie2_sf, aes(fill = Location), color = "white", size = 0.2) +
  geom_segment(
    data = plot_data_filtered %>% filter(SegmentType == "PieSegments_A"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_A", color = "red"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data_filtered %>% filter(SegmentType == "PieSegments_B"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_B", color = "blue"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  geom_segment(
    data = plot_data_filtered %>% filter(SegmentType == "PieSegments_C"),
    aes(x = Longitude, y = Latitude, xend = Longitude + (end - start), yend = Latitude, 
        size = SegmentType == "PieSegments_C", color = "green"),
    arrow = arrow(length = unit(0.02, "npc")),
    lineend = "round"
  ) +
  scale_fill_manual(values = c("A" = "red", "B" = "blue", "C" = "green")) +
  scale_size_manual(values = c("PieSegments_A" = 5, "PieSegments_B" = 5, "PieSegments_C" = 5)) +
  scale_color_manual(values = c("red", "blue", "green"), guide = "none") +
  labs(
    title = "Spatial Map with Pie Chart Segments",
    subtitle = "Concentration of Interest",
    fill = "Location",
    size = "SegmentType"
  ) +
  theme_minimal()











ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")
install.packages("ozmaps")
library(ozmaps)






library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))







ggplot(long, aes(x=Type, y=log_corrected,  fill=Length)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "PFAS Compound in Residential and Control Sites") +
  ylab(label = "Log Concentration (ug/kg)") +
  labs(title = "PFAS concentration in Current Study")+
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face="bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")

long$log_corrected<- long$Concentration
long$log_corrected<- log(long$log_corrected)

ggplot(long, aes(x=Type, y=log_corrected,  fill=functionalgroup)) + 
  stat_boxplot(geom ='errorbar') +
  geom_boxplot()+
  xlab(label = "PFAS Compound in Residential and Control Sites") +
  ylab(label = "Log Concentration (ug/kg)") +
  labs(title = "PFAS concentration in Current Study")+
  scale_fill_manual(values=c("#F9A785", "#00A8C6", "#95CACA","#4068B2"))+
  theme_bw() +
  theme(strip.text = element_blank(),
        text = element_text(size=17, family = "Arial", face= "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(hjust=.5, face = "bold"),
        plot.subtitle = element_text(hjust=.5),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, face= "bold"),
        legend.position = "bottom")

#adding to github