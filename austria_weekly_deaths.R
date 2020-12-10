library("dplyr")
library("rgdal")
library("RColorBrewer")
library("ggplot2")
library(tidyverse)
library(gganimate)
library(viridis)
library(hrbrthemes)
library(directlabels)
library(ggpubr)
library(magick)

EU_NUTS <- readOGR(dsn = "G:/Projects/austria_deaths_2020/map_data", layer = "NUTS_RG_01M_2021_4326_LEVL_3")
#ggplot() +
#  geom_polygon(data = EU_NUTS, aes(x = Longitude, y = Latitude, group = group), fill="lightgray", colour = "white")

country <- substring(as.character(EU_NUTS$NUTS_ID), 1, 2)
map <- c("AT")
aut_nuts <- EU_NUTS[country %in% map,]
aut_nuts@data
aut_df <- fortify(aut_nuts, region="FID")

aut_data <- read.csv("G:/Projects/austria_deaths_2020/data/demo_r_mwk3_ts_1_Data.csv")
aut_data <- aut_data %>% select(-Flag.and.Footnotes, -UNIT) %>% spread(SEX, Value)

aut_data_cumm <- aut_data

for(row in 1:nrow(aut_data)){
  if(row > 35){
    prev <- aut_data_cumm[row-35, "Insgesamt"]
    num <- aut_data_cumm[row, "Insgesamt"]
    aut_data_cumm[row, "Insgesamt"] <- num+prev
    #cat("Row:",row,", Prev:",prev," , Num:",num,", Sum:",num+prev,"\n")
  }
}

map_df <- left_join(aut_df, aut_data_cumm, by = c('id'='GEO'))

vmin_insg <- min(map_df$Insgesamt, na.rm=T)
vmax_insg <- max(map_df$Insgesamt, na.rm=T)

p <- ggplot(data = map_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Insgesamt), color = "black") +
  transition_states(TIME) +
  #transition_states(TIME, transition_length = 0.5, state_length = 2.0) +
  scale_fill_viridis(name="Overall Deaths per NUTS3 Area", begin = 0, end = 1, limits = c(vmin_insg,vmax_insg), na.value="gray99") +
  ease_aes('sine-in-out') +
  labs(title = "Week: {closest_state}") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.05, size=25)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme( #legend.position = "bottom", 
         legend.direction = "horizontal",
         legend.position = c(0.2, .08),
         legend.key.size = unit(1.1, "cm"),)

aut_data <- read.csv("G:/Projects/austria_deaths_2020/data/demo_r_mwk3_ts_1_Data.csv")
gender_sum_weekly <- aut_data %>% select(-Flag.and.Footnotes, -UNIT) %>% spread(SEX, Value) %>% select(-Insgesamt, -GEO) %>% group_by(TIME) %>% summarize_all(sum) %>% mutate(TIME = str_remove(TIME, "^2020"))
gender_sum_weekly <- pivot_longer(gender_sum_weekly, cols=2:3, names_to = "Gender", values_to = "Anzahl")
gender_sum_weekly$Gender[gender_sum_weekly$Gender == "Frauen"] <- "Women"
gender_sum_weekly$Gender[gender_sum_weekly$Gender == "Männer"] <- "Men"

line <- ggplot(data = gender_sum_weekly, aes(x=TIME, y=Anzahl, group=Gender, color=Gender)) +
  geom_line(size=1.3) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #transition_manual(TIME)
  transition_reveal(readr::parse_number(TIME))+
  labs(y="Number of Deaths", x="Week")


line_anim <- animate(line, fps=7, nframes=100, width = 800, height = 200, renderer = gifski_renderer())
map_anim <- animate(p, fps=7, nframes=100, width = 800, height = 450, renderer = gifski_renderer())

#animate(p, nframes = 150, fps = 20, duration = 15, width = 800, height = 400, renderer = gifski_renderer(), rewind = FALSE)
#anim_save("G:/Projects/austria_deaths_2020/austria_animation.mp4", p)

