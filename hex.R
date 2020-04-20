#library(cowplot)
# library(paletteer)
# library(extrafont)
# library(sf)
# 
# library(prismatic)
# library(here)
# library(magick)

#custom theme
theme_aidan <- function () { 
  theme_minimal(base_size=12) %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'snow1', color = "snow1"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}

#Get hex boundaries 
us_hex <- st_read("Hex States Shapefile/HexStates.shp") %>%
  janitor::clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  st_transform(crs = 3395)


#Combine hex boundaries with our covid data
DF <- left_join(us_hex, DF, by = c("state_abbr" = "state"))


#Find the center of each hex (state) so that we can add text 
centers <- 
  st_centroid(us_hex) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  set_names(str_to_lower)

DF_center <- tibble(abbr = us_hex$state_abbr) %>% 
  bind_cols(centers)

#Combine the centeroid data with the orginial data frame
DF <- left_join(DF, DF_center, by = c("state_abbr" = "abbr"))


#Make the text color white if it's above the third quartile deaths per million and black otherwise
DF$textColor <- ifelse(DF$total.deaths.per.million >= quantile(DF$total.deaths.per.million, .75, na.rm = TRUE), "white", "black")


#Create an outline of the United States
us_hex_outline <- us_hex %>%
  st_union() %>%
  st_buffer(dist = 30000)


output$plot2 <- renderImage({
  # Read plot2's width and height. These are reactive values, so this
  # expression will re-run whenever these values change.
  width  <- session$clientData$output_plot2_width
  height <- session$clientData$output_plot2_height
  
  
  #Create plot
  p <- DF %>%
    ggplot() +
    geom_sf(aes(fill = total.deaths.per.million), size = 1, color = 'white') +
    geom_sf(data = us_hex_outline, color = "#e32b1e", fill = "transparent", size = 1) +
    geom_text(data = DF, aes(x, y, label = state_abbr, color = textColor), 
              size = 2.1, fontface = 'bold') +
    geom_text(data = DF, aes(x, y, label = round(total.deaths.per.million, 2), color = textColor), 
              size = 2.25, fontface = 'bold', vjust = 2)+ 
    rcartocolor::scale_fill_carto_c(
      name = "Confirmed Deaths Per Million Residents",
      palette = 10,
      trans = "log10", 
      breaks = scales::log_breaks(n = 8)) +
    theme_aidan()  +
    coord_sf(ndiscr = 0) +
    scale_color_identity() +
    theme(text=element_text(size=14), 
          plot.title = element_text(hjust = 0.5, face = "bold",  vjust = 0, size = 15),
          plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 0), 
          plot.caption = element_text(face = "italic", size = 8, hjust = .5, vjust = 8), 
          legend.spacing.x = unit(0, 'cm'), 
          legend.title=element_text(size=11), 
          legend.text = element_text(size = rel(0.6)), 
          legend.margin=margin(10,0,-1,0),
          legend.position = 'bottom',
          plot.margin = margin(0, -.5, 0, -.5, "cm"), 
          legend.box.margin=margin(-30,0,15,0))  +
    guides(fill=guide_legend(
      keywidth=.5,
      keyheight=.15,
      default.unit="inch", 
      label.position = 'bottom', 
      title.position = 'top',
      title.hjust = .5,
      title.vjust = 0,
      label.vjust = 3,
      nrow = 1)) +
    labs(title = "Confirmed Deaths Due To COVID-19 Per Million Residents", 
         caption  = paste0("Data updated ", format(Sys.time(), "%b %d %X")), 
         subtitle = paste0("Total Confirmed Deaths Due To COVID-19 In The United States: ", scales::comma_format()(sum(DF$death, na.rm = TRUE))))
  
  
  #Use the cowplot package to color in the white area
  # cowplot::ggdraw(p) + 
  #   theme(plot.background = element_rect(fill="snow1", color = NA))
  
  ggsave('DeathsPerMillion.png', width = 7, height = 8, dpi = 400)
  
  image_read('DeathsPerMillion.png') %>% image_write('DeathsPerMillion.png')
  # Return a list containing the filename
  list(src = 'DeathsPerMillion.png',
       width = min(width+200, 1200),
       height = min(height+400, 1600))
}, deleteFile = F)
