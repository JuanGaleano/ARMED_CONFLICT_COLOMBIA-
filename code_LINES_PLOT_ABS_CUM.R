library(readr)
library(tidyverse)
library(sf)
library(gganimate)
library(RColorBrewer)
library(ggh4x)
library(grid)
library(rsvg)
library(png)
library(magick)

# line plots ########

cumdata<-centroids|>
  st_drop_geometry()|>
  group_by(year)|>
  summarise(hom_imp=round(sum(hom_imp),0),
            des_imp=round(sum(des_imp),0))|>
  ungroup()|>
  mutate(hom_cum=cumsum(hom_imp),
         des_cum=cumsum(des_imp))|> 
  pivot_longer(cols=c("hom_imp","des_imp","hom_cum","des_cum"), names_to = "cats")|>
  mutate(facet=if_else(substr(cats,5,7)=="cum", "Eventos Acumulados sobre los años","Eventos Absolutos por año"),
         tipo =if_else(substr(cats,1,3)=="des", "Desapariciones","Homicidios"))


# 2.1 CREATE YOUR OWN THEME #####
# https://ggplot2.tidyverse.org/reference/theme.html

theme_HS<-list(theme(plot.title = element_text(lineheight=1, size=25, face="bold"), #set parameters of title
                     plot.subtitle = element_text(lineheight=1, size=12, face="bold"), # set parameters of subtitle
                     plot.caption = element_text(lineheight=1, size=13, hjust=1), # set parameters of caption
                     legend.title = element_blank (), # legend title (we don't want a title for the legens)
                     legend.text = element_text(colour="black", size = 15), # set parameters of legend text
                     legend.position="bottom", # set position of legend
                     # legend.justification=c(1,0), # set justification of legend
                     legend.background = element_rect(fill=NA, colour = NA), # set legend background
                     legend.key.size = unit(1.5, 'lines'), # set size of simbols of the legend
                     legend.key = element_rect(colour = NA, fill = NA), #set background of simbols in the legend
                     axis.title.x = element_blank (), # set x axis title (we don't want a title for the legend)
                     axis.text.x  = element_text(angle = 0,vjust=0.5, size=18,colour="black"), #set parameters of x axis text
                     axis.title.y = element_text(vjust=0.5, size=18,colour="black"), # set y axis title 
                     axis.text.y  = element_text(vjust=0.5, size=18,colour="black"), #set parameters of y axis text
                     strip.text = element_text(size=15, face="bold"), #text for facets
                     plot.background =  element_rect(fill = "white"), # set color of the background of the plot
                     panel.grid.major=element_line(colour="#E6E6E6",linewidth=.5), # set color of major grid lines
                     panel.grid.minor=element_line(colour="#E6E6E6",linewidth=.15), # set color of minor grid lines
                     panel.border = element_rect(colour = "#585858", fill=NA, linewidth=.75), #set color of panel border line
                     panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF"))) # set color of the background of the panel of the plot

options(scipen = 999)



# annotation #######
#read logo ######
png_file <- "Logo.png"
img <- image_read(png_file)
raster_img <- as.raster(image_resize(img, "500x500")) 
img_grob <- rasterGrob(img, interpolate = TRUE)


pal<-"Set1"

a4<-ggplot(cumdata,
           aes(x = year, y = value/1000)) +
  geom_line(aes(color = tipo,group=tipo), linewidth = 2) + # html color codes: https://html-color-codes.info/
  #geom_point(aes(color = tipo), size = 3) + 
  geom_text(aes(x = year,
                y = value/1000,
                label=tipo,
                color= tipo),
            nudge_x = c(3.5, 4.5),
            check_overlap = TRUE,
            #color =  "#2E2E2E",
            fontface = "bold",
            size=5.5)+
  ggh4x::facet_grid2(.~facet, scales = "free_y", independent = "y")+
  scale_y_continuous(limits = c(0, 45), breaks=c(seq(0,45,5))) +
  scale_y_facet(COL == 2, limits = c(0, 450), breaks=c(seq(0,450,50)))+
  
  #Logo ######
annotation_custom(img_grob,  xmin = 1983, xmax = 1995, ymin = 385, ymax = 485)+
  annotate("text", x = 1989, y = 400, label = "Dataviz: @juan_galeano",size=3.5)+
  scale_x_continuous(breaks = seq(1985, 2025, by = 5), limits=c(1985,2025)) +
  scale_color_manual(values=c(brewer.pal(3, pal)[1],brewer.pal(3, pal)[2]))+
  labs(
    title = "Personas desaparecidas y asesinadas, Colombia 1985-2018",
    subtitle ='Datos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).',
    x = "",
    y = "Evento (en miles)\n",
    caption = ""
  ) +
  
  theme_HS+theme(legend.position = "none",
                 plot.margin = margin(1,1,1,1,"cm"),
                 plot.title=element_text(size=45, 
                                         hjust=0.5,
                                         face="bold", 
                                         color="#A4A4A4",
                                         vjust=-1,
                                         lineheight=2),
                 plot.subtitle =element_text(size=25, 
                                             hjust=0.5,
                                             face="bold", 
                                             color="#A4A4A4",
                                             vjust=-1,
                                             lineheight=2),
                 strip.text = element_text(size=20, 
                                           hjust=0.5,
                                           face="bold"),
                 strip.background =  element_rect(colour = "#585858", fill=NA, linewidth=.75),
                 plot.caption = element_text(size=15, 
                                             hjust=.5,
                                             face="bold",  
                                             color="#A4A4A4"))+
  transition_reveal(year)

#a4

library(av)
df <- animate(a4, renderer = av_renderer('line_plot_both.mp4'),
              nframes = 150,# length(unique(deaths$state)),
              width = 3000,
              height = 1800,
              res = 150,
              duration=15)

anim_save(
  nframes = 150,# length(unique(deaths$state)),
  width = 3000,
  height = 1800,
  res = 150,
  duration=15,
  "line_ploth_both.gif", a4)
