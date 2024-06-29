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
# read data ####
datos <- read_csv("tabla.desaparicionesyhomicidios.csv")
names(datos)

departamentos <- datos |> distinct(nom.depto, dept_code_hecho)

##### clean data ######
colnames(datos)[3] <- "NAME_1"

datos <- datos |> select(-1)
colnames(datos) <- c(
  "year",
  "NAME_1",
  "code_1",
  "des_obv",
  "des_low",
  "des_imp",
  "des_upp",
  "hom_obv",
  "hom_low",
  "hom_imp",
  "hom_upp"
)


# read shp #######
shp_col <- read_sf("./shps", "gadm41_COL_1")

shp_col_data <- shp_col |>
  left_join(datos, by = "NAME_1") |>
  mutate(across(where(is.numeric), ~ replace_na(.x, .5)))
st_crs(shp_col)

# GET A MAPTILE #####
library(ggmap)
st_bbox(shp_col)

bbox<-st_bbox(shp_col)

left<-as.numeric(st_bbox(shp_col)[1])
bottom<-as.numeric(st_bbox(shp_col)[2])
right<-as.numeric(st_bbox(shp_col)[3])
top<-as.numeric(st_bbox(shp_col)[4])


left<-(as.numeric(bbox[1])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
right<-(as.numeric(bbox[3])-mean(as.numeric(bbox[c(1,3)])))*1+mean(as.numeric(bbox[c(1,3)]))
bottom<-(as.numeric(bbox[2])-mean(as.numeric(bbox[c(2,4)])))*.85+mean(as.numeric(bbox[c(2,4)]))
top<-(as.numeric(bbox[4])-mean(as.numeric(bbox[c(2,4)])))*.85+mean(as.numeric(bbox[c(2,4)]))


TILE_COL<-get_stadiamap(bbox = c(left = left, 
                                 bottom = bottom, 
                                 right = right,
                                 top = top), 
                        zoom = 8, 
                        maptype = c("stamen_terrain"), 
                        crop = TRUE, 
                        messaging = FALSE)
TILE_COL_G <- ggmap(TILE_COL, 
                    extent = "panel", 
                    legend = "topleft", 
                    darken=0)
TILE_COL_G


# desapariciones_ipm ######
summary(shp_col_data$des_imp)
quantile(shp_col_data$des_imp, probs = seq(0, 1, by = .1), na.rm=TRUE)

shp_col_data$indicator<-shp_col_data$des_imp

shp_col_data<-shp_col_data|>mutate(indicator2=ifelse(des_imp==0, "Zero",
                                                     ifelse(des_imp<=2, "2",
                                                            ifelse(des_imp<=100, "100",
                                                                   ifelse(des_imp<=500, "500",
                                                                          ifelse(des_imp<=1000, "1000",">1500"))))))


shp_col_data$indicator2 <-factor(shp_col_data$indicator2,
                                 levels = c( 
                                   
                                   "Zero",
                                   "2",
                                   "100",
                                   "500",
                                   "1000",
                                   ">1500"))


centroids<- st_centroid(shp_col_data)

levels(centroids$indicator2)

mysizes <-c(.1,
            .6,
            3,
            7.1,
            10,
            15)


names(mysizes) <- levels(centroids$indicator2)

myalphas <-c(0,
             1,
             .5,
             .35,
             .20,
             .1)

names(myalphas) <- levels(centroids$indicator2)

# centroides ######
centroids <- cbind(centroids, st_coordinates(centroids))

left
right

# annotations and logo #######

#read logo ######
png_file <- "Logo.png"
img <- image_read(png_file)
raster_img <- as.raster(image_resize(img, "500x500")) 
img_grob <- rasterGrob(img, interpolate = TRUE)

#year######
year<-data.frame(year=c(1985:2018),
                 LON= -74.33963,
                 LAT=-1.4)

year_sf<-st_as_sf(year, 
                  coords=c("LON", "LAT"),
                  crs=4236)

year_sf<- cbind (year_sf,st_coordinates(year_sf))

#cumsum data ######
cumdata<-centroids|>
  st_drop_geometry()|>
  group_by(year)|>
  summarise(des_imp=round(sum(des_imp),0))|>
  ungroup()|>
  mutate(cum_des=cumsum(des_imp),
         LON= -74.33963,
         LAT=-2.4)

cumdata_sf<-st_as_sf(cumdata, 
                     coords=c("LON", "LAT"),
                     crs=4236)

cumdata_sf<- cbind (cumdata_sf,st_coordinates(cumdata_sf))

# titulo ####
titol<-data.frame(titol="DESAPARICIONES POR DEPARTAMENTO, COLOMBIA 1985-2018",
                  LON= -74.33963,
                  LAT=14.1)
#caption #######
caption<-data.frame(titol="Datos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).",
                    LON= -74.33963,
                    LAT=13.55)

#dataviz######
dataviz<-data.frame(titol="Dataviz: @juan_galeano",
                    LON= -68.52,
                    LAT=-2.4)

#colors #########
pal<-"Set1"
brewer.pal(3, pal)
my_colors1<-c(brewer.pal(3, pal))[1]

# gganimate map 1 ######
a1 <- TILE_COL_G +
  #title ######
geom_text(data=titol, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "#2E2E2E",
          fontface = "bold",
          size=2.95,
          inherit.aes = FALSE)+
  #caption ########
geom_text(data=caption, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "#2E2E2E",
          fontface = "bold",
          size=1.5,
          inherit.aes = FALSE)+
  #dataviz ######
geom_text(data=dataviz, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "black",
          fontface = "bold",
          size=1.3,
          inherit.aes = FALSE)+
  #year #######
geom_text(data=year, aes(x=LON, y=LAT,label=as.character(year)),
          color =  "#D8D8D8",
          fontface = "bold",
          size=12,
          inherit.aes = FALSE)+
  #cumsum text ######
geom_text(data=cumdata, aes(x=LON, y=LAT,label=paste("Desapariciones acumuladas: ", as.character(format(cum_des,big.mark="'")),sep="")),
          color =  "#D8D8D8",
          fontface = "bold",
          size=2,
          inherit.aes = FALSE)+
  #centroids ######
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(group = NAME_1),
  alpha = .5,
  size = .25,
  color =my_colors1,
  inherit.aes = FALSE
) +
  #bubbles ######
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(size = indicator2, group = NAME_1),
  color = my_colors1,
  shape = 21,
  size=.05,
  inherit.aes = FALSE
) +
  #bubbles ######
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(
    size = indicator2,
    alpha = indicator2,
    group = NAME_1
  ),
  color = my_colors1,
  inherit.aes = FALSE
) +
  #scale size ######
scale_size_manual(
  values = mysizes * 2,
  name = "Personas\ndesaparecidas",
  breaks = c("2" , 
             "100", 
             "500", 
             "1000",
             ">1500"),
  labels = c("<2", 
             "100", 
             "500", 
             "1000", 
             ">1500"),
  #guide legend #####
  guide = guide_legend(
    direction = "vertical",
    nrow = 5,
    keywidth = 2,
    keyheight=1,
    label.position = "bottom"
  )
) +
  #scale alpha ######
scale_alpha_manual(values = myalphas * 1) +
  #Logo ######
annotation_custom(img_grob, xmin = -70.5 , xmax = -66.5 , ymin =  -3.8, ymax =  0.8)+
  #legend guide #####
guides(colour = "none", alpha = "none") +
  #theme ######
theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      lineheight = 1,
      size = (24 / 2) - 4,
      color  =  "#bdbdbd",
      hjust = 0.5
    ),
    plot.caption = element_text(
      # vjust = 0.5,
      size = 18 / 2,
      colour = "#bdbdbd",
      hjust = 0.5
    ),
    plot.margin = margin(.1,.1,.1,.1,"cm"),
    legend.position = "right",
    legend.text = element_text(size = (18 / 2)-1.5, color  =  "#bdbdbd",face = "bold", hjust = 0.5),
    legend.title = element_text(size = (18 / 2)-1.5, color  =  "#bdbdbd",face = "bold",hjust = 0.5),
    panel.background = element_rect(fill = "#2E2E2E", color  =  "#2E2E2E"),
    plot.background = element_rect(fill = "#2E2E2E", color  = "#2E2E2E"),
    panel.grid.major = element_line(
      color = "white",
      linewidth = 0.15,
      linetype = 2
    )
  ) +
  #animated the plot ######
transition_time(as.integer(year)) +
  ease_aes('linear')

# animated gif ######
anim_save(duration=15,
          nframes=150,
          width = 1480,
          height = 1260,
          res = 300,
          "1_MAP_DESAPARICIONES.gif", a1)

# animated mp4 #######
library(av)
df <- animate(a1, renderer = av_renderer('1_MAP_DESAPARICIONES.mp4'),
              nframes = 150,# length(unique(deaths$state)),
              width = 1480,
              height = 1260,
              res = 300,
              duration=15)


# hom_imp ######
summary(centroids$hom_imp)
quantile(centroids$hom_imp, probs = seq(0, 1, by = .1), na.rm=TRUE)

centroids$indicator<-centroids$hom_imp

centroids<-centroids|>mutate(indicator3=ifelse(hom_imp==0, "Zero",
                                               ifelse(hom_imp<=8, "8",
                                                      ifelse(hom_imp<=210, "200",
                                                             ifelse(hom_imp<=800, "800",
                                                                    ifelse(hom_imp<=3000, "3000",">5000"))))))


centroids$indicator3 <-factor(centroids$indicator3,
                              levels = c( 
                                
                                "Zero",
                                "8",
                                "200",
                                "800",
                                "3000",
                                ">5000"))

centroids<- st_centroid(centroids)

levels(centroids$indicator3)

mysizes <-c(.1,
            .6,
            3,
            7.1,
            10,
            15)



names(mysizes) <- levels(centroids$indicator3)

myalphas <-c(0,
             1,
             .5,
             .35,
             .20,
             .1)

names(myalphas) <- levels(centroids$indicator3)


# annotation #######
#read logo ######
png_file <- "Logo.png"
img <- image_read(png_file)
raster_img <- as.raster(image_resize(img, "500x500")) 
img_grob <- rasterGrob(img, interpolate = TRUE)
#year #####
year<-data.frame(year=c(1985:2018),
                 LON= -74.33963,
                 LAT=-1.4)

year_sf<-st_as_sf(year, 
                  coords=c("LON", "LAT"),
                  crs=4236)

year_sf<- cbind (year_sf,st_coordinates(year_sf))

#cum sum #####
cumdata<-centroids|>
  st_drop_geometry()|>
  group_by(year)|>
  summarise(hom_imp=round(sum(hom_imp),0))|>
  ungroup()|>
  mutate(cum_hom=cumsum(hom_imp),
         LON= -74.33963,
         LAT=-2.4)

cumdata_sf<-st_as_sf(cumdata, 
                     coords=c("LON", "LAT"),
                     crs=4236)

cumdata_sf<- cbind (cumdata_sf,st_coordinates(cumdata_sf))

# titulo ######
titol<-data.frame(titol="HOMICIDIOS POR DEPARTAMENTO, COLOMBIA 1985-2018",
                  LON= -74.33963,
                  LAT=14.1)

#caption #######
caption<-data.frame(titol="Datos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).",
                    LON= -74.33963,
                    LAT=13.55)

#dataviz######
dataviz<-data.frame(titol="Dataviz: @juan_galeano",
                    LON= -68.52,
                    LAT=-2.4)

# gganimate map 2 ######

library(RColorBrewer)

pal<-"Set1"
brewer.pal(3, pal)

my_colors<-c(brewer.pal(3, pal))[2]


a1 <- TILE_COL_G +
  #title #####
geom_text(data=titol, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "#2E2E2E",
          fontface = "bold",
          size=2.95,
          inherit.aes = FALSE)+
  
  #caption ########
geom_text(data=caption, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "#2E2E2E",
          fontface = "bold",
          size=1.5,
          inherit.aes = FALSE)+
  #dataviz ######
geom_text(data=dataviz, aes(x=LON, y=LAT,label=as.character(titol)),
          color =  "black",
          fontface = "bold",
          size=1.3,
          inherit.aes = FALSE)+
  # year ######
geom_text(data=year, aes(x=LON, y=LAT,label=as.character(year)),
          color =  "#D8D8D8",
          fontface = "bold",
          size=12,
          inherit.aes = FALSE)+
  # homicidios acu ######
geom_text(data=cumdata, aes(x=LON, y=LAT,label=paste("Homicidios acumulados: ", as.character(format(cum_hom,big.mark="'")),sep="")),
          color =  "#D8D8D8",
          fontface = "bold",
          size=2,
          inherit.aes = FALSE)+
  
  #annotate("text", x =-74.33963 , y = -.717861, label = "año: {frame_time}", fontface="bold",size=42)+
  #geom_sf(data=shp_col,color="black",fill=NA, linewidth=.1,inherit.aes = FALSE)+
  # layer 1 #####
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(group = NAME_1),
  alpha = .5,
  size = .25,
  color =my_colors,
  inherit.aes = FALSE
) +
  # layer 2 #####
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(size = indicator3, group = NAME_1),
  color = my_colors,
  shape = 21,
  linewidth=.05,
  inherit.aes = FALSE
) +
  # layer 3 #####
geom_sf(
  data = centroids |> filter(year <= 2018),
  aes(
    size = indicator3,
    alpha = indicator3,
    group = NAME_1
  ),
  color = my_colors,
  inherit.aes = FALSE
) +
  # scales size ######
scale_size_manual(
  values = mysizes * 2,
  name = "Personas\nasesinadas",
  breaks = c("8" , 
             "200", 
             "800", 
             "3000",
             ">5000"),
  labels = c("8",
             "200",
             "800",
             "3000",
             ">5000"),
  guide = guide_legend(
    direction = "vertical",
    nrow = 5,
    keywidth = 3,
    label.position = "bottom"
  )
) +
  # scales alpha #####
scale_alpha_manual(values = myalphas * 1) +
  #Logo ######
annotation_custom(img_grob, xmin = -70.5 , xmax = -66.5 , ymin =  -3.8, ymax =  0.8)+
  #legend guide #####
guides(colour = "none", alpha = "none") +
  
  #theme ######
theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      lineheight = 1,
      size = (24 / 2) - 4,
      color  =  "#bdbdbd",
      hjust = 0.5
    ),
    plot.caption = element_text(
      # vjust = 0.5,
      size = 18 / 2,
      colour = "#bdbdbd",
      hjust = 0.5
    ),
    plot.margin = margin(.1,.1,.1,.1,"cm"),
    legend.position = "right",
    legend.text = element_text(size = (18 / 2)-1.5, color  =  "#bdbdbd",face = "bold", hjust = 0.5),
    legend.title = element_text(size = (18 / 2)-1.5, color  =  "#bdbdbd",face = "bold",hjust = 0.5),
    panel.background = element_rect(fill = "#2E2E2E", color  =  "#2E2E2E"),
    plot.background = element_rect(fill = "#2E2E2E", color  = "#2E2E2E"),
    panel.grid.major = element_line(
      color = "white",
      linewidth = 0.15,
      linetype = 2
    )
  ) +
  # animate ######
transition_time(as.integer(year)) +
  ease_aes('linear')

# animated gif ######
anim_save(duration=15,
          nframes=150,
          width = 1480,
          height = 1260,
          res = 300,
          "2_MAP_HOMICIDIOS.gif", a1)

# animated mp4 #######
library(av)
df <- animate(a1, renderer = av_renderer('2_MAP_HOMICIDIOS.mp4'),
              nframes = 150,# length(unique(deaths$state)),
              width = 1480,
              height = 1260,
              res = 300,
              duration=15)