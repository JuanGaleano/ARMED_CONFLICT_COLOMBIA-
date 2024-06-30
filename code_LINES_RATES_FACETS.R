##### RATES FACETS #####
# Desapariciones ######
library(readr)
rates <- read_csv("desaparicionesyhomicidios.conpoblacion.csv")

rates<-rates|>
  select(-1,-5)
colnames(rates)<-c("name_dp","nom_depto","year","pop","des_obv",
                   "des_low",
                   "des_imp",
                   "des_upp",
                   "hom_obv",
                   "hom_low",
                   "hom_imp",
                   "hom_upp",
                   "pop_m",
                   "des_rate",
                   "hom_rate",
                   "des_hom_rate")


rates_mean<-rates|>
  group_by(nom_depto)|>
  summarise(mean_rate=mean(des_rate),
            cat_col=if_else(mean_rate<6.603323,"1 quintil de peligrosidad (- peligroso)",
                            if_else(mean_rate<8.962127,"2 quintil de peligrosidad",
                                    if_else(mean_rate<13.790209,"3 quintil de peligrosidad",
                                            if_else(mean_rate<23.698760,"4 quintil de peligrosidad",
                                                    if_else(mean_rate<=129,"5 quintil de peligrosidad (+ peligroso)","0"))))))|>
  ungroup()|>
  arrange(-mean_rate)

rates<-rates|>
  left_join(rates_mean, by="nom_depto")|>
  mutate(nom_depto=as.factor(nom_depto),
         nom_depto=fct_relevel(nom_depto,
                               rates_mean$nom_depto),
         cat_col=as.factor(cat_col),
         cat_col=fct_rev(cat_col))

levels(rates$nom_depto)
levels(rates$cat_col)
quantile(rates_mean$mean_rate, probs = seq(0, 1, by = .2), na.rm=TRUE)

myColors <- c(brewer.pal(5, "Reds"))

#read logo ######
png_file <- "Logo.png"
img <- image_read(png_file)
raster_img <- as.raster(image_resize(img, "500x500")) 
img_grob <- rasterGrob(img, interpolate = TRUE)


gg2<-ggplot(rates, 
            aes(x=year, 
                y=des_rate))+
  geom_rect(data = rates,aes(fill = as.factor(cat_col)),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1)+
  geom_line(color= "#2E2E2E",linewidth=1.25)+
  # coord_cartesian(clip = 'off') +
  # annotation_custom(img_grob,  xmin = 2020, xmax = 2025, ymin = -100, ymax = 0)+
  
  facet_wrap(.~nom_depto,ncol=6, scales = "free")+
  scale_y_facet(PANEL  == 4, limits = c(0, 132), breaks=c(seq(0,132,33)))+
  scale_y_facet(PANEL  == 5, limits = c(0, 100), breaks=c(seq(0,100,25)))+
  scale_y_facet(PANEL  == 7, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 9, limits = c(0, 120), breaks=c(seq(0,120,30)))+ 
  scale_y_facet(PANEL  == 10, limits = c(0, 40), breaks=c(seq(0,40,10)))+ 
  scale_y_facet(PANEL  == 11, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 16, limits = c(0, 40), breaks=c(seq(0,40,10)))+ 
  scale_y_facet(PANEL  == 22, limits = c(0, 28), breaks=c(seq(0,25,5)))+ 
  scale_y_facet(PANEL  == 23, limits = c(0, 20), breaks=c(seq(0,20,5)))+ 
  scale_y_facet(PANEL  == 28, limits = c(0, 20), breaks=c(seq(0,20,5)))+ 
  scale_y_facet(PANEL  == 30, limits = c(0, 20), breaks=c(seq(0,20,5)))+
  scale_y_facet(PANEL  == 31, limits = c(0, 15), breaks=c(seq(0,15,5)))+
  scale_y_facet(PANEL  == 32, limits = c(0, 15), breaks=c(seq(0,15,5)))+
  scale_y_facet(PANEL  == 33, limits = c(0, 15), breaks=c(seq(0,15,5)))+
  scale_x_continuous(limits=c(1985, 2020),breaks = seq(1985,2020, by= 10))+
  scale_fill_manual(values=rev(myColors))+
  labs(title="Tasa bruta de desapariciones por departamento, Colombia 1985-2018\n",
       subtitle = "",
       x="",
       y="Tasa x 100,000 personas\n", 
       caption="\nDatos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).")+
  theme_HS+theme(legend.position="bottom",
                 legend.key.width=unit(1.5,"cm"),
                 plot.margin = margin(1,1,1,1,"cm"),
                 plot.title=element_text(size=35, 
                                         hjust=0.5,
                                         face="bold", 
                                         color="#A4A4A4",
                                         vjust=-1,
                                         lineheight=.5),
                 strip.background =  element_rect(colour = "#585858", fill=NA, linewidth=.75),
                 plot.caption = element_text(size=20, hjust=.5,face="bold",  color="#A4A4A4",))


ggsave(
  "3_lineplot_EDITEDC.png",
  scale = 1,
  height = 15,
  width = 20,
  dpi = 300
)

quantile(rates_mean$mean_rate, probs = seq(0, 1, by = .2), na.rm=TRUE)

# Homicidios ######

library(readr)
rates <- read_csv("desaparicionesyhomicidios.conpoblacion.csv")

rates<-rates|>
  select(-1,-5)
colnames(rates)<-c("name_dp","nom_depto","year","pop","des_obv",
                   "des_low",
                   "des_imp",
                   "des_upp",
                   "hom_obv",
                   "hom_low",
                   "hom_imp",
                   "hom_upp",
                   "pop_m",
                   "des_rate",
                   "hom_rate",
                   "des_hom_rate")



rates_mean<-rates|>
  group_by(nom_depto)|>
  summarise(mean_rate=mean(hom_rate),
            cat_col=if_else(mean_rate<15.74957,"1 quintil de peligrosidad (- peligroso)",
                            if_else(mean_rate<27.02746,"2 quintil de peligrosidad",
                                    if_else(mean_rate<33.47397,"3 quintil de peligrosidad",
                                            if_else(mean_rate<54.91671,"4 quintil de peligrosidad",
                                                    if_else(mean_rate<=153,"5 quintil de peligrosidad (+ peligroso)","0"))))))|>
  ungroup()|>
  arrange(-mean_rate)

rates<-rates|>
  left_join(rates_mean, by="nom_depto")|>
  mutate(nom_depto=as.factor(nom_depto),
         nom_depto=fct_relevel(nom_depto,
                               rates_mean$nom_depto),
         cat_col=as.factor(cat_col),
         cat_col=fct_rev(cat_col))

levels(rates$nom_depto)
levels(rates$cat_col)
quantile(rates_mean$mean_rate, probs = seq(0, 1, by = .2), na.rm=TRUE)

myColors <- c(brewer.pal(5, "Blues"))

gg2<-ggplot(rates, 
            aes(x=year, 
                y=hom_rate))+
  geom_rect(data = rates,aes(fill = as.factor(cat_col)),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1)+
  geom_line(color= "#2E2E2E",linewidth=1.25)+
  facet_wrap(.~nom_depto,ncol=6, scales = "free")+
  scale_y_facet(PANEL  == 1, limits = c(0, 300), breaks=c(seq(0,300,75)))+
  scale_y_facet(PANEL  == 4, limits = c(0, 300), breaks=c(seq(0,300,75)))+
  scale_y_facet(PANEL  == 5, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 6, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 7, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 8, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 9, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 10, limits = c(0, 160), breaks=c(seq(0,160,40)))+
  scale_y_facet(PANEL  == 11, limits = c(0, 100), breaks=c(seq(0,100,25)))+ 
  scale_y_facet(PANEL  == 12, limits = c(0, 120), breaks=c(seq(0,120,30)))+ 
  scale_y_facet(PANEL  == 15, limits = c(0, 80), breaks=c(seq(0,80,20)))+ 
  scale_y_facet(PANEL  == 16, limits = c(0, 80), breaks=c(seq(0,80,20)))+ 
  scale_y_facet(PANEL  == 17, limits = c(0, 90), breaks=c(seq(0,80,20)))+ 
  scale_y_facet(PANEL  == 18, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 19, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 20, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 21, limits = c(0, 80), breaks=c(seq(0,80,20)))+ 
  scale_y_facet(PANEL  == 22, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 23, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 25, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 26, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 27, limits = c(0, 60), breaks=c(seq(0,60,15)))+ 
  scale_y_facet(PANEL  == 28, limits = c(0, 40), breaks=c(seq(0,40,10)))+ 
  scale_y_facet(PANEL  == 29, limits = c(0, 30), breaks=c(seq(0,30,10)))+ 
  scale_y_facet(PANEL  == 30, limits = c(0, 35), breaks=c(seq(0,30,10)))+ 
  scale_y_facet(PANEL  == 31, limits = c(0, 30), breaks=c(seq(0,30,10)))+
  scale_y_facet(PANEL  == 32, limits = c(0, 50), breaks=c(seq(0,45,15)))+
  scale_y_facet(PANEL  == 33, limits = c(0, 20), breaks=c(seq(0,20,5)))+
  # # scale_y_facet(PANEL  == 30, limits = c(0, 20), breaks=c(seq(0,20,5)))+
  # # scale_y_facet(PANEL  == 31, limits = c(0, 15), breaks=c(seq(0,15,5)))+
  
  scale_x_continuous(limits=c(1985, 2020),breaks = seq(1985,2020, by= 10))+
  scale_fill_manual(values=rev(myColors))+
  labs(title="Tasa bruta de homicidios por departamento, Colombia 1985-2018\n",
       subtitle = "",
       x="",
       y="Tasa x 100,000 personas\n", 
       caption="\nDatos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).")+
  theme_HS+theme(legend.position="bottom",
                 legend.key.width=unit(1.5,"cm"),
                 plot.margin = margin(1,1,1,1,"cm"),
                 plot.title=element_text(size=35, 
                                         hjust=0.5,
                                         face="bold", 
                                         color="#A4A4A4",
                                         vjust=-1,
                                         lineheight=.5),
                 strip.background =  element_rect(colour = "#585858", fill=NA, linewidth=.75),
                 plot.caption = element_text(size=20, hjust=.5,face="bold",  color="#A4A4A4",))




ggsave(
  "3_lineplot_EDITEDD.png",
  scale = 1,
  height = 15,
  width = 20,
  dpi = 300
)



# Desapariciones + Homicidios ######

library(readr)
rates <- read_csv("desaparicionesyhomicidios.conpoblacion.csv")

rates<-rates|>
  select(-1,-5)

colnames(rates)<-c("name_dp","nom_depto","year","pop","des_obv",
                   "des_low",
                   "des_imp",
                   "des_upp",
                   "hom_obv",
                   "hom_low",
                   "hom_imp",
                   "hom_upp",
                   "pop_m",
                   "des_rate",
                   "hom_rate",
                   "des_hom_rate")

rates_mean<-rates|>
  group_by(nom_depto)|>
  summarise(mean_rate=mean(des_hom_rate),
            cat_col=if_else(mean_rate<25.53131,"1 quintil de peligrosidad (- peligroso)",
                            if_else(mean_rate< 36.67748,"2 quintil de peligrosidad",
                                    if_else(mean_rate<49.17398,"3 quintil de peligrosidad",
                                            if_else(mean_rate<74.31790,"4 quintil de peligrosidad",
                                                    if_else(mean_rate<=266,"5 quintil de peligrosidad (+ peligroso)","0"))))))|>
  ungroup()|>
  arrange(-mean_rate)

rates<-rates|>
  left_join(rates_mean, by="nom_depto")|>
  mutate(nom_depto=as.factor(nom_depto),
         nom_depto=fct_relevel(nom_depto,
                               rates_mean$nom_depto),
         cat_col=as.factor(cat_col),
         cat_col=fct_rev(cat_col))

levels(rates$nom_depto)
levels(rates$cat_col)
quantile(rates_mean$mean_rate, probs = seq(0, 1, by = .2), na.rm=TRUE)

myColors <- c(brewer.pal(5, "Purples"))

gg2<-ggplot(rates, 
            aes(x=year, 
                y=des_hom_rate))+
  geom_rect(data = rates,aes(fill = as.factor(cat_col)),xmin = -Inf,xmax = Inf,
            ymin = -Inf,ymax = Inf,alpha = 1)+
  geom_line(color= "#2E2E2E",linewidth=1.25)+
  facet_wrap(.~nom_depto,ncol=6, scales = "free")+
  scale_y_facet(PANEL  == 2, limits = c(0, 500), breaks=c(seq(0,500,100)))+
  scale_y_facet(PANEL  == 3, limits = c(0, 400), breaks=c(seq(0,400,100)))+
  scale_y_facet(PANEL  == 4, limits = c(0, 400), breaks=c(seq(0,400,100)))+
  scale_y_facet(PANEL  == 5, limits = c(0, 250), breaks=c(seq(0,250,50)))+
  scale_y_facet(PANEL  == 6, limits = c(0, 200), breaks=c(seq(0,200,50)))+
  scale_y_facet(PANEL  == 7, limits = c(0, 250), breaks=c(seq(0,250,50)))+
  scale_y_facet(PANEL  == 8, limits = c(0, 250), breaks=c(seq(0,250,50)))+
  scale_y_facet(PANEL  == 9, limits = c(0, 300), breaks=c(seq(0,300,75)))+
  scale_y_facet(PANEL  == 10, limits = c(0, 1200), breaks=c(seq(0,1200,300)))+
  scale_y_facet(PANEL  == 11, limits = c(0, 250), breaks=c(seq(0,250,50)))+
  scale_y_facet(PANEL  == 12, limits = c(0, 200), breaks=c(seq(0,200,50)))+
  scale_y_facet(PANEL  == 13, limits = c(0, 125), breaks=c(seq(0,125,25)))+
  scale_y_facet(PANEL  == 14, limits = c(0, 125), breaks=c(seq(0,125,25)))+
  scale_y_facet(PANEL  == 15, limits = c(0, 125), breaks=c(seq(0,125,25)))+
  scale_y_facet(PANEL  == 16, limits = c(0, 125), breaks=c(seq(0,125,25)))+
  scale_y_facet(PANEL  == 17, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 18, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 20, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 21, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 22, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 23, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 24, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 25, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 26, limits = c(0, 60), breaks=c(seq(0,60,15)))+
  scale_y_facet(PANEL  == 27, limits = c(0, 80), breaks=c(seq(0,80,20)))+
  scale_y_facet(PANEL  == 28, limits = c(0, 65), breaks=c(seq(0,60,15)))+
  scale_y_facet(PANEL  == 29, limits = c(0, 60), breaks=c(seq(0,60,15)))+
  scale_y_facet(PANEL  == 30, limits = c(0, 40), breaks=c(seq(0,40,10)))+
  scale_y_facet(PANEL  == 31, limits = c(0, 35), breaks=c(seq(0,30,10)))+
  scale_y_facet(PANEL  == 32, limits = c(0, 30), breaks=c(seq(0,30,10)))+
  scale_y_facet(PANEL  == 33, limits = c(0, 50), breaks=c(seq(0,50,10)))+
  scale_x_continuous(limits=c(1985, 2020),breaks = seq(1985,2020, by= 10))+
  scale_fill_manual(values=rev(myColors))+
  labs(title="Tasa bruta de desapariciones-homicidios por departamento, Colombia 1985-2018\n",
       subtitle = "",
       x="",
       y="Tasa x 100,000 personas\n", 
       caption="\nDatos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).")+
  theme_HS+theme(legend.position="bottom",
                 legend.key.width=unit(1.5,"cm"),
                 plot.margin = margin(1,1,1,1,"cm"),
                 plot.title=element_text(size=35, 
                                         hjust=0.5,
                                         face="bold", 
                                         color="#A4A4A4",
                                         vjust=-1,
                                         lineheight=.5),
                 strip.background =  element_rect(colour = "#585858", fill=NA, linewidth=.75),
                 plot.caption = element_text(size=20, hjust=.5,face="bold",  color="#A4A4A4"))




ggsave(
  "3_lineplot_EDITEDE.png",
  scale = 1,
  height = 15,
  width = 20,
  dpi = 300
)
