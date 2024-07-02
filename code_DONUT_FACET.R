# donut chart
library(tidyverse)
# Desapariciones ######
library(readr)
rates <- read_csv("desaparicionesyhomicidios.conpoblacion.csv")
rates<-rates|>
  select(-1,-5)
colnames(rates)<-c("name_dp","nom_depto","year","pop",
                   "des_obv",
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


rates <- rates %>%
  mutate(nom_depto = if_else(str_detect(nom_depto, "Nari"), "Nariño", nom_depto))


# rates<-rates|>
#   filter(nom_depto!="Quindio")

rates_dec<-rates|>
  group_by(nom_depto)|>
  summarise(des_rate=sum(des_rate)/34,
            hom_rate=sum(hom_rate)/34,
            des_hom_rate=sum(des_hom_rate)/34,
            des_p=des_rate/des_hom_rate,
            hom_p=hom_rate/des_hom_rate,
            cat_col=if_else(des_hom_rate<21.82416,"1 quintil de peligrosidad (- peligroso)",
                            if_else(des_hom_rate<36.23802,"2 quintil de peligrosidad",
                                    if_else(des_hom_rate<48.57453,"3 quintil de peligrosidad",
                                            if_else(des_hom_rate<73.96357,"4 quintil de peligrosidad",
                                                    if_else(des_hom_rate<=266,"5 quintil de peligrosidad (+ peligroso)","0"))))))|>
  ungroup()|>
  #filter(nom_depto!="Quindio")|>
  arrange(-des_hom_rate)|>
  pivot_longer(c(des_p,hom_p),
               names_to = "Evento",
               values_to= "SHARE")|>
  mutate(Evento=case_when(Evento== "des_p" ~ "Desapariciones",
                          Evento== "hom_p" ~ "Homicidios"))|>
  select(c(1,6,7))

rates_dec<- rates_dec|>
  mutate(ymax=cumsum(SHARE),.by="nom_depto")

# Compute the bottom of each rectangle
rates_dec<- rates_dec|>
  mutate(ymin=c(0, head(ymax, n=-1)),.by="nom_depto")

# Compute label position
rates_dec<- rates_dec|>
  mutate(labelPosition=(ymax + ymin) / 2,.by="nom_depto")

rates_dec<-rates_dec|>
  mutate(nom_depto=as.factor(nom_depto))
levels(rates_dec$nom_depto)

bcnF<-rates_dec|>filter(Evento=="Desapariciones")|>arrange(SHARE)|>
  select(nom_depto)


rates_dec<-rates_dec|>
  mutate(nom_depto=fct_relevel(nom_depto,c(as.character(bcnF$nom_depto))))

pal<-"Set1"
# Make the plot
ggplot(rates_dec, aes(ymax=ymax, 
                      ymin=ymin, 
                      xmax=4, 
                      xmin=3, 
                      fill=Evento)) +
  geom_rect(color="black") +
  geom_text( x=1, aes(y=labelPosition, 
                      label=round(SHARE*100,2), 
                      color=Evento), 
             size=6, fontface ="bold") + # x here controls label position (inner / outer)
  coord_polar(theta="y") +
  scale_fill_manual(values=c(brewer.pal(3, pal)[1],brewer.pal(3, pal)[2]),
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 2,
                                         keywidth=14,
                                         keyheight=2.5,
                                         label.position = "bottom"))+
  scale_color_manual(values=c(brewer.pal(3, pal)[1],brewer.pal(3, pal)[2]))+
  xlim(c(-1, 4)) +
  labs(title="Distribución de evento por departamento, Colombia 1985-2018\n",
       subtitle = " ",
       x="",
       y="", 
       caption="\nDatos: Integración de datos y estimación estadística de víctimas en el marco del conflicto armado (DANE).")+
  facet_wrap(~nom_depto)+
  theme_void() +
  theme(plot.title=element_text(size=40, 
                                hjust=0.5,
                                face="bold", 
                                color="#A4A4A4",
                                vjust=-1,
                                lineheight=.5),
        plot.caption = element_text(size=25, hjust=.5,face="bold",  color="#A4A4A4"),
        legend.position = c(.6, .0666), 
        plot.margin = margin(1,1,1,1,"cm"),
        legend.title = element_blank(),
        strip.text = element_text(size=22.2, face="bold"), #text for facets
        panel.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF"),
        plot.background =element_rect(fill ="#FFFFFF", colour = "#FFFFFF"),
        legend.text = element_text(colour="black", size = 24.5,face="bold",vjust = 7.5))

ggsave("9_DONUTS_DES_HOM_DEPTO.png", 
       scale = 1,
       height = 22,
       width=18, 
       dpi = 300)

