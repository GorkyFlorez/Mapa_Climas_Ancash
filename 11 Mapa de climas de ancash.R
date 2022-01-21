
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
Climas =st_read( "SHP/Climas_Peru.geojson")
Clima  <- st_transform(Climas ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Peru     <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Peru_D   <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Anchash      <- subset(Peru, NAME_1  == "Ancash")
Anchashclima  =st_read( "SHP/Anchashclima.shp")
Anchash_xy <- cbind(Anchash , st_coordinates(st_centroid(Anchash$geometry)))
alt      <- getData('alt', country='Peru')
Anchash_alt<- crop(alt,Anchash)                           #   
Anchash_alt<- Anchash_alt <- mask(Anchash_alt, Anchash)
plot(Anchash_alt)

slope = terrain(Anchash_alt  , opt = "slope") 
aspect = terrain(Anchash_alt , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

hill.pa          <-  rasterToPoints(hill)
hill.pa_a            <-  data.frame(hill.pa)
dem.p          <-  rasterToPoints(Anchash_alt)
df             <-  data.frame(dem.p)
cols <-c("#5F8141","#779F44","#93C649","#A9DD55","#CBD689","#ECE5B2","#E1C678","#978055","#45280E")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6543)

Perr =ggplot() +
  geom_sf(data=Peru_D, color="white", fill="gray90", size=0.5)+
  geom_sf(data=Anchash, fill="gray", color="gray")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.4))

Mapa=ggplot()+
  geom_sf(data = Peru_D, fill = "grey90",color = "white")+
  geom_raster(data = hill.pa_a, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill() +
  geom_sf(data = Anchashclima, aes(fill=CODIGO, color= CODIGO), alpha=0.4)+
  scale_fill_brewer(palette   = "RdYlGn")+
  scale_color_brewer(palette   = "RdYlGn")+
  theme_map(base_family = font_rc)+
  theme(panel.border = element_rect(color = "black",size = .5,fill = NA),
        panel.background = element_rect(fill = "lightblue1"),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        legend.title =element_text(size=10, face = "bold"), #tamaño de titulo de leyenda
        legend.position = c(.8, .4),
        legend.background = element_blank(),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        legend.text =element_text(size=9))+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = NULL)+
  geom_sf(data= Anchash, fill=NA, color="white")+
  geom_label(data =  Anchash_xy , aes(x= X, y=Y, label = NAME_2), size = 2.5, color="black", fontface = "bold",fontfamily = "serif", alpha=0.4)+
  coord_sf(xlim = c(-79,-76.1), ylim = c(-11,-7.7),expand = FALSE)+
  annotate(geom = "text", x=-77, y = -10.5, hjust = 0, vjust = 0,
           label = "Gorky Florez Castillo", fontface = 2,
           size = 4, family = font_rc, color = "grey20")+
  annotate(geom = "text", x = -77, y = -10.6, hjust = 0, vjust = 0,
           label = "Ing. Forestal y Medio Ambiente",
           size = 3 , family = font_rc, color = "grey40")+
  annotate(geom = "text", x = -78.9, y = -9.8, hjust = 0, vjust = 0,
           label = "OCEANO \nPACIFICO",
           size = 3 , family = font_rc, color = "blue")+
  annotate(geom = "text", x = -76.8, y = -10.7, hjust = 0, vjust = 0,
           label = "&", fontface = 2,
           size = 5, family = font_rc, color = "#35978f")+
  annotate(geom = "text", x = -76.5, y = -10.77, hjust = 1, vjust = 0,
           label = "Codigo en Githab", fontface = 2,
           size = 3, family = font_rc, color = "grey20")+
  annotate(geom = "text", x = -76.5, y = -10.8, hjust = 1, vjust = 0,
           label = "https://github.com/GorkyFlorez/Mapa_Climas_Ancash",
           size = 3, family = font_rc, color = "grey40")+
  # date
  annotate(geom = "text", x = -77.2, y = -10.6, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = font_rc, color = "#35978f")+
  # data
  annotate(geom = "text", x = -78.9, y = -10.9, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
           size = 3, family = font_rc, color = "grey50")+
  # title
  annotate(geom = "text", x = -78.5, y = -7.8, hjust = 0, vjust = 1, 
           label = "MAPA DE TIPO DE CLIMAS\n                         EN ANCASH",
           size = 8, family="serif", color = "grey20")+
  annotate(geom = "text", x = -77.8, y = -10.6, hjust = 0, vjust = 1, 
           label = "LIMA",
           size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -77, y = -9, hjust = 0, vjust = 1, 
         label = "HUANUCO",
         size = 3, family="serif", color = "black")+
  annotate(geom = "text", x = -78.55, y = -8.4, hjust = 0, vjust = 1, 
         label = "LA LIBERTAD",
         size = 3, family="serif", color = "black")+
  annotation_custom(ggplotGrob(Perr), xmin = -76.9, xmax = -76.3, ymin = -8.7, ymax = -7.6) +
  geom_vline(xintercept = c(-79,-78.5 ,-78, -7.5,-77, -76.5 ,-76.1), color = "gray50",linetype = "dashed", size = 0.05)+ 
  geom_hline(yintercept = c(-11, -10.5, -10, -9.5, -9, -8.5, -8), color = "gray50",linetype = "dashed", size = 0.05)



ggsave("Mapas/Ancash.png", Mapa, width = 14, height = 11.76, 
       dpi = 900, type = "cairo-png")

  

  