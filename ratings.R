

#### hot sauce ratings --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#### prelims ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse);library(reshape2);library(readxl)
library(hrbrthemes);library(ggridges);library(wesanderson);library(cowplot)

library("rnaturalearth");library("rnaturalearthdata")

path<-'/Users/collnell/Dropbox/rstats/hotsauce/'

#### ratings data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sauce<-read_excel(paste0(path,'ratings.xlsx'))

# clean locations
unique(sauce$ORIGIN)

sauce

#### flavor wheel -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sauce%>%
  dplyr::select(-SCOVILLE, -CHOCOLATE)%>%melt(id.vars=c('ORIGIN','NAME','MAKER','STARS','HEAT','CHILE','BASE'))%>%
  filter(!(variable %in% c('DARK FRUIT')))%>%
  arrange(desc(STARS))%>%
  ggplot()+
  geom_bar(stat='identity',aes(x=variable, y=value, fill=variable), color=NA)+
  coord_polar()+theme(axis.text=element_blank())+
  facet_wrap(~NAME, ncol=8)+theme_void()+
  theme(panel.grid.major.y=element_line(color='white'), panel.ontop=TRUE)

ggsave(paste0(path, 'flavor_polar_pie.pdf'), width=10, height=10)

#### map -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(ggmap);library(sf);library(rmapshaper);library(raster)
library(mapproj)

## get map of world
world <- ne_countries(scale = "medium", returnclass = "sf")%>%
  mutate(land=ifelse(continent == "Seven seas (open ocean)", 'no', 'yes'))%>%
  ms_simplify(keep=0.5)

## geocode hot saucd origins
# this requries google geocoding API
register_google(key=API_KEY, write=TRUE) ## need to do this evrery session - this wrote it to .Renviron

origin.coded<-lapply(unique(sauce$ORIGIN), function(x)geocode(x, output='latlona', source='google'))%>%setNames(unique(sauce$ORIGIN))
origin.df<-origin.coded%>%
  bind_rows(.id='ORIGIN')%>%
  filter(!is.na(lon))%>%
  left_join(sauce)%>%
  mutate(country = str_trim(word(address, 3, sep=',')),
         country=case_when(ORIGIN == 'Johannesburg, South Africa' ~'south africa', TRUE ~ country),
         country = case_when(tolower(address) %in% c('china','india','thailand','brazil','nicaragua','malaysia','tunisia','indonesia') ~ address,
                             str_trim(word(tolower(address), 2)) %in% c('jamaica','china','rwanda','uganda','korea') ~str_trim(word(address), 2),
                             country == 'usa' ~ 'united states of america',
                             TRUE ~ country))%>%
  st_as_sf(coords=c('lon','lat'), crs=4326)
origin.df
unique(origin.df$country)
origin.df%>%filter(is.na(country))

setdiff(unique(sauce$ORIGIN),origin.df$ORIGIN)

## countries
sauce.cos<-world%>%
  filter(tolower(admin) %in% origin.df$country)%>%
  filter(admin != 'United States of America')
unique(sauce.cos$admin)
unique(origin.df$country) #12
str(sauce.cos)

## remove alaska from selection, misleading
usa.parts<-sauce.cos%>%filter(admin == 'United States of America')%>%st_cast(to='POLYGON', group_or_split=TRUE)
usa.parts$part<-seq(1:length(usa.parts$admin))
usa.alaska<-usa.parts%>%filter(part == 22)
usa.48<-usa.parts%>%filter(part %in% c(8:20))
usa.hawaii<-usa.parts%>%filter(part %in% c(1:7))
sauce.countries<-rbind(usa.48%>%dplyr::select(-part), usa.hawaii%>%dplyr::select(-part), sauce.cos)
sauce.countries$admin # this is missin a bunch of countries

ggplot(world)+
  geom_sf(data=usa.48, color=NA, fill='lightgreen', alpha=.3)
str(origin.df)

## map sauce origins wiht labels
origin.map<-ggplot(world)+
  geom_sf(color=NA, fill='#70d2a9', alpha=.3)+
  geom_sf(data=sauce.countries, fill='slateblue', alpha=.4, color=NA)+
  geom_sf_interactive(data=origin.df, color='gold', shape=21, size=2.5, alpha=.7, aes(tooltip=paste0(MAKER,'\n', NAME)))+
  geom_sf_interactive(data=origin.df, color='orangered', shape=16, size=1.2, alpha=.7, aes(tooltip=paste0(MAKER, '\n', NAME)))+
  theme_void(base_size=14)+
  theme(legend.position='none')+
  coord_sf(xlim=c(extent(origin.df)[1],extent(origin.df)[2]*1.2), 
           ylim=c(extent(origin.df)[3]*4,extent(origin.df)[4]*1.7), 
           expand=TRUE)

tooltip_css <- "background-color:gray;color:white;padding:5px;border-radius:5px;font-family:sans-serif"

inter.origin<-girafe(ggob=origin.map, 
       options = list(opts_tooltip(offx = 20, offy = 20, css = tooltip_css, opts_hover(css='fill:violet;'))))
htmlwidgets::saveWidget(inter.origin, "originmap.html" )
browseURL( "originmap.html" )


library(mapview)
library(leaflet)
library(ggiraph)


ggsave(paste0(path, 'map_origin.pdf'), width=9, height=5)

#   geom_sf_text(data=origin.df, aes(label=NAME), color='black')+
#   geom_sf_text(data=origin.df%>%filter(ORIGIN =='Washington, D.C.'), aes(label=ORIGIN), color='black', label='current\nlocation')+
## plot on round earth
## map sauce origins
ggplot(world)+
  geom_sf(color=NA, fill='lightgreen', alpha=.3)+
  geom_sf(data=origin.df, color='orangered', shape=21, alpha=.7)+
  geom_sf(data=sauce.countries, fill='orangered', alpha=.4, color=NA)+
  theme_ipsum(base_size=14)+
  theme(legend.position='none')+
  coord_sf(xlim=c(extent(origin.df)[1],extent(origin.df)[2]*1.2), 
           ylim=c(extent(origin.df)[3]*1.6,extent(origin.df)[4]*1.3), 
           expand=TRUE)


# coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")+
separador('#### lollipop')

#### lollipop ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(sauce)
unique(sauce$CHILE)
sauce%>%filter(is.na(CHILE))

co.count<-sauce%>%
  group_by(CHILE)%>%
  summarize(n=length(unique(NAME)))









