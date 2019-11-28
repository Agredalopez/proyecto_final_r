# Proyecto Final de R
Desarrollo de los temas seguidos en clase aplicados a información real del país de EE.UU.

Se mostrarán algunos descriptivos estadísticos, tanto de población, densidad, zona geográfica, ingresos, salud, entre otros.

La idea de este documento es que sea una guía para hacer más llevadero el documento `.Rmd`.

Para poder obtener el documento en formato html y no tener ningún inconviente en ello se requieren las siguientes librerías

```
library(rmdformats)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(rbokeh)
library(leaflet)
library(plotly)
library(RColorBrewer)
library(raster)
library(kableExtra)
```

Parte de la información proporcionado en el documento final al que respalda este archivo estará fundamentada en cálculos con los códigos que se muestren aquí.
## Gráfica

```
q<-ggplot(data = tab_usa_total) +
  geom_point(mapping = aes(x = pop/1000  , y = density, color=division)) +
  theme (axis.text.x = element_text(angle=90)) +
  facet_wrap(year_id~.)+
  scale_color_brewer(palette = "Dark2")+
  labs(x="Población en miles",
       y="Densidad por km2")

``` 
##Análisis Agregado

```url<-"https://datausa.io/api/data?drilldowns=State&measures=Population"
usa <- readLines(url)
tab_popusa <- jsonlite::fromJSON(usa)[[1]]
tab_popusa<-tab_popusa[,c(1:3,5)]

tmp_names<-c("state_id","state","year_id","pop")
colnames(tab_popusa)<-tmp_names

estates.usa<-cbind.data.frame(state.division,state.region,state.name,state.abb,state.area)
tmp_names<-c("region","division","state","abb","area_miles")
colnames(states.usa)<-tmp_names
mile_km_sq<-2.58999
states.usa<-states.usa %>% mutate(area_kms=area_miles*mile_km_sq)

tab_usa_total<-merge(tab_popusa,states.usa)
tab_usa_total<-tab_usa_total %>% mutate(density=pop/area_kms)
tab_usa_total_div<-tab_usa_total %>%
  group_by(year_id,division) %>%
  summarize(pop_mean=mean(pop),pop_median=median(pop),pop_agg=sum(pop),area_agg=sum(area_kms))%>%
  mutate(densi_agg=pop_agg/area_agg,percent=pop_agg/sum(pop_agg)) %>% 
  ungroup() %>% 
  arrange(year_id,desc(densi_agg))
usa$NAME_1
usa <- getData("GADM", country="USA", level=1) 
pal <- colorQuantile("Greens", NULL, n = 5)
polygon_popup <- paste0("<strong>Name: </strong>", usa$NAME_1, "<br>")
usa$randomData <- rnorm(n=nrow(usa), 150, 30)
polygon_popup
map = leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-98.35, 39.7,
          zoom = 2) %>% 
  addPolygons(data = usa, 
              fillColor= ~pal(randomData),
              fillOpacity = 0.4, 
              weight = 2, 
              color = "green",
              popup = polygon_popup)%>% 
  addControl("Estados Unidos",position = "topleft",className = "map-title") %>% 
  addMarkers(lng = -98.35, lat=39.7)
map

tab_2017<-tab_usa_total %>% filter(year_id==2017) %>%
  summarize(pop_mean=mean(pop),pop_median=median(pop),pop_agg=sum(pop),area_agg=sum(area_kms))%>%
  mutate(densi_agg=pop_agg/area_agg)
  
```
```
url_inc<-"http://datausa.io/api/data?Geography=01000US:children&measure=Household Income by Race,Household Income by Race Moe&drilldowns=Race"
income <- readLines(url_inc)
income_usa <- jsonlite::fromJSON(income)[[1]]
income_usa
str(income_usa)
income_usa_tot<-income_usa[c(1:2,4:5,7:8)]
income_usa_tot
tmp_names<-c("id_race","race","state","id_year","inc_median_house","moe_inc_med")
colnames(income_usa_tot)<-tmp_names
income_td<-income_usa_tot %>% arrange(desc(inc_median_house)) %>% filter(id_year==2017, race=="Total", state!="Puerto Rico")
income_td
income_td<-income_td[c(1:5,47:51),]

income_agrup<-income_usa_tot %>%filter(id_year==2017) %>%  group_by(race, id_year) %>% summarise(income_mean=mean(inc_median_house)) %>% 
  arrange(desc(id_year),desc(income_mean))%>% ungroup()
income_agrup
```