#==========================================================================
# Topic : Geometry in R
#         
# Date : 2019. 12. 06
#
# Author : Junmo Nam
#==========================================================================



#0. Load Packages ----
sapply(c('dplyr','sf','ggplot2'),require,character.only = T)


#1. Point and Polygons ----

#point

point = st_point(c(128,27));point
plot(point)


#polygon

polygon = list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))) %>% st_polygon;polygon
plot(polygon,axes = T)

#multipoint

multipoint = st_multipoint(rbind(c(128,37),c(128,27)));multipoint
plot(multipoint,axes = T)



#multipolygon

polygon2 = list(rbind(c(1,1), c(2,1), c(2,2), c(1,2), c(1,1))) %>% st_polygon

multipol = list(polygon,polygon2) %>% st_multipolygon;multipol
c(polygon,polygon2)

plot(multipol,axes = T)


#2. Intersect, within and Union ----

#intersect
st_intersection(polygon,polygon2)
st_intersects(polygon,polygon2)

polygon3 = list(rbind(c(.5,.5), c(1.5,.5), c(1.5,1.5), c(.5,1.5), c(.5,.5))) %>% st_polygon

ggplot()+
  geom_sf(data = polygon,aes(geometry = geometry),fill = 'red')+
  geom_sf(data = polygon3,aes(geometry = geometry),fill = 'blue')+
  geom_sf(data = st_intersection(polygon3,polygon),
          aes(geometry = geometry),fill = 'green')


#within
st_within(st_point(c(.5,.5)),polygon) #instide polygon
st_within(st_point(c(.5,.5)),polygon3) #outside polygon
st_within(polygon,polygon3)


#union

st_union(polygon,polygon3) %>% plot(axes = T)
polygon4 = list(rbind(c(10,10), c(20,10), c(20,20), c(10,20), c(10,10))) %>% st_polygon
st_union(polygon,polygon4) %>% plot(axes = T)

#3. Centroid, Buffer, bbox with ggplot2 representation

#centroid
centr = st_centroid(polygon4);centr

#buffer
buffer = st_buffer(centr,dist = 3);buffer
st_buffer(centr,dist = 3,endCapStyle = 'SQUARE')

plot(buffer)
plot(st_buffer(centr,dist = 3,endCapStyle = 'SQUARE'))


#bounding box
bbox = st_bbox(buffer);bbox



#with ggplot2
ggplot()+
  geom_sf(data = polygon4,aes(geometry = geometry),col = 'grey',size = 1.1,fill = NA)+
  geom_sf(data = centr ,aes(geometry = geometry),col = 'black',size = 4)+
  geom_sf(data = buffer,aes(geometry = geometry),size = 1.5,col = 'red',fill = NA)+
  geom_sf(data = list(rbind(bbox[1:2],bbox[c(1,4)],bbox[3:4],bbox[c(3,2)],bbox[1:2])) %>% st_polygon,
          aes(geometry = geometry),fill = NA,col = 'blue',size = 2.5)+
  labs(title = "Apple's R-box")+
  theme_bw()
  

