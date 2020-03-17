#==========================================================================
# Topic : Geometry in R (2)
#         
# Date : 2020. 01.
#
# Author : Junmo Nam
#==========================================================================
options(stringsAsFactors = F,scipen = 100)

#0. Load Packages ----
sapply(c('dplyr','sf','ggplot2'),require,character.only = T)

#1. Make sf object from dataframe
df = data.frame(x = c(1,5,3,2,7),
                y = c(3,1,2,5,5))

# convert to sf
df_sf = df %>% st_as_sf(coords = c('x','y'))
plot(df_sf$geometry,axes = T) #plot

#add id and transform
df %>% mutate(id = row_number()) %>% 
  st_as_sf(coords = c('x','y'))



#2. Read shp file ----
# download shp file at http://www.gisdeveloper.co.kr/?p=2332
sig = read_sf('C:/Users/jmnam47/Desktop/Blog/post17_geometry2/TL_SCCO_SIG.shp',
              options = 'ENCODING=CP949')
emd = read_sf('C:/Users/jmnam47/Desktop/Blog/post17_geometry2/TL_SCCO_EMD.shp',
               options = 'ENCODING=CP949')

class(sig)

#data.frame like 
sumed_sig = sig %>% mutate(id = substr(SIG_CD,1,2)) %>% 
  filter(id %in% c(11,30,26)) %>% 
  group_by(id) %>% summarise(gsum = sum(as.integer(SIG_CD)))

plot(sumed_sig$geometry,axes = T)

#example
ggplot(data = filter(sig,substr(SIG_CD,1,2) == 11),
       aes(geometry = geometry,fill = SIG_KOR_NM))+
  geom_sf()+
  theme_void()+
  theme(legend.position = 'none')

#3. crs ----
st_crs(sig)

#change crs : make it as longitude - latitude
plot(sig$geometry[1],axes = T)
st_transform(sig$geometry[1],crs = 4326) %>% plot(axes = T)


#4. write sf ----
write_sf(sig %>% filter(substr(SIG_CD,1,2)==11),
         'C:/Users/jmnam47/Desktop/Blog/post17_geometry2/sig_seoul.shp',
         layer_options = 'ENCODING=UTF-8')

read_sf('C:/Users/jmnam47/Desktop/Blog/post17_geometry2/sig_seoul.shp',
        options = 'ENCODING=CP949')


#5. exercises ----

#5.1 find union of emd and sig ----
index = st_intersects(sig$geometry[1],emd$geometry)

ggplot()+
  #intersected area
  geom_sf(data = st_intersection(sig$geometry[1],emd$geometry),
          aes(geometry = geometry),fill = 'skyblue')+
  #emd
  geom_sf(data = emd[index[[1]],],aes(geometry = geometry),fill = NA)+
  #target sig
  geom_sf(data = sig[1,],aes(geometry = geometry),fill = NA,
          col = 'red')


#5.2 calculate distance ----
st_distance(emd$geometry[1],emd$geometry[4]) 
st_transform(emd$geometry[c(1,4)],crs = 4326) %>% 
  st_distance #crs is epgs:4326, great circle distance



