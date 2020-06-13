#======================================================================
# TOPIC :google map API in R
#
#   Date : 2020.06.13
#
# Author : Junmo Nam
#======================================================================

options(scipen = 10,stringsAsFactors = F)

#0. 패키지 로딩 ----
sapply(c('devtools','ggmap'),require,character.only = T)
#register_google() when using api key

#1. get_map 함수 ----
gmap_obj = get_map(c(127,37.4,127.2,37.52))

gmap_obj2 = get_map(c(127.19,37.51,127.2,37.52))#fail

#2. ggmap ----
ggmap(gmap_obj)

ggmap(gmap_obj2)

#add point
ggmap(gmap_obj)+
  #bounding box
  geom_point(data = data.frame(x = c(127,127,127.2,127.2),y = c(37.4,37.52,37.4,37.52)),
             aes(x,y),col = 'red',size = 5)+
  geom_point(data = data.frame(x = runif(10,min = 127,max = 127.2),
                               y = runif(10,min = 37.4,max = 37.52)),
             aes(x,y),col = 'blue')

#랜덤좌표 생성 후 밀도 표현
df = data.frame(x = runif(1000,min = 127,max = 127.2),
                y = runif(1000,min = 37.4,max = 37.52))

z = MASS::kde2d(x = df$x,y = df$y,n = 100)

ggmap(gmap_obj)+
  geom_tile(data = expand.grid(x = z$x,y = z$y) %>% 
              mutate(z = as.vector(z$z)),aes(x,y,fill = z,alpha = z))+
  scale_fill_gradient(low = 'white',high = 'blue')+
  scale_alpha(range = c(0.05,0.6))
  

#3. preset 활용한 응용법 ----
source_gist('https://gist.github.com/JunmoNam/1380ed1ba91c0fa1bcfeb55849d2f0eb')

#3.1 create_gmap ----
create_gmap(df)
create_gmap(df$x,df$y)

#3.2 tr_to_4326----

#테스트용 카텍 좌표계 생성
df_katec = df %>% st_as_sf(coords = c('x','y'),crs = 4326) %>% st_transform(crs = .katec) %>% 
   st_coordinates %>% as.data.frame

#전체 반환
tr_to_4326(df_katec) %>% head

#변환된것만 반환
tr_to_4326(df_katec,drop_cols = T) %>% head
