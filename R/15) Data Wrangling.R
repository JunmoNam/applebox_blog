#==========================================================================
# Topic : Data Wrangling
#         
# Date : 2019. 08. 19
#
# Author : Junmo Nam
#==========================================================================



#==========================================================================
# Load packages
#==========================================================================

sapply(c('dplyr','tidyr'),require,character.only = T)


#==========================================================================
# Spread - Gather
#==========================================================================

df = data.frame(name = c(rep('Apple',3),rep('Banana',3)),
           key = rep(c('A','B','C'),2),count = sample(1:5,6,replace = T))


df %>% spread(key = key,value = count)

gather(data = df %>% spread(key = key,value = count),key = 'key',value = 'count',2:4) 


#==========================================================================
# Separate - Unite
#==========================================================================

df2 = data.frame(key = c(rep('A',2),rep('B',2),rep('C',2)),date = paste0('2019-08-0',1:6))
df2 %>% separate(date,sep = '-',c('y','m','d'))

df2 %>% separate(date,sep = '-',c('y','m','d')) %>% unite('date',sep = '-',y,m,d)
df2 %>% separate(date,sep = '-',c('y','m','d')) %>% unite('date',sep = '-',y,m,d,remove = F)


#==========================================================================
# lead, lag
#==========================================================================

iris[sample(1:nrow(iris),150),] %>% mutate(lag = lag(Species),lead = lead(Species)) %>% head

iris %>% mutate(rm = row_number())  %>% filter(Species != lead(Species)) 

iris %>% mutate(rm = row_number())  %>% filter(Species != lag(Species)) 


#==========================================================================
# dplyr do and rowwise dataframe
#==========================================================================

#nested work example : do and unnest
df2_nested = df2 %>% group_by(key) %>% do(diff = difftime(.$date[2],.$date[1]))
df2_nested[1,2]$diff
df2 %>% group_by(key) %>% do(diff = difftime(.$date[2],.$date[1])) %>% unnest

#more complicated format to rowwise dataframe
df3 = data.frame(key = sample(c('A','B','C','D'),20,replace = T),
                 x = runif(min = 0,max = 40,20),
                 y = runif(min = 0,max = 40,20))


df3_dist = df3 %>% group_by(key) %>% do(dist = select(.,x,y) %>% dist) #distance matrix as row

df3_dist[3,]$dist

df3 %>% filter(key == 'C') %>% select(-key) %>% dist


#is nested dataframe time-efficient?
df4 = data.frame(name = sample(c('a','b','c','d'),replace = T,100000000),
           cnt = runif(100000000,10,50) %>% as.integer,
           price = runif(100000000,1000,50000) %>% as.integer)

df4_nested = df4 %>% group_by(name) %>% do(cnt = .$cnt,price = .$price)

system.time({df4 %>% filter(name == 'a')})
system.time({df4_nested %>% filter(name == 'a') %>% unnest})


