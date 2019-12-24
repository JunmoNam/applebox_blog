#==========================================================================
# Topic : Dataframe in R (dplyr)
# Date : 2019. 03. 14
# Author : Junmo Nam
#==========================================================================

#==========================================================================
# Load package 
#==========================================================================

require(dplyr)


#==========================================================================
# chain function
#==========================================================================

'Apple-Rbox provides R tips' %>% gsub('Apple','')
'Apple-Rbox provides R tips' %>% gsub('Apple','',.)


#==========================================================================
# Filtering and Selecting data
#==========================================================================

#Filtering
head(iris[which(iris$Sepal.Length<5),])
iris %>% filter(Sepal.Length<5) %>% head

#and, or
iris %>% filter(Sepal.Length>5, Species == 'virginica') %>% head
iris %>% filter(Sepal.Length>5 | Species == 'virginica') %>% head


#select
iris %>% filter(Sepal.Length>5, Species == 'virginica') %>%
  select(Sepal.Length,Species)


#==========================================================================
# Making variables
#==========================================================================

#mutate
iris %>% mutate(k = Sepal.Length*Sepal.Width) %>% head
  
#mutate tip
vname = c('apple_var')
iris %>% mutate(!!vname := 'apple') %>% head


#==========================================================================
# Merging
#==========================================================================

#sample dataframe
join_df1 = data.frame(a = 1:10,b = runif(10))
join_df2 = data.frame(a = seq(1,19,2),c = c('a','b','c',rep('d',7)))

#inner
inner_join(join_df1,join_df2,'a')
inner_join(join_df2,join_df1,'a')

#left and right
left_join(join_df1,join_df2,'a')
right_join(join_df1,join_df2,'a')

#full and semi
full_join(join_df1,join_df2,'a')
semi_join(join_df1,join_df2,'a')

#anti
anti_join(join_df1,join_df2,'a')

#nested
nested = nest_join(join_df1,join_df2 %>% mutate(d = runif(10),e = 'a'),'a')
tidyr::unnest(nested)
tidyr::unnest(nested,.drop=NA)


#==========================================================================
# Grouping and summarise
#==========================================================================

iris %>% group_by(Species)

#sampling from group
iris %>% group_by(Species) %>% sample_n(2)

#disticnt from group
data.frame(a = c(rep('a',3),rep('b',4)),
           b = c(1,1,2,3,4,5,5)) %>% group_by(a) %>% distinct()

#summarise and summarise_all
iris %>% group_by(Species) %>% summarise(smean = mean(Sepal.Length))

iris %>% group_by(Species) %>% summarise_all(mean)

iris %>% group_by(Species) %>% summarise_all(list(~mean,~var))
