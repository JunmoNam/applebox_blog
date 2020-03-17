#==========================================================================
# Topic : Parallel in R
# Date : 2019. 04. 03
# Author : Junmo Nam
#==========================================================================




#==========================================================================
# Load packages : using apply
#==========================================================================


pkg = c('dplyr','foreach','doParallel','purrr')
sapply(pkg,require,character.only = T)
temp = sapply(pkg,require,character.only = T) #named logical vector

lapply(pkg,require,character.only = T) #list

select(iris,-Species) %>% apply(1,mean)
select(iris,-Species) %>% apply(2,mean)

#purrr : map function
map(pkg,require,character.only = T)#list


#apply is not for parallel work!
sapply(1:10,function(x){print(x)})


#==========================================================================
# foreach vs for
#==========================================================================

# %do% operation
system.time({foreach(i = 1:10000) %do%{
  i+5
}})

# basic for loop
system.time({for(i in 1:10000){
  i+5
}})


# %dopar% :take more time!
n_core = detectCores()
cl = makeCluster(n_core-1)
registerDoParallel(cl)
system.time(
  {foreach(i = 1:10000) %dopar%{
    i+5
  }}
) 
stopCluster(cl)


# complicated work

system.time({
  for(i in 1:1000){
   iris %>% sample_n(200,replace = T) %>% lm(Sepal.Length~.,data = .)
  }
})

# cluster = max core
cl = makeCluster(n_core-1)
registerDoParallel(cl)

system.time({ #better than for
  {res = foreach(i = 1:1000,.packages = 'dplyr') %dopar%{
    iris %>% sample_n(200,replace = T) %>% lm(Sepal.Length~.,data = .)
  }}
})

stopCluster(cl)


#bit smaller core clusters
cl = makeCluster(6)
registerDoParallel(cl)

system.time({ #better work than n_core - 1
  {res = foreach(i = 1:1000,.packages = 'dplyr') %dopar%{
    iris %>% sample_n(200,replace = T) %>% lm(Sepal.Length~.,data = .)
  }}
})

stopCluster(cl)


#==========================================================================
# How to use foreach
#==========================================================================

# how to combine
cl = makeCluster(6)
registerDoParallel(cl)
foreach(i = 1:10,.combine = append) %dopar% {
  i
}

foreach(i = 1:10,.combine = rbind) %dopar% {
  i
}

#.packages

foreach(i = 1:10,.combine = rbind) %dopar%{
    (iris %>% sample_n(200,replace = T) %>% lm(Sepal.Length~.,data = .))$coef
}

foreach(i = 1:10,.combine = rbind, .packages = 'dplyr') %dopar%{
  
  (iris %>% sample_n(200,replace = T) %>% lm(Sepal.Length~.,data = .))$coef
}

#.noexport
a = 1
foreach(i = 1:10,.combine = rbind, .packages = 'dplyr',.noexport = c('a')) %dopar%{
  
  (iris %>% sample_n(200+a,replace = T) %>% lm(Sepal.Length~.,data = .))$coef
}


#stop
stopCluster(cl)
