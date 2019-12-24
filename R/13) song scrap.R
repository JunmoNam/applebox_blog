#==========================================================================
# Topic : Scrapping Music information from wikipedia
# 
# Author : Junmo Nam
# Blog : http://apple-rbox.tistory.com
#==========================================================================

sapply(c('dplyr','XML','rvest'),require,character.only = T)


scrap_wiki_billboard = function(year){
  
  #create html object
  html_obj = paste0("http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",year) %>% lapply(read_html)
  
  
  
  #title and artist, rank
  wiki_table = html_obj %>% 
    lapply(function(x){(html_nodes(x,'table.wikitable.sortable') %>%
                          html_table %>% as.data.frame)[,-1] %>% 
        mutate(rank = 1:nrow(.))}) %>% bind_rows %>%
    mutate(Title = gsub('\\"','',Title),
           year = lapply(year,function(x){rep(x,100)}) %>% unlist)
  
  gc()
  
  
  #hyperlinks
  
  hplink = lapply(html_obj,function(x){
    
    temp <- (html_nodes(x,'table.wikitable.sortable') %>%
               html_nodes('tr'))[-1]
    
    res = data.frame(song_hplink = '',artists_hplink = '',stringsAsFactors = F)[1:100,]
    
    for(i in 1:length(temp)){
      link = html_nodes(temp[i],'a') %>% html_attr('href')
      res[i,1] = link[1]
      res[i,2] = link[-1] %>% paste(collapse = ' & ')
    }
    
    rownames(res) = NULL
    
    return(res)
  })
  
  
  total_scraped = bind_cols(wiki_table,hplink %>% bind_rows)

  return(total_scraped)
  
}

#2015 ~ 2018 top 100 billboard
res = scrap_wiki_billboard(2015:2018)


#summarised by year
res %>% group_by(year) %>% summarise(n = n(),
                                     singers = n_distinct(Artist.s.),
                                     more_exact_singers = n_distinct(artists_hplink))
#summarised by title
res %>% group_by(Title) %>% summarise(n = n(),
                                      mean_rank = mean(rank)) %>% arrange(desc(n),mean_rank)


require(ggplot2)
res %>% group_by(Artist.s.) %>% summarise(n = n()) %>% arrange(desc(n)) %>% top_n(10) %>% 
  ggplot(aes(Artist.s.,n,fill = Artist.s.))+
  geom_bar(stat = 'identity')+
  theme_bw()+
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 7))+
  labs(title = 'Top 10 Artists',subtitle = 'by freq in billboard 2015-2018')
  
