#==========================================================================
# Topic : Web Scrapping
#         
# Date : 2019. 07.
# Author : Junmo Nam
#==========================================================================



#==========================================================================
# Load packages
#==========================================================================

pkg = c('dplyr','XML','rvest')

sapply(pkg,require,character.only = T)


#==========================================================================
# read html structure from link
#==========================================================================
url = "http://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_2018"
html = read_html(url) #head, body


html_attrs(html)



#==========================================================================
# read html structure from link
#==========================================================================

#nodes
html_nodes(html,'a')
html_nodes(html,'b')

#text : title
html_node(html,'title') %>% html_text

#table
html_node(html,'title') %>% html_table
html_nodes(html,'table') %>% html_table



#==========================================================================
# track nodes from html structure from link
#==========================================================================

wiki_table = html_nodes(html,'table.wikitable.sortable')

html_table(wiki_table)[[1]] %>% head

html_nodes(wiki_table,'a') %>% html_attrs

wiki_table_attrs = html_nodes(wiki_table,'a') %>% html_attrs


paste0('http://en.wikipedia.org/',wiki_table_attrs[[2]][1]) %>% read_html %>% html_nodes('table') 
