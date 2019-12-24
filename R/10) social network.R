#==========================================================================
# Topic : Social Network
#         
# Date : 2019. 04.
# Author : Junmo Nam
#==========================================================================



#==========================================================================
# Load packages
#==========================================================================

pkg = c('dplyr','igraph','networkD3','visNetwork','ggplot2')

sapply(pkg,require,character.only = T)


#==========================================================================
# Make graph in igraph
#==========================================================================


df = data.frame(start = c('a','a','b','c','d','e','f','f','f','f'),
                end = c('a','b','c','d','e','e','a','b','c','e'))

#make network
G = graph_from_data_frame(df)
graph_from_data_frame(df,directed = F) #undirected

plot(G)
v = V(G)
attributes(v)

#shortest path
shortest.paths(G)
shortest_paths(G,from = 'a',to = 'e')

#degree
degree(G) #all
degree(G,mode = 'in') #in
degree(G,mode = 'out') #out

#Power law fit or not
d.dist = degree_distribution(G, cumulative = T)
p.fit = power.law.fit(d.dist,impelementation = "plfit");p.fit

#eccentricity
eccentricity(G) #undirected
eccentricity(G,mode = 'in')
eccentricity(G,mode = 'out')

#diameter and radius
diameter(G)
radius(G)

#centrality compare
central.df = data.frame(degree = degree(G) / (length(names(v))-1),
                        between = betweenness(G),
                        close = closeness(G,normalized = T));central.df

#k core
coreness(G)

#LCC(local clustering coefficient) and Transitivity
trans = transitivity(G, type = "globalundirected") 
LCC = transitivity(G, type = "localundirected")  

#===========================
# 네트워크 이론 추가
#===========================

#Power Law
g <- static.power.law.game(500, 1000, exponent.out= 2.2, exponent.in = -1,
                           loops = FALSE, multiple = TRUE, finite.size.correction = TRUE)
V(g)

data.frame(degree = 0:max(degree(g)),p_k = degree.distribution(g)) %>% 
  ggplot(aes(degree,p_k))+
  geom_point()+
  theme_bw()+
  ylab('P(k)')+
  ggtitle('Power law fit of graph g')

#transitivity = 3 * 삼각형 개수 / 3개의 노드가 연결된 경우

small_g = data.frame(start = c('a','a','b','d'),end = c('c','b','c','a')) %>%
  graph_from_data_frame(directed = F)

plot(small_g)

transitivity(small_g)

#clustering coefficient = 3개의 노드가 닫힌 형태로 연결됨 / 모든 연결된 경우 = transitivity

#local clustering coefficient = 2 * 이웃과 연결된 엣지 수 / 이웃 수*(이웃수-1)
transitivity(small_g,type = 'local')


#==========================================================================
# Visualize network by visNetwork
#==========================================================================

#build node and edge dataframe
nodes = data.frame(id = names(v),label = names(v))
edges = data.frame(from = df$start,to=df$end)

visNetwork(nodes = nodes, #visualize
           edges = edges,main = 'graph') 

#add arrow option
edges$arrows = 'to'
visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow') 


#set node color
nodes$color = c(rep('red',nrow(nodes)-1),'blue')
visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow + color') 

#set node title
nodes$title = paste0(nodes$id,'<br/>','Node number :',1:nrow(nodes))
visNetwork(nodes = nodes,edges = edges,main = 'graph with arrow + color + title') 

#turn off physics 
visNetwork(nodes = nodes,edges = edges,
           main = 'graph with arrow + color + title + no physics') %>%
  visIgraphLayout(physics = F)

#highlight connected one
visNetwork(nodes = nodes,edges = edges,
           main = 'graph with arrow + color + title + no physics + highlight') %>%
  visIgraphLayout(physics = F) %>% 
  visOptions(highlightNearest = T)
