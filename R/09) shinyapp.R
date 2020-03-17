#==========================================================================
# Topic : Shinyapp
#         Publishing shinyapp on shinyapps.io
# Date : 2019. 04.
# Author : Junmo Nam
#==========================================================================



library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(readxl)
library(rgl)


options(shiny.use.cairo = F)

#======================================================================
# UI
#======================================================================

ui = dashboardPage(
  
  #header
  dashboardHeader(
    title = 'Clustering Dashboard'
  ),
  #sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem('Load data',tabName = 'data_tab',icon = icon('table')),
      menuItem('PCA and clustering',tabName = 'clust_tab',icon = icon('sitemap')),
      sliderInput('k','K for clustering',min = 1,max = 100,value = 1),
      actionButton('do_clust','Do k-means clustering')
    )
  ),
  #body
  dashboardBody(
    #tabs
    tabItems(
      #first tab
      tabItem(
        tabName = 'data_tab',
        fileInput('file','Data Loading(.csv,.xlsx,.xls)',accept = c('.csv','.xlsx','.xls','text/csv')),
        h3(' '),
        actionButton('select','Select numeric data',icon = icon('filter')),
        actionButton('normalization','Min-max normalization',icon = icon('calculator')),
        h3('DATA'),
        dataTableOutput('dt')
      ),
      #second tab
      tabItem(
        tabName = 'clust_tab',
        #first box
        tabBox(title = 'Searching K',
               tabPanel('Scree Plot',plotOutput('scree')),
               tabPanel('Dendrogram',plotOutput('dendro')),
               tabPanel('2D plot',plotOutput('pca_biplot'))
        ),
        #second box
        tabBox(title = 'k-means',
               tabPanel('Console',verbatimTextOutput('kmeans_console')),
               tabPanel('K-means 2D',plotOutput('kmeans_2d')),
               tabPanel('K-means 3D',rglwidgetOutput('kmeans_3d'))
        )
      )
    )
  )
)

#======================================================================
# Server
#======================================================================

server = function(input,output){
  
  #load data
  vars = reactiveValues(df = NULL,render_df=NULL,km=NULL)
  observeEvent(input$file$datapath,{
    req(input$file)
    if(grepl('[.]csv',input$file$datapath)){
      df= fread(input$file$datapath)
    }else{
      df = read_excel(input$file$datapath)
    }
    vars$df = as.data.frame(df)
    vars$render_df = as.data.frame(df)
  })
  
  #select numeric
  observeEvent(input$select,{
    req(vars$df)
    vars$render_df = vars$df[,sapply(vars$df,class) %in% c('numeric','integer','double')]
  })
  
  #min-max normalization
  observeEvent(input$normalization,{
    req(vars$df)
    vars$render_df = lapply(vars$render_df,function(x){(x-min(x))/(max(x)-min(x))}) %>% data.frame
  })
  
  
  #render datatable
  output$dt = renderDataTable({
    req(vars$render_df)
    vars$render_df
  })
  
  #PCA
  pca = reactive({
    req(vars$df)
    princomp(vars$df[,sapply(vars$df,class) %in% c('numeric','integer','double')])
  })
  
  #scree plot
  output$scree = renderPlot({
    req(pca)
    
    data.frame(component = 1:length(pca()$sdev),variance = (pca()$sdev)^2) %>%
      ggplot(aes(component,variance))+
      geom_point(color = 'blue',size = 1.5)+
      geom_text(aes(label = round(100*(pca()$sdev)^2/sum((pca()$sdev)^2),2)%>% paste0('%')),
                nudge_y = 0.01)+
      geom_line()+
      geom_bar(stat = 'identity',alpha = 0.3)+
      theme_bw()+
      ggtitle('PCA scree plot','plot by variance')
  })
  
  #dendrogram
  output$dendro = renderPlot({
    dist(vars$render_df) %>% hclust %>% plot
  })
  
  #2d plot
  output$pca_biplot = renderPlot({
    data.frame(Comp1 = pca()$scores[,1],Comp2 = pca()$scores[,2]) %>%
      ggplot(aes(Comp1,Comp2))+
      geom_point()+
      theme_bw()+
      ggtitle('PCA Bi-plot','TOP2 Component from PCA')
  })
  
  #kmeans object
  observeEvent(input$do_clust,{
    req(vars$render_df)
    vars$km = kmeans(vars$render_df,input$k)
    
  })
  
  #clustering : console
  output$kmeans_console = renderPrint({
    req(vars$km)
    vars$km
  })
  
  #clustering : 2d plot
  output$kmeans_2d = renderPlot({
    req(vars$km)
    req(pca)
  
    df = data.frame(Comp1 = pca()$scores[,1],Comp2 = pca()$scores[,2],cluster = as.factor(vars$km$cluster))
    centers = df %>% group_by(cluster) %>% summarise(Comp1 = mean(Comp1),Comp2 = mean(Comp2))
      
    ggplot(data = df,aes(Comp1,Comp2,color = cluster,fill = cluster))+
      geom_point(alpha = 0.4)+
      stat_ellipse(geom="polygon", type='euclid', alpha=0.1)+
      theme_bw()+
      ggtitle('PCA Bi-plot with Cluster','TOP2 Component from PCA')+
      geom_point(data=centers,aes(x = Comp1,y = Comp2,color = cluster),size = 4,shape = 4)
    
  })
  
  #clustering : 3d plot
  output$kmeans_3d = renderRglwidget({
    req(pca)
    req(vars$km)
    open3d(useNULL = T)
    points3d(pca()$scores[,1:3],col = vars$km$cluster)
    title3d(main = 'PCA 3D plot with cluster')
    axes3d()
    rglwidget()
  })
  
  
}



#======================================================================
# Run App
#======================================================================



shinyApp(ui,server)