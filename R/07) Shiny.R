#==========================================================================
# Topic : Shiny
# Date : 2019. 04.
# Author : Junmo Nam
#==========================================================================


#==========================================================================
# Load packages
#==========================================================================

sapply(c('dplyr','shiny','shinydashboard','ggplot2'),require,character.only = T)


#==========================================================================
# Claim UI
#==========================================================================


ui = dashboardPage(
  dashboardHeader(
    title = "Applebox's R Shiny example"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Load and View Data',tabName = 'data',icon = icon('save')),
      menuItem('Visualizing data',tabName = 'visual',icon = icon('chalkboard-teacher'))
    )
  ),
  
  dashboardBody(
    #First Tab
    tabItems(
      tabItem(
        tabName = 'data',
        fileInput("data", "Load Data(CSV Only!)",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        DT::dataTableOutput('dt'),
        h3('Data Type Converter'),
        uiOutput('colname'),
        selectInput('coltype','Change type to :',choices = c('Categorical','Numerical')),
        actionButton('convert','Convert')
      ),
      
      #Second Tab
      tabItem(
        tabName = 'visual',
              uiOutput('draw_colname'),
              actionButton('draw','Draw Plot'),
              plotOutput('plot_selected')
      )
    )
    
    
    
  )
  
)


server = function(input,output){
  
  df = reactive(data.table::fread(input$data$datapath))
  
  #Render Dataframe 
  output$dt = DT::renderDataTable({
    req(input$data)
    df()
  })
  
  #Colnames for data type converter
  output$colname = renderUI({
    req(input$data)
    selectInput('colname','Variable Name',choices = colnames(df()))
  })
  
  #Colnames for draw data
  output$draw_colname = renderUI({
    req(input$data)
    selectInput('draw_colname','Variable Name',choices = colnames(df()),multiple = T)
  })
  
  #Select variables from data and make plot
  observeEvent(input$draw,{
    req(input$data)
    output$plot_selected = renderPlot({
      select(df(),input$draw_colname) %>% plot})
  })
  
  
  
  
}


shinyApp(ui,server)