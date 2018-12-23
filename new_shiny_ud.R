'''
Building Shinny app for UDpipe
'''


'''
Member 1: Navya Yerrabochu  PGID:11810063
Member 2: Akhilesh Karamsetty Venkata Subba  PGID:11810115
'''







library(shiny)
library(shinydashboard)
library(shinythemes)
library(udpipe)
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
options(shiny.maxRequestSize=30*1024^2) 
ui <- fluidPage(
  theme = shinytheme("yeti"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             # "td { padding:5px; }",
             ".skin-blue .wrapper{ background-color:#ffffff; }",
             # ".well {position:fixed; width:350px;}",
             # ".sidebar-toggle {display:none;}",
             ".sidebar-toggle{ visibility: hidden; }",
             ".skin-blue .main-header .navbar { background-color:#367fa9 }",
             ".content-wrapper, .right-side { background-color:white; }"
             # "#box_variance{background-color:lightblue;width: 200px; height: 80px;border:1px solid black; margin-left:30px; padding-left:10px;}"
             
  ),
  tags$head(
    tags$title(
      ""
    )
  ),
  
  dashboardPage(
    dashboardHeader(title = ""),
    dashboardSidebar(width = 0,
                     menuItem("", tabName = "dashboard")
    ),
    dashboardBody(
      title = "",
      sidebarPanel(width=4, fileInput("file1", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c(".txt")),
                   
                   fileInput("model","Choose a model",
                             multiple = FALSE,
                             accept =".udpipe" ),
                   
                   checkboxGroupInput("POS", "POS",
                                      c("Adjective" = "JJ",
                                        "Noun" = "NN",
                                        "Proper noun" = "NNP",
                                        "Adverb"="RB",
                                        "Verb"="VB"),selected=c("JJ","NN","NNP")),
                   sliderInput("max", "Minimum Frequency in Wordcloud:", min = 0,  max = 200, value = 2)
                   
      ),
      mainPanel(width = 8,plotOutput("cooc",height = 600))
      
      # tabsetPanel(
      #   
      #   tabPanel()
      #   #         #,
      #   #         tabPanel()
      #   #       )
      # )
    )
  )
)


server <- function(input, output, session) {
  x <- reactive({
    req(input$file1)
    fi<-input$file1
    text <- readLines(fi$datapath)
    print(text)
    req(input$model)
    mod<-input$model
    print(mod)
    ud_model<-udpipe_load_model(mod$datapath)
    x <- udpipe_annotate(ud_model, x = text) #%>% as.data.frame() %>% head()
    x <- as.data.frame(x)
    return(x)
  })
  sub<-reactive({
    x<-x()
    print(input$POS)
    sub<-subset(x,xpos %in% input$POS)
    print(sub)
    return(sub)
  })
  output$cooc<-renderPlot({
    sub=sub()
    View(sub)
    cooc <- cooccurrence(x=sub,
                         term = "lemma", 
                         group = c("doc_id", "paragraph_id", "sentence_id"))  # 0.02 secs
    wordnetwork <- head(cooc,input$max)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
  })
  
}
shinyApp(ui = ui, server = server)
