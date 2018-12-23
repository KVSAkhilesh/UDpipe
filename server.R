'''
Building Shinny app for UDpipe
'''


'''
Member 1: Navya Yerrabochu  PGID:11810063
Member 2: Akhilesh Karamsetty Venkata Subba  PGID:11810115
'''


library(shiny)
ui <- fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
    
    sidebarPanel(
      width=4,
      fileInput("file1", "Choose CSV File",
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
                    "Verb"="VB"))
    ),
    mainPanel (width=8,plotOutput("cooc",height = 600)) ))

server<-function(input,output){
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
    wordnetwork <- head(cooc,100)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = "Nouns & Adjective")
  })
}
shinyApp(ui=ui,server=server)