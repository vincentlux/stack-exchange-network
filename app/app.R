require(shiny)
require(visNetwork)
library(tidyverse)
library(XML)
library(methods)
library(dplyr)
library(igraph)
library(anytime)
#Set this to where you have the Posts.xml file:

setwd("./eda/Group7 - Shiny App with Demo Data")

xml <- xmlParse("Posts_ch.xml")
xmlroot <- xmlRoot(xml)
dfres <- XML:::xmlAttrsToDataFrame(xmlroot)
subdfres <- dfres

subdfres <- as.data.frame(subdfres)
subdfres <- mutate(subdfres, CreationDate = anytime(CreationDate))
subdfres <- split(subdfres, format(subdfres$CreationDate, "%y-%m-%d"))

server <- function(input, output) {
  
  graph_data_frame <- reactive ({
    subdfrestemp <- subdfres[input$date]
    subdfrestemp <- as.data.frame(subdfrestemp)
    subdfrestemp2 <- filter(subdfrestemp, subdfrestemp[paste0("X",gsub("-",".",input$date),".","PostTypeId")] == 2)
    idval <- paste0("X",gsub("-",".",input$date),".","Id")
    parentidval <- paste0("X",gsub("-",".",input$date),".","ParentId")
    charvec <- c(idval = parentidval)
    names(charvec) <- idval
    subdfrestest <- left_join(x=subdfrestemp, y=subdfrestemp2, by = charvec)
    edgelist <- select(subdfrestest, ends_with("OwnerUserId.x"), ends_with("OwnerUserId.y"))
    edgelist[is.na(edgelist)] <- -1
    m_edgelist <- as.matrix(edgelist)
    graph_edges_only <- graph_from_edgelist(m_edgelist, directed = TRUE)
    graph_edges_only <- graph_edges_only - vertex("-1")
    return(graph_edges_only)
  })
  
  output$network <- renderVisNetwork({
    df_graph <- as_data_frame(graph_data_frame(), what = "both")
    degree_value <- degree(graph_data_frame(), mode = "in")
    nodes <- data.frame(id = df_graph$vertices$name)
    if(nrow(nodes) != 0) {
      nodes$value <- degree_value[match(nodes$id, names(degree_value))]
      nodes$label <- paste("Out degree is", nodes$value)
    }
    edges <- data.frame(from = df_graph$edges$from, to = df_graph$edges$to)
    
    visNetwork(nodes, edges)  %>% 
      visEdges(arrows = 'from')
  })
}

ui <- fluidPage(
  selectInput("date", "Choose a date:",
              names(subdfres)
  ),
  visNetworkOutput("network")
)

shinyApp(ui = ui, server = server)
