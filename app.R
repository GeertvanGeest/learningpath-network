library(visNetwork)
library(shiny)

library(visNetwork)
library(igraph)

gtp <- read.csv("network_lp.csv")

# metadata per alias
mtp <- read.csv("metadata_courses.csv")

g <- graph_from_data_frame(gtp)

vng <- toVisNetworkData(g)
vng$nodes <- merge(vng$nodes, mtp)
vng$nodes$label <- ifelse(nchar(vng$nodes$title) > 20, 
                          paste0(substr(vng$nodes$title, 1, 20), ".."),
                          vng$nodes$title)

ui <- fluidPage(
  fluidRow(
    column(12, 
           h1("Learning path network"),
           p("Each node represents a course. Click on a node to see the full course learning path. You might have to zoom in to see the coures names."),
           visNetworkOutput("network" , height = "500px")
    )
  ),
  hr(),
  fluidRow(
    column(8,
           h3("Learning path"),
           visNetworkOutput("subnetwork")
    ),
    column(4,
           wellPanel(htmlOutput("shiny_return"))))
  
)

server <- function(input, output) {
  output$network <- renderVisNetwork({
    visNetwork(vng$nodes, vng$edges) |> 
      visNodes(shape = "box", widthConstraint = 80) |>
      visPhysics(barnesHut = list(gravitationalConstant = -2000)) |>
      visEdges(arrows = "to", color = "black") |>
      visGroups(groupname = "Beginner", color = "lightgreen") |>
      visGroups(groupname = "Intermediate", color = "orange") |>
      visGroups(groupname = "AND", color = "purple", shape = "circle",
                widthConstraint = 20, font = list(size = 12,
                                                  color = "white")) |>
      visGroups(groupname = "OR", color = "blue", shape = "circle",
                widthConstraint = 20, font = list(size = 12,
                                                  color = "white")) |>
      visLegend(zoom = FALSE, width = 0.1) |>
      visOptions(highlightNearest = TRUE) |>
      visEvents(click = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}")
  })
  
  output$shiny_return <- renderText({
    req(input$current_node_id)
    if (length(input$current_node_id$nodes) == 0) {
      return(NULL)
    } else {
      id <- input$current_node_id$nodes[[1]]
      
      reqs <- neighbors(g, input$current_node_id$nodes[[1]],
                        mode = "in") |> names()
      
      out <- c(
        paste(h3(id)),
        paste(h3("level"), mtp$group[mtp$id == id]),
        paste(h3("title"), mtp$title[mtp$id == id])
      )
      return(paste(out))
    }
    
  })
  
  output$subnetwork <- renderVisNetwork({
    req(input$current_node_id)
    if (length(input$current_node_id$nodes) == 0) {
      return(NULL)
    } else {
      gs <- make_ego_graph(g, order = 3,
                           nodes = input$current_node_id$nodes[[1]], mode = "in")
      
      vng <- toVisNetworkData(gs[[1]])
      
      vng$nodes <- merge(vng$nodes, mtp)
      vng$nodes$label <- vng$nodes$title
      
      visNetwork(vng$nodes, vng$edges) |> 
        visEdges(arrows = "to", color = "black") |>
        visNodes(shape = "box", widthConstraint = 120) |>
        visInteraction(dragNodes = FALSE, 
                       dragView = FALSE, 
                       zoomView = FALSE) |>
        visHierarchicalLayout(direction = "LR", sortMethod = "directed") |>
        visGroups(groupname = "Beginner", color = "lightgreen") |>
        visGroups(groupname = "Intermediate", color = "orange") |>
        visGroups(groupname = "AND", color = "purple", shape = "circle",
                  widthConstraint = 20, font = list(size = 12,
                                                    color = "white")) |>
        visGroups(groupname = "OR", color = "blue", shape = "circle",
                  widthConstraint = 20, font = list(size = 12,
                                                    color = "white")) |>
        visOptions(highlightNearest = TRUE)
    }
    
  })
}



shinyApp(ui = ui, server = server)