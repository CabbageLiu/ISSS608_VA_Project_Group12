library(shiny)
library(tidyverse)
library(jsonlite)
library(tidygraph)
library(ggraph)
library(lubridate)
library(SmartEDA)
library(igraph)
library(viridis)
library(ggrepel)
library(scales)
library(DT)
library(visNetwork)

# ─── Load and Clean Data ────────────────────────────────────────────────
MC3 <- fromJSON("Data/MC3_graph.json")
MC3_schema <- fromJSON("Data/MC3_schema.json")

mc3_nodes <- as_tibble(MC3$nodes)
mc3_edges <- as_tibble(MC3$edges)

mc3_nodes_cleaned <- mc3_nodes %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-thing_collected)

mc3_edges_cleaned <- mc3_edges %>%
  rename(from_id = source, to_id = target) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  filter(from_id %in% mc3_nodes_cleaned$id,
         to_id %in% mc3_nodes_cleaned$id) %>%
  filter(!is.na(from_id), !is.na(to_id))

node_index_lookup <- mc3_nodes_cleaned %>%
  mutate(.row_id = row_number()) %>%
  select(id, .row_id)

mc3_edges_indexed <- mc3_edges_cleaned %>%
  left_join(node_index_lookup, by = c("from_id" = "id")) %>%
  rename(from = .row_id) %>%
  left_join(node_index_lookup, by = c("to_id" = "id")) %>%
  rename(to = .row_id) %>%
  select(from, to, is_inferred, type) %>%
  filter(!is.na(from) & !is.na(to))

used_node_indices <- sort(unique(c(mc3_edges_indexed$from, mc3_edges_indexed$to)))

mc3_nodes_final <- mc3_nodes_cleaned %>%
  slice(used_node_indices) %>%
  mutate(new_index = row_number())

old_to_new_index <- tibble(
  old_index = used_node_indices,
  new_index = seq_along(used_node_indices))

mc3_edges_final <- mc3_edges_indexed %>%
  left_join(old_to_new_index, by = c("from" = "old_index")) %>%
  rename(from_new = new_index) %>%
  left_join(old_to_new_index, by = c("to" = "old_index")) %>%
  rename(to_new = new_index) %>%
  select(from = from_new, to = to_new, is_inferred, type)

mc3_graph <- tbl_graph(nodes = mc3_nodes_final,
                       edges = mc3_edges_final,
                       directed = TRUE)

# ─── Shiny UI ────────────────────────────────────────────────────────────
ui <- navbarPage(
  "VAST Challenge Mini Challenge 3 Group 12",
  
  tags$head(
    tags$style(HTML("
      .frontPage {
        position: fixed;
        top: 0; left: 0; right: 0; bottom: 0;
        background-image: url('dolphin-cow-jumping-sea-meme-desktop-wallpaper-preview-2.jpg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        color: white;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
        background-color: rgba(0,0,0,0.4);
        padding: 100px 40px;
        font-size: 22px;
        z-index: -1;
      }
    "))
  ),
  
  # ─── Home Tab ─────────────────────────────────────────────────────────
  tabPanel("Home",
           div(
             class = "frontPage",
             h1("Welcome to the VAST MC3 Dashboard"),
             p("Group 12 · SMU MITB · 2025"),
             p("Explore the graph-based analysis and uncover hidden connections.")
           )
  ),
  
  # ─── Background Tab ──────────────────────────────────────────────────
  tabPanel("Background",
           htmlOutput("background_html")
  ),
  
  # ─── EDA Tab ──────────────────────────────────────────────────────────
tabPanel("EDA",
  sidebarLayout(
    sidebarPanel(
      width = 2,  # Reduced width for sidebar
      style = "background-color: transparent; border: none;",
      selectInput("eda_choice", "Select EDA Visualization:",
        choices = c(
          "1. Relationship between entities and events" = "graph_rel",
          "2. Entity distribution" = "entity_dist",
          "3. Event type distribution" = "event_dist",
          "4. Communication participants & network" = "comm_network"
        ),
        width = "100%"  # Full width within sidebarPanel
      )
    ),
    mainPanel(
      width = 10,  # Increased width for main panel
      conditionalPanel(
        condition = "input.eda_choice == 'graph_rel'",
        plotOutput("plot_graph_rel")
      ),
      conditionalPanel(
        condition = "input.eda_choice == 'entity_dist'",
        plotOutput("plot_entity_dist")
      ),
      conditionalPanel(
        condition = "input.eda_choice == 'event_dist'",
        plotOutput("plot_event_dist")
      ),
      conditionalPanel(
        condition = "input.eda_choice == 'comm_network'",
        tagList(
          DT::dataTableOutput("comm_table"),
          visNetwork::visNetworkOutput("comm_visnet", height = "800px")
        )
      )
    )
  )
),
  
  # ─── Task Tabs ────────────────────────────────────────────────────────
  tabPanel("Task 1", h3("Task 1 content here")),
  tabPanel("Task 2", h3("Task 2 content here")),
  tabPanel("Task 3", h3("Task 3 content here")),
  tabPanel("Task 4", h3("Task 4 content here"))
)

# ─── Shiny Server ────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Tab Content Switcher ──────────────────────────────────────────────
  output$tab_content <- renderUI({
    switch(input$tabs,
           "front" = div(
             id = "frontPage",
             h1("Welcome to the VAST MC3 Dashboard"),
             p("Group 12 · SMU MITB · 2025"),
             p("Explore the graph-based analysis and uncover hidden connections.")
           ),
           
           "background" = HTML("
             <p>This take home exercise is based on the VAST Challenge Mini Case 3</p>
             <p>Over the past decade, the community of Oceanus has faced numerous transformations and challenges evolving from its fishing-centric origins. Following major crackdowns on illegal fishing activities, suspects have shifted investments into more regulated sectors such as the ocean tourism industry, resulting in growing tensions. This increased tourism has recently attracted the likes of international pop star Sailor Shift, who announced plans to film a music video on the island.</p>
             <p>Clepper Jessen, a former analyst at FishEye and now a seasoned journalist for the Hacklee Herald, has been keenly observing these rising tensions. Recently, he turned his attention towards the temporary closure of Nemo Reef. By listening to radio communications and utilizing his investigative tools, Clepper uncovered a complex web of expedited approvals and secretive logistics. These efforts revealed a story involving high-level Oceanus officials, Sailor Shift’s team, local influential families, and local conservationist group The Green Guardians, pointing towards a story of corruption and manipulation.</p>
             <p><strong>Your task is to develop new and novel visualizations and visual analytics approaches to help Clepper get to the bottom of this story.</strong></p>"
           ),
           
           "structure" = verbatimTextOutput("graph_str"),
           "nodes" = tableOutput("node_table"),
           "edges" = tableOutput("edge_table"),
           
           "eda" = tagList(
             selectInput("eda_choice", "Select EDA Visualization:",
                         choices = c(
                           "1. Relationship between entities and events" = "graph_rel",
                           "2. Entity distribution" = "entity_dist",
                           "3. Event type distribution" = "event_dist",
                           "4. Communication participants & network" = "comm_network"
                         )),
             uiOutput("eda_output")
           ),
           
           "graphplot" = plotOutput("network_plot")
    )
  })
  
  # ─── Outputs for Structure Tab ─────────────────────────────────────────
  output$graph_str <- renderPrint({ str(mc3_graph) })
  output$node_table <- renderTable({ head(mc3_nodes_final, 10) })
  output$edge_table <- renderTable({ head(mc3_edges_final, 10) })
  
  # ─── EDA Switcher ──────────────────────────────────────────────────────
  output$eda_output <- renderUI({
    req(input$eda_choice)
    switch(input$eda_choice,
           "graph_rel" = plotOutput("plot_graph_rel"),
           "entity_dist" = plotOutput("plot_entity_dist"),
           "event_dist" = plotOutput("plot_event_dist"),
           "comm_network" = tagList(
             DT::dataTableOutput("comm_table"),
             visNetwork::visNetworkOutput("comm_visnet", height = "800px")
           )
    )
  })
  
  # ─── 1. Relationship between Entities and Events ──────────────────────
  output$plot_graph_rel <- renderPlot({
    ggraph(mc3_graph, layout = "fr") +
      geom_edge_link(alpha = 0.3, colour = "gray") +
      geom_node_point(aes(color = type), size = 2) +
      geom_node_text(aes(label = type), repel = TRUE, size = 2.5) +
      theme_void()
  })
  
  # ─── 2. Entity Distribution ───────────────────────────────────────────
  output$plot_entity_dist <- renderPlot({
    mc3_nodes_final %>%
      filter(type == "Entity") %>%
      count(sub_type, sort = TRUE) %>%
      ggplot(aes(x = reorder(sub_type, n), y = n, fill = sub_type)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label = n), hjust = -0.1) +
      labs(title = "Entity Sub-type Distribution", x = "Sub-type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ─── 3. Event Type Distribution ───────────────────────────────────────
  output$plot_event_dist <- renderPlot({
    mc3_nodes_final %>%
      filter(type == "Event") %>%
      count(sub_type, sort = TRUE) %>%
      ggplot(aes(x = reorder(sub_type, n), y = n, fill = sub_type)) +
      geom_col() +
      coord_flip() +
      geom_text(aes(label = n), hjust = -0.1) +
      labs(title = "Event Sub-type Distribution", x = "Sub-type", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # ─── 4a. Communication Participants Table ─────────────────────────────
  output$comm_table <- DT::renderDataTable({
    comm_event_ids <- mc3_nodes_cleaned %>%
      filter(type == "Event", sub_type == "Communication") %>%
      pull(id)
    comm_sent_edges <- mc3_edges_cleaned %>%
      filter(type == "sent", to_id %in% comm_event_ids) %>%
      select(comm_id = to_id, sender_id = from_id)
    comm_received_edges <- mc3_edges_cleaned %>%
      filter(type == "received", from_id %in% comm_event_ids) %>%
      select(comm_id = from_id, receiver_id = to_id)
    comm_pairs <- comm_sent_edges %>%
      inner_join(comm_received_edges, by = "comm_id")
    participants_named <- comm_pairs %>%
      left_join(mc3_nodes_cleaned %>% select(id, sender_label = label), by = c("sender_id" = "id")) %>%
      left_join(mc3_nodes_cleaned %>% select(id, receiver_label = label), by = c("receiver_id" = "id"))
    participants_named %>%
      count(sender_label, receiver_label, sort = TRUE) %>%
      datatable(
        caption = "Top Communication Pairs (Sender → Receiver)",
        colnames = c("Sender", "Receiver", "Message Count"),
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
  })
  
  # ─── 4b. Communication Participants Network ───────────────────────────
  output$comm_visnet <- visNetwork::renderVisNetwork({
    comm_event_ids <- mc3_nodes_cleaned %>%
      filter(type == "Event", sub_type == "Communication") %>%
      pull(id)
    comm_sent_edges <- mc3_edges_cleaned %>%
      filter(type == "sent", to_id %in% comm_event_ids) %>%
      select(comm_id = to_id, sender_id = from_id)
    comm_received_edges <- mc3_edges_cleaned %>%
      filter(type == "received", from_id %in% comm_event_ids) %>%
      select(comm_id = from_id, receiver_id = to_id)
    comm_pairs <- comm_sent_edges %>%
      inner_join(comm_received_edges, by = "comm_id")
    participants_named <- comm_pairs %>%
      left_join(mc3_nodes_cleaned %>% select(id, sender_label = label), by = c("sender_id" = "id")) %>%
      left_join(mc3_nodes_cleaned %>% select(id, receiver_label = label), by = c("receiver_id" = "id"))
    comm_edges_vis <- participants_named %>%
      count(sender_id, receiver_id, sort = TRUE) %>%
      rename(from = sender_id, to = receiver_id, value = n)
    message_counts <- comm_edges_vis %>%
      group_by(from) %>%
      summarise(sent_count = sum(value), .groups = "drop")
    nodes_vis <- mc3_nodes_cleaned %>%
      filter(id %in% unique(c(comm_edges_vis$from, comm_edges_vis$to))) %>%
      select(id, label, sub_type) %>%
      left_join(message_counts, by = c("id" = "from")) %>%
      mutate(
        size = if_else(
          sub_type == "Person",
          rescale(sent_count, to = c(10, 40), na.rm = TRUE),
          15
        ),
        title = paste0(label, "<br>Sub-type: ", sub_type,
                       ifelse(!is.na(sent_count), paste0("<br>Sent: ", sent_count, " messages"), "")),
        color = case_when(
          sub_type == "Person" ~ "#2ca5ff",
          sub_type == "Organization" ~ "#f5ee15",
          sub_type == "Vessel" ~ "#FB7E81",
          sub_type == "Group" ~ "#25e158",
          sub_type == "Location" ~ "#ec4bff",
          TRUE ~ "black"
        ),
        shape = case_when(
          sub_type == "Person" ~ "dot",
          sub_type == "Organization" ~ "square",
          sub_type == "Vessel" ~ "triangle",
          sub_type == "Group" ~ "star",
          sub_type == "Location" ~ "diamond",
          TRUE ~ "dot"
        )
      )
    legend_nodes <- data.frame(
      label = c("Person", "Organization", "Vessel", "Group", "Location"),
      color = c("#2ca5ff", "#f5ee15", "#FB7E81", "#25e158", "#ec4bff"),
      shape = c("dot", "square", "triangle", "star", "diamond"),
      stringsAsFactors = FALSE
    )
    edges_vis <- comm_edges_vis %>%
      mutate(
        arrows = "to",
        width = rescale(value, to = c(1, 6)),
        title = paste("Messages:", value)
      )
    visNetwork(nodes_vis, edges_vis, width = "100%", height = "800px") %>%
      visNodes(size = nodes_vis$size) %>%
      visLegend(
        addNodes = lapply(1:nrow(legend_nodes), function(i) {
          list(
            label = legend_nodes$label[i],
            shape = legend_nodes$shape[i],
            color = legend_nodes$color[i]
          )
        }),
        useGroups = FALSE,
        width = 0.15
      ) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visIgraphLayout(layout = "layout_on_sphere") %>%
      visPhysics(enabled = FALSE) %>%
      visLayout(randomSeed = 1818)
  })
}
shinyApp(ui = ui, server = server)