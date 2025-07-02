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
library(plotly)

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

# ─── Task 4: Nadia Investigation Preparation ───────────────────────────────

# Prepare comms_data
comms <- mc3_nodes_cleaned %>%
  filter(type == "Event", sub_type == "Communication") %>%
  select(id, content, timestamp)

sent_edges <- mc3_edges_cleaned %>%
  filter(type == "sent") %>%
  select(source = from_id, comm_id = to_id)

recv_edges <- mc3_edges_cleaned %>%
  filter(type == "received") %>%
  select(comm_id = from_id, target = to_id)

comms_data <- comms %>%
  left_join(sent_edges, by = c("id" = "comm_id")) %>%
  rename(sender = source) %>%
  left_join(recv_edges, by = c("id" = "comm_id")) %>%
  rename(receiver = target) %>%
  left_join(mc3_nodes_cleaned %>% select(id, sender_label = label), by = c("sender" = "id")) %>%
  left_join(mc3_nodes_cleaned %>% select(id, receiver_label = label), by = c("receiver" = "id"))

# ─── Task 4a: Nadia message counts ──────────────────────────────
nadia_counts <- comms_data %>%
  summarise(
    Sent = sum(sender_label == "Nadia Conti", na.rm = TRUE),
    Received = sum(receiver_label == "Nadia Conti", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Count") %>%
  mutate(
    Percent = Count / sum(Count),
    Label = paste0(round(Percent * 100), "%\n(", Count, " msgs)")
  )

# ─── Task 4a: Daily and hourly frequencies ──────────────────────
nadia_data <- comms_data %>%
  filter(sender_label == "Nadia Conti" | receiver_label == "Nadia Conti") %>%
  mutate(timestamp = ymd_hms(timestamp),
         date = as.Date(timestamp),
         hour = hour(timestamp))

daily_freq <- nadia_data %>%
  group_by(date) %>%
  summarise(count = n(), .groups = "drop")

hourly_freq <- nadia_data %>%
  group_by(date, hour) %>%
  summarise(count = n(), .groups = "drop")

# ─── Task 4b: Relationship network preparation ──────────────────────
nadia_edges <- nadia_data %>%
  count(sender_label, receiver_label) %>%
  filter(!is.na(sender_label), !is.na(receiver_label)) %>%
  rename(from = sender_label, to = receiver_label, value = n)

entity_info <- bind_rows(
  nadia_data %>%
    left_join(mc3_nodes_cleaned %>% select(id, name = label, type = sub_type),
              by = c("sender" = "id")) %>%
    select(name, type),
  nadia_data %>%
    left_join(mc3_nodes_cleaned %>% select(id, name = label, type = sub_type),
              by = c("receiver" = "id")) %>%
    select(name, type)
) %>%
  distinct()

nadia_nodes <- tibble(name = unique(c(nadia_edges$from, nadia_edges$to))) %>%
  left_join(entity_info, by = "name") %>%
  mutate(
    group = ifelse(name == "Nadia Conti", "Nadia Conti", type),
    id = name,
    label = name,
    color = case_when(
      group == "Person" ~ "#fc8d62",
      group == "Organization" ~ "#6baed6",
      group == "Vessel" ~ "#66c2a2",
      group == "Location" ~ "#c6dbef",
      group == "Nadia Conti" ~ "#ffd92f",
      TRUE ~ "#d9d9d9"
    ),
    shape = case_when(
      group == "Person" ~ "dot",
      group == "Organization" ~ "square",
      group == "Vessel" ~ "triangle",
      group == "Location" ~ "diamond",
      group == "Nadia Conti" ~ "star",
      TRUE ~ "dot"
    )
  )

# ─── Task 4c: Top contacts preparation ───────────────────────────
nadia_id <- mc3_nodes_cleaned$id[mc3_nodes_cleaned$label == "Nadia Conti"]

nadia_comm_ids <- mc3_edges_cleaned %>%
  filter(type %in% c("sent", "received")) %>%
  filter(from_id == nadia_id | to_id == nadia_id) %>%
  mutate(comm_id = ifelse(type == "sent", to_id, from_id)) %>%
  pull(comm_id) %>%
  unique()

nadia_related_edges <- mc3_edges_cleaned %>%
  filter(from_id %in% nadia_comm_ids | to_id %in% nadia_comm_ids)

nadia_contacts_ids <- nadia_related_edges %>%
  mutate(person_id = ifelse(from_id %in% nadia_comm_ids, to_id, from_id)) %>%
  filter(!person_id %in% nadia_comm_ids, person_id != nadia_id) %>%
  count(person_id, sort = TRUE)

top_contacts_named <- nadia_contacts_ids %>%
  left_join(mc3_nodes_cleaned %>% filter(sub_type == "Person") %>% select(id, name = label),
            by = c("person_id" = "id")) %>%
  filter(!is.na(name))

# ─── Task 4c: Messages table preparation ─────────────────────────
top_contact_comm_ids <- mc3_edges_cleaned %>%
  filter(
    (from_id %in% nadia_comm_ids & to_id %in% top_contacts_named$person_id) |
      (to_id %in% nadia_comm_ids & from_id %in% top_contacts_named$person_id)
  ) %>%
  mutate(comm_id = ifelse(from_id %in% nadia_comm_ids, from_id, to_id)) %>%
  pull(comm_id) %>%
  unique()

nadia_messages <- mc3_nodes_cleaned %>%
  filter(id %in% top_contact_comm_ids) %>%
  filter(type == "Event", sub_type == "Communication") %>%
  select(id, timestamp, content) %>%
  left_join(mc3_edges_cleaned %>% filter(type == "sent") %>% select(id = to_id, sender = from_id),
            by = "id") %>%
  left_join(mc3_edges_cleaned %>% filter(type == "received") %>% select(id = from_id, receiver = to_id),
            by = "id") %>%
  left_join(mc3_nodes_cleaned %>% select(id, sender_name = label), by = c("sender" = "id")) %>%
  left_join(mc3_nodes_cleaned %>% select(id, receiver_name = label), by = c("receiver" = "id")) %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    sender_receiver = paste(sender_name, "→", receiver_name)
  ) %>%
  arrange(timestamp) %>%
  select(timestamp, sender_receiver, content)

# ─── ✅ END OF TASK 4 PREP ────────────────────────────────────────

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
  tabPanel("Task 1",
           tabsetPanel(
             tabPanel("1a: Daily Patterns",
                      plotOutput("task1a_barplot"),
                      plotlyOutput("task1a_heatmap"),
                      plotOutput("task1a_density_overview")
             ),
             tabPanel("1b: Shifts Over 2 Weeks",
                      plotOutput("task1b_hourly_barplot"),
                      plotOutput("task1b_halfhourly_barplot")
             ),
             tabPanel("1c: Nadia Conti Influence",
                      visNetworkOutput("nadia_network"),
                      DT::dataTableOutput("pagerank_table"),
                      DT::dataTableOutput("betweenness_table"),
                      DT::dataTableOutput("degree_table")
             )
           )
  ),

  tabPanel("Task 2",
           tabsetPanel(
             tabPanel("2a: People-Vessel Clusters",
                      plotOutput("task2a_cluster_plot", height = "900px"),
                      DT::dataTableOutput("task2a_table")
             ),
             tabPanel("2b: Theme Color-coded Network",
                      plotOutput("task2b_theme_plot", height = "900px")
             ),
             tabPanel("2c: Faceted Theme Network",
                      plotOutput("task2c_facet_plot", height = "900px")
             ),
             tabPanel("2d: Interactive Communication Network",
                      visNetworkOutput("task2d_visnet", height = "900px")
             )
           )
  ),
  
  tabPanel("Task 3",
           tabsetPanel(
             tabPanel("3a: Pseudonym Network",
                      plotOutput("task3a_pseudonym_graph", height = "800px"),
                      plotOutput("task3a_pseudonym_barplot", height = "600px"),
                      visNetworkOutput("task3a_pseudonym_visnet", height = "800px")
             ),
             tabPanel("3b: Top Central Pseudonyms",
                      plotOutput("task3b_topcentral_plot", height = "600px")
             ),
             tabPanel("3c: Shared Pseudonyms Network",
                      visNetworkOutput("task3c_shared_pseudonym_network", height = "800px")
             )
           )
  ),
  
  tabPanel("Task 4",
           tabsetPanel(
             tabPanel("4a: Message Count",
                      plotOutput("task4a_msgcount_plot"),
                      plotOutput("task4a_dailyfreq_plot"),
                      plotlyOutput("task4a_hourlyfreq_plot")
             ),
             tabPanel("4b: Relationship Network",
                      visNetworkOutput("task4b_network")
             ),
             tabPanel("4c: Top Contacts",
                      plotOutput("task4c_topcontacts_plot"),
                      DT::dataTableOutput("task4c_topcontacts_table")
             ),
             tabPanel("4d: Spike Days & Suspicious Dates",
                      plotlyOutput("task4d_spike_plot")
             ),
             tabPanel("4e: Oct 8 Details",
                      DT::dataTableOutput("task4e_oct8msgs_table"),
                      DT::dataTableOutput("task4e_oct8flagged_table"),
                      visNetworkOutput("task4e_oct8network")
             ),
             tabPanel("4f: Timeline Alignment",
                      DT::dataTableOutput("task4f_suspicious_events_table"),
                      plotlyOutput("task4f_timeline_plot")
             )
           )
  )
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
  # ─── Task 1a ──────────────────────────────────────────────────────────
  comm_events <- mc3_nodes_cleaned %>%
    filter(type == "Event", sub_type == "Communication") %>%
    mutate(timestamp = ymd_hms(timestamp),
           date_label = format(timestamp, "%d/%m/%Y (%a)"),
           hour = hour(timestamp),
           minute = minute(timestamp),
           time_bin = hour + ifelse(minute < 30, 0, 0.5))
  
  output$task1a_barplot <- renderPlot({
    comm_events %>%
      count(date_label) %>%
      ggplot(aes(x = date_label, y = n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Daily Communication Volume", x = "Date", y = "Messages") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$task1a_heatmap <- renderPlotly({
    heatmap_data <- comm_events %>%
      count(date_label, time_bin)
    p <- ggplot(heatmap_data, aes(time_bin, fct_rev(date_label), fill = n)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "inferno") +
      labs(title = "Half-Hourly Heatmap", x = "Time Bin", y = "Date") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$task1a_density_overview <- renderPlot({
    ggplot(comm_events, aes(time_bin)) +
      geom_density(fill = "steelblue", alpha = 0.7) +
      facet_wrap(~date_label, ncol = 4) +
      labs(title = "Daily Communication Density (Half-Hourly)", x = "Time Bin", y = "Density") +
      theme_minimal()
  })
  
  # ─── Task 1b ──────────────────────────────────────────────────────────
  output$task1b_hourly_barplot <- renderPlot({
    comm_events %>%
      count(hour) %>%
      ggplot(aes(hour, n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Overall Hourly Communication Volume", x = "Hour", y = "Messages") +
      theme_minimal()
  })
  
  output$task1b_halfhourly_barplot <- renderPlot({
    comm_events %>%
      count(time_bin) %>%
      ggplot(aes(time_bin, n)) +
      geom_col(fill = "steelblue") +
      labs(title = "Overall Half-Hourly Communication Volume", x = "Time Bin", y = "Messages") +
      theme_minimal()
  })
  
  # ─── Task 1c ──────────────────────────────────────────────────────────
  
  # Define reactive for Nadia's ego graph
  nadia_ego <- reactive({
    sent_edges <- mc3_edges_cleaned %>%
      filter(type == "sent") %>%
      select(source_entity = from_id, event = to_id)
    
    received_edges <- mc3_edges_cleaned %>%
      filter(type == "received") %>%
      select(event = from_id, target_entity = to_id)
    
    paired_edges <- sent_edges %>%
      inner_join(received_edges, by = "event") %>%
      select(from = source_entity, to = target_entity)
    
    single_sent_edges <- sent_edges %>%
      select(from = source_entity, to = event)
    
    single_received_edges <- received_edges %>%
      select(from = event, to = target_entity)
    
    all_edges <- bind_rows(paired_edges, single_sent_edges, single_received_edges) %>%
      distinct()
    
    entity_ids <- mc3_nodes_cleaned %>%
      filter(sub_type %in% c("Person", "Organization", "Vessel", "Group", "Location")) %>%
      pull(id) %>% as.character()
    
    entity_edges <- all_edges %>%
      filter(from %in% entity_ids, to %in% entity_ids)
    
    entity_nodes <- mc3_nodes_cleaned %>%
      filter(sub_type %in% c("Person", "Organization", "Vessel", "Group", "Location")) %>%
      select(id, label, sub_type)
    
    g <- graph_from_data_frame(d = entity_edges, vertices = entity_nodes, directed = TRUE)
    
    V(g)$pagerank <- page_rank(g)$vector
    V(g)$betweenness <- betweenness(g)
    V(g)$degree <- degree(g)
    
    target_index <- which(V(g)$label == "Nadia Conti")
    ego_graph <- make_ego_graph(g, order = 2, nodes = target_index, mode = "all")[[1]]
    
    return(ego_graph)
  })
  
  output$nadia_network <- renderVisNetwork({
    ego_graph <- nadia_ego()
    
    nodes_df <- data.frame(
      id = V(ego_graph)$name,
      label = V(ego_graph)$label,
      group = V(ego_graph)$sub_type,
      title = paste0("<b>", V(ego_graph)$label, "</b><br>",
                     "Degree: ", round(V(ego_graph)$degree, 2), "<br>",
                     "Betweenness: ", round(V(ego_graph)$betweenness, 2), "<br>",
                     "PageRank: ", round(V(ego_graph)$pagerank, 4)),
      shape = ifelse(V(ego_graph)$sub_type == "Person", "dot",
                     ifelse(V(ego_graph)$sub_type == "Organization", "square",
                            ifelse(V(ego_graph)$sub_type == "Vessel", "triangle",
                                   ifelse(V(ego_graph)$sub_type == "Group", "star", "diamond")))),
      value = V(ego_graph)$pagerank * 30 + 5
    )
    
    edges_df <- as_data_frame(ego_graph, what = "edges") %>%
      rename(from = from, to = to)
    
    visNetwork(nodes_df, edges_df, width = "100%", height = "1000px") %>%
      visNodes(scaling = list(min = 5, max = 30)) %>%
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.3)),
               color = list(color = "gray")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = FALSE) %>%
      visInteraction(dragNodes = FALSE, dragView = FALSE, zoomView = FALSE) %>%
      visLegend() %>%
      visLayout(randomSeed = 1818)
  })
  
  output$pagerank_table <- DT::renderDataTable({
    ego_graph <- nadia_ego()
    pagerank_df <- data.frame(
      label = V(ego_graph)$label,
      sub_type = V(ego_graph)$sub_type,
      pagerank = round(V(ego_graph)$pagerank, 4)
    ) %>% arrange(desc(pagerank))
    
    datatable(pagerank_df, caption = "PageRank Centrality (Nadia's Ego Network)")
  })
  
  output$betweenness_table <- DT::renderDataTable({
    ego_graph <- nadia_ego()
    betweenness_df <- data.frame(
      label = V(ego_graph)$label,
      sub_type = V(ego_graph)$sub_type,
      betweenness = round(V(ego_graph)$betweenness, 2)
    ) %>% arrange(desc(betweenness))
    
    datatable(betweenness_df, caption = "Betweenness Centrality (Nadia's Ego Network)")
  })
  
  output$degree_table <- DT::renderDataTable({
    ego_graph <- nadia_ego()
    degree_df <- data.frame(
      label = V(ego_graph)$label,
      sub_type = V(ego_graph)$sub_type,
      degree = V(ego_graph)$degree
    ) %>% arrange(desc(degree))
    
    datatable(degree_df, caption = "Degree Centrality (Nadia's Ego Network)")
  })
  
  # ─── Task 2a: People-Vessel Clusters ─────────────────────────────
  output$task2a_cluster_plot <- renderPlot({
    igraph_undirected <- as.undirected(as.igraph(mc3_graph), mode = "collapse")
    mc3_graph_undirected <- as_tbl_graph(igraph_undirected) %>%
      mutate(community = as.factor(group_louvain()))
    
    ggraph(mc3_graph_undirected, layout = "fr") +
      geom_edge_link(alpha = 0.1, colour = "grey") +
      geom_node_point(aes(color = community, shape = sub_type), size = 3, alpha = 0.9) +
      geom_node_text(aes(label = ifelse(sub_type %in% c("Person", "Vessel"), label, NA_character_)),
                     size = 2.5, repel = TRUE) +
      labs(
        title = "Community Clusters of People and Vessels",
        subtitle = "Detected using Louvain Algorithm"
      ) +
      theme_graph() +
      theme(legend.position = "bottom")
  })
  
  output$task2a_table <- DT::renderDataTable({
    igraph_undirected <- as.undirected(as.igraph(mc3_graph), mode = "collapse")
    mc3_graph_undirected <- as_tbl_graph(igraph_undirected) %>%
      mutate(community = as.factor(group_louvain()))
    
    people_vessels_comm <- mc3_graph_undirected %>%
      as_tibble() %>%
      filter(sub_type %in% c("Person", "Vessel")) %>%
      select(label, sub_type, community)
    
    DT::datatable(people_vessels_comm,
                  caption = "People and Vessels by Community",
                  options = list(pageLength = 10))
  })
  
  # ─── Task 2b: Theme Color-coded Network ─────────────────────────
  output$task2b_theme_plot <- renderPlot({
    igraph_undirected <- as.undirected(as.igraph(mc3_graph), mode = "collapse")
    mc3_graph_undirected <- as_tbl_graph(igraph_undirected) %>%
      mutate(
        theme = case_when(
          str_detect(label, regex("Green|Guardian", ignore_case = TRUE)) ~ "Environmental",
          str_detect(label, regex("Sailor Shift|Pop", ignore_case = TRUE)) ~ "Sailor Shift",
          sub_type == "Vessel" ~ "Vessel",
          TRUE ~ "Other"
        ),
        community = as.factor(group_louvain())
      )
    
    ggraph(mc3_graph_undirected, layout = "fr") +
      geom_edge_link(alpha = 0.1, colour = "grey") +
      geom_node_point(aes(color = theme, shape = sub_type), size = 3, alpha = 0.9) +
      geom_node_text(aes(label = ifelse(sub_type %in% c("Person", "Vessel"), label, NA_character_)),
                     size = 2.5, repel = TRUE) +
      labs(
        title = "Network of People and Vessels",
        subtitle = "Color-coded by Theme (Environmental, Sailor Shift, Vessel, Other)"
      ) +
      theme_graph() +
      theme(legend.position = "bottom")
  })
  
  # ─── Task 2c: Faceted Theme Network ─────────────────────────────
  output$task2c_facet_plot <- renderPlot({
    igraph_undirected <- as.undirected(as.igraph(mc3_graph), mode = "collapse")
    mc3_graph_undirected <- as_tbl_graph(igraph_undirected) %>%
      mutate(
        theme = case_when(
          str_detect(label, regex("Green|Guardian", ignore_case = TRUE)) ~ "Environmental",
          str_detect(label, regex("Sailor Shift|Pop", ignore_case = TRUE)) ~ "Sailor Shift",
          sub_type == "Vessel" ~ "Vessel",
          TRUE ~ "Other"
        ),
        community = as.factor(group_louvain())
      ) %>%
      filter(!is.na(theme))
    
    ggraph(mc3_graph_undirected, layout = "fr") +
      geom_edge_link(alpha = 0.1, colour = "grey") +
      geom_node_point(aes(color = community, shape = sub_type), size = 3, alpha = 0.9) +
      geom_node_text(aes(label = ifelse(sub_type %in% c("Person", "Vessel"), label, NA_character_)),
                     size = 2.5, repel = TRUE) +
      facet_wrap(~ theme) +
      labs(
        title = "Community Clusters Faceted by Theme",
        subtitle = "Facets: Environmental, Sailor Shift, Vessel, Other"
      ) +
      theme_graph() +
      theme(legend.position = "none")
  })
  
  # ─── Task 2d: Interactive Communication Network ──────────────────
  output$task2d_visnet <- renderVisNetwork({
    communication_events <- mc3_nodes_cleaned %>%
      filter(type == "Event", sub_type == "Communication") %>%
      select(id, label)
    
    comm_sent_edges <- mc3_edges_cleaned %>%
      filter(type == "sent", to_id %in% communication_events$id)
    
    comm_received_edges <- mc3_edges_cleaned %>%
      filter(type == "received", from_id %in% communication_events$id)
    
    comm_links <- comm_sent_edges %>%
      select(comm_id = to_id, sender = from_id) %>%
      inner_join(comm_received_edges %>% select(comm_id = from_id, receiver = to_id), by = "comm_id") %>%
      filter(sender != receiver)
    
    people_vessels <- mc3_nodes_cleaned %>%
      filter(sub_type %in% c("Person", "Vessel")) %>%
      select(id, label, group = sub_type)
    
    comm_links_filtered <- comm_links %>%
      filter(sender %in% people_vessels$id, receiver %in% people_vessels$id)
    
    edge_df <- comm_links_filtered %>%
      count(sender, receiver, name = "weight")
    
    nodes_df <- people_vessels %>%
      filter(id %in% c(edge_df$sender, edge_df$receiver)) %>%
      mutate(
        shape = ifelse(group == "Person", "dot", "triangle"),
        color = ifelse(group == "Person", "#fc8d62", "#66c2a2")
      )
    
    comm_vis_edges <- edge_df %>%
      rename(from = sender, to = receiver) %>%
      mutate(width = weight)
    
    visNetwork(nodes_df, comm_vis_edges, width = "100%", height = "900px") %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -80,
          centralGravity = 0.01,
          springLength = 50,
          springConstant = 0.02
        ),
        stabilization = list(enabled = TRUE, iterations = 100)
      ) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend(
        useGroups = FALSE,
        addNodes = list(
          list(label = "Person", shape = "dot", color = "#fc8d62"),
          list(label = "Vessel", shape = "triangle", color = "#66c2a2")
        ),
        width = 0.1,
        position = "left",
        stepY = 80,
        ncol = 1
      )
  })
  
  # ─── Task 3a: Pseudonym Network ───────────────────────────────────────────────
  
  pseudonym_keywords <- c("Boss", "The Lookout", "The Intern", "Mrs. Money", 
                          "The Accountant", "The Middleman", "Small Fry")
  
  pseudonym_nodes <- mc3_nodes_final %>%
    filter(
      sub_type == "Person",
      str_detect(name, regex(paste(pseudonym_keywords, collapse = "|"), ignore_case = TRUE))
    )
  
  pseudonym_node_indices <- pseudonym_nodes$new_index
  
  pseudonym_edges_final <- mc3_edges_final %>%
    filter(from %in% pseudonym_node_indices | to %in% pseudonym_node_indices)
  
  used_node_indices <- unique(c(pseudonym_edges_final$from, pseudonym_edges_final$to))
  
  pseudonym_nodes_final <- mc3_nodes_final %>%
    filter(new_index %in% used_node_indices) %>%
    mutate(label_type = ifelse(new_index %in% pseudonym_node_indices, "Pseudonym", "Regular")) %>%
    mutate(temp_index = row_number())
  
  index_map <- pseudonym_nodes_final %>%
    select(old = new_index, new = temp_index)
  
  pseudonym_edges_final <- pseudonym_edges_final %>%
    left_join(index_map, by = c("from" = "old")) %>%
    rename(from_new = new) %>%
    left_join(index_map, by = c("to" = "old")) %>%
    rename(to_new = new) %>%
    filter(!is.na(from_new), !is.na(to_new)) %>%
    select(from = from_new, to = to_new, type)
  
  pseudonym_graph <- tbl_graph(
    nodes = pseudonym_nodes_final,
    edges = pseudonym_edges_final,
    directed = TRUE
  )
  
  output$task3a_pseudonym_graph <- renderPlot({
    ggraph(pseudonym_graph, layout = "fr") +
      geom_edge_link(alpha = 0.3) +
      geom_node_point(aes(color = label_type), size = 4) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      labs(
        title = "Pseudonym Communication Network",
        subtitle = "Highlighting entities and their use of pseudonyms",
        color = "Entity Type"
      ) +
      theme_void()
  })
  
  output$task3a_pseudonym_barplot <- renderPlot({
    pseudonym_links <- pseudonym_edges_final %>%
      left_join(pseudonym_nodes_final, by = c("from" = "temp_index")) %>%
      rename(pseudonym = name) %>%
      filter(!is.na(pseudonym)) %>%
      group_by(pseudonym) %>%
      summarise(connection_count = n()) %>%
      arrange(desc(connection_count))
    
    ggplot(pseudonym_links, aes(x = reorder(pseudonym, connection_count), y = connection_count)) +
      geom_col(fill = "tomato") +
      coord_flip() +
      labs(
        title = "Communication Frequency by Pseudonym",
        x = "Pseudonym Name",
        y = "Number of Connections"
      )
  })
  
  output$task3a_pseudonym_visnet <- renderVisNetwork({
    nodes_vis <- pseudonym_nodes_final %>%
      transmute(
        id = temp_index,
        label = name,
        group = ifelse(label_type == "Pseudonym", "Pseudonym", "Regular"),
        title = paste("Name:", name, "<br>Type:", label_type)
      )
    
    edges_vis <- pseudonym_edges_final %>%
      transmute(
        from = from,
        to = to,
        label = type,
        arrows = "to"
      )
    
    visNetwork(nodes_vis, edges_vis, height = "600px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visGroups(groupname = "Pseudonym", color = "tomato") %>%
      visGroups(groupname = "Regular", color = "steelblue") %>%
      visLegend(addNodes = list(
        list(label = "Pseudonym", shape = "dot", color = "tomato"),
        list(label = "Regular", shape = "dot", color = "steelblue")
      )) %>%
      visLayout(randomSeed = 42) %>%
      visPhysics(stabilization = TRUE)
  })
  
  # ─── Task 3b: Top Central Pseudonyms ──────────────────────────────────────────
  output$task3b_topcentral_plot <- renderPlot({
    pseudonym_graph_tbl <- tbl_graph(
      nodes = pseudonym_nodes_final,
      edges = pseudonym_edges_final,
      directed = TRUE
    ) %>%
      mutate(degree_centrality = centrality_degree(mode = "all"))
    
    top_central <- pseudonym_graph_tbl %>%
      as_tibble() %>%
      filter(label_type == "Pseudonym") %>%
      arrange(desc(degree_centrality)) %>%
      slice_head(n = 10)
    
    ggplot(top_central, aes(x = reorder(name, degree_centrality), y = degree_centrality)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top 10 Central Pseudonym Entities",
        x = "Pseudonym Name",
        y = "Degree Centrality"
      )
  })
  
  # ─── Task 3c: Shared Pseudonyms Network ───────────────────────────────────────
  output$task3c_shared_pseudonym_network <- renderVisNetwork({
    shared_pseudonyms <- pseudonym_nodes_final %>%
      group_by(name) %>%
      filter(n() > 1) %>%
      ungroup()
    
    vis_nodes_3c <- shared_pseudonyms %>%
      transmute(id = id, 
                label = id, 
                group = "Entity",
                title = paste("Entity ID:", id)) %>%
      bind_rows(
        shared_pseudonyms %>%
          select(id = name) %>%
          distinct() %>%
          mutate(label = id,
                 group = "Pseudonym",
                 title = paste("Pseudonym:", id))
      )
    
    vis_edges_3c <- shared_pseudonyms %>%
      transmute(from = id, to = name)
    
    visNetwork(vis_nodes_3c, vis_edges_3c, height = "600px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visGroups(groupname = "Entity", color = "steelblue") %>%
      visGroups(groupname = "Pseudonym", color = "tomato") %>%
      visLegend(addNodes = list(
        list(label = "Entity", shape = "dot", color = "steelblue"),
        list(label = "Pseudonym", shape = "dot", color = "tomato")
      )) %>%
      visLayout(randomSeed = 123)
  })
  
  # ─── Task 4a ──────────────────────────────────────────────────────────
  output$task4a_msgcount_plot <- renderPlot({
    req(nadia_counts)
    ggplot(nadia_counts, aes(x = Count, y = reorder(Type, Count), fill = Type)) +
      geom_col(color = "white") +
      geom_text(aes(label = paste0(Count, " msgs (", round(Percent * 100), "%)")),
                hjust = -0.1, size = 4) +
      scale_fill_manual(values = c("Sent" = "deepskyblue3", "Received" = "cyan")) +
      labs(title = paste0("Nadia Conti's Messages (Total: ", sum(nadia_counts$Count), ")"),
           x = "Message Count", y = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold")) +
      xlim(0, max(nadia_counts$Count) * 1.2)
  })
  
  output$task4a_dailyfreq_plot <- renderPlot({
    req(daily_freq)
    ggplot(daily_freq, aes(x = date, y = count)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = count), vjust = -0.5, size = 3) +
      labs(
        title = "Nadia Conti's Daily Message Frequency",
        x = "Date",
        y = "Message Count"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$task4a_hourlyfreq_plot <- renderPlotly({
    req(hourly_freq)
    plot_ly(
      data = hourly_freq,
      x = ~hour,
      y = ~count,
      color = ~as.factor(date),
      type = 'bar',
      text = ~paste("Date:", date, "<br>Hour:", hour, "<br>Messages:", count),
      hoverinfo = 'text'
    ) %>%
      layout(
        barmode = 'dodge',
        title = "Nadia Conti's Hourly Message Frequency",
        xaxis = list(title = "Hour of Day"),
        yaxis = list(title = "Message Count"),
        legend = list(title = list(text = "Date"))
      )
  })
  
  # ─── Task 4b ──────────────────────────────────────────────────────────
  output$task4b_network <- renderVisNetwork({
    req(nadia_nodes, nadia_edges)
    visNetwork(nadia_nodes, nadia_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 123)
  })
  
  # ─── Task 4c ──────────────────────────────────────────────────────────
  output$task4c_topcontacts_plot <- renderPlot({
    req(top_contacts_named)
    top_contacts_named %>%
      slice_max(n, n = 3) %>%
      ggplot(aes(x = reorder(name, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Top 3 Contacts Communicating with Nadia Conti",
        x = "Contact Person",
        y = "Number of Messages"
      ) +
      theme_minimal()
  })
  
  output$task4c_topcontacts_table <- DT::renderDataTable({
    req(nadia_messages)
    DT::datatable(
      nadia_messages,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ─── Task 4d: Spike Days & Suspicious Dates ───────────────────────────────
  threshold <- mean(daily_freq$count) + 2 * sd(daily_freq$count)
  
  daily_freq_plot <- daily_freq %>%
    mutate(
      status = ifelse(count >= threshold, "Spike", "Normal")
    )
  
  status_colors <- c("Spike" = "red", "Normal" = "grey")
  
  output$task4d_spike_plot <- renderPlotly({
    plot_ly(
      data = daily_freq_plot,
      x = ~date,
      y = ~count,
      type = 'bar',
      color = ~status,
      colors = status_colors,
      text = ~paste("Date:", date, "<br>Messages:", count, "<br>Status:", status),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Nadia Conti's Daily Communication with Spike Detection",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Message Count"),
        barmode = 'group',
        legend = list(title = list(text = "Status"))
      ) %>%
      add_lines(
        x = ~date,
        y = rep(threshold, nrow(daily_freq_plot)),
        line = list(dash = 'dash', color = 'red'),
        name = 'Spike Threshold',
        inherit = FALSE
      )
  })
  
  # ─── Task 4e: Oct 8 Details ──────────────────────────────────────────────
  
  # 1️⃣ Filter Oct 8 messages
  oct8_msgs <- nadia_data %>%
    filter(date == as.Date("2040-10-08")) %>%
    select(timestamp, sender_label, receiver_label, content) %>%
    arrange(timestamp)
  
  # 2️⃣ Define suspicious keywords
  keywords <- c("permit", "approval", "reef", "cargo", "shipment", "illegal")
  
  # 3️⃣ Flag suspicious messages containing keywords
  oct8_flagged_msgs <- oct8_msgs %>%
    filter(!is.na(content)) %>%
    filter(str_detect(content, regex(paste(keywords, collapse = "|"), ignore_case = TRUE)))
  
  # 4️⃣ Build oct8_edges for network visualisation
  oct8_edges <- nadia_data %>%
    filter(date == as.Date("2040-10-08")) %>%
    count(sender_label, receiver_label) %>%
    filter(!is.na(sender_label), !is.na(receiver_label)) %>%
    rename(from = sender_label, to = receiver_label, value = n)
  
  # 5️⃣ Build oct8_nodes
  oct8_nodes <- tibble(name = unique(c(oct8_edges$from, oct8_edges$to))) %>%
    left_join(mc3_nodes_cleaned %>% select(label, sub_type), by = c("name" = "label")) %>%
    mutate(
      group = ifelse(name == "Nadia Conti", "Nadia Conti", sub_type),
      id = name,
      label = name
    )
  
  # 6️⃣ Render outputs in Shiny
  output$task4e_oct8msgs_table <- DT::renderDataTable({
    DT::datatable(
      oct8_msgs,
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  output$task4e_oct8flagged_table <- DT::renderDataTable({
    DT::datatable(
      oct8_flagged_msgs,
      options = list(pageLength = 5, autoWidth = TRUE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size:16px; color:#444;',
        '📌 Oct 8 Messages with Suspicious Keywords'
      )
    )
  })
  
  output$task4e_oct8network <- renderVisNetwork({
    visNetwork(oct8_nodes, oct8_edges) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 456) %>%
      visPhysics(stabilization = TRUE) %>%
      visLegend()
  })
  
  # ─── Task 4f: Timeline Alignment ──────────────────────────────────────────────
  
  # 1️⃣ Prepare suspicious vessel/harbor events after Oct 8
  suspicious_events_alt <- mc3_nodes_cleaned %>%
    filter(type == "Event", sub_type %in% c("VesselMovement", "Monitoring", "HarborReport", "Fishing", "Enforcement")) %>%
    mutate(timestamp = ymd_hms(timestamp)) %>%
    filter(timestamp >= as.POSIXct("2040-10-08"))
  
  # 2️⃣ Display table of suspicious events
  output$task4f_suspicious_events_table <- DT::renderDataTable({
    DT::datatable(
      suspicious_events_alt %>%
        select(id, timestamp, label, sub_type),
      options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE),
      rownames = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; font-size:16px; color:#444;',
        'Suspicious Vessel & Harbor Events'
      )
    )
  })
  
  # 3️⃣ Prepare entity-related vessel/harbor events for timeline plot
  entity_events <- suspicious_events_alt %>%
    filter(str_detect(label, regex("Neptune|Miesel|Mako", ignore_case = TRUE))) %>%
    mutate(entity = case_when(
      str_detect(label, regex("Neptune", ignore_case = TRUE)) ~ "Neptune",
      str_detect(label, regex("Miesel", ignore_case = TRUE)) ~ "Miesel",
      str_detect(label, regex("Mako", ignore_case = TRUE)) ~ "Mako",
      TRUE ~ "Other"
    ))
  
  # 4️⃣ Build interactive timeline plot
  output$task4f_timeline_plot <- renderPlotly({
    plot_ly() %>%
      # Add Nadia communications
      add_markers(
        data = nadia_data,
        x = ~timestamp,
        y = ~"Nadia Message",
        marker = list(color = "red", size = 10),
        text = ~paste0("Nadia Message<br>", timestamp),
        hoverinfo = "text",
        name = "Nadia Message"
      ) %>%
      # Add Neptune events
      add_markers(
        data = entity_events %>% filter(entity == "Neptune"),
        x = ~timestamp,
        y = ~entity,
        marker = list(color = "#1f77b4", size = 10),
        text = ~paste0(entity, " Event<br>", label),
        hoverinfo = "text",
        name = "Neptune Event"
      ) %>%
      # Add Miesel events
      add_markers(
        data = entity_events %>% filter(entity == "Miesel"),
        x = ~timestamp,
        y = ~entity,
        marker = list(color = "#17becf", size = 10),
        text = ~paste0(entity, " Event<br>", label),
        hoverinfo = "text",
        name = "Miesel Event"
      ) %>%
      # Add Mako events
      add_markers(
        data = entity_events %>% filter(entity == "Mako"),
        x = ~timestamp,
        y = ~entity,
        marker = list(color = "#7f7f7f", size = 10),
        text = ~paste0(entity, " Event<br>", label),
        hoverinfo = "text",
        name = "Mako Event"
      ) %>%
      layout(
        title = "Nadia Messages + Vessel/Harbor Events Timeline",
        xaxis = list(title = "Time"),
        yaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.1, y = -0.3)
      )
  })
}
shinyApp(ui = ui, server = server)