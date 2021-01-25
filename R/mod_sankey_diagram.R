process_data_for_sankey <- function(data) {

  ## calculate transitions
  temp <- data %>%
    distinct(patientid, trt, ae, ncycle, .keep_all = T) %>%
    group_by(patientid) %>%
    arrange(ncycle) %>%
    mutate(
      ae_grade = factor(
        ae_grade,
        levels = c('0-1', '2', '3', '4', 'OFF'),
        labels = c('0-1', 2:4, 'OFF')
      ),
      transition = paste0(
        'cycle',
        lag(ncycle),
        '.grade',
        lag(ae_grade),
        '_cycle',
        ncycle,
        '.grade',
        ae_grade
      )
    ) %>%
    arrange(patientid) %>%
    ungroup() %>%
    select(patientid, trt, transition) %>%
    filter(!str_detect(transition, 'NA'))

  ## calculate transition counts
  temp <- temp %>%
    group_by(transition) %>%
    count() %>%
    ungroup()

  temp <- temp %>%
    separate(transition, c('source', 'target'), sep = '_')

  temp <- temp %>%
    arrange(source)

  ## this is to fix the labels
  temp <- temp %>%
    separate(
      source, c('source_cycle', 'source_grade'),  remove = F, sep = '\\.'
    ) %>%
    separate(
      target, c('target_cycle', 'target_grade'), remove = F, sep = '\\.'
    ) %>%
    mutate(
      across(c(source_cycle, target_cycle), ~ str_remove(.x, 'cycle') %>% as.numeric()),
      across(c(source_grade, target_grade), ~ str_remove(.x, 'grade'))
    ) %>%
    mutate(
      source = glue::glue('Cycle {source_cycle} - Grade {source_grade}'),
      target = glue::glue('Cycle {target_cycle} - Grade {target_grade}')
    )

  temp <- temp %>%  select(source, target, n)

  temp <- as_tbl_graph(temp)

  temp <- temp %>% igraph_to_networkD3()
  
  temp$nodes <- temp$nodes %>%
    mutate(
      group = case_when(
        str_detect(name, 'Grade 0-1') ~ '0',
        str_detect(name, 'Grade 2') ~ '1',
        str_detect(name, 'Grade 3') ~ '2',
        str_detect(name, 'Grade 4') ~ '3',
        str_detect(name, 'Grade 5') ~ '4',
        str_detect(name, 'Grade OFF') ~ '5'
      )
    )

  return(temp)
}

# debugonce(process_data_for_sankey)
# 
# temp <- ae_data %>%
#   filter(trt == 'Anastrozole') %>%
#   filter(ae == 'ARTHRL') %>%
#   group_by(patientid) %>%
#   filter(any(
#     ncycle == 1 &
#       ae_grade == 3
#   )) %>%
#   ungroup() %>%
#   filter(ncycle %in% c(1:5))
# 
# data <- process_data_for_sankey(temp)
# 
# my_color <- 'd3.scaleOrdinal() .domain(["0", "1", "2", "3", "4", "5"]) .range(["#feedde", "#fdbe85" , "#fd8d3c", "#e6550d", "#a63603", "#74c476"])'
# 
# 
# sankeyNetwork(
#   Links = data$links,
#   Nodes = data$nodes,
#   Source = "source",
#   Target = "target",
#   Value = "value",
#   NodeID = "name",
#   colourScale = my_color,
#   NodeGroup = "group",
#   sinksRight = F,
#   nodeWidth = 20,
#   units = 'Patients',
#   fontSize = 15,
#   nodePadding = 25,
#   iterations = 0,
#   margin = c(
#     'top' = 50,
#     'right' = 0,
#     'bottom' = 0,
#     'left' = 0
#   )
# )


sankeyUI <- function(id) {
  tagList(sankeyNetworkOutput(NS(id, 'sankey_output')))
}

sankeyServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    sankey_data <- reactive({
      process_data_for_sankey(data())
      
    })
    
    my_color <- 'd3.scaleOrdinal() .domain(["0", "1", "2", "3", "4", "5"]) .range(["#feedde", "#fdbe85" , "#fd8d3c", "#e6550d", "#a63603", "#74c476"])'
    
    output$sankey_output <- renderSankeyNetwork({
      sankeyNetwork(
        Links = sankey_data()$links,
        Nodes = sankey_data()$nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        colourScale = my_color,
        NodeGroup = 'group',
        sinksRight = F,
        nodeWidth = 20,
        units = 'Patients',
        fontSize = 15,
        nodePadding = 25,
        iterations = 0,
        margin = c(
          'top' = 50,
          'right' = 0,
          'bottom' = 0,
          'left' = 0
        )
      )

    })
    
  })
}

