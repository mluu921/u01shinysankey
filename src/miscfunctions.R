

process_sankey_plot_data <-
  function(data,
           selected_treatment,
           selected_start_cycle,
           selected_end_cycle,
           selected_ae,
           selected_grade) {
    

    ## resume sankey
    temp <- temp %>%
      group_by(patientid) %>%
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
      ungroup() %>%
      select(patientid, trt, transition) %>%
      filter(!str_detect(transition, 'NA'))
    
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
    
    ## this is to limit the view from the app 
    
    temp <- temp %>%
      filter(
        source_cycle %in% selected_start_cycle:(selected_end_cycle-1)
      )
    
    nodes <-
      tibble(name = c(temp$source, temp$target) %>% unique()) %>%
      mutate(group = 'group')
    
    temp$IDsource = match(temp$source, nodes$name) - 1
    temp$IDtarget = match(temp$target, nodes$name) - 1
    
    nodes <- nodes %>%
      mutate(name = glue::glue('{name} '))
    
    ## this is the new table to calculate the proportion in the final cycle?
    output_table <- function(data, selected_start_cycle, selected_end_cycle) {
      source_patients <- data %>%
        select(
          source_cycle, source, target, n
        ) %>%
        filter(
          source_cycle %in% c(selected_start_cycle)
        ) %>%
        group_by(source) %>%
        summarise(
          total_patients = sum(n)
        )
      
      table_prop_grade <- data %>%
        select(
          source_cycle, source, target, n
        ) %>%
        filter(
          source_cycle %in% c(selected_end_cycle - 1)
        ) %>%
        group_by(target) %>%
        summarise(
          n = sum(n),
          total_patients = source_patients$total_patients,
          prop = n / total_patients,
          prop_text = paste0(format(round((n / total_patients) * 100, 1), 1), '%')
        ) %>%
        filter(
          row_number() != nrow(.)
        )
      
      table_prop_off <- data %>%
        select(
          source_cycle, source, target, n
        ) %>%
        filter(
          source_cycle %in% c(selected_end_cycle - 1)
        ) %>%
        group_by(target) %>%
        summarise(
          n = source_patients$total_patients -  (sum(table_prop_grade$n)),
          total_patients = source_patients$total_patients,
          prop = n / total_patients,
          prop_text = paste0(format(round((n / total_patients) * 100, 1), 1), '%')
        ) %>%
        filter(
          row_number() == nrow(.)
        )
      
      out <- bind_rows(table_prop_grade, table_prop_off)
      return(out)
    }
    
    table <- output_table(temp, selected_start_cycle, selected_end_cycle)
    
    out <- list('links' = temp, 'nodes' = nodes, 'table' = table, 'ti_dist_fig' = ti_dist_fig,
                'grade_duration_fig' = grade_duration_fig)
    
    return(out)
    
  }


identify_list_ae <- function(data, selected_treatment) {
  data %>%
    filter(trt == selected_treatment) %>%
    summarise(
      available_ae = list(unique(ae))
    )
}

identify_cycle_ranges <- function(data, selected_treatment, selected_ae) {
  data %>%
    filter(trt == selected_treatment & ae == selected_ae) %>%
    summarise(
      ncycle_min = min(ncycle),
      ncycle_max = max(ncycle)
    )
}

identify_grade_ranges <- function(data, selected_treatment, selected_ae, selected_start_cycle) {
  data %>%
    filter(trt == selected_treatment & ae == selected_ae & ncycle == selected_start_cycle) %>%
    summarise(
      grade_range = list(unique(ae_grade) %>% sort())
    )
}

summarize_table <- function(data) {
  
  res <- data
  
  res <- res %>%
    separate(target, c('cycle', 'grade'), ' - ') %>%
    mutate(
      grade = str_remove(grade, 'Grade '),
      cycle = str_remove(cycle, 'Cycle ')
    )
  
  grade <- res %>%
    filter(
      grade != 'OFF'
    ) %>%
    mutate(
      text = glue::glue('{n} ({prop_text}) of the patients progressed to Grade {grade},')
    )
  
  off <- res %>%
    filter(
      grade == 'OFF'
    ) %>%
    mutate(
      text = glue::glue('and {n} ({prop_text}) were {grade} the trial by Cycle {as.numeric(cycle[[1]])}')
    )
  
  text <- bind_rows(
    grade, off
  )
  
  out <- glue::glue(
    'Among a total of {text$total_patients[[1]]} patients in the selected initial cycle and AE. {paste0(text$text, collapse = " ")}'
  )
  
  return(out)
  
}

calculate_ti <- function( tox_vect ) {
  
  tox_vect <- sort(tox_vect, decreasing = T)
  ti <- NULL
  
  for (i in 1:length(tox_vect)) {
    
    ti[i] <- tox_vect[i] * prod((tox_vect[1:i-1] + 1)^-1)
    
  }
  
  return(sum(ti))
}


# dat %>%
#   filter(
#     ncycle %in% 1:5 & ae == 'ARTHRL' & trt == 'Anastrozole' &
#       !(ae_grade %in% c('0-1', 'OFF'))
#   ) %>% 
#   group_by(
#     patientid
#   ) %>%
#   summarise(
#     ti = calculate_ti(as.numeric(ae_grade))
#   ) %>%
#   summarise(
#     mean_ti = mean(ti),
#     median_ti = median(ti)
#   )

# debugonce(process_sankey_plot_data)
# 
# plot_data <- process_sankey_plot_data(
#   data = dat,
#   selected_treatment = 'Anastrozole',
#   selected_start_cycle = 2,
#   selected_end_cycle = 5,
#   selected_ae = 'ARTHRL',
#   selected_grade = c('2')
# )
# 
# sankeyNetwork(
#   Links = plot_data$links,
#   Nodes = plot_data$nodes,
#   Source = "IDsource",
#   Target = "IDtarget",
#   Value = "n",
#   NodeID = "name",
#   NodeGroup = 'group',
#   sinksRight = FALSE,
#   nodeWidth = 20,
#   units = 'Patients',
#   fontSize = 15
# )
# 
# summarize_table(plot_data$table)



# selected_treatment <- 'Anastrozole'
# selected_ae <- 'ABDPN'
# selected_start_cycle <- 1
# selected_grade <- '0-1'
# 
# ## this is to select the treatment and the selected ae 
# temp <- dat %>%
#   filter(trt == selected_treatment & ae == selected_ae)
# 
# ## this is to select the cycle and the grade
# temp <- temp %>%
#   group_by(patientid) %>%
#   filter(
#     any(ncycle == selected_start_cycle & ae_grade %in% selected_grade)
#   ) %>%
#   ungroup()
