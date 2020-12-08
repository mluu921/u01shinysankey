


process_data_for_summary <- function(data) {
  
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

  return(temp)
}

# temp <- data %>%
#   filter(trt == 'Anastrozole') %>%
#   filter(ae == 'ABDPN') %>%
#   group_by(patientid) %>%
#   filter(any(
#     ncycle == 1 &
#       ae_grade == 3
#   )) %>%
#   ungroup() %>%
#   filter(ncycle %in% c(1:18))


# debugonce(process_data_for_summary)
# res <- process_data_for_summary(temp)


summary_desc <- function(data) {
  
  
  temp <- data %>%
    separate(
      source, c('source_cycle', 'source_grade'), sep = ' - '
    ) %>%
    separate(
      target, c('target_cycle', 'target_grade'), sep = ' - '
    ) 
  
  initial <- temp %>%
    mutate(
      source_cycle = as_factor(source_cycle),
      source_grade = as_factor(source_grade)
    ) %>%
    filter(as.numeric(source_cycle) == 1 & as.numeric(source_grade) == 1) %>%
    group_by(source_cycle, source_grade) %>%
    summarise(total = sum(n), .groups = 'drop') %>%
    ungroup()
    
  final <- temp %>%
    filter(str_extract(target_cycle, '\\d+') == max(as.numeric(str_extract(target_cycle, '\\d+')))) %>%
    group_by(target_cycle, target_grade) %>%
    summarise(total = sum(n), .groups = 'drop') %>%
    ungroup() %>%
    filter(!str_detect(target_grade, 'OFF')) %>%
    mutate(
      overall = initial$total,
      prop = scales::percent(total / overall, accuracy = .1),
      text = glue::glue('A total of {total} ({prop}) of the patients exhibited {target_grade}')
    )
  
  off <- temp %>%
    filter(
      str_detect(target_grade, 'OFF')
    ) %>%
    summarise(n = sum(n), .groups = 'drop')
  

  if(nrow(final) == 0) {
    
    final_cycle <- max(str_extract(temp$target_cycle, "\\d+"))
    out <- glue::glue(
      'Among a total of {initial$total} patients selected in {initial$source_cycle} with {initial$source_grade}. A total of {off$n} ({scales::percent(off$n/initial$total, accuracy = .1)}) of the patients were OFF the trial by Cycle {final_cycle}.')
    
    
  } else {
    
    out <- glue::glue(
      'Among a total of {initial$total} patients selected in {initial$source_cycle} with {initial$source_grade}. By {final$target_cycle[[1]]}, {paste0(final$text, collapse = ". ")}. Finally, a total of {off$n} ({scales::percent(off$n/initial$total, accuracy = .1)}) of the patients were OFF the trial.'
    )
    
  }
  
  return(out)
  
}


summarydescOutput <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns('summary_res'))
  )
}

summarydescServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    summary_data <- reactive({
      process_data_for_summary(data())
    })
    
    summary_res <- reactive({
      
      summary_desc(summary_data())
      
    })
    
    output$summary_res <- renderText({summary_res()})
    
  })
}
