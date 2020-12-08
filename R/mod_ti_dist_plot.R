

plot_ti_dist <- function(data) {
  
  calculate_ti <- function(x) {
    
    x <- sort(x, decreasing = T)
    ti <- NULL
    
    for (i in 1:length(x)) {
      
      ti[i] <- x[i] * prod((x[1:i-1] + 1)^-1)
      
    }
    
    return(sum(ti))
  }
  
  
  plot_data <- data %>%
    filter(ae_grade != 'OFF') %>%
    mutate(
      ae_grade = case_when(
        ae_grade == '0-1' ~ '0',
        TRUE ~ ae_grade
      ) %>% as.numeric()
    ) %>%
    group_by(patientid) %>%
    summarise(ti = suppressWarnings(calculate_ti(ae_grade)), .groups = 'drop') %>%
    ungroup() %>% 
    drop_na()
  
  gg <- ggplot(plot_data, aes(x = ti)) +
    geom_histogram(binwidth = .1) +
    theme_light(base_size = 15) +
    scale_x_continuous(breaks = seq(0, 6, .5)) +
    coord_cartesian(xlim = c(-0.5, 6.5)) +
    geom_vline(aes(xintercept = mean(ti)), linetype = 'dotted', color = 'blue', size = 1) +
    geom_vline(aes(xintercept = median(ti)),
               linetype = 'dashed',
               color = 'red',
               size = 1) +
    labs(
      x = 'Toxicity Index (TI)',
      y = 'n',
      title = glue::glue(
        'Mean TI: {scales::comma(mean(plot_data$ti), accuracy = .01)}, Median TI: {scales::comma(median(plot_data$ti), accuracy = .01)}'
      )
    )
  
  return(gg)
  
}

# debugonce(plot_ti_dist)
# 
# 
# temp <- data %>%
#   filter(
#     trt == 'Anastrozole' &
#       ae == 'ARTHRL' &
#       ncycle %in% 1:5 &
#       ae_grade != '0-1'
#   )
# 
# plot_ti_dist(temp)



tidistUI <- function(id) {
  tagList(plotOutput(NS(id, 'ti_dist_plot')))
}

tidistServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$ti_dist_plot <- renderPlot({
      plot_ti_dist(data())
    })
    
    
  })
}

