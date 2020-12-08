grade_duration_plot <- function(data) {
  
  plot_data <- data %>%
    filter(ae_grade != 'OFF')
  
  plot_data <- plot_data %>%
    mutate(
      ae_grade = factor(ae_grade, levels = c('0-1', 2:5), labels = c('Grade 0-1', paste0('Grade ', 2:5))),
      ncycle = factor(ncycle, levels = c(min(ncycle):max(ncycle)))
    ) %>%
    group_by(ae_grade, ncycle) %>%
    count() %>%
    ungroup() %>%
    drop_na()
  
  gg <- ggplot(plot_data, aes(x = n, y = fct_rev(ncycle)))  +
    geom_col() + 
    scale_x_continuous(labels = scales::label_comma(accuracy = 1)) +
    facet_wrap(~ae_grade, scales = 'free') +
    theme_light(base_size = 15) +
    labs(x = 'n', y = 'Cycle')
  
  return(gg)
}


# debugonce(grade_duration_plot)
# 
# temp <- data %>%
#   filter(
#     trt == 'Anastrozole' &
#       ae == 'ARTHRL' &
#       ncycle %in% 1:5 &
#       ae_grade != '0-1'
#   )
# 
# grade_duration_plot(temp)


gradedurationUI <- function(id) {
  tagList(plotOutput(NS(id, 'ti_dist_plot')))
}

gradedurationServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    output$ti_dist_plot <- renderPlot({
      grade_duration_plot(data())
    })
    
    
  })
}

