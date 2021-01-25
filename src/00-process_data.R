library(tidyverse)

process_data <- function(file) {
  
  # browser()
  
  load_base_data <- function(file) {
    
    ae <- readxl::read_excel(file, sheet = 2) %>% janitor::clean_names()
    
    main <- readxl::read_excel(file, sheet = 1) %>% janitor::clean_names()
    
    trt_data <- main %>% 
      select(
        patientid, trt
      )
    
    data <- ae %>%
      left_join(
        ., trt_data, by = 'patientid'
      ) %>%
      select(
        patientid, ncycle, trt, ae, ae_grade
      )
    
    return(data)
  }
  
  complete_base_data <- function(data) {
    
    data %>%
      group_by(patientid) %>%
      complete(., nesting(patientid, trt), ncycle = c(0:max(ncycle))) %>%
      ungroup() %>%
      complete(., nesting(patientid, trt, ncycle), ae, fill = list(ae_grade = '0-1')) %>%
      arrange(patientid, trt, ae, ncycle)%>%
      mutate(ae_grade = as.character(ae_grade)) %>%
      filter(
        !is.na(ae)
      )
    
  }
  
  add_additional_cycle_off <- function(data) {
    
    data %>%
      group_nest(
        patientid, trt, ae
      ) %>% 
      mutate(
        data = map(data, ~ add_row(.x, ncycle = max(.x$ncycle) + 1, ae_grade = 'OFF'))
      ) %>%
      unnest(
        data
      )
    
  }
  
  data <- load_base_data(file)
  
  data <- complete_base_data(data)
  
  data <- add_additional_cycle_off(data)
  
  data <- data %>% distinct(patientid, trt, ae, ncycle, .keep_all = T)
  
  return(data)
  
}


# debugonce(process_data)
data <- process_data(here::here('data/B35data_Main_AE_QOL 4-10-2020.xlsx'))

write_rds(data, here::here('data/processed_data.rds'), compress = 'gz')


# data <- read_rds('data/processed_data.rds')
# 
# ae <- readxl::read_excel('data/B35data_Main_AE_QOL 4-10-2020.xlsx', sheet = 2)
# 
# description <- ae %>%
#   janitor::clean_names() %>%
#   select(
#     ae, description
#   ) %>%
#   distinct(
#     ae, .keep_all = T
#   ) %>%
#   filter(
#     !is.na(ae)
#   )
# 
# data <- data %>%
#   left_join(
#     description
#   ) %>%
#   select(
#     patientid, trt, ae, description, ncycle, ae_grade
#   )
# 
# write_rds(data, 'data/processed_data_labeled.rds')
# 
# 
# 
# 
# 
# 
# 
# 

































