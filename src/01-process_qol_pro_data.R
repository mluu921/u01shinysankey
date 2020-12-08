library(tidyverse)

df <- readxl::read_excel('data/B35data_Main_AE_QOL 8-25-2020.xlsx', sheet = 'QOL (long fmt)')

df <- df %>% janitor::clean_names()

df <- df %>%
  relocate(
    time_point, .after = 'patientid'
  ) %>%
  select(
    patientid, time_point, hotfl:othpro
  ) %>%
  pivot_longer(
    3:ncol(.)
  )

treatment_data <- readxl::read_excel('data/B35data_Main_AE_QOL 8-25-2020.xlsx', sheet = 'Main') %>%
  janitor::clean_names() %>%
  select(
    patientid, trt
  )

df <- df %>%
  left_join(
    treatment_data
  ) %>%
  relocate(
    trt, .after = patientid
  )


time_point <- c("Baseline form", "6 Month form", "12 Month form", "18 Month form", "24 Month form", "30 Month form", 
                "36 Month form", "42 Month form", "48 Month form", "54 Month form", 
                "60 Month form", "66 Month form", "72 Month form")

df <- df %>%
  mutate(
    time_point = factor(time_point, levels = time_point, labels = time_point)
  )


df <- df %>%
  complete(
    nesting(
      patientid, trt, name
    ),
    time_point
  )

df <- df %>%
  mutate(
    time_point_numeric = as.numeric(time_point) - 1
  ) %>%
  relocate(
    time_point_numeric, .before = 'time_point'
  )

write_rds(df, 'data/processed_full_qol_pro_data.rds')


