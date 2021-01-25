data <- read_rds(here::here('data/processed_data_labeled.rds'))

keep_ae <- data %>%
  group_by(
    trt, ae, ae_grade
  ) %>%
  count() %>%
  filter(
    ae_grade != '0-1' & ae_grade != 'OFF'
  ) %>%
  group_by(
    trt, ae
  ) %>%
  summarise(
    total = sum(n)
  ) %>%
  arrange(desc(total)) %>%
  ungroup() %>%
  group_nest(trt) %>%
  mutate(
    data = map(data, ~ slice_head(.x, n = 50))
  ) %>%
  unnest(data)

keep_ae <- keep_ae %>% pull(ae)

data <- data %>%
  filter(
    ae %in% keep_ae
  )


# for(i in seq_along(unique(data$ae))) {
#   
#   val_label(data$ae, unique(data$ae)[[i]]) <- unique(data$description)[[i]]
#   
# }

write_rds(data, 'data/processed_condensed_data_labelled.rds')
