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

df <- df %>%
  mutate(
    value = factor(value, levels = c('Not at all', 'Slightly', 'Moderately', 'Quite a bit', 'Extremely'))
  )



desc <- tibble::tribble(
      ~name,                                                                   ~description,
    "HOTFL",                   "Symptom Checklist: Hot Flashes (PRO-CTCAE equivalent item)",
   "HEADAC",                     "Symptom Checklist: Headaches (PRO-CTCAE equivalent item)",
   "BLADLC",   "Symptom Checklist: Difficulty with bladder control–when laughing or crying",
   "BLADOT",            "Symptom Checklist: Difficulty with bladder control-at other times",
   "VAGDIS",             "Symptom Checklist: Vaginal discharge (PRO-CTCAE equivalent item)",
   "VAGBLD",  "Symptom Checklist: Vaginal bleeding or spotting (PRO-CTCAE equivalent item)",
   "GENITC",                                "Symptom Checklist: Genital itching/irritation",
   "VAGDRY",               "Symptom Checklist: Vaginal dryness (PRO-CTCAE equivalent item)",
   "PAININ",         "Symptom Checklist: Pain with intercourse (PRO-CTCAE equivalent item)",
   "PAINBR",                                               "Symptom Checklist: Breast pain",
    "BRSTS",                               "Symptom Checklist: Difficulty with breast skin",
    "BRSTT", "Symptom Checklist: Breast sensitivity/tenderness (PRO-CTCAE equivalent item)",
  "GENACHE",       "Symptom Checklist: General aches and pains (PRO-CTCAE equivalent item)",
   "JOINTP",                   "Symptom Checklist: Joint pains (PRO-CTCAE equivalent item)",
    "STIFF",              "Symptom Checklist: Muscle stiffness (PRO-CTCAE equivalent item)",
   "WTGAIN",                                               "Symptom Checklist: Weight gain",
   "WTLOSS",                                               "Symptom Checklist: Weight loss",
   "UNHAPP",                        "Symptom Checklist: Unhappy with appearance of my body",
   "DECAPP",                                        "Symptom Checklist: Decreased appetite",
   "FORGET",                 "Symptom Checklist: Forgetfulness (PRO-CTCAE equivalent item)",
   "EXCITE",                                              "Symptom Checklist: Excitability",
   "SHORTT",                                              "Symptom Checklist: Short temper",
     "NAPS",                        "Symptom Checklist: Tendency to take naps; stay in bed",
  "NIGHTSW",                                              "Symptom Checklist: Night sweats",
   "COLDSW",                                               "Symptom Checklist: Cold sweats",
  "DIFFCON",      "Symptom Checklist: Difficulty concentrating (PRO-CTCAE equivalent item)",
   "EASDIS",                                         "Symptom Checklist: Easily distracted",
   "TROUSL",                                          "Symptom Checklist: Trouble sleeping",
   "EAAWAK",                                           "Symptom Checklist: Early awakening",
   "OTHPRO",                                        "Symptom Checklist: Any other problems"
  )


desc <- desc %>%
  mutate(
    description = str_remove(description, 'Symptom Checklist: '),
    description = str_remove(description, '\\(PRO-CTCAE equivalent item\\)'),
    description = str_trim(description)
  )

df <- df %>%
  mutate(
    name = str_to_upper(name)
  ) %>%
  left_join(desc)



write_rds(df, 'data/processed_full_qol_pro_data.rds')


