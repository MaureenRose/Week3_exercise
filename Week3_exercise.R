install.packages("tidyverse")
install.packages("readr")

library(readr)
library(tidyverse)

#IMPORT DATA ----
clinic_raw = read_csv("data/clinic_raw.csv", skip = 2) #skip junk lines
head(clinic_raw)

#CHECK DATA ----
str(clinic_raw) #check data type
sum(is.na(clinic_raw$age)) # TRUE counts as 1, so sum() tallies the NAs
range(clinic_raw$age, na.rm = TRUE) # na.rm drops NAs before computing min/max
summary(clinic_raw) 

#CLEAN PROBLEMS ----
library(stringr)
clinic_clean = clinic_raw

clinic_clean = clinic_clean %>% #Problem: site, sex, and notes are stored inconsistently
  mutate(
    site = tolower(trimws(site)) # lowercase + strip whitespace
    , site = gsub("_", " ", site)  # replace underscore with space
    , sex = toupper(sex)
    , notes = tolower(trimws(notes))
    ) %>% #Problem: visit_date is stored as 'character' instead of 'date' object
  mutate(
    visit_date = as.Date(visit_date, format = "%d/%m/%Y") #dd/mm/yyyy
  ) %>% #Problem: blood pressure is stored as 'character' instead of 'double' because of 'mmHg' and 'N/A'
  mutate(
    blood_pressure = na_if(blood_pressure, "N/A"), #turn all N/A into NA
    bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")) # digits before the "/"
    , bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+")) # digits after the "/"
  ) %>% #Problem: weight is stored as 'character' instead of 'double' because NA is recorded as '-'
  mutate (
    weight_kg = na_if(weight_kg, "-")
    , weight_kg = as.numeric (weight_kg)
  ) %>% #remove original blood pressure column
  select(patient_id, site, sex, age, visit_date, bp_upper, bp_lower, weight_kg, notes)
  
#CALCULATE MEAN AGE BY SITE ----
mean_age_by_site = clinic_clean %>%
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE)
  )

#EXPORTING CLEAN DATA ----
write_csv(clinic_clean, "data/clinic_clean.csv")

#Visualisation ----
library(ggplot2)
ggplot(clinic_clean, aes(x = site, y = age, fill = site)) +
  geom_boxplot(na.rm = TRUE) +
  stat_summary(
    na.rm = TRUE,
    fun = median,
    geom = "text",
    aes(label = after_stat(round(y, 1))),
    vjust = -0.5,
    colour = "black",
    size = 4
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribution of Patient Age by Site",
    x = "Site",
    y = "Age"
  ) +
  geom_jitter(width = 0.2, alpha = 0.3, na.rm = TRUE) +
  theme_classic()

#renv ----
renv::snapshot()
