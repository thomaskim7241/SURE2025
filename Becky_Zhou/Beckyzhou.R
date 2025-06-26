install.packages("readxl")  
library(readxl)

# Set the file path
file_path <- "/Users/beckyzhou/SURE2025/analytic_data2025_v2.xlsx"

df <- read_excel(file_path)
colnames(df) <- df[2,]
## sanity check
# 'v001_race_nhopi_flag' %in% colnames(df)
# df$v001_race_nhopi_flag
# df$v138_rawvalue

# Preview the data
head(df)
View(df)     

#renaming the race varibles for drug overdoes
df_race_overdose <- df %>%
  select(
    `Drug Overdose Deaths (Asian)`,
    `Drug Overdose Deaths (Black)`,
    `Drug Overdose Deaths (Hispanic)`,
    `Drug Overdose Deaths (White)`,
    `Drug Overdose Deaths (AIAN)`,
    `Drug Overdose Deaths (NHOPI)`
  ) %>%
  rename(
    Asian = `Drug Overdose Deaths (Asian)`,
    Black = `Drug Overdose Deaths (Black)`,
    Hispanic = `Drug Overdose Deaths (Hispanic)`,
    White = `Drug Overdose Deaths (White)`,
    `Native American` = `Drug Overdose Deaths (AIAN)`,
    `Pacific Islander` = `Drug Overdose Deaths (NHOPI)`
  )

#checking
library(stringr)
names(df)[str_detect(names(df), "Drug Overdose")]
library(stringr)

# Find all column names containing "overdose" (case-insensitive, trimmed)
names(df)[str_detect(str_to_lower(str_trim(names(df))), "overdose")]
