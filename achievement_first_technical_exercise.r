# Analyze the data set and prepare a set of findings to present to the team and 
# school principals. 
# Convert scores to proficiency levels

setwd('/home/drake/R')
library("readxl")
library('dplyr')

school_data <- read_excel('F&P Sample Data Set.xlsx')
# ID, School name, Beginning of year score, End of year score

# Searches for variation within the values. I omitted the student IDs since
# every id would vary. This will allow us to easily see any misspellings or 
# differences in how values were recorded (for example, "5.0" vs. "5th")
unique_values <- apply(school_data[, c('School Name', 
                      'Grade Level', 
                      'BOY F&P Score', 
                      'EOY F&P Score')], 2, unique)
# places the scores in numerical order and assigns them as integers
unique_values$`BOY F&P Score`=sort(as.integer(unique_values$`BOY F&P Score`))
unique_values$`EOY F&P Score`=sort(as.integer(unique_values$`EOY F&P Score`))
unique_values

# renames the score columns to remove whitespace and special character from name
school_data <- school_data %>%
  rename(boy_score = 'BOY F&P Score',
         eoy_score = 'EOY F&P Score',
         grade = 'Grade Level')

# remove NA values
school_data <- na.omit(school_data)

# Since there are only a couple of values that need changing, I use grepl 
# (similar to grep in shell scripts) to replace the values
school_data$grade[grepl(
  '5',school_data$grade)] <- '5.0'
school_data$grade[grepl(
  '6',school_data$grade)] <- '6.0'
school_data$`School Name`[grepl(
  'Bush',school_data$`School Name`)] <-'Bushwick Middle School'
school_data$`School Name`[grepl(
  'Crown',school_data$`School Name`)] <- 'Crown Heights Middle School'

# Another way this could have been done without additional packages:
#school_data$`Grade Level`[school_data$`Grade Level`=='5th'] <- '5.0'
#school_data$`Grade Level`[school_data$`Grade Level`=='6th'] <- '6.0'

# Now a quick double check to make sure the values have been replaced:
unique_values_2 <- apply(school_data[, c('School Name', 
                                       'Grade Level', 
                                       'boy_score', 
                                       'eoy_score')], 2, unique)
unique_values_2$`boy_score`=sort(as.integer(unique_values_2$`boy_score`))
unique_values_2$`eoy_score`=sort(as.integer(unique_values_2$`eoy_score`))
c(unique_values_2$`School Name`, unique_values_2$`Grade Level`)

# potential 'ifelse' way of computing proficiency
# school_data <- school_data %>%
#   mutate(boy_proficiency = ifelse(boy_score<=9 & grade=='5.0', 'remedial',
#                            ifelse(boy_score<=11 & grade=='5.0', 'below proficient', 
#                            ifelse(boy_score<= 13 & grade=='5.0', 'proficient',
#                            ifelse(boy_score>=14 & grade=='5.0', 'advanced', 
#                            ifelse(boy_score<=11&grade=='6.0', 'remedial',
#                            ifelse(boy_score<=13&grade=='6.0', 'below proficient',
#                            ifelse(boy_score<=15&grade=='6.0', 'proficient',
#                            ifelse(boy_score>=16&grade=='6.0', 'advanced', ' '
# )))))))))

# Use dplyr to convert BOY and EOY scores to proficiency levels, according to
# the "F&P Proficiency Levels" tab
school_data <- school_data %>%
  mutate(boy_proficiency = case_when(
    boy_score <= 9 & grade == '5.0' ~ 'remedial',
    boy_score <= 11 & grade == '5.0' ~ 'below proficient',
    boy_score <= 13 & grade == '5.0' ~ 'proficient',
    boy_score >= 14 & grade == '5.0' ~ 'advanced',
    boy_score <= 11 & grade == '6.0' ~ 'remedial',
    boy_score <= 13 & grade == '6.0' ~ 'below proficient',
    boy_score <= 15 & grade == '6.0' ~ 'proficient',
    boy_score >= 16 & grade == '6.0' ~ 'advanced'
  ))


school_data <- school_data %>%
  mutate(eoy_proficiency = case_when(
    eoy_score <= 11 & grade == '5.0' ~ 'remedial',
    eoy_score <= 13 & grade == '5.0' ~ 'below proficient',
    eoy_score <= 15 & grade == '5.0' ~ 'proficient',
    eoy_score >= 16 & grade == '5.0' ~ 'advanced',
    eoy_score <= 13 & grade == '6.0' ~ 'remedial',
    eoy_score <= 15 & grade == '6.0' ~ 'below proficient',
    eoy_score <= 17 & grade == '6.0' ~ 'proficient',
    eoy_score >= 18 & grade == '6.0' ~ 'advanced'
  ))


# improvement column
# I am listing the difference as an integer, where each integer is the difference
# of one proficiency level
school_data <- school_data %>%
  mutate(difference = case_when(
    boy_proficiency == eoy_proficiency ~ '0',
    boy_proficiency == 'remedial' & eoy_proficiency == 'below proficient' ~ '1',
    boy_proficiency == 'remedial' & eoy_proficiency == 'proficient' ~ '2',
    boy_proficiency == 'remedial' & eoy_proficiency == 'advanced' ~ '3',
    boy_proficiency == 'below proficient' & eoy_proficiency == 'remedial' ~ '-1',
    boy_proficiency == 'below proficient' & eoy_proficiency == 'proficient' ~ '1',
    boy_proficiency == 'below proficient' & eoy_proficiency == 'advanced' ~ '2',
    boy_proficiency == 'proficient' & eoy_proficiency == 'remedial' ~ '-2',
    boy_proficiency == 'proficient' & eoy_proficiency == 'below proficient' ~ '-1',
    boy_proficiency == 'proficient' & eoy_proficiency == 'advanced' ~ '1',
    boy_proficiency == 'advanced' & eoy_proficiency == 'remedial' ~ '-3',
    boy_proficiency == 'advanced' & eoy_proficiency == 'below proficient' ~ '-2',
    boy_proficiency == 'advanced' & eoy_proficiency == 'proficient' ~ '-1'
  ))
# set difference column as an integer for future calculations
school_data$difference = as.integer(school_data$difference)


