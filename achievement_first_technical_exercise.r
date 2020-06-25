# Analyze the data set and prepare a set of findings to present to the team and 
# school principals. 
# Convert scores to proficiency levels

setwd('/home/drake/R')
library("readxl")
library('dplyr')
library('ggplot2')

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

# plotting the differences in proficiency over the year
ggplot(school_data, aes(x=difference)) + 
  geom_bar(width=.5, fill="blue") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title="Proficiency Difference", 
       subtitle="Difference between BOY and EOY proficiency levels",
       y = '# of Students',
       x = 'Difference in Proficiency Levels',
       caption='F&P Proficiency')

school_data_bushwick <- school_data %>%
  filter(`School Name`=="Bushwick Middle School")

school_data_crown <- school_data %>%
  filter(`School Name`=="Crown Heights Middle School")

# We can clearly see that this is skewed right
ggplot(school_data_bushwick, aes(x=difference)) + 
  geom_bar(width=.5, fill="red") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title="Proficiency Difference: Bushwick", 
       y = '# of Students',
       x = 'Difference in Proficiency Levels',
       caption='F&P Proficiency')

# Visually, this appears skewed slightly to the left
ggplot(school_data_crown, aes(x=difference)) + 
  geom_bar(width=.5, fill="green") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 05)) +
  labs(title="Proficiency Difference: Crown Heights", 
       y = '# of Students',
       x = 'Difference in Proficiency Levels',
       caption='F&P Proficiency')


##### p value and analysis between the two schools' improvements

# Here, I take the two data frames I created of the separate schools and select
# only the student ids that have "proficient" or "advanced" scores recorded for
# the end of year tests, assuming EOY scores are our most current data...
num_prof_bush <- school_data_bushwick %>%
  filter(eoy_proficiency == 'proficient' | eoy_proficiency == 'advanced')

num_prof_crown <- school_data_crown %>%
  filter(eoy_proficiency == 'proficient' | eoy_proficiency == 'advanced')

# Now I calculate the percentage of students who recorded "proficient" or 
# "advanced" end of year scores. We see that Crown has a higher percentage
# than Bushwick does (71.27% and 78.41%, respectively)
percent_prof_bush <- nrow(num_prof_bush)/nrow(school_data_bushwick)
percent_prof_crown <- nrow(num_prof_crown)/nrow(school_data_crown)

# same thing with boy
num_prof_bush_boy <- school_data_bushwick %>%
  filter(boy_proficiency == 'proficient' | boy_proficiency == 'advanced')

num_prof_crown_boy <- school_data_crown %>%
  filter(boy_proficiency == 'proficient' | boy_proficiency == 'advanced')

percent_prof_bush_boy <- nrow(num_prof_bush_boy)/nrow(school_data_bushwick)
percent_prof_crown_boy <- nrow(num_prof_crown_boy)/nrow(school_data_crown)

# Here I make a graph to visualize the improvement in test scores over the
# course of the year, by school.
boy_eoy_by_school <- data.frame(school=rep(c('Bushwick', 'Crown'), each=2),
                                test=rep(c('BOY', 'EOY'), 2),
                                len=c(percent_prof_bush_boy, percent_prof_bush,
                                      percent_prof_crown_boy, percent_prof_crown))
ggplot(data=boy_eoy_by_school, aes(x=test, y=len, group=school)) +
  ylab('Percent Proficient') +
  xlab('Test Date') +
  geom_line(aes(linetype=school, color=school)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_point()

# Now I want to see the trends according to the grade level. Again, I filter the
# dataset to only contain those who tested proficient or advanced, but instead of
# grouping by school, I group by grade
by_grade_5 <- school_data %>%
  filter(grade == '5.0')
by_grade_6 <- school_data %>%
  filter(grade == '6.0')


num_prof_5_boy <- school_data %>%
  filter((boy_proficiency == 'proficient' | boy_proficiency == 'advanced') & grade == '5.0')
num_prof_5_eoy <- school_data %>%
  filter((eoy_proficiency == 'proficient' | eoy_proficiency == 'advanced') & grade == '5.0')

# percent of 5th graders that are proficient, by test
percent_prof_5_boy <- nrow(num_prof_5_boy)/nrow(by_grade_5)
percent_prof_5_eoy <- nrow(num_prof_5_eoy)/nrow(by_grade_5)
  
num_prof_6_boy <- school_data %>%
  filter((boy_proficiency == 'proficient' | boy_proficiency == 'advanced') & grade == '6.0')
num_prof_6_eoy <- school_data %>%
  filter((eoy_proficiency == 'proficient' | eoy_proficiency == 'advanced') & grade == '6.0')

# percent of 6th graders that are proficient, by test
percent_prof_6_boy <- nrow(num_prof_6_boy)/nrow(by_grade_6)
percent_prof_6_eoy <- nrow(num_prof_6_eoy)/nrow(by_grade_6)

boy_eoy_by_grade <- data.frame(grade=rep(c('5th', '6th'), each=2),
                               test=rep(c('BOY', 'EOY'), 2),
                               len=c(percent_prof_5_boy, percent_prof_5_eoy,
                                      percent_prof_6_boy, percent_prof_6_eoy))

ggplot(data=boy_eoy_by_grade, aes(x=test, y=len, group=grade)) +
  ylab('Percent Proficient') +
  xlab('Test Date') +
  geom_line(aes(linetype=grade, color=grade)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_point()


