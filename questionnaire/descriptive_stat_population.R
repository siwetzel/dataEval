library(readr)
library(tidyverse)
library(dplyr)
library(rstatix)
library(ggplot2)

replace_commas <- function(x) {
  gsub(",", ".", x)
}

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R/questionnaire")

# Read coded data of first questionnaire
data_char = data.frame(read_csv2("input_data/quest1_raw.csv", show_col_types = FALSE), row.names = 1)

#### PREPARE DATA ####
# Correctly code missing values
data_char[data_char == "na"] = NA

# replace decimal commmas with points
data_char <- data_char %>% mutate(across(everything(), replace_commas))

# convert gender strings to numbers
# 1 = male, 2 = female, 3 = diverse
data_char[data_char == "m"] = "1"
data_char[data_char == "w"] = "2"
data_char[data_char == "d"] = "3"
# there are two occurences of the code "0" which is invalid, they are re-coded as NA
data_char[data_char == "0"] = NA

# Replace character entries with numbers and add column for treatment group
data = data.frame(matrix(0,nrow(data_char),ncol(data_char)+1),row.names = rownames(data_char))
colnames(data) = c("group",colnames(data_char))
for (i in 1:nrow(data_char)) {
  group = substr(rownames(data)[i],1,1)
  data[i,1] = group
  for (j in 1:ncol(data_char)) {
    if (!is.na(data_char[i,j])) {
      data[i,j+1] = as.double(data_char[i,j])
    } else {
      data[i,j+1] = NA
    }
  }
}
 
# gender statistics
number_male = sum(data$gender == 1, na.rm = TRUE)
number_female = sum(data$gender == 2, na.rm = TRUE)
number_diverse = sum(data$gender == 3, na.rm = TRUE)

# average age
# Remove age outliers
for (i in 1:nrow(data)) {
  if (!is.na(data[i,"age"])) {
    if (data[i,"age"] > 20 || data[i,"age"] < 13) {
      print("Age Outlier")
      print(data[i,"age"])
      data[i,"age"] = NA
    }
  }
}
avg_age = mean(data$age, na.rm = TRUE)
sd_age = sd(data$age, na.rm = TRUE)
min_age = min(data$age, na.rm = TRUE)
max_age = max(data$age, na.rm = TRUE)

# grade statistics
number_NA_grade = sum(is.na(data$mark))
median_grade = median(data$mark, na.rm = TRUE)
mad_grade = mad(data$mark, na.rm = TRUE)
min_grade = min(data$mark, na.rm = TRUE)
max_grade = max(data$mark, na.rm = TRUE)

# participants in each group / testlet
q_groups = c(0,0,0,0)
r_groups = c(0,0,0,0)
s_groups = c(0,0,0,0)
t_groups = c(0,0,0,0)

for (i in 1:nrow(data)) {
  testlet = substr(rownames(data)[i],5,5)
  if (testlet == "Q") {
    q_groups[as.numeric(data[i,1])] = q_groups[as.numeric(data[i,1])] + 1
  } else if (testlet == "R") {
    r_groups[as.numeric(data[i,1])] = r_groups[as.numeric(data[i,1])] + 1
  } else if (testlet == "S") {
    s_groups[as.numeric(data[i,1])] = s_groups[as.numeric(data[i,1])] + 1
  } else if (testlet == "T") {
    t_groups[as.numeric(data[i,1])] = t_groups[as.numeric(data[i,1])] + 1
  }
}

# gender distribution in groups
gender_groups = data.frame(matrix(0,4,4),row.names = c("m","w","d","missing"))
colnames(gender_groups) = c("1","2","3","4")
counter = 0
for (i in 1:nrow(data)) {
  if (data[i,"group"]==4) {
    print(data[i,"gender"])
  }
  if (is.na(data[i,"gender"])) {
    gender_groups["missing",data[i,"group"]] = gender_groups["missing",data[i,"group"]] + 1
  } else {
    gender_groups[data[i,"gender"],data[i,"group"]] = gender_groups[data[i,"gender"],data[i,"group"]] + 1
  }
}

# Chi-squared test to compare distribution of gender within groups
gender_table = table(data$group, data$gender)

# Perform the Chi-square test
chi_square_gender = chisq.test(gender_table)

# Print the result
print("Chi-square Test Result:")
print(chi_square_gender)

# age distribution in groups
age_stat = data %>%
  group_by(group) %>%
  dplyr::summarize(Mean = mean(age, na.rm=TRUE), SD = sd(age, na.rm=TRUE))

# Compare age distribution with one-way anova
age_table = data[,c("group","age")]
age_table = na.omit(age_table)
res.aov <- age_table %>% anova_test(age ~ group)
res.aov

# grade distribution
grade_stat = data %>%
  group_by(group) %>%
  dplyr::summarize(Median = median(mark, na.rm=TRUE), Mad = mad(mark, na.rm=TRUE))

# compare grade distribution with Kruskal Wallis Test
grade_table = data[,c("group","mark")]
grade_table = na.omit(grade_table)

# visualise grade distribution with a boxplot
# lwd = 1.1 => Liniendicke Umrandung Boxen und Medianstrich
# theme_classic() => sorgt u.a. dafür dass Gitternetz verschwindet
# legend.position = "none" => blendet Legende aus
grade_bp = ggplot(grade_table, aes(x = group, y = mark, fill = group)) +
  geom_boxplot(lwd=1.1) +
  theme_classic() + 
  labs(x = "Gruppe",
       y = "Mathematiknote") +
  theme(axis.text=element_text(size=18,face="bold"), 
        axis.title=element_text(size=20), 
        legend.position = "none") +
  scall_fill_gradient(low="red",high="yellow")
  
  #scale_fill_brewer(palette="Set2") 

kruskal.test(mark ~ group, data = grade_table)
