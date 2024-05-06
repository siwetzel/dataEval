library(readr)
library(TAM)
library(tidyverse)
library(report)
library(rstatix)
library(ggpubr)

# Set working directory to folder where all R code related to the study is stored
setwd("C:/Users/Sina-/Dropbox/Lehrstuhl/Diss/Auswertung/R")

# load helper function for Q matrix
source("helper_functions/create_Q_matrix.R")

diffics = as.matrix(read.table("output_data/pc_item_diffics.csv", sep=",", header=FALSE))

# get data
data = read_csv2("output_data/data_pc_final_noNA.csv", show_col_types = FALSE)
data = data.frame(data, row.names = 1)


# create a dataframe with 48 columns and as many rows as the data set has
# first 24 columns are values from the pretest
# second 24 columns are values from the posttest
data_prepost = data.frame(matrix(0,nrow(data),48),row.names = rownames(data))

names = c(
  "a1_1",
  "a1_2",
  "a1_3",
  "a1_4",
  "a1_5",
  "a1_6",
  "a2_1",
  "a2_2",
  "a2_3",
  "a2_4",
  "a2_5",
  "a6_1",
  "b1_1",
  "b1_2",
  "b1_3",
  "b1_4",
  "b1_5",
  "b1_6",
  "b2_1",
  "b2_2",
  "b2_3",
  "b2_4",
  "b2_5",
  "b2_6"
)
names_pre = names
names_post = names
for (i in 1:length(names)) {
  names_pre[i] = paste("pre", names[i], sep="_")
  names_post[i] = paste("post", names[i], sep="_")
}

colnames(data_prepost) = append(names_pre, names_post)

# write pre- and postest scores in the appropriate columns
#TODO: crosschecken ob alles richtig zugeordnet wird
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    if (substr(rownames(data)[i],5,5) == "Q") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "R") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "S") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      }
    } else if (substr(rownames(data)[i],5,5) == "T") {
      if (substr(colnames(data)[j],1,2)=="a1") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      } else if (substr(colnames(data)[j],1,2)=="a2") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b1") {
        data_prepost[i,j+24] = data[i,j]
        data_prepost[i,j] = NA
      } else if (substr(colnames(data)[j],1,2)=="b2") {
        data_prepost[i,j] = data[i,j]
        data_prepost[i,j+24] = NA
      }
    }
  }
}

###### 4-dimensional Rasch-Analysis ######

# first dimension: pretest procedural
# second dimension: pretest conceptual
# third dimension: posttest procedural
# fourth dimension: posttest conceptual

# create Q matrix
Q = create_Q_4dim(ncol(data))


# the difficulty vector of the stored difficulties only has 24 items however we need 48 entries
# as pretest and posttest are treated separately
# model expects array with 2 columns, first indicates index of item
diffics_model = cbind(1:48, append(diffics,diffics))

# analysis of items
mod <- TAM::tam.mml( resp=data_prepost, Q=Q, irtmodel = "PCM", xsi.fixed = diffics_model, pid=rownames(data_prepost), control=list(snodes=2000) )
mod <- TAM::tam.mml( resp=data_prepost, Q=Q, xsi.fixed = diffics_model, pid=rownames(data_prepost), control=list(snodes=2000) )

# Person abilities (TODO: PLausible Values?)
abil = tam.wle(mod)

# create a results table
results = data.frame(cbind(abil$theta.Dim01,abil$theta.Dim02,abil$theta.Dim03,abil$theta.Dim04,matrix(0,nrow(data),1)),row.names = rownames(data))
colnames(results) = c("pre_pro","pre_con","post_pro","post_con","group")

# indicate to which groups the participants belonged
for (i in 1:nrow(results)) {
  results[i,5] = substr(rownames(results)[i],1,1)
}

# make a scatter plot of the results for the conceoptual items where the different groups have a different color
ggplot(results, aes(x = pre_con, y = post_con, color = factor(group))) +
  geom_point() +
  labs(title = "Pretest vs. Posttest", x = "Pretest", y = "Posttest") +
  scale_color_manual(values = c("blue", "red", "yellow", "green")) 

# summarise means of different groups for all four dimensions
m_pre_pro <- results %>%
  group_by(group) %>%
  summarise(m = mean(pre_pro)) %>%
  arrange(m)

m_pre_con <- results %>%
  group_by(group) %>%
  summarise(m = mean(pre_con)) %>%
  arrange(m)

m_post_pro <- results %>%
  group_by(group) %>%
  summarise(m = mean(post_pro)) %>%
  arrange(m)

m_post_con <- results %>%
  group_by(group) %>%
  summarise(m = mean(post_con)) %>%
  arrange(m)


# create diff score results table
diff_pro = results$post_pro - results$pre_pro
diff_con = results$post_con - results$pre_con
results_diff = data.frame(cbind(diff_pro,diff_con,results$group),row.names = rownames(results))
colnames(results_diff) = c("diff_pro","diff_con","group")

# Summarise diff scores
m_diff_pro <- results_diff %>%
  group_by(group) %>%
  summarise(m = mean(diff_pro)) %>%
  arrange(m)

m_diff_con <- results_diff %>%
  group_by(group) %>%
  summarise(m = mean(diff_con)) %>%
  arrange(m)


##### Mixed ANOVA #####
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/

# split procedural and conceptual results into two tables where the particpants codes are also a columns
# procedural
results_pro = select(results, pre_pro, post_pro, group)
results_pro = cbind(rownames(results_pro), results_pro)
rownames(results_pro) = NULL
colnames(results_pro) = c("code","pre_pro","post_pro","group")
# conceptual
results_con = select(results, pre_con, post_con, group)
results_con = cbind(rownames(results_con), results_con)
rownames(results_con) = NULL
colnames(results_con) = c("code","pre_con","post_con","group")

# convert columns of pretest and posttest into single columns (with two rows per subject)
# procedural
results_pro_sc = results_pro %>%
  gather(key = "test", value = "score", pre_pro, post_pro) %>%
  convert_as_factor(code, test)
# conceptual
results_con_sc = results_con %>%
  gather(key = "test", value = "score", pre_con, post_con) %>%
  convert_as_factor(code, test)

# replace 0 and 1 integer values with strings (for boxplot to work)
# ##### TODO: was macht dasß???? habe dochresults_pro_sc[results_pro_sc == 2] = "2"
results_pro_sc[results_pro_sc == 1] = "1"
results_pro_sc[results_pro_sc == 0] = "0"
results_con_sc[results_con_sc == 1] = "1"
results_con_sc[results_con_sc == 0] = "0"

##### Divide into lower and upper performing students #########
# hypothesis: lower performing students profit more from interactive tasks
# Get the order of scores in the pretest (for conceptual items)
order_indices_con <- order(results_con$pre_con)

# Use the order to rearrange the entire dataframe
results_con_ordered <- results_con[order_indices_con, ]
# lower 150 students in pretest
results_con_low = results_con_ordered[1:150,]
###############################################################

#### BOXPLOTS #####
# CONCEPTUAL ITEMS
# make copy of data for boxplot to rename data etc.
results_con_copy = results_con
colnames(results_con_copy) = c("code","Pretest","Posttest","Gruppe")
results_con_sc_copy = results_con_copy %>%
  gather(key = "test", value = "score", Pretest, Posttest) %>%
  convert_as_factor(code, test)
#results_con_sc_copy[results_con_sc == 1] = "VO"
#results_con_sc_copy[results_con_sc == 0] = "KO"

# prescribe order in boxplot (first pretest, than posttest)
results_con_sc_copy$test = factor(results_con_sc_copy$test , levels=c("Pretest", "Posttest"))

boxplot_con <- ggboxplot(
  results_con_sc_copy, x = "test", y = "score",
  color="Gruppe",palette = "jco", add="mean",
  xlab = "Konzeptuelle Items", ylab="Score in Logit"
)

### for lower performing students
results_con_low_copy = results_con_low
colnames(results_con_low_copy) = c("code","Pretest","Posttest","Gruppe")
results_con_low_sc_copy = results_con_low_copy %>%
  gather(key = "test", value = "score", Pretest, Posttest) %>%
  convert_as_factor(code, test)

# prescribe order in boxplot (first pretest, than posttest)
results_con_low_sc_copy$test = factor(results_con_low_sc_copy$test , levels=c("Pretest", "Posttest"))

boxplot_con_low <- ggboxplot(
  results_con_low_sc_copy, x = "test", y = "score",
  color="Gruppe",palette = "jco", add="mean",
  xlab = "Konzeptuelle Items", ylab="Score in Logit"
)

# PROCEDURAL ITEMs
# make copy of data for boxplot to rename data etc.
results_pro_copy = results_pro
colnames(results_pro_copy) = c("code","Pretest","Posttest","Gruppe")
results_pro_sc_copy = results_pro_copy %>%
  gather(key = "test", value = "score", Pretest, Posttest) %>%
  convert_as_factor(code, test)
#results_pro_sc_copy[results_pro_sc == 1] = "VO"
#results_pro_sc_copy[results_pro_sc == 0] = "KO"

# prescribe order in boxplot (first pretest, than posttest)
results_pro_sc_copy$test = factor(results_pro_sc_copy$test , levels=c("Pretest", "Posttest"))

boxplot_pro <- ggboxplot(
  results_pro_sc_copy, x = "test", y = "score",
  color="Gruppe",palette = "jco", add="mean",
  xlab = "Prozedurale Items", ylab="Score in Logit"
)




##### CTT ANALYSIS #####
# TODO: Wie funktioniert das mit den WLEs da ich vier Dimensionen habe?
# TODO: Im offiziellen TAM Hanndbook S. 88 steht man kann auch eine group variable mitgeben. Kann ich also ein kombiniertes theta bauen und dann eine Gruppenvariable mitgeben und dann nur eine Berechnung machen?
# TODO: vll das machen und mit dem anderen ansatz (dem manuellen) vergleichen, ob das gleiche herauskommt
ctt1 = tam.ctt(data_prepost, abil$theta.Dim01)
ctt2 = tam.ctt(data_prepost, abil$theta.Dim02)
ctt3 = tam.ctt(data_prepost, abil$theta.Dim03)
ctt4 = tam.ctt(data_prepost, abil$theta.Dim04)

# determine pointbiserial corelation (trennschärfe) with the right dimension
# pretest procedural
pbc1 = c()
# pretest conceptual
pbc2 = c()
# posttest procedural
pbc3 = c()
# posttest conceptual
pbc4 = c()

# get the corresponding values from the ctt dataframes
items_pro = c("a1_1","a1_3","a1_5","a2_1","a2_3","a2_5","b1_1","b1_3","b1_5","b2_1","b2_3","b2_5")
for (i in 1:nrow(ctt1)) {
  if (substr(ctt1$item[i],1,3) == "pre") {
    item = substr(ctt1$item[i],5,8)
    if (item %in% items_pro) {
      pbc1 = append(pbc1,ctt1$rpb.WLE[i])
    } else { # conceptual item
      pbc2 = append(pbc2,ctt2$rpb.WLE[i])
    }
  } else { # posttest items
    item = substr(ctt1$item[i],6,9)
    print(substr(ctt1$item[i],1,3))
    print(substr(ctt1$item[i],6,9))
    if (item %in% items_pro) {
      pbc3 = append(pbc3,ctt3$rpb.WLE[i])
    } else { # conceptual item
      pbc4 = append(pbc4,ctt4$rpb.WLE[i])
    }
  }
}


# alt von rasch
#for (i in 1:96) {
 #if ((i-1) %% 4 == 1 && i <= 48) {#    pbc1 = append(pbc1,ctt1$rpb.WLE[i])
  #} else if ((i-1) %% 4 == 3 && i <= 48) {
   # pbc2 = append(pbc2,ctt2$rpb.WLE[i])
  #} else if ((i-1) %% 4 == 1 && i > 48) {
   # pbc3 = append(pbc3,ctt3$rpb.WLE[i])
  #} else if ((i-1) %% 4 == 3 && i > 48) {
   # pbc4 = append(pbc4,ctt4$rpb.WLE[i])
  #} 
#}


# Check assumptions for ANOVA
# Outliers
results_pro_sc %>%
  group_by(test, group) %>%
  identify_outliers(score)

results_con_sc %>%
  group_by(test, group) %>%
  identify_outliers(score)

# Normality
# Kolmogorov-Smirnov test
ks.test(results_pro$pre_pro,"pnorm",mean=mean(results_pro$pre_pro),sd=sd(results_pro$pre_pro))
ks.test(results_pro$post_pro,"pnorm",mean=mean(results_pro$post_pro),sd=sd(results_pro$post_pro))
ks.test(results_con$pre_con,"pnorm",mean=mean(results_con$pre_con),sd=sd(results_con$pre_con))
ks.test(results_con$post_con,"pnorm",mean=mean(results_con$post_con),sd=sd(results_con$post_con))


# Homgenity of variance
results_pro_sc %>%
  group_by(test) %>%
  levene_test(score ~ group)

results_con_sc %>%
  group_by(test) %>%
  levene_test(score ~ group)

# Homogenity of covariance
box_m(results_pro_sc[, "score", drop = FALSE], results_pro_sc$understanding)
box_m(results_con_sc[, "score", drop = FALSE], results_con_sc$understanding)
# TODO: evtl. problematisch für konzeptuelle Gruppe?


# Two-way mixed ANOVA test
# procedural
anova_pro <- anova_test(
  data = results_pro_sc, dv = score, wid = code,
  between = group, within = test
)
get_anova_table(anova_pro)
# conceptual
anova_con <- anova_test(
  data = results_con_sc, dv = score, wid = code,
  between = group, within = test
)
get_anova_table(anova_con)

# calculate Cohen's f
# https://www.youtube.com/watch?v=GKvRAlAmb9s
f_pro = sqrt(anova_pro$ges[3]/(1-anova_pro$ges[3]))
f_con = sqrt(anova_con$ges[3]/(1-anova_con$ges[3]))


########### separate by pretest scores ###########
results_con_ordered = results_con[order(results_con$pre_con),]
# top 50% pretest scorers
results_con_ordered_top = results_con_ordered[47:92,]
# bottom 50% pretest scorers
results_con_ordered_bottom = results_con_ordered[1:46,]

results_con_ordered_bottom_sc = results_con_ordered_bottom %>%
  gather(key = "test", value = "score", pre_con, post_con) %>%
  convert_as_factor(code, test)

results_con_ordered_top_sc = results_con_ordered_top %>%
  gather(key = "test", value = "score", pre_con, post_con) %>%
  convert_as_factor(code, test)

anova_con <- anova_test(
  data = results_con_ordered_bottom_sc, dv = score, wid = code,
  between = understanding, within = test
)
get_anova_table(anova_con)


# BOXPLOTS of top and bottom scorers
# replace 0 and 1 integer values with strings (for boxplot to work)
results_con_ordered_top_sc[results_con_ordered_top_sc == 1] = "1"
results_con_ordered_top_sc[results_con_ordered_top_sc == 0] = "0"
results_con_ordered_bottom_sc[results_con_ordered_bottom_sc == 1] = "1"
results_con_ordered_bottom_sc[results_con_ordered_bottom_sc == 0] = "0"

# create boxplots
ggboxplot(
  results_con_ordered_top_sc, x = "test", y = "score",
  color="understanding",palette = "jco", add="mean"
)

ggboxplot(
  results_con_ordered_bottom_sc, x = "test", y = "score",
  color="understanding",palette = "jco", add="mean"
)
