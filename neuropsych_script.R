library(tidyverse)
library(rstatix)
library(ggpubr)
library(openxlsx)
library(tictoc)

tic("total")
setwd("C:/Users/crodriguez/Desktop/fasd_fnc/analyses/neuropsych") #set the working directory where files are located
neuropsych <- read.csv("C:/Users/crodriguez/Desktop/fasd_fnc/analyses/neuropsych/fasd_fnc_neurpsych.csv")
neuropsych$Group[43] <- "'ARND'"#change pfas to arnd
neuropsych$Group <- as.factor(neuropsych$Group) #convert to factor
meas <- c(5:7) # The columns that need to be analyzed

for (x in meas){
data <- neuropsych[,c(4,x)]
label <- names(data)[2]
names(data)[2] <- "score"

# Check for outliers ####
outliers <- data %>% group_by(Group) %>% identify_outliers(score) #check for extreme outliers

# Check normality ####
model <- lm(score ~ Group, data = data)
#ggqqplot(residuals(model))
norm_n <- shapiro_test(residuals(model)) # p > 0.05 is good

# Check normality by group ####
norm_g <- data %>% group_by(Group) %>% shapiro_test(score)
#ggqqplot(data, "score", facet.by = "Group")

# Check homogeneity of variance ####
#plot(model, 1) #check visually
hom_var <- data %>% levene_test(score ~ Group) # p > 0.05 is good

#ANOVA ####
res.aov <- data %>% anova_test(score ~ Group, effect.size = "pes", type = 3) #will exclude the rows with NAs
res.aov

fx <- data %>% select(Group, score) %>% cohens_d(score ~ Group)
fx
sum_stats <- data %>% group_by(Group) %>% get_summary_stats(score) %>% select(Group, variable, n, min, max, median, mean, sd, se)
sum_stats
# Post-Hoc Tests ####
  #pwc <- data %>% tukey_hsd(IQ ~ Group)
  #pwc

  pwc <- data %>% pairwise_t_test(score ~ Group, p.adjust.method = "bonferroni") #p values aren't adjusted bc within k only two groups
  pwc
  
# Visualization ####
pwc_plot <- pwc
pwc_plot <- pwc_plot %>% add_xy_position(x = "Group")
p <- ggbarplot(data, x = "Group", y = "score", ylab = label, color = "Group", add = "mean_se", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) +
  stat_pvalue_manual(pwc_plot, hide.ns = FALSE) + rremove("x.title")#+ rremove("legend") #
p


  if (label == "Matrix"){
    p <- ggbarplot(data, x = "Group", y = "score", ylab = label, color = "Group", add = "mean_se", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) +
      stat_pvalue_manual(pwc_plot, hide.ns = FALSE, y.position = c(65, 68, 73)) +  rremove("x.title")
    p <- ggpar(p, legend.title = "Mean Matrix Reasoning")
    } else if (label == "IQ") {
      p <- ggbarplot(data, x = "Group", y = "score", ylab = label, color = "Group", add = "mean_se", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) +
        stat_pvalue_manual(pwc_plot, hide.ns = FALSE, y.position = c(114, 119, 127)) + rremove("x.title")
      p <- ggpar(p, legend.title = "Mean IQ")
    } else {
      p <- ggbarplot(data, x = "Group", y = "score", ylab = label, color = "Group", add = "mean_se", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) +
        stat_pvalue_manual(pwc_plot, hide.ns = FALSE) + rremove("x.title")
      p <- ggpar(p, legend.title = "Mean Vocabulary")
  }

  # adjust comparison lines for the matrix data, order of groups can be changed, but then the significants lines will be out of whack. 
  # if (label == "Matrix"){
  #   p <- ggboxplot(data, x = "Group", y = "score", ylab = label, color = "Group", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) + 
  #     stat_pvalue_manual(pwc_plot, hide.ns = FALSE, y.position = c(65, 68, 72)) + rremove("legend") + rremove("x.title") #+ #, to modify the position of the ns lines
  #     #labs(subtitle = get_test_label(res.aov, detailed = TRUE),
  #       #caption = get_pwc_label(pwc_plot))
  #   } else {
  #     p <- ggboxplot(data, x = "Group", y = "score", ylab = label, color = "Group", palette = c("#868686FF","#0073C2FF", "#EFC000FF")) + 
  #   stat_pvalue_manual(pwc_plot, hide.ns = FALSE) + rremove("legend") + rremove("x.title") #+
  #   #labs(subtitle = get_test_label(res.aov, detailed = TRUE),
  #     #caption = get_pwc_label(pwc_plot))
  # }

# Save Figure ####
tiff(file = paste("fig_plot_", label, ".tiff", sep = ""), units = 'in', width = 5, height = 5, res = 300)
print(p)
dev.off()

# Output Data Tables  
# Save Tables ####
list_of_datasets <- list("outliers" = outliers, "normality_n" = norm_n, "normality_g" = norm_g, "hom_var" = hom_var, "anova1" = res.aov, 'multcomp' = pwc)
write.xlsx(list_of_datasets, file = paste("results_", label, ".xlsx", sep = ""))

} 
toc()