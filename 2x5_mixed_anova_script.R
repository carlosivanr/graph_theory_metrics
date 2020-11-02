# Description ####
# Carlos Rodriguez, Ph.D., Mind Research Network
# This script takes output from Brain Connectivity Toolbox
# Input data is in wide format, each row is a subject, and
# columns represent grapth theory metrics for multiple
# thresholds. Will perform statistics, generate tables with
# results output, and generate figures.
#
# Run this script from the directory containing the k_data suffix files
#
## Script modified to look at thresholds 1 - 5 (10/27/2020)


# Load Packages ####
library(tidyverse) #for filtering and selecting
library(rstatix) #for performing statistics
library(ggpubr) #for creating graphs
library(openxlsx) #for outputting tables, avoid java errors
library(pracma) #for comparing strings
library(tictoc) #for timing the code

tic("total")

## List files and start loop. List the files in the directory that end in k_data.csv ####
#setwd("C:/Users/crodriguez/Desktop/fasd_fnc/analyses/graph_threshold")
files <- list.files(path = ".", pattern = "*k_data.csv", full.names=TRUE, recursive=FALSE)

# Set Levels of K to analyze, but need to manuall change line 57 until I figure out how to set a variable for value
klev <- 5

for (x in files){
  t <- read_csv(x)
  trsh <- c("0.1", "0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0")
  names(t)[3:12] <- trsh #c("0.1", "0.2","0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0") #changes the names values
  metric_name <- substr(x, 3, nchar(x) - 11)

  # Set the ouput of files and graph labels based on the data
   if (strcmp(metric_name, "eff_glob")){
     output_name <- "Global Efficiency"
     print(output_name)
   } else if (strcmp(metric_name, "modularity")){
     output_name <- "Modularity"
     print(output_name)
   } else if (strcmp(metric_name, "transt")){
     output_name <- "Transitivity"
     print(output_name)
   } else if (strcmp(metric_name, "ave_clust_coeff")){
     output_name <- "Clustering Coefficient"
     print(output_name)
   } else if (strcmp(metric_name, "chpath")){
     output_name <- "Characteristic Path Length"
     print(output_name)
   }
  
  # Convert to long format ####
  metric <- t %>% select(., id, group, trsh[1:klev]) %>% filter(., group == "CNTRL" | group == "FAS") %>% 
    gather(key = "threshold", value = "score", '0.1', '0.2', '0.3', '0.4', '0.5') %>%
    convert_as_factor(id, threshold, group)
  
  # Check and remove outliers for each metric in the loop ####
    # Create an observation id to use for removing subjects
    obs <- as.data.frame(c(1:nrow(metric))) # Create a list of observations
    colnames(obs) = "obs" #change colname
    metric <- bind_cols(obs, metric) #bind columns
    
    
    # Determine outliers, +/- 1.5*IQR Q3/Q1 are outliers, +/- 3*IQR Q3/Q1 are extreme points
    # outliers <- metric %>%
    #   group_by(threshold, group) %>%
    #   identify_outliers(score)
  
    # Remove Outliers, need to check what happens when removing outliers or extreme points
    '%notin%' <- Negate('%in%')
    # metric <- metric %>% filter(., obs %notin% outliers$obs)
  
  
  # Normality, all p-values should be p > 0.05, should result in 2 p-values for each level of k ####
  norm <- metric %>%
    group_by(threshold, group) %>%
    shapiro_test(score)
  
  # Homogeneity of Variance, all p-values should be > 0.05 ####
  hom_var <- metric %>%
    group_by(threshold) %>%
    levene_test(score ~ group)
  
  # Homogeneity of covariance, p-value should be greater than 0.001 ####
  hom_covar <- box_m(metric[, "score", drop = FALSE], metric$group)
  
  # Sphericity & ANOVA, automatically checks sphericity using Mauchly's test ####
    # Mauchly's test for sphericity will hold if there are only 2 levels. If more than 2 levels, p<0.05 indicates violation of sphericity
    metric <- metric[,2:5] #the obs column is removed to avoid errors with anova_test
    res.aov <- anova_test(
      data = metric, dv = score, wid = id,
      between = group, within = threshold
    )
    get_anova_table(res.aov)
    
  # Post Hoc Tests ####
    # This is to test each threshold level with an ANOVA, but not really needed here because there are only two groups
    one.way <- metric %>%
      group_by(threshold) %>%
      anova_test(dv = score, wid = id, between = group) %>%
      get_anova_table() %>%
      adjust_pvalue(method = "bonferroni")
    one.way 
    
    # Pairwise comparisons between group levels
    pwc <- metric %>%
      group_by(threshold) %>%
      pairwise_t_test(score ~ group, p.adjust.method = "bonferroni") #p values aren't adjusted bc within k only two groups
    pwc
    
    pwc_plot <- pwc %>% add_xy_position(x = "threshold")
    
  # Save Tables ####
    list_of_datasets <- list("outliers" = outliers, "normality" = norm, "hom_var" = hom_var, "hom_covar" = hom_covar, "mixed_anova" = res.aov[[1]],
                             "mauchlys" = res.aov[[2]], "sphericity" = res.aov[[3]],  
                             "post_hoc" = one.way, 'pair_wise_comp' = pwc)
    write.xlsx(list_of_datasets, file = paste("results_", metric_name, ".xlsx", sep = ""))
    
  # Figures ####
    # Box Plot
    bxp <- ggboxplot(
      metric, x = "threshold", y = "score",
      color = "group", palette = "jco", xlab = "Threshold", ylab = output_name, legend.title = 'Group',
    )
    bxp <- bxp + stat_pvalue_manual(pwc_plot) + rremove("legend.title") #+ labs(caption = "*** p<0.001; ** p<0.01; * p<0.05") #get_pwc_label(pwc_plot)) #to place p.value adjustment caption
    bxp
    tiff(file = paste("fig_boxplot_", metric_name, ".tiff", sep = ""), units = 'in', width = 7, height = 4.25, res = 300)
    print(bxp)
    dev.off()
    
    # # Error Plot
    # erp <-  ggerrorplot(
    #   metric, x = "threshold", y = "score",
    #   add = "mean_se",
    #   error.plot = "pointrange",
    #   color = "group", palette = "jco", xlab = "Threshold", ylab = output_name, legend.title = 'Group'
    # )
    # erp
    # pdf(file = paste("fig_errplot_", metric_name, ".pdf", sep = ""))
    # print(erp)
    # dev.off()
    # 
    # # Line Plot
    # lpt <-  ggline(
    #   metric, x = "threshold", y = "score",
    #   #desc_stat = mean_sd,
    #   #error.plot = "pointrange
    #   add = c("mean_se"),
    #   color = "group", palette = "jco", xlab = "Threshold", ylab = output_name, legend.title = 'Group'
    # )
    # lpt
    # pdf(file = paste("fig_linplot_", metric_name, ".pdf", sep = ""))
    # print(lpt)
    # dev.off()
}
toc()
