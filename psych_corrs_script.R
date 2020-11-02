# Description ####
# Carlos Rodriguez, Ph.D., Mind Research Network
# This script will load .csv files that contain the graph theory metrics
# computed at various threshold levels 
# load measures of intelligence
# Calculate the correlation between graph theory metrics and bx and
# generate plots with asterisks denoting which correlations are significant
#
# Script modified to look at thresholds 1 - 5 (10/27/2020)
# Script modified to output r and p values to table

# Load Packages ####
library(tidyverse) #for filtering and selecting
library(rstatix) #for performing statistics
library(ggpubr) #for creating graphs
library(openxlsx) #for outputting tables, avoid java errors
library(pracma) #for comparing strings
library(Hmisc) # for calculating p values of correlation coefficient
library(tictoc) #for timing the code

tic("total")
# Load Data ####
## List and load graph theory metric data 
setwd("C:/Users/crodriguez/Desktop/fasd_fnc/analyses/psych_correlations") #set the working directory where files are located
files <- list.files(path = ".", pattern = "*k_data.csv", full.names=TRUE, recursive=FALSE) #list the files that end in the suffix of interes

# Set the number of levels
klev <- 5

#List and load the iq data
npsy <- list.files(path = ".", pattern = "*psych.csv", full.names = TRUE, recursive = FALSE) #List the bx data of interest
bx <- read.csv(npsy[1]) #load the iq data wich also includes Age, sex, group, and IQ data 

# Loop through each graph metric, transitivity, path length, modularity, etc ####
for (x in files){
  t <- read_csv(x)
  
  #Tidy the data a bit by changing the column name of k = .1
  names(t)[12] <- "1.0" #changes the names values
  
  #Initialize the metric name to be used in file output
  metric_name <- substr(x, 3, nchar(x) - 11) #changed bc of the files used
  
  #Initialize the metric name to be used in plots/figures
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
  
  # Correlates bx with graph measure at each level of k
  # Initialize empty data frames the will be appended to in a loop
  cor_data <- as.data.frame(c())
  p_vals_c <- as.data.frame(c())
  p_vals_f <- as.data.frame(c())
  
  # Loop through each column of k data
  for (i in c(3:(klev+2))){ #data for each k are in columns 3 through 12
    #print(colnames(t[i])) #prints out the colnames to check which columns is being loaded in the loop
    
    # Create the Controls Data ####
      a <- t %>% filter(., group == "'CNTRL'") %>% select(., group, names(t[i]))#filters the controls for k(i) graph metrics
      b <- bx %>% filter(., Group == "'CNTRL'") #filters the controls for neuropsych measures
      r <- c(cor(a[2], b$IQ, use = "complete.obs"), cor(a[2], b$Vocab, use = "complete.obs"), cor(a[2], b$Matrix, use = "complete.obs"))
      p_vals_c <- rbind(p_vals_c, c(rcorr(as.matrix(cbind(a[2], b$IQ)), type = "pearson")$P[2],
        rcorr(as.matrix(cbind(a[2], b$Vocab)), type = "pearson")$P[2],
        rcorr(as.matrix(cbind(a[2], b$Matrix)), type = "pearson")$P[2]))
      names(p_vals_c) <- c("iq", "vocab", "matrix") #renames the p_values matrix
      k <- names(t)[i] # creates the values to name the r value according to the level of k
      m <- metric_name # adds the metric name to the row
      g <-  "CNTRL"
      r_values <- cbind(k, t(r), m, g)
      r_values <- as.data.frame(r_values)
      names(r_values)[2] <- "iq"
      names(r_values)[3] <- "vocab"
      names(r_values)[4] <- "matrix"
      names(r_values)[6] <- "group"
      cols <- c(1,5,6) #set the columns to change as factor
      r_values[cols] <- lapply(r_values[cols], factor) #change to factor   
      cor_data <- rbind(cor_data, r_values)
      
    # Create the FAS Data ####
      a <- t %>% filter(., group == "'FAS'") %>% select(., group, names(t[i]))#filters the controls for k(i) graph metrics
      b <- bx %>% filter(., Group == "'FAS'") #filters the controls for neuropsych measures
      r <- c(cor(a[2], b$IQ, use = "complete.obs"), cor(a[2], b$Vocab, use = "complete.obs"), cor(a[2], b$Matrix, use = "complete.obs"))
      
      p_vals_f <- rbind(p_vals_f, c(rcorr(as.matrix(cbind(a[2], b$IQ)), type = "pearson")$P[2],
                                  rcorr(as.matrix(cbind(a[2], b$Vocab)), type = "pearson")$P[2],
                                  rcorr(as.matrix(cbind(a[2], b$Matrix)), type = "pearson")$P[2]))
      names(p_vals_f) <- c("iq", "vocab", "matrix")
      k <- names(t)[i] # creates the values to name the r value according to the level of k
      m <- metric_name # adds the metric name to the row
      g <-  "FAS"
      r_values <- cbind(k, t(r), m, g)
      r_values <- as.data.frame(r_values)
      names(r_values)[2] <- "iq"
      names(r_values)[3] <- "vocab"
      names(r_values)[4] <- "matrix"
      names(r_values)[6] <- "group"
      cols <- c(1,5,6) #set the columns to change as factor
      r_values[cols] <- lapply(r_values[cols], factor) #change to factor   
      cor_data <- rbind(cor_data, r_values)
  }
  
  # Print if any vals less than 0.05 to command line, uncomment to take a peek
  # print(metric_name)
  # print(p_vals_c < 0.05)
  # print(p_vals_f < 0.05)
 
  # Convert the wide format correlations data to long format
  cor_data <- gather(cor_data, test, r, iq:matrix, factor_key=TRUE) #convert to long format
  cor_data$r <- as.numeric(cor_data$r) # change cor_data to numeric
  
  # Line Plot CNTR and FAS faceted by subtest
  lpt <-  ggline(cor_data, x = "k", y = "r", color = "group", palette = "jco", xlab = "Threshold",
                 legend.title = paste('WASI II-', output_name, " Correlations", sep = ""), facet.by = "test")
  
  # Copy the line plot to add asterisks in a loop
  p <- lpt
  
  # Add asterisks to CNTRLs ####
  index <-  c()
  for (i in c(1:dim(p_vals_c)[2])){ # for each column
   for (j in c(1:dim(p_vals_c)[1])){ #for each row
     if (p_vals_c[j,i] < 0.05){
        index <- rbind(index, print(c(j,i))) #sets up the index of where p vals are less than 0.05
     }
   }
  }
  if (!is.null(index)){ #if the dimensions are greater than 0 then a significant difference exists
    for (i in c(1:dim(index)[1])){ # i is the level of k
        f <- filter(cor_data, k == index[i,1]/10, group == "CNTRL", test == levels(cor_data$test)[index[i,2]])
        ann_text <- data.frame(k = as.numeric(f[1,1]), r = (f[1,5])+ .05, lab = "*", test = factor(f[1,4], levels = c("iq", "vocab", "matrix")))
        p <- p + geom_text(data = ann_text,label="*")
    }
  }
  
  # Add asterisks to FAS ####
  index <-  c()
  for (i in c(1:dim(p_vals_f)[2])){ # for each column
    for (j in c(1:dim(p_vals_c)[1])){ #for each row
      if (p_vals_f[j,i] < 0.05){
        index <- rbind(index, print(c(j,i))) #sets up the index of where p vals are less than 0.05
      }
    } 
  }
  if (!is.null(index)){ #if the dimensions are greater than 0 then a significant difference exists
    for (i in c(1:dim(index)[1])){ # i is the level of k
      f <- filter(cor_data, k == index[i,1]/10, group == "FAS", test == levels(cor_data$test)[index[i,2]])
      ann_text <- data.frame(k = as.numeric(f[1,1]), r = (f[1,5])+ .05, lab = "*", test = factor(f[1,4], levels = c("iq", "vocab", "matrix")))
      p <- p + geom_text(data = ann_text,label= "*")
    }
  }
  
  
  
  
  # Change position of x ticks
  #p + scale_x_discrete(guide = guide_axis(n.dodge = 2)) #to stratify labels
  if (klev == 10 ){
    #p <-  p + scale_x_discrete(breaks = c(.1, .3, .5, .7, .9)) #to stratify labels
    # Change the background fill of the facet strip 
    p + theme(strip.background = element_rect(fill="white"))
    }
  
  # Change the facet grid labels, creates Variable and value paired argument function
  facet_labels <-  list('iq' = "IQ", 'vocab' = "Vocab", 'matrix' = "Matrix")
  facet_labeller <- function(variable,value){
    + return(facet_labels[value])}
  p <- p + facet_grid(. ~ test, labeller=facet_labeller)
  
  #p + facet_grid(test ~., labeller=facet_labeller) # Change the facet labels and flip 90 degrees
  
  
  # Save the plot to a  .tiff file
  tiff(file = paste("fig_linplot_corbx_", metric_name, ".tiff", sep = ""), units = 'in', width = 10, height = 4.25, res = 300)
  print(p)
  dev.off()
  
  # Save data to an excel files
  p_vals_c$group="CNTRL" #Add a factor to organize data
  p_vals_f$group="FAS"
  #gather(p_vals_c, key = "group")
  #gather(p_vals_f, key = "group")
  cor_data <- cbind(cor_data[order(cor_data$group),], rbind(gather(p_vals_c, key = "group"), gather(p_vals_f, key = "group"))[2])
  write.xlsx(cor_data, file = paste("cor_results_", metric_name, ".xlsx", sep = ""))
  
}
toc()
