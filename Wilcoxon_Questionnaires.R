rm(list=ls())

### Setup ------------------------------------------------------------------------------------

#install.packages("dplyr") 
#install.packages("tidyr")
#install.packages("rstatix")
#install.packages("ggpubr")
#install.packages("psych")

library(dplyr) 
library(tidyr)
library(rstatix)
library(ggpubr)
library(psych)

setwd( ) #folder including the excel with all questionnaires

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

questionnaires_results_total = read.xlsx("Questionnaire results.xlsx", rowNames = TRUE)

#transpose output vector from other script
output_t = as.data.frame(t(questionnaires_results_total))

#extract 'group' variable from rownames
output_t$subject = rownames(output_t)
output_t$group = sub('(^.*?)-.*', '\\1', output_t$subject)

dat_wide = output_t

tests = c("MoCA", "FAB", "BDI-II", "AES", "HCL-32", "PDQ-39", "QUIP-RS")


#Wilcox-Test IPS vs. DBS
for (i in tests){
  
  dat_wide = select(output_t, c(all_of(i), 'group', 'subject'))
  dat_wide = dat_wide[dat_wide$group != "Gesund",]
  dat <- gather(dat_wide, emotion, acc, i)
  
  w = wilcox.test(acc~group, data = dat, exact = FALSE, correct = FALSE, alternative = "two.sided")
  
  print(i)
  print(w)
  
}

tests_PD_Healthy = c("MoCA", "FAB", "BDI-II", "AES", "HCL-32") #PDQ-39 & QUIP-RS not conducted with healthy controls

#Wilcox-Test PD vs. Healthy Controls ("Gesund")
for (i in tests_PD_Healthy){
  
  dat_wide = select(output_t, c(all_of(i), 'group', 'subject'))
  dat_wide$group[dat_wide$group == "IPS"] = "PD"
  dat_wide$group[dat_wide$group == "DBS"] = "PD"
  
  dat <- gather(dat_wide, emotion, acc, i)
  
  w = wilcox.test(acc~group, data = dat, exact = FALSE, correct = FALSE, alternative = "two.sided")
  
  
  print(i)
  print(w)
  
}

### Change score calc-------------------------------------------------------------------------------------------------------


questionnaires_output = data.frame(matrix(NA,    # Create empty data frame
                           nrow = length(tests),
                           ncol = 0))

rownames(questionnaires_output) = tests

questionnaires_output2 = questionnaires_output

#absolute change score
for (i in 1:ncol(questionnaires_results_total)){
  
  #only continue if column is a follow up
  #For simplicity we can assume if follow up exists, baseline will be column left of it
  if(substrRight((colnames(questionnaires_results_total)[i]),2) == "FU"){
    
    questionnaires_output = cbind(questionnaires_output,questionnaires_results_total[,i] - questionnaires_results_total[,i-1])
    colnames(questionnaires_output)[ncol(questionnaires_output)] = colnames(questionnaires_results_total[i])
    
  }
}

#relative change score
for (i in 1:ncol(questionnaires_results_total)){
  
  #only continue if column is a follow up
  #For simplicity we can assume if follow up exists, baseline will be column left of it
  if(substrRight((colnames(questionnaires_results_total)[i]),2) == "FU"){
    
    questionnaires_output2 = cbind(questionnaires_output2,((questionnaires_results_total[,i] - questionnaires_results_total[,i-1])/questionnaires_results_total[,i-1]))
    colnames(questionnaires_output2)[ncol(questionnaires_output2)] = colnames(questionnaires_results_total[i])
    
  }
}


write.xlsx(questionnaires_output, "questionnaires_absolute-change_scores.xlsx", rowNames=TRUE)
write.xlsx(questionnaires_output2, "questionnaires_relative-change_scores.xlsx", rowNames=TRUE)

save(questionnaires_output, file = "questionnaires_output.Rdata")
save(questionnaires_output2, file = "questionnaires_output2.Rdata")
