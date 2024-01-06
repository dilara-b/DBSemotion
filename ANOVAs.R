rm(list=setdiff(ls(), "output"))

### Functions --------------------------------------------------------------------------------

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

### Setup ------------------------------------------------------------------------------------

#install.packages("dplyr") 
#install.packages("tidyr")
#install.packages("rstatix")
#install.packages("ggpubr")

library(dplyr) 
library(tidyr)
library(rstatix)
library(ggpubr)
library(readr)
library(openxlsx)

### Data conversion --------------------------------------------------------------------------

#transpose output vector from other script
output_t = as.data.frame(t(output))

#extract 'group' variable from rownames
output_t$subject = rownames(output_t)
output_t$group = sub('(^.*?)-.*', '\\1', output_t$subject)

#convert change scores to long format
changescores_long = gather(output_t, emotion, acc, '1 Imp. X error rate':'recog_surprise_acc')

#combinations for primary outcomes, comparing emotional vs. non-emotional tasks
ANOVA_combinations = rbind(c("impulsivity_emo_errorrate","impulsivity_nonemo_errorrate"),
                           c("impulsivity_emo_go_errorrate","impulsivity_nonemo_go_errorrate"),
                           c("impulsivity_emo_nogo_errorrate","impulsivity_nonemo_nogo_errorrate"),
                           c("impulsivity_emo_time","impulsivity_nonemo_time"),
                           c("5.3 FitC Emo_Odd","6.1 FitC NonEmo_Odd"),
                           c("5.4 FitC Emo_Equal", "6.2 FitC NonEmo_Equal"),
                           c("5.4 FitCEmo_time", "6.3 FitC NonEmo_time"),
                           c("5.8 FitC Emo_Odd_time", "6.4 FitC NonEmo_Odd_time"),
                           c("5.9 FitC Emo_Equal_time", "6.5 FitC NonEmo_Equal_time"),
                           c("recog_emo_acc", "8 N.-EmoRec")) #recog_emo_acc including accuracy for test 7 and 9 in total; 8 N.-EmoRec including accuracy for non-emotional gender recognition task
    
for (i in 1:nrow(ANOVA_combinations)){
  
  changescores_subset = changescores_long[(changescores_long$emotion == ANOVA_combinations[i,1]|changescores_long$emotion == ANOVA_combinations[i,2]),]
  
  print(ANOVA_combinations[i,])
  
  print(shapiro_test(group_by(changescores_subset,emotion,group), acc))

  print(anova_test(changescores_subset, dv = acc, wid = subject, between = c(group, emotion)))
  
}
