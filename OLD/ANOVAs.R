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
ANOVA_combinations = rbind(c("impulsivity_emo_errorrate","impulsivity_nonemo_errorrate"), #error rates for emotional (test 2+4) vs. non-emotional (test 1+3) impulsivity tasks in total
                           c("impulsivity_emo_go_errorrate","impulsivity_nonemo_go_errorrate"), #impulsivity error rates including only "go"-stimuli for emotional vs. non-emotional tasks in total
                           c("impulsivity_emo_nogo_errorrate","impulsivity_nonemo_nogo_errorrate"), #impulsivity error rates including only "no-go"-stimuli for emotional vs. non-emotioanl tasks in total
                           c("impulsivity_emo_time","impulsivity_nonemo_time"), #reaction time for "go"-stimuli in emotional vs. non-emotional impulsivity tasks in total
                           c("5.3 FitC Emo_Odd","6.1 FitC NonEmo_Odd"), #accuracy for emotional vs. non-emotional face-in-the-crowd tasks, when a distractor is present among targets
                           c("5.4 FitC Emo_Equal", "6.2 FitC NonEmo_Equal"), #accuracy for emotional vs. non-emotional face-in-the-crowd tasks for trials without distractors
                           c("5.4 FitCEmo_time", "6.3 FitC NonEmo_time"), #reaction time for emotional vs. non-emotional face-in-the-crowd tasks
                           c("5.8 FitC Emo_Odd_time", "6.4 FitC NonEmo_Odd_time"), #reaction time for emotional vs. non-emotional face in the crowd tasks, when a distractor is present among targets
                           c("5.9 FitC Emo_Equal_time", "6.5 FitC NonEmo_Equal_time"), #reaction time for emotional vs. non-emotional face in the crowd tasks for trials without distractors
                           c("recog_emo_acc", "8 N.-EmoRec")) #recog_emo_acc including accuracy for FER in total (test 7+9) vs. 8 N.-EmoRec including accuracy for non-emotional gender recognition task
    
for (i in 1:nrow(ANOVA_combinations)){
  
  changescores_subset = changescores_long[(changescores_long$emotion == ANOVA_combinations[i,1]|changescores_long$emotion == ANOVA_combinations[i,2]),]
  
  print(ANOVA_combinations[i,])
  
  print(shapiro_test(group_by(changescores_subset,emotion,group), acc))

  print(anova_test(changescores_subset, dv = acc, wid = subject, between = c(group, emotion)))
  
}
