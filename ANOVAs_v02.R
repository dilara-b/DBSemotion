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

ANOVA_combinations = rbind(c("1 Imp. X error rate","2 Imp. Happy error rate"),
                           c("3 Imp. K error rate","2 Imp. Happy error rate"),
                           c("1 Imp. X error rate","4 Imp. Anger error rate"),
                           c("3 Imp. K error rate","4 Imp. Anger error rate"),
                           c("impulsivity_emo_errorrate", "impulsivity_nonemo_errorrate"))

for (i in 1:nrow(ANOVA_combinations)){
  
  changescores_subset = changescores_long[(changescores_long$emotion == ANOVA_combinations[i,1]|changescores_long$emotion == ANOVA_combinations[i,2]),]
  
  print(ANOVA_combinations[i,])
  
  print(shapiro_test(group_by(changescores_subset,emotion,group), acc))

  print(anova_test(changescores_subset, dv = acc, wid = subject, between = c(group, emotion)))
  
}
