#rm(list=setdiff(ls(), "output"))

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

#transpose output vector from other script
output_t = as.data.frame(t(output))

#extract 'group' variable from rownames
output_t$subject = rownames(output_t)
output_t$group = sub('(^.*?)-.*', '\\1', output_t$subject)

dat_wide = output_t

tests = c("1 Imp. X error rate", "2 Imp. Happy error rate","3 Imp. K error rate","4 Imp. Anger error rate", "impulsivity_emo_errorrate", "impulsivity_emo_time", "impulsivity_nonemo_errorrate", "impulsivity_nonemo_time", "5 FitCEmo Ges.", "5.5 FitCEmo_time","6 FitC NonEmo", "6.3 FitC NonEmo_time","8 N.-EmoRec","recog_emo_acc", "recog_afraid_acc", "recog_anger_acc", "recog_disgust_acc", "recog_happy_acc", "recog_neutral_acc", "recog_sad_acc", "recog_surprise_acc")

#Wilcox-Test IPS vs. DBS
for (i in tests){
  
  dat_wide = select(output_t, c(all_of(i), 'group', 'subject'))
  dat_wide = dat_wide[dat_wide$group != "Gesund",]
  dat <- gather(dat_wide, emotion, acc, i)
  
  w = wilcox.test(acc~group, data = dat, exact = FALSE, correct = FALSE, alternative = "two.sided")
  
  print(i)
  print(w)
  
}

#Wilcox-Test PD vs. Healthy Controls
for (i in tests){
  
  dat_wide = select(output_t, c(all_of(i), 'group', 'subject'))
  dat_wide$group[dat_wide$group == "IPS"] = "PD"
  dat_wide$group[dat_wide$group == "DBS"] = "PD"
  dat <- gather(dat_wide, emotion, acc, i)
  
  w = wilcox.test(acc~group, data = dat, exact = FALSE, correct = FALSE, alternative = "two.sided")
  
  
  print(i)
  print(w)
  
}