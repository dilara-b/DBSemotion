### Setup ------------------------------------------------------------------------------------

rm(list = ls())
#install.packages("readr") #uncomment as necessary
#install.packages("openxlsx") #uncomment as necessary
library(readr)
library(openxlsx)

min_response_time = 100 #minimum response time to include
max_sd = 3  #how many standard deviations above mean to include 

### Functions & Definitions ---------------------------------------------------------------------

#using only last two letters to avoid encoding issues
dict = list('el' = 'DIS', #Ekel
            'ng' = 'SUS', #Ueberraschung
            'it' = 'HAS', #Froehlichkeit
            'er' = 'SAS', #Trauer
            'ck' = 'NES', #neutraler Gesichtsausdruck
            'ut' = 'ANS', #Wut
            'st' = 'AFS') #Angst

emotions = c("AFS", "ANS", "DIS", "HAS", "NES", "SAS", "SUS")

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#returns fraction of wrong answers for tests 1 to 4
correct1 = function(subject,number){
  
  #convert list to dataframe
  df = subject[[number]]
  
  #calculate mean/sd response times for responses where candidate pressed space
  response_time_mean = mean(df$response_time[df$response == "space" | df$response == "space "])
  response_time_sd = sd(df$response_time[df$response == "space" | df$response == "space "])
  
  #only keep rows where response is "None" or rows where response time is >min_response_time milliseconds AND <Mean+max_sd SDs
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = 1 - mean(df$correct) #fraction of wrong answers, space when no-go-stimulus appears and none when go-stimulus appears
  res_go = 1 - mean(df$correct[df$correct_response == "space" | df$correct_response == "space "]) #fraction of wrong answers for the go stimulus, i.e. no action taken when go-stimulus appears
  res_nogo = 1 - mean(df$correct[df$correct_response == "None"]) #fraction of wrong answers for the no-go stimulus, i.e. action taken when no-go-stimulus appears
  
  res_time = mean(df$response_time[(df$correct_response == "space" | df$correct_response == "space ") & (df$response == "space" | df$response == "space ")]) #mean response time, including only correct go-stimulus answers, i.e. time of actions taken in response to a go-stimulus 
  
  return(c(res,res_go,res_nogo,res_time))
  
}

#returns fraction of correct answers for test 5, emotional face-in-the-crowd test
correct5 = function(subject,number){
  
  df = subject[[number]]
  
  response_time_mean = mean(df$response_time)
  response_time_sd = sd(df$response_time)
  
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = mean(df$correct) #fraction of correct answers, i.e. pressing left arrow when one face displays a different emotion (happy/angry), right arrow when all faces display the same emotion (neutral)
  res_anger = mean(df$correct[df$target == "anger"]) #fraction of correct answers when an angry face is displayed
  res_happy = mean(df$correct[df$target == "happy"]) #fraction of correct answers when a happy face is displayed
  res_odd = mean(df$correct[(df$target == "anger") | (df$target == "happy")]) #fraction of correct answers when one face displays a varied emotion, either happy or angry
  res_neutral = mean(df$correct[df$target == "neutral"]) #fraction of correct answers when all faces are neutral
  
  res_time = mean(df$response_time[(df$response == "left" & df$target == "anger") | (df$response == "left" & df$target == "happy") | df$response == "right" & df$target == "neutral"])
  res_time_anger = mean(df$response_time[df$response == "left" & df$target == "anger"]) #response time of correct answers when an angry face is displayed
  res_time_happy = mean(df$response_time[df$response == "left" & df$target == "happy"]) #response time of correct answers when a happy face is displayed
  res_time_odd = mean(df$response_time[(df$response == "left" & df$target == "anger") | (df$response == "left" & df$target == "happy")]) #response time of correct answers when one face displays a varied emotion, either happy or angry
  res_time_neutral = mean(df$response_time[df$response == "right" & df$target == "neutral"]) #response time of correct answers when all faces are neutral
  
  return(c(res, res_anger, res_happy, res_odd, res_neutral, res_time, res_time_anger, res_time_happy, res_time_odd, res_time_neutral))
  
}

#returns fraction of correct answers for test 6, non-emotional face-in-the-crowd test
correct6 = function(subject,number){

  df = subject[[number]]
  
  response_time_mean = mean(df$response_time)
  response_time_sd = sd(df$response_time)
  
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  
  res = mean(df$correct) #total of correct answers
  res_odd = mean(df$correct[df$correct_response == "left"]) #fraction of correct answers when one individual is different
  res_equal = mean(df$correct[df$correct_response == "right"]) #fraction of correct answers when all images show the same individual
  res_time = mean(df$response_time[(df$correct_response == "left" & df$response == "left") | df$correct_response == "right" & df$response == "right"]) #response time when the response is correct, i.e. time taken to press the left arrow for a different individual and right arrow for the same individual
  res_time_odd = mean(df$response_time[df$correct_response == "left" & df$response == "left"]) #response time for correct responses when one individual is different
  res_time_equal = mean(df$response_time[df$correct_response == "right" & df$response == "right"]) #response time for correct responses when all images show the same indivudal
  
  return(c(res,res_odd,res_equal,res_time,res_time_odd,res_time_equal))
  
}

#returns fraction of correct answers for tests 7 & 9
#first changes all emotion responses to their last two letters (to avoid encoding issues), then replaces with dictionary to compare to correct emotion
correct2 = function(subject,number){
  
  for (m in 1:nrow(subject[[number]]["response"])){
    subject[[number]]["response"][m,] = substrRight(subject[[number]]["response"][m,],2)
    
  }
  
  for (j in 1:7){
    subject[[number]]["response"] <- unlist(
      replace(
        subject[[number]]["response"], 
        subject[[number]]["response"] == names(dict[j]), 
        dict[j])
      )
  }
  
  res = sum(subject[[number]]["response"] == subject[[number]]["emotion"])/nrow(subject[[number]]["response"])
  
  res_d = c()
  for (m in emotions){
    res_d = c(res_d, sum((subject[[number]]["response"] == subject[[number]]["emotion"]) * (subject[[number]]["emotion"] == m)) / sum(subject[[number]]["emotion"] == m))
  
    for (n in emotions){
      
      res_d = c(res_d, sum((subject[[number]]["response"] == n) * (subject[[number]]["emotion"] == m)))
      
    }
    
  }
  
  #print(res_d)
  
  return(c(res,res_d))
  
}

#returns fraction of correct answers for test 8
#counts what fraction of responses identify the correct gender in a single-choice field with two options (woman, man), assuming the answer is woman if "man" ("Mann") is not selected
correct3 = function(subject,number){
  
  count = 0
  max = nrow(subject[[number]]["stimulus"])
  
  for (j in 1:max){
    if(grepl("Mann",subject[[number]]["response"][j,]) == grepl("AM",subject[[number]]["stimulus"][j,])){
      count = count + 1
      
    }
    
  }
  
  return(count/max)
  
}

### Setup-------------------------------------------------------------------------------------------------------

setwd( ) #Folder including all participant folders

subjects_dirs = list.dirs(getwd(),recursive = FALSE)
subjects_names = list.dirs(getwd(),recursive = FALSE, full.names = FALSE)

### Data read-in------------------------------------------------------------------------------------------------

for(h in 1:length(subjects_dirs)){
  filenames = list.files(subjects_dirs[h], pattern="*.csv", full.names=TRUE)
  
  for(k in 1:length(filenames)){
    assign(subjects_names[h],lapply(filenames, read.csv, encoding="latin1"))
    
  }
  
    print(c("Read-in subject",subjects_names[h],"concluded"))
  
}

### Analysis----------------------------------------------------------------------------------------------------

output_fields = c("1 Imp. X error rate","1.1 Imp. X_GO error rate","1.2 Imp. X_NOGO error rate","1.3 Imp. X_GO_Time",
                  "2 Imp. Happy error rate","2.1 Imp. Happy_GO error rate","2.2 Imp. Happy_NOGO error rate","2.3 Imp. Happy_GO_Time",
                  "3 Imp. K error rate","3.1 Imp. K_GO error rate","3.2 Imp. K_NOGO error rate","3.3 Imp. K_GO_Time",
                  "4 Imp. Anger error rate","4.1 Imp. Anger_GO error rate","4.2 Imp. Anger_NOGO error rate","4.3 Imp. Anger_GO_Time",
                  "impulsivity_emo_errorrate", "impulsivity_emo_go_errorrate", "impulsivity_emo_nogo_errorrate", "impulsivity_emo_time",
                  "impulsivity_nonemo_errorrate", "impulsivity_nonemo_go_errorrate", "impulsivity_nonemo_nogo_errorrate", "impulsivity_nonemo_time",
                  "5 FitCEmo Ges.","5.1 FitC Emo_Anger","5.2 FitC Emo_Happy","5.3 FitC Emo_Odd","5.4 FitC Emo_Equal","5.5 FitCEmo_time","5.6 FitC Emo_Anger_Time","5.7 Fitc Emo_Happy_Time","5.8 FitC Emo_Odd_Time","5.9 FitC Emo_Equal_Time",
                  "6 FitC NonEmo","6.1 FitC NonEmo_Odd","6.2 FitC NonEmo_Equal","6.3 FitC NonEmo_time","6.4 FitC NonEmo_Odd_time","6.5 FitC NonEmo_Equal_time",
                  "7 EmoRecog1","7.1 Afraid","7.1.1 Afraid","7.1.2 Anger","7.1.3 Disgust","7.1.4 Happy","7.1.5 Neutral","7.1.6 Sad","7.1.7 Surprise","7.2 Anger","7.2.1 Afraid","7.2.2 Anger","7.2.3 Disgust","7.2.4 Happy","7.2.5 Neutral","7.2.6 Sad","7.2.7 Surprise","7.3 Disgust","7.3.1 Afraid","7.3.2 Anger","7.3.3 Disgust","7.3.4 Happy","7.3.5 Neutral","7.3.6 Sad","7.3.7 Surprise","7.4 Happy","7.4.1 Afraid","7.4.2 Anger","7.4.3 Disgust","7.4.4 Happy","7.4.5 Neutral","7.4.6 Sad","7.4.7 Surprise","7.5 Neutral","7.5.1 Afraid","7.5.2 Anger","7.5.3 Disgust","7.5.4 Happy","7.5.5 Neutral","7.5.6 Sad","7.5.7 Surprise","7.6 Sad","7.6.1 Afraid","7.6.2 Anger","7.6.3 Disgust","7.6.4 Happy","7.6.5 Neutral","7.6.6 Sad","7.6.7 Surprise","7.7 Surprise","7.7.1 Afraid","7.7.2 Anger","7.7.3 Disgust","7.7.4 Happy","7.7.5 Neutral","7.7.6 Sad","7.7.7 Surprise",
                  "8 N.-EmoRec",
                  "9 EmoRecog2","9.1 Afraid","9.1.1 Afraid","9.1.2 Anger","9.1.3 Disgust","9.1.4 Happy","9.1.5 Neutral","9.1.6 Sad","9.1.7 Surprise","9.2 Anger","9.2.1 Afraid","9.2.2 Anger","9.2.3 Disgust","9.2.4 Happy","9.2.5 Neutral","9.2.6 Sad","9.2.7 Surprise","9.3 Disgust","9.3.1 Afraid","9.3.2 Anger","9.3.3 Disgust","9.3.4 Happy","9.3.5 Neutral","9.3.6 Sad","9.3.7 Surprise","9.4 Happy","9.4.1 Afraid","9.4.2 Anger","9.4.3 Disgust","9.4.4 Happy","9.4.5 Neutral","9.4.6 Sad","9.4.7 Surprise","9.5 Neutral","9.5.1 Afraid","9.5.2 Anger","9.5.3 Disgust","9.5.4 Happy","9.5.5 Neutral","9.5.6 Sad","9.5.7 Surprise","9.6 Sad","9.6.1 Afraid","9.6.2 Anger","9.6.3 Disgust","9.6.4 Happy","9.6.5 Neutral","9.6.6 Sad","9.6.7 Surprise","9.7 Surprise","9.7.1 Afraid","9.7.2 Anger","9.7.3 Disgust","9.7.4 Happy","9.7.5 Neutral","9.7.6 Sad","9.7.7 Surprise",
                  "recog_emo_acc", "recog_afraid_acc", "recog_anger_acc", "recog_disgust_acc", "recog_happy_acc", "recog_neutral_acc", "recog_sad_acc", "recog_surprise_acc")

results_total = data.frame(matrix(NA,    # Create empty data frame
                                  nrow = length(output_fields),
                                  ncol = 0))

rownames(results_total) = output_fields

for (k in subjects_names){
  results = data.frame()
  
  j = get(k)
  
    #Analyse tests
  #Tests 1-4
  for(i in 1:4){
    for(m in 1:4){results = rbind(results, correct1(j,i)[m])}}
  
  #Combination of emotional impulsivity tests (2 & 4)
  impulsivity_emo_errorrate = (results[5,1]+results[13,1])/2
  impulsivity_emo_go_errorrate = (results[6,1]+results[14,1])/2
  impulsivity_emo_nogo_errorrate = (results[7,1]+results[15,1])/2
  impulsivity_emo_go_time = (results[8,1]+results[16,1])/2
  
  #Combination of non-emotional impulsivity tests (1 & 3)
  impulsivity_nonemo_errorrate = (results[1,1]+results[9,1])/2
  impulsivity_nonemo_go_errorrate = (results[2,1]+results[10,1])/2
  impulsivity_nonemo_nogo_errorrate = (results[3,1]+results[11,1])/2
  impulsivity_nonemo_go_time = (results[4,1]+results[12,1])/2
  
  results = rbind(results,impulsivity_emo_errorrate, impulsivity_emo_go_errorrate, impulsivity_emo_nogo_errorrate, impulsivity_emo_go_time, impulsivity_nonemo_errorrate, impulsivity_nonemo_go_errorrate, impulsivity_nonemo_nogo_errorrate, impulsivity_nonemo_go_time)  
  
  #Test 5 Emotional face-in-the-crowd test
  for(i in 1:10){results = rbind(results, correct5(j,5)[i])}
  
  #Test 6 Non-emotional face-in-the-crowd test
  for(i in 1:6){results = rbind(results, correct6(j,6)[i])}
  
  #Test 7 Facial Emotion Recognition, part one
  for(i in 1:57){results = rbind(results, correct2(j,7)[i])}
  
  #Test 8 Gender Recognition 
  results = rbind(results, correct3(j,8))
    
  #Test 9 Facial Emotion Recognition, part two
  for(i in 1:57){results = rbind(results, correct2(j,9)[i])}
  
  #Combination of Facial Emotion Recognition tests (7 & 9)
  recog_emo_acc = (results[41,1]+results[99,1])/2
  recog_afraid_acc = (results[42,1]+results[100,1])/2
  recog_anger_acc = (results[50,1]+results[108,1])/2
  recog_disgust_acc = (results[58,1]+results[116,1])/2
  recog_happy_acc = (results[66,1]+results[124,1])/2
  recog_neutral_acc = (results[74,1]+results[132,1])/2
  recog_sad_acc = (results[82,1]+results[140,1])/2
  recog_surprise_acc = (results[90,1]+results[148,1])/2
  
  results = rbind(results,recog_emo_acc, recog_afraid_acc, recog_anger_acc, recog_disgust_acc, recog_happy_acc, recog_neutral_acc, recog_sad_acc, recog_surprise_acc)
  
  #Result handling
  colnames(results) = k
  results_total = cbind(results_total, results)
  print(c("Analysis subject",k,"concluded"))
  
}

#Write complete results to Excel file
write.xlsx(results_total, "analysis.xlsx", rowNames=TRUE)

save(results_total,file="scores.Rda")

### Change score calc-------------------------------------------------------------------------------------------------------


output = data.frame(matrix(NA,    # Create empty data frame
                           nrow = length(output_fields),
                           ncol = 0))

rownames(output) = output_fields

output2 = output

for (i in 1:ncol(results_total)){
  
  #only continue if column is a follow up
  #For simplicity we can assume if follow up exists, baseline will be column left of it
  if(substrRight((colnames(results_total)[i]),2) == "FU"){
    
    output = cbind(output,results_total[,i] - results_total[,i-1])
    colnames(output)[ncol(output)] = colnames(results_total[i])
    
  }
}

for (i in 1:ncol(results_total)){
  
  #only continue if column is a follow up
  #For simplicity we can assume if follow up exists, baseline will be column left of it
  if(substrRight((colnames(results_total)[i]),2) == "FU"){
    
    output2 = cbind(output2,((results_total[,i] - results_total[,i-1])/results_total[,i-1]))
    colnames(output2)[ncol(output2)] = colnames(results_total[i])
    
  }
}


write.xlsx(output, "absolute-change_scores.xlsx", rowNames=TRUE)
write.xlsx(output2, "relative-change_scores.xlsx", rowNames=TRUE)

save(output, file = "output.Rdata")
save(output2, file = "output2.Rdata")

