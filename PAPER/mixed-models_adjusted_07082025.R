library(readr)
library(plyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(stringr)
library(rstatix)
library(ggplot2)
library(ggtext)
library(tidyr)
library(patchwork)


## Setup ------------------------------------------------------------------------------------
setwd("")

patients_dirs = list.dirs(getwd(),recursive = FALSE)
patients_names = list.dirs(getwd(),recursive = FALSE, full.names = FALSE)

### Functions & Definitions ---------------------------------------------------------------------
min_response_time = 100 #minimum response time to include
max_sd = 3  #how many standard deviations above mean to include


#using only last two letters to avoid encoding issues
dict = list('el' = 'DIS', #Ekel
            'ng' = 'SUS', #?berraschung
            'it' = 'HAS', #Fr?hlichkeit
            'er' = 'SAS', #Trauer
            'ck' = 'NES', #neutraler Gesichtsausdruck
            'ut' = 'ANS', #Wut
            'st' = 'AFS') #Angst

emotions = c("AFS", "ANS", "DIS", "HAS", "NES", "SAS", "SUS")

#substring extraction
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#define function to get df per subject and remove trials with outlier RTs for impulsivity task
correct = function(patient,number){
  df = patient[[number]]
  response_time_mean = mean(df$response_time[df$response == "space" | df$response == "space "])
  response_time_sd = sd(df$response_time[df$response == "space" | df$response == "space "])
  df = df[(df$response_time<(response_time_mean+max_sd*response_time_sd) & df$response_time > min_response_time) | df$response == "None",]
  return(df)
}


# same for fitc task
correct_fitc = function(patient,number){
  
  df = patient[[number]]
  
  response_time_mean = mean(df$response_time[df$response == "right" | df$response == "left"])
  response_time_sd = sd(df$response_time[df$response == "right" | df$response == "left"])
  
  df = df[((df$response_time<(response_time_mean+max_sd*response_time_sd) & (df$response_time > min_response_time)) | df$response == "None"),]
  return(df)
}

# emotional part of the emo recog task
#first changes all emotion responses to their last two letters (to avoid encoding issues), then replaces with dictionary to compare to correct emotion
correct_emo = function(patient,number){
  
  df = patient[[number]]
  
  for (m in 1:nrow(df["response"])){
    df["response"][m,] = substrRight(df["response"][m,],2)
    
  }
  
  for (j in 1:7){
    df["response"] <- unlist(
      replace(
        df["response"],
        df["response"] == names(dict[j]),
        dict[j])
    )
  }
  
  return(df)
}

#non-emotional part of the emo recog task
correct8 = function(patient,number){
  df = patient[[number]]
  return(df)
}

### Data read-in------------------------------------------------------------------------------------------------
# load trial-data per subject

for(h in 1:length(patients_dirs)){
  filenames = list.files(patients_dirs[h], pattern="*.csv", full.names=TRUE)
  
  for(k in 1:length(filenames)){
    assign(patients_names[h],lapply(filenames, read.csv, encoding="latin1"))
    
  }
  print(c("Read-in patient",patients_names[h],"concluded"))
  
}

#create separate dataframes for each task
imp= data.frame()
#  get dataframe for first tests (impulsivity)
# add columns with id, time, group, emotion info
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct(j,1)
  trials1$task = "imp_non-emo"
  trials3=correct(j,3)
  trials3$task = "imp_non-emo"
  trials2=correct(j,2)
  trials2$task = "imp_emo"
  trials4=correct(j,4)
  trials4$task = "imp_emo"
  trials = rbind.fill(trials1, trials2, trials3, trials4)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  imp = rbind.fill(imp, trials)}

# get separate df only for emotional imp. tasks, separate by "go"-emotion
imp_emo= data.frame()
for (k in patients_names){
  trials = data.frame()
  trials2 = data.frame()
  j = get(k)
  trials2=correct(j,2)
  trials2$task = "happy"
  trials4=correct(j,4)
  trials4$task = "anger"
  trials = rbind.fill(trials2, trials4)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  imp_emo = rbind.fill(imp_emo, trials)}


fitc = data.frame()
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct_fitc(j,5)
  trials1$task = "fitc_emo"
  trials2=correct_fitc(j,6)
  trials2$task = "fitc_non-emo"
  trials = rbind.fill(trials1, trials2)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  fitc = rbind.fill(fitc, trials)}

# get df for non-emotional part of emo recog task and calculate accuracy
non_emo = data.frame()
for (k in patients_names){
  trials = data.frame()
  m = get(k)
  trials=correct8(m,8)
  trials$task = "emorecog_non-emo"
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  non_emo = rbind.fill(non_emo, trials)}

non_emo$stimulus = str_sub(non_emo$stimulus,-4,-3)
non_emo$correct[(non_emo$response == "Mann" & non_emo$stimulus== "AM") | (non_emo$response == "Frau" & non_emo$stimulus== "AF") ] <- 1
non_emo$correct[(non_emo$response == "Mann"& non_emo$stimulus== "AF") | (non_emo$response == "Frau" & non_emo$stimulus== "AM") ] <- 0
non_emo$correct <- as.numeric(non_emo$correct)


emo = data.frame()
#  get dataframe for first tests (impulsivity)
for (k in patients_names){
  trials = data.frame()
  j = get(k)
  trials1=correct_emo(j,7)
  trials1$task = "emorecog_emo"
  trials3=correct_emo(j,9)
  trials3$task = "emorecog_emo"
  trials= rbind.fill(trials1, trials3)
  trials$sub_id=k
  trials$group = sub('(^.*?)-.*', '\\1', trials$sub_id)
  trials$time  = str_sub(trials$sub_id,-2,-1)
  trials$subj = str_sub(trials$sub_id,-7,-4)
  emo = rbind.fill(emo, trials)}

emo$correct[(emo$response == emo$emotion) ] <- 1
emo$correct[(emo$response != emo$emotion)] <- 0
emo$correct <- as.numeric(emo$correct)

emorecog <- rbind.fill(emo, non_emo)

############### IMPULSIVITY ###########################

imp$group<- factor(imp$group, levels = c("DBS", "Gesund", "IPS"))

###### commisson errors
imp_nogo <- filter(imp, imp$correct_response == "None")

#get mean and sd accuracy per group,task and time
imp_nogo %>%
  group_by(task, group, time) %>%
  summarise(
    mean_accuracy = mean(correct, na.rm = TRUE),
    sd_accuracy = sd(correct, na.rm = TRUE),
    n = n()
  )

# build model
imp_nogo_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_nogo, family = binomial)
summary(imp_nogo_logmodel)

contrast(emmeans(imp_nogo_logmodel, ~ time*group*task),
         interaction = c("poly", "consec", "consec"))
# visualize
emmip(imp_nogo_logmodel, task ~ time | group)
#get ANOVA-style table
joint_tests(imp_nogo_logmodel)

# ANOVA-style table with grouping to check for time effects within the DBS group only per task
joint_tests(imp_nogo_logmodel, by = c("group", "task"))

##### omission errors
imp_go <- filter(imp, imp$correct_response == "space"| imp$correct_response == "space ")

#get mean and sd accuracy per group,task and time
imp_go %>%
  group_by(task, group, time) %>%
  summarise(
    mean_accuracy = mean(correct, na.rm = TRUE),
    sd_accuracy = sd(correct, na.rm = TRUE),
    mean_response_time = mean(response_time[response == "space"| response == "space "], na.rm = TRUE),
    sd_response_time = sd(response_time[response == "space"| response == "space "], na.rm = TRUE),
    n = n()
  )

# build model
imp_go_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_go, family = binomial)
summary(imp_go_logmodel)
contrast(emmeans(imp_go_logmodel, ~ time*group*task),
         interaction = c("poly", "consec", "consec"))
# visualize
emmip(imp_go_logmodel, task ~ time | group)
#get ANOVA-style table
joint_tests(imp_go_logmodel)
# ANOVA-style table by group to check for efects within each group
joint_tests(imp_go_logmodel, by = "group")

# ANOVA-style table with grouping to check for time effects within the DBS group only per task
joint_tests(imp_go_logmodel, by = c("group", "task"))

###### response time correct trials (i.e., only go trials with correct response)
imp_rt <- filter(imp, imp$correct ==1 & (imp$response == "space" | imp$response == "space "))
#get mean and sd accuracy per group,task and time
imp_rt %>%
  group_by(task, group, time) %>%
  get_summary_stats(response_time, type = "mean_sd")


# build model
imp_rt_logmodel <- lmer(response_time ~ group*time*task+ (1 | subj), data = imp_rt,REML = T)
summary(imp_rt_logmodel)
# get ANOVA-style table for linear reg. model
anova(imp_rt_logmodel, type =3)
# should be the same as:
joint_tests(imp_rt_logmodel)
# visualize
emmip(imp_rt_logmodel, task ~ time | group)

joint_tests(imp_rt_logmodel, by = c("group", "task"))

##### Secondary analysis emotional only  ################
###### commisson errors
imp_emo_nogo <- filter(imp_emo, imp_emo$correct_response == "None")
imp_emo_nogo_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_emo_nogo, family = binomial)
summary(imp_emo_nogo_logmodel)
##### omission errors
imp_emo_go <- filter(imp_emo, imp_emo$correct_response == "space"| imp_emo$correct_response == "space ")
# build model
imp_emo_go_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = imp_emo_go, family = binomial)
summary(imp_emo_go_logmodel)
# visualize
emmip(imp_emo_go_logmodel, task ~ time | group)
joint_tests(imp_emo_go_logmodel)

###### response time correct trials (i.e., only go trials with correct response)
imp_emo_rt <- filter(imp_emo, imp_emo$correct ==1 & (imp_emo$response == "space" | imp_emo$response == "space "))
# build model
imp_emo_rt_logmodel <- lmer(response_time ~ group*time*task + (1 | subj), data = imp_emo_rt,REML = T)
summary(imp_emo_rt_logmodel)
anova(imp_emo_rt_logmodel)

################## FACE IN THE CROWD ############################

fitc %>%
  group_by(task, group, time) %>%
  summarise(
    mean_accuracy = mean(correct, na.rm = TRUE),
    sd_accuracy = sd(correct, na.rm = TRUE),
    mean_response_time = mean(response_time[response %in% c("left", "right")], na.rm = TRUE),
    sd_response_time = sd(response_time[response %in% c("left", "right")], na.rm = TRUE),
    n = n()  # This will include the count of observations
  )

fitc_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = fitc, family = binomial)
summary(fitc_logmodel)
# ANOVA-style table by group to check for time effects within each group
joint_tests(fitc_logmodel, by = c("group", "task"))


fitc_logmodel_emm <- emmeans(fitc_logmodel,
                             specs = ~ group * time * task,  
                             type = "response")             

######FITC FIGURE 1


tiff("FitC_accuracy.tiff", units="in", width=4, height=3, res=300)

emmip(fitc_logmodel, group ~ time | task, type = "response") +
  labs(
    y = "Accuracy",
    x = "Time point"
  ) +
  scale_color_manual(
    name = "Group",
    values = c("DBS" = "coral2", "Gesund" = "darkseagreen3", "IPS" = "darkblue"),
    labels = c("DBS" = "DBS", "Gesund" = "HC", "IPS" = "non-DBS")
  ) +
  facet_wrap(~ task, labeller = as_labeller(
    c("fitc_emo" = "Emo Task", "fitc_non-emo" = "Non-Emo Task")
  )) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(face = "bold", size = 9),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 8)
  )

dev.off()


fitc_rt <- fitc %>%
  filter(response %in% c("left", "right"))

fitc_rt_logmodel <- lmer(response_time ~ group*time*task + (1 | subj), data = fitc_rt)
summary(fitc_rt_logmodel)
# ANOVA-style table by group to check for time effects within each group
joint_tests(fitc_rt_logmodel, by = c("group", "task"))

fitc_rt_logmodel_emm <- emmeans(fitc_rt_logmodel,
                                specs = ~ group * time * task,  
                                type = "response")             

###########FITC FIGURE 2

tiff("FitC_response time.tiff", units="in", width=4, height=3, res=300)

emmip(fitc_rt_logmodel, group ~ time | task, type = "response") +
  labs(
    y = "Mean Reaction Time (ms)",
    x = "Time point",
  ) +
  scale_color_manual(
    name = "Group",
    values = c("DBS" = "coral2", "Gesund" = "darkseagreen3", "IPS" = "darkblue"),
    labels = c("DBS" = "DBS", "Gesund" = "HC", "IPS" = "non-DBS")
  ) +
  facet_wrap(~ task, labeller = as_labeller(
    c("fitc_emo" = "Emo Task", "fitc_non-emo" = "Non-Emo Task")
  )) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(face = "bold", size = 9),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 8)
  )

dev.off()


##Secondary analysis emotional only
fitc_emo <- filter(fitc, fitc$task == "fitc_emo")
fitc_emo%>%
  group_by(target, group, time) %>%
  get_summary_stats(correct, type = "mean_sd")

# compare angry vs. happy stimuli only
fitc_emo <- filter(fitc_emo, fitc_emo$target != "neutral")
fitc_emo_logmodel <- glmer(correct ~ group*time*target + (1 | subj), data = fitc_emo, family = binomial)
summary(fitc_emo_logmodel)
# visualize
emmip(fitc_emo_logmodel, target ~ time | group)
joint_tests(fitc_emo_logmodel)
# ANOVA-style table by group to check for effects within each group
joint_tests(fitc_emo_logmodel, by = c("group", "target"))


fitc_emo_rt <- fitc_emo %>%
  filter(response %in% c("left", "right"))

#rt
fitc_emo_rt_logmodel <- lmer(response_time ~ group*time*target + (1 | subj), data = fitc_emo_rt)
summary(fitc_emo_rt_logmodel)

############# EMOTION RECOGNITION TASK #####################

emorecog_logmodel <- glmer(correct ~ group*time*task + (1 | subj), data = emorecog, family = binomial)
summary(emorecog_logmodel)
# visualize
emmip(emorecog_logmodel, task ~ time | group)
joint_tests(emorecog_logmodel)
# ANOVA-style table by group to check for effects within each group
joint_tests(emorecog_logmodel, by = c("group", "task"))

## only emotional task
# get summary (mean, sd) per group, time and emotion
emo_summary <- emo %>%
  group_by(emotion, group, time) %>%
  get_summary_stats(correct, type = "mean_sd") %>%
  mutate(group = recode(group,
                        "DBS" = "DBSi",
                        "IPS" = "non-DBS",
                        "Gesund" = "HC"),
         emotion = recode(emotion,
                          "AFS" = "Fear",
                          "ANS" = "Anger",
                          "DIS" = "Disgust",
                          "HAS" = "Happiness",
                          "NES" = "Neutral",
                          "SAS" = "Sadness",
                          "SUS" = "Surprise"))

# Print all rows to check the result
print(emo_summary, n = 42)

emo_summary_agg <- emo %>%
  group_by(group, time) %>%
  get_summary_stats(correct, type = "mean_sd") %>%
  mutate(group = recode(group,
                        "DBS" = "DBSi",
                        "IPS" = "non-DBS",
                        "Gesund" = "HC"),
         emotion = "All Emotions")

### PLOT 1: "All Emotions" as a Separate Large Plot on the Left ###
all_emotions_plot <- ggplot(emo_summary_agg, aes(x = time, y = mean, color = group, group = group)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_discrete(expand = c(0.2, 0)) +  # Remove extra space
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(y = "Accuracy", x = "", color = "Group", title = "All Emotions") +
  theme(
    legend.position = "none",  # Remove legend (avoid duplication)
    strip.text = element_text(size = 12, face = "bold"),  # Ensure facet label consistency
    plot.title = element_text(size = 12, face = "bold"),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),
    panel.spacing = unit(0.3, "lines")  
  )

### PLOT 2: Faceted Plot for Individual Emotions on the Right ###
faceted_emotions_plot <- ggplot(emo_summary, aes(x = time, y = mean, color = group, group = group)) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~emotion, nrow = 2, ncol = 4) +  # Adjust grid layout for better alignment
  scale_x_discrete(expand = c(0.2, 0)) +  # Remove extra space
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  labs(y = "Accuracy", x = "", color = "Group") +
  theme(
    legend.position = "bottom",
    panel.spacing = unit(0.5, "lines"),
    strip.text = element_text(size = 12, face = "bold"),  # Ensure facet label consistency
    plot.title = element_text(size = 12),  
    axis.title = element_text(size = 12),  
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )

### COMBINE BOTH PLOTS SIDE-BY-SIDE ###
final_plot <- (all_emotions_plot | faceted_emotions_plot) + plot_layout(widths = c(1,4))

# Save the combined plot
tiff("accuracy_combined_left.tiff", units = "in", width = 10.5, height = 7, res = 300)
print(final_plot)
dev.off()

fear <- filter(emo, emo$emotion=="AFS")
fear_logmodel <- glmer(correct ~ group*time + (1 | subj), data = fear, family = binomial)
summary(fear_logmodel)
joint_tests(fear_logmodel)
joint_tests(fear_logmodel, by = "group")

disgust <- filter(emo, emo$emotion=="DIS")
disgust_logmodel <- glmer(correct ~ group*time + (1 | subj), data = disgust, family = binomial)
summary(disgust_logmodel)
joint_tests(disgust_logmodel)
joint_tests(disgust_logmodel, by = "group")

happiness <- filter(emo, emo$emotion=="HAS")
happiness_logmodel <- glmer(correct ~ group*time + (1 | subj), data = happiness, family = binomial)
summary(happiness_logmodel)
joint_tests(happiness_logmodel)
joint_tests(happiness_logmodel, by = "group")

sadness <- filter(emo, emo$emotion=="SAS")
sadness_logmodel <- glmer(correct ~ group*time + (1 | subj), data = sadness, family = binomial)
summary(sadness_logmodel)
joint_tests(sadness_logmodel)
joint_tests(sadness_logmodel, by = "group")

anger <- filter(emo, emo$emotion=="ANS")
anger_logmodel <- glmer(correct ~ group*time + (1 | subj), data =anger, family = binomial)
summary(anger_logmodel)
joint_tests(anger_logmodel)
joint_tests(anger_logmodel, by = "group")

surprise <- filter(emo, emo$emotion=="SUS")
surprise_logmodel <- glmer(correct ~ group*time + (1 | subj), data =surprise, family = binomial)
joint_tests(surprise_logmodel)
joint_tests(surprise_logmodel, by = "group")

neutral <- filter(emo, emo$emotion=="NES")
neutral_logmodel <- glmer(correct ~ group*time + (1 | subj), data = neutral, family = binomial)
joint_tests(neutral_logmodel)
joint_tests(neutral_logmodel, by = "group")


##### Lollipop Plot

emo_wide <- emo_summary %>%
  filter(variable == "correct") %>%
  select(emotion, group, time, mean) %>%
  mutate(group = recode(group, "DBSi" = "DBS")) %>%
  pivot_wider(names_from = time, values_from = mean) %>%
  mutate(
    change_pct = (FU - BL) * 100,
    line_color = ifelse(FU < BL, "decrease", "increase"),
    emotion = factor(emotion, levels = c("Surprise", "Sadness", "Neutral", "Happiness", "Fear", "Disgust", "Anger")),
    group = factor(group, levels = c("DBS", "HC", "non-DBS"))
  ) %>%
  arrange(emotion, group)

Overall_wide <- emo_summary %>%
  filter(variable == "correct") %>%
  mutate(group = recode(group, "DBSi" = "DBS")) %>%
  group_by(group, time) %>%
  summarize(mean = mean(mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(emotion = "Overall") %>%
  pivot_wider(names_from = time, values_from = mean) %>%
  mutate(
    change_pct = (FU - BL) * 100,
    line_color = ifelse(FU < BL, "decrease", "increase"),
    emotion = factor("Overall", levels = c("Overall")),
    group = factor(group, levels = c("DBS", "HC", "non-DBS"))
  )

emo_wide <- bind_rows(emo_wide, Overall_wide) %>%
  mutate(
    emotion = factor(emotion, levels = c("Overall", "Surprise", "Sadness", "Neutral", "Happiness", "Fear", "Disgust", "Anger"))
  )

point_colors <- c("Baseline" = "gray86", "Follow-up" = "gray60")
line_colors <- c("decrease" = "lightsalmon1", "increase" = "aquamarine3")

emo_plot <- ggplot(emo_wide, aes(y = group)) +
  geom_segment(
    aes(x = BL * 100, xend = FU * 100, yend = group, color = line_color),
    linewidth = 1.5, alpha = 0.7, lineend = "round", show.legend = FALSE
  ) +
  geom_point(aes(x = BL * 100, fill = "Baseline"), size = 2.5, shape = 21, stroke = 0) +
  geom_point(aes(x = FU * 100, fill = "Follow-up"), size = 2.5, shape = 21, stroke = 0) +
  geom_text(
    aes(x = pmax(BL, FU) * 100 + 2, label = sprintf("%+.1f%%", change_pct)),
    size = 3.6, hjust = 0, fontface = "bold", color = "black"
  ) +
  scale_color_manual(values = line_colors, guide = "none") +
  scale_fill_manual(
    name = "Time Point",
    values = point_colors,
    breaks = c("Baseline", "Follow-up"),
    labels = c("Baseline", "Follow-up")
  ) +
  scale_x_continuous(
    name = "Accuracy (%)",
    limits = c(0, 110),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  facet_grid(
    emotion ~ .,
    switch = "y",
    scales = "free_y",
    space = "free_y"
  ) +
  labs(
    y = NULL,
    title = "Emotion Recognition Accuracy by Group (Baseline vs Follow-up)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 1)),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    strip.text.y.left = element_text(angle = 0, face = "bold", size = 11),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.spacing.y = unit(0.07, "cm"),
    panel.background = element_rect(fill = "grey98", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box ="horizontal",
    legend.margin = margin(t = 0),
    legend.box.margin = margin (t = 0),
    plot.margin = margin(6,7,6,7),
                  
  )

tiff("EmotionRecognition_Clean.tiff", width = 3000, height = 2200, res = 300)
print(emo_plot)
dev.off()
