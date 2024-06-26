library(haven)
library(foreign)
library(tidyverse)
library(gt)
library(psych)
library(lavaan)
library(semPlot)
library(psych)
library(dplyr)
library(MVN)
library(robustbase)
library(ggcorrplot)
library(webshot2)

df <- read.spss("q1_dataset.sav", to.data.frame = T)

#------------------ IV: Maternal Warmth ----------------------------------------
recode_par <- function(df, columns) {
  for (col in columns) {
    col_sym <- ensym(col) 
    df <- df %>%
      mutate(!!col_sym := recode(!!col_sym,
                                 "Always" = 1,
                                 "Almost always" = 2,
                                 "Fairly often" = 3,
                                 "About half" = 4,
                                 "Not too often" = 5,
                                 "Almost never" = 6,
                                 "Never" = 7
      ))
  }
  return(df)
}

df <- recode_par(df, c('C1_PWHA1','C1_PWHA3','C1_PWHA4','C1_PWHA8','C1_PWHA2','C1_PWHA5','C1_PWHA6','C1_PWHA7','C1_PWHA9','C1_PWHA10','C1_PWHB1','C1_PWHB3','C1_PWHB4','C1_PWHB8','C1_PWHB2','C1_PWHB5','C1_PWHB6', 'C1_PWH7', 'C1_PWHB9', 'C1_PWHB10'))

df <- recode_par(df, c('C2_PWHA1','C2_PWHA3','C2_PWHA4','C2_PWHA8','C2_PWHA2','C2_PWHA5','C2_PWHA6','C2_PWHA7','C2_PWHA9','C2_PWHA10','C2_PWHB1','C2_PWHB3','C2_PWHB4','C2_PWHB8','C2_PWHB2','C2_PWHB5','C2_PWHB6', 'C2_PWHB7', 'C2_PWHB9', 'C2_PWHB10'))

df <- recode_par(df, c('C3_PWHA2','C3_PWHA5','C3_PWHA6','C3_PWHA7','C3_PWHA9','C3_PWHA10','C3_PWHB2','C3_PWHB5','C3_PWHB6', 'C3_PWHB9', 'C3_PWHB10'))


#2) reverse code parental warmth and hostility (w1,2,3)

rv_vars <- function(df, max_p_one, columns){
  for (col in columns){
    col_sym <- ensym(col)
    for (i in 1:nrow(df)){
      df[[col_sym]][i] <- max_p_one - df[[col_sym]][i]
    }
  }
  return(df)
}

df <- rv_vars(df,8, c('C1_PWHA1','C1_PWHA3','C1_PWHA4','C1_PWHA8','C1_PWHA2','C1_PWHA5','C1_PWHA6','C1_PWHA7','C1_PWHA9','C1_PWHA10','C1_PWHB1','C1_PWHB3','C1_PWHB4','C1_PWHB8','C1_PWHB2','C1_PWHB5','C1_PWHB6', 'C1_PWH7', 'C1_PWHB9', 'C1_PWHB10'))

df <- rv_vars(df,8, c('C2_PWHA1','C2_PWHA3','C2_PWHA4','C2_PWHA8','C2_PWHA2','C2_PWHA5','C2_PWHA6','C2_PWHA7','C2_PWHA9','C2_PWHA10','C2_PWHB1','C2_PWHB3','C2_PWHB4','C2_PWHB8','C2_PWHB2','C2_PWHB5','C2_PWHB6', 'C2_PWHB7', 'C2_PWHB9', 'C2_PWHB10'))

df <- rv_vars(df,8, c('C3_PWHA2','C3_PWHA5','C3_PWHA6','C3_PWHA7','C3_PWHA9','C3_PWHA10','C3_PWHB2','C3_PWHB5','C3_PWHB6', 'C3_PWHB9', 'C3_PWHB10'))


#3) remove NAs and calculate parental warmth score for each child in each wave 

cl_df <- function(n, m, df, columns) {
  df <- df %>%
    rowwise() %>%
    mutate(mean_score = ifelse(sum(!is.na(c_across(all_of(columns)))) >= n,
                               round(rowMeans(across(all_of(columns)) * m, na.rm = TRUE)),
                               NA)) %>%
    ungroup()
  return(df)
}

for(ID in length(df$ID)){
  df <- cl_df(4,6,df,c('C1_PWHA2', 'C1_PWHA5', 'C1_PWHA6', 'C1_PWHA7', 'C1_PWHA9', 'C1_PWHA10'))
}

df <- df %>% rename(parental_warmth_w1 = mean_score)


for(ID in length(df$ID)){
  df <- cl_df(4,6,df,c('C2_PWHA2', 'C2_PWHA5', 'C2_PWHA6', 'C2_PWHA7', 'C2_PWHA9', 'C2_PWHA10'))
}

df <- df %>% rename(parental_warmth_w2 = mean_score)

for(ID in length(df$ID)){
  df <- cl_df(4,6,df,c('C3_PWHA2', 'C3_PWHA5', 'C3_PWHA6', 'C3_PWHA7', 'C3_PWHA9', 'C3_PWHA10'))
}

df <- df %>% rename(parental_warmth_w3 = mean_score)

#------------------IV: Peer support influence emotional and cognitive outcomes------------------------------------------------------

#IV: 
# Peer Support variable - loneliness
peersupport <- as_tibble(df)

#i. reverse code loneliness 
peersupport <- peersupport %>%
  select(ID, C1_L1:C1_L16, C2_L1,C2_L2,C2_L6,C2_L7,C2_L11,C2_L14,C2_L15,C3_L1,C3_L2,C3_L6,C3_L7,C3_L11,C3_L14,C3_L15) %>%
  unique() %>%
  arrange(ID)

peersupport_recode <- function(x) {
  factor_x <- factor(x, levels = c("Not true at all", "Hardly ever true", "Sometimes true", "True most of the time", "Always true"), labels = c("1","2","3","4","5"))
  as.numeric(as.character(factor_x))
}

peersupport <- peersupport %>%
  mutate(across(C1_L1:C3_L15, peersupport_recode))

print(peersupport)

loneliness_recode <- peersupport %>% 
  mutate(
    C1_L1 = recode(C1_L1, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C1_L3 = recode(C1_L3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C1_L5 = recode(C1_L5, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C1_L7 = recode(C1_L7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C1_L10 = recode(C1_L10, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C1_L15 = recode(C1_L15, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C2_L1 = recode(C2_L1, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C2_L7 = recode(C2_L7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C2_L15 = recode(C2_L15, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C3_L1 = recode(C2_L1, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C3_L7 = recode(C2_L7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    C3_L15 = recode(C2_L15, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    mutate(across(where(is.factor), as.numeric))
  )

print(loneliness_recode)

#ii. composite loneliness scores for peer support score:(higher score = more peer support)

# Calculate peer support score for Wave 1
loneliness_recode <- cl_df(4, 16, loneliness_recode, c('C1_L1', 'C1_L2', 'C1_L6', 'C1_L7', 'C1_L11', 'C1_L14', 'C1_L15')) %>%
  rename(peer_support_w1 = mean_score)

# Calculate peer support score for Wave 2
loneliness_recode <- cl_df(4, 7, loneliness_recode, c('C2_L1', 'C2_L2', 'C2_L6', 'C2_L7', 'C2_L11', 'C2_L14', 'C2_L15')) %>%
  rename(peer_support_w2 = mean_score)

# Calculate peer support score for Wave 3
loneliness_recode <- cl_df(4, 7, loneliness_recode, c('C3_L1', 'C3_L2', 'C3_L6', 'C3_L7', 'C3_L11', 'C3_L14', 'C3_L15')) %>%
  rename(peer_support_w3 = mean_score)

#----------------DV : SDQ - emotional outcomes subscale dataframe------------------
sdq_emotion <- as_tibble(df) %>%
  select(ID, C1_B3, C1_B8, C1_B13, C1_B16, C1_B24, C2_B3, C2_B8, C2_B13, C2_B16, C2_B24, C3_B3, C3_B8, C3_B13, C3_B16, C3_B24) %>%
  unique() %>%
  arrange(ID) %>% 
  mutate(across(C1_B3:C3_B24, ~ recode(., "Not true" = 0, "Sort of true" = 1, "Certainly true" = 2))) %>%
  mutate(across(C1_B3:C3_B24, ~ as.numeric((as.character(.)))))

#ii. composite emotional SDQ scores:(higher score = more emotional problem)
#WAVE 1:
sdq_emotion <- cl_df(5, 5, sdq_emotion, c('C1_B3', 'C1_B8', 'C1_B13', 'C1_B16', 'C1_B24')) %>%
  rename(emotion_w1 = mean_score)

# Calculate SDQ emotional composite score for Wave 2
sdq_emotion <- cl_df(5, 5, sdq_emotion, c('C2_B3', 'C2_B8', 'C2_B13', 'C2_B16', 'C2_B24')) %>%
  rename(emotion_w2 = mean_score)

# Calculate SDQ emotional composite score for Wave 3
sdq_emotion <- cl_df(5, 5, sdq_emotion, c('C3_B3', 'C3_B8', 'C3_B13', 'C3_B16', 'C3_B24')) %>%
  rename(emotion_w3 = mean_score)

#______________________________ Self Control DV _______________________________________-

# ii) Brief Self-Control Scale (BSCS): behavioral-regulation outcome
self_control <- as_tibble(df) %>% 
  select(ID, C1_SControl1, C1_SControl2, C1_SControl3, C1_SControl4, C1_SControl5, C1_SControl6, C1_SControl7, C1_SControl8, C1_SControl9, C1_SControl10, C1_SControl11, C1_SControl12, C1_SControl13, C2_SControl1, C2_SControl2, C2_SControl3, C2_SControl4, C2_SControl5, C2_SControl6, C2_SControl7, C2_SControl8, C2_SControl9, C2_SControl10, C2_SControl11, C2_SControl12, C2_SControl13, C3_SControl1, C3_SControl2, C3_SControl3, C3_SControl4, C3_SControl5, C3_SControl6, C3_SControl7, C3_SControl8, C3_SControl9, C3_SControl10, C3_SControl11, C3_SControl12, C3_SControl13) %>%
  unique() %>%
  arrange(ID)

self_control_recode_func <- function(x) {
  factor_x <- factor(x, levels = c("Not at all", "Very little", "Somewhat", "Quite a bit", "Very much"), labels = c("1","2","3","4","5"))
  as.numeric(as.character(factor_x))
}

self_control <- self_control %>%
  mutate(across(C1_SControl1:C3_SControl13, self_control_recode_func))

print(self_control)

# self control  reverse code w1
self_control <- rv_vars(self_control, 6, c('C1_SControl2', 'C1_SControl3', 'C1_SControl4', 'C1_SControl5', 'C1_SControl7', 'C1_SControl9', 'C1_SControl10', 'C1_SControl12', 'C1_SControl13'))

# Calculate self-control composite score for Wave 1
self_control <- cl_df(13, 1, self_control, c('C1_SControl1', 'C1_SControl2', 'C1_SControl3', 'C1_SControl4', 'C1_SControl5', 'C1_SControl6','C1_SControl7', 'C1_SControl8','C1_SControl9', 'C1_SControl10', 'C1_SControl11','C1_SControl12', 'C1_SControl13')) %>%
  rename(self_control_w1 = mean_score)

# self control  reverse code w2
self_control <- rv_vars(self_control, 6, c('C2_SControl2', 'C2_SControl3', 'C2_SControl4', 'C2_SControl5', 'C2_SControl7', 'C2_SControl9', 'C2_SControl10', 'C2_SControl12', 'C2_SControl13'))

# Calculate self-control composite score for Wave 2
self_control <- cl_df(13, 1, self_control, c('C2_SControl1', 'C2_SControl2', 'C2_SControl3', 'C2_SControl4', 'C2_SControl5', 'C2_SControl6','C2_SControl7', 'C2_SControl8','C2_SControl9', 'C2_SControl10', 'C2_SControl11','C2_SControl12', 'C2_SControl13')) %>%
  rename(self_control_w2 = mean_score)

self_control <- rv_vars(self_control, 6, c('C3_SControl2', 'C3_SControl3', 'C3_SControl4', 'C3_SControl5', 'C3_SControl7', 'C3_SControl9', 'C3_SControl10', 'C3_SControl12', 'C3_SControl13'))

#self control  reverse code w3

# Calculate self-control composite score for Wave 3
self_control <- cl_df(13, 1, self_control, c('C3_SControl1', 'C3_SControl2', 'C3_SControl3', 'C3_SControl4', 'C3_SControl5', 'C3_SControl6','C3_SControl7', 'C3_SControl8','C3_SControl9', 'C3_SControl10', 'C3_SControl11','C3_SControl12', 'C3_SControl13')) %>%
  rename(self_control_w3 = mean_score)

# Create final_df with ID and composite scores
final_df <- df %>%
  select(ID, parental_warmth_w1, parental_warmth_w2, parental_warmth_w3, Gender, Age_C1,SENstatus, Ethnicity5) %>%
  left_join(select(loneliness_recode, ID, peer_support_w1, peer_support_w2, peer_support_w3), by = "ID") %>%
  left_join(select(sdq_emotion, ID, emotion_w1, emotion_w2, emotion_w3), by = "ID") %>%
  left_join(select(self_control, ID, self_control_w1, self_control_w2, self_control_w3), by = "ID") %>%
  mutate(Age_C1_years= if_else(!is.na(Age_C1), Age_C1 / 12, NA_real_))

final_df


#METHODS: DEMOGRAPHICS ---------------------------------------
total_participants <- final_df %>% 
  summarize(Count = n())

final_df <- final_df %>%
  filter(!is.na(Age_C1))

# Age range
age_range <- range(final_df$Age_C1_years)

# Mean age
mean_age <- mean(final_df$Age_C1_years)

# Age standard deviation
age_sd <- sd(final_df$Age_C1)

# Gender counts
gender_counts <- final_df %>% 
  group_by(Gender) %>% 
  summarize(Count = n())

#Ethnicity
ethnicity <- final_df %>% 
  group_by(Ethnicity5) %>% 
  summarize(Count = n())

ethnicity

#SEN
sen <- final_df %>% 
  group_by(SENstatus) %>% 
  summarize(Count = n())

sen

# Create a table with the demographic information
demographics <- data.frame(
  Demographic = c('Total Participants: ', 'Age Range: ', 'Mean Age: ', 'Age SD: ', paste0('Gender: ', gender_counts$Gender)),
  N = c(total_participants, paste(round(age_range, 2), collapse = " - "), round(mean_age, 2), round(age_sd, 2), as.character(gender_counts$Count))
)

# Display the table
print(demographics)

#-------- REMOVE OUTLIERS USING IQR--
variables_to_test <- final_df %>% 
  select(ID, parental_warmth_w1, parental_warmth_w2, parental_warmth_w3, 
         peer_support_w1, peer_support_w2, peer_support_w3, 
         emotion_w1, emotion_w2, emotion_w3, 
         self_control_w1, self_control_w2, self_control_w3)

# Create box plots for relevant variables
box_plot_data <- final_df %>%
  select(parental_warmth_w1, parental_warmth_w2, parental_warmth_w3,
         peer_support_w1, peer_support_w2, peer_support_w3,
         emotion_w1, emotion_w2, emotion_w3,
         self_control_w1, self_control_w2, self_control_w3) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

box_plot <- ggplot(box_plot_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(box_plot)

# Save the plot
ggsave("../team_1_project/plots/combined_box_plots.png", plot = box_plot, width = 8, height = 5)

# Define the variables of interest
variables <- c("parental_warmth_w1", "parental_warmth_w2", "parental_warmth_w3", 
               "peer_support_w1", "peer_support_w2", "peer_support_w3", 
               "emotion_w1", "emotion_w2", "emotion_w3", 
               "self_control_w1", "self_control_w2", "self_control_w3")

# Function to identify outliers using IQR method
identify_outliers <- function(df, var) {
  Q1 <- quantile(df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- which(df[[var]] < lower_bound | df[[var]] > upper_bound)
  return(outliers)
}

# Identify outliers for all variables and combine them
all_outliers <- unique(unlist(lapply(variables, function(var) identify_outliers(final_df, var))))

# Create a clean dataframe excluding the outliers
final_df_clean <- final_df[-all_outliers, ]

# Check the dimensions of the cleaned dataframe
dim(final_df_clean)

# Optional: Plot boxplots for each variable in the cleaned dataframe
box_plot_data_clean <- final_df_clean %>%
  select(all_of(variables)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

box_plot_clean <- ggplot(box_plot_data_clean, aes(x = variable, y = value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(box_plot_clean)

#------------------------DESCRIPTIVE STATISTICS----------------
# Calculate overall descriptive statistics
variable_final_df <- final_df_clean %>%
  rename(
    "Parental Warmth 1" = parental_warmth_w1,
    "Peer Support 1" = peer_support_w1,
    "Emotional Symptoms 1" = emotion_w1,
    "Self-Control 1" = self_control_w1,
    "Parental Warmth 2" = parental_warmth_w2,
    "Peer Support 2" = peer_support_w2,
    "Emotional Symptoms 2" = emotion_w2,
    "Self-Control 2" = self_control_w2,
    "Parental Warmth 3" = parental_warmth_w3,
    "Peer Support 3" = peer_support_w3,
    "Emotional Symptoms 3" = emotion_w3,
    "Self-Control 3" = self_control_w3
  )

# Calculate overall descriptive statistics for each wave
wave1_descriptive <- variable_final_df %>%
  select(`Parental Warmth 1`, `Peer Support 1`, `Emotional Symptoms 1`, `Self-Control 1`) %>%
  describe() %>%
  as.data.frame() %>%
  select(mean, sd) %>%
  mutate(mean = round(mean, 2), sd = round(sd, 2))

wave2_descriptive <- variable_final_df %>%
  select(`Parental Warmth 2`, `Peer Support 2`, `Emotional Symptoms 2`, `Self-Control 2`) %>%
  describe() %>%
  as.data.frame() %>%
  select(mean, sd) %>%
  mutate(mean = round(mean, 2), sd = round(sd, 2))

wave3_descriptive <- variable_final_df %>%
  select(`Parental Warmth 3`, `Peer Support 3`, `Emotional Symptoms 3`, `Self-Control 3`) %>%
  describe() %>%
  as.data.frame() %>%
  select(mean, sd) %>%
  mutate(mean = round(mean, 2), sd = round(sd, 2))

# Display the descriptive statistics for each wave
wave1_descriptive
wave2_descriptive
wave3_descriptive

final_total_participants <- final_df_clean %>% 
  summarize(Count = n())
final_total_participants

#-------------SEM: parental warmth vs emotional symptoms----------------------
library(lavaan)

m1_urs <- "
  parental_warmth_w2 ~ 1 + emotion_w1 + parental_warmth_w1
  parental_warmth_w3 ~ 1 + emotion_w2 + parental_warmth_w2
  emotion_w2 ~ 1 + parental_warmth_w1 + emotion_w1
  emotion_w3 ~ 1 + parental_warmth_w2 + emotion_w2

  parental_warmth_w1 ~~ emotion_w1
  parental_warmth_w2 ~~ emotion_w2
  parental_warmth_w3 ~~ emotion_w3
"

m1_urs <- sem(m1_urs, data = final_df_clean)
summary(m1_urs)

m1_rs <- "parental_warmth_w2 ~ 1 + emotion_w1 + a*parental_warmth_w1
          parental_warmth_w3 ~ 1 + emotion_w2 + a*parental_warmth_w2
          emotion_w2 ~ 1 + parental_warmth_w1 + b*emotion_w1
          emotion_w3 ~ 1 + parental_warmth_w2 + b*emotion_w2

          parental_warmth_w1 ~~ emotion_w1
          parental_warmth_w2 ~~ emotion_w2
          parental_warmth_w3 ~~ emotion_w3"

m1_rs <- sem(m1_rs, data = final_df_clean)
summary(m1_rs)

stand_m1 <- standardizedsolution(m1_rs)
stand_m1

anova(m1_urs, m1_rs) 
#Chi-square indicates that restricted model is not significantly better, so keep the urs model. 

#final clm:
m1_clm <- sem(m1_rs, data = final_df_clean, estimator = "MLR")
summary(m1_clm, fit.measures = TRUE, standardized = TRUE)

#---------- SEM: parental warmth vs self-control -----------------
m2_urs <- "parental_warmth_w3 ~ 1 + self_control_w2 + parental_warmth_w2
          self_control_w3 ~ 1 + parental_warmth_w2 + self_control_w2
          parental_warmth_w2 ~ 1 + self_control_w1 + parental_warmth_w1
          self_control_w2 ~ 1 + parental_warmth_w1 + self_control_w1

          parental_warmth_w2 ~~ self_control_w2
          parental_warmth_w3 ~~ self_control_w3
          parental_warmth_w1 ~~ self_control_w1"

m2_urs <- sem(m2_urs, data = final_df_clean)
summary(m2_urs)

m2_rs <- "parental_warmth_w3 ~ 1 + self_control_w2 + a*parental_warmth_w2
          self_control_w3 ~ 1 + parental_warmth_w2 + b*self_control_w2
          parental_warmth_w2 ~ 1 + self_control_w1 + a*parental_warmth_w1
          self_control_w2 ~ 1 + parental_warmth_w1 + b*self_control_w1

          parental_warmth_w2 ~~ self_control_w2
          parental_warmth_w3 ~~ self_control_w3
          parental_warmth_w1 ~~ self_control_w1"

m2_rs <- sem(m2_rs, data = final_df_clean)
summary(m2_rs)

anova(m2_urs, m2_rs) 
#Chi-square indicates that restricted model is not significantly better, so keep the urs model. 

stand_m2 <- standardizedsolution(m2_rs)
stand_m2

#final clm:
m2_clm <- sem(m2_rs, data = final_df_clean, estimator = "MLR")
summary(m2_clm, fit.measures = TRUE, standardized = TRUE)

#---------- SEM: peer support vs emotional symptoms -----------------
m3_urs <- "peer_support_w3 ~ 1 + emotion_w2 + peer_support_w2
          emotion_w3 ~ 1 + peer_support_w2 + emotion_w2
          peer_support_w2 ~ 1 + emotion_w1 + peer_support_w1
          emotion_w2 ~ 1 + peer_support_w1 + emotion_w1

          peer_support_w2 ~~ emotion_w2
          peer_support_w3 ~~ emotion_w3
          peer_support_w1 ~~ emotion_w1"

m3_urs <- sem(m3_urs, data = final_df_clean)
summary(m3_urs)

stand_m3 <- standardizedsolution(m3_urs)
stand_m3

m3_rs <- "peer_support_w3 ~ 1 + emotion_w2 + b*peer_support_w2
                      emotion_w3 ~ 1 + b*peer_support_w2 + c*emotion_w2
                      peer_support_w2 ~ 1 + emotion_w1 + b*peer_support_w1
                      emotion_w2 ~ 1 + b*peer_support_w1 + c*emotion_w1
                      
                      peer_support_w2 ~~ emotion_w2
                      peer_support_w3 ~~ emotion_w3
                      peer_support_w1 ~~ emotion_w1
                      "

m3_rs <- sem(m3_rs, data = final_df_clean)
summary(m3_rs)

anova(m3_urs, m3_rs)
#Chi-square comparison indicates that restricted model has a significantly poorer fit to data (p< .001), so the unrestricted model is kept. 

#final clm:
m3_clm <- sem(m3_urs, data = final_df_clean, estimator = "MLR")
summary(m3_clm, fit.measures = TRUE, standardized = TRUE)

#---------- SEM: peer support vs behavioural (self-control) outcomes-----------------

m4_urs <- "peer_support_w3 ~ 1 + self_control_w2 + peer_support_w2
          self_control_w3 ~ 1 + peer_support_w2 + self_control_w2
          peer_support_w2 ~ 1 + self_control_w1 + peer_support_w1
          self_control_w2 ~ 1 + peer_support_w1 + self_control_w1

          peer_support_w2 ~~ self_control_w2
          peer_support_w3 ~~ self_control_w3
          peer_support_w1 ~~ self_control_w1"

m4_urs <- sem(m4_urs, data = final_df_clean)
summary(m4_urs)

stand_m4 <- standardizedsolution(m4_urs)
stand_m4

m4_rs <- "peer_support_w3 ~ 1 + self_control_w2 + b*peer_support_w2
                      self_control_w3 ~ 1 + b*peer_support_w2 + c*self_control_w2
                      peer_support_w2 ~ 1 + self_control_w1 + b*peer_support_w1
                      self_control_w2 ~ 1 + b*peer_support_w1 + c*self_control_w1
                      
                      peer_support_w2 ~~ self_control_w2
                      peer_support_w3 ~~ self_control_w3
                      peer_support_w1 ~~ self_control_w1
                      "

m4_rs <- sem(m4_rs, data = final_df_clean)
summary(m4_rs)

anova(m4_urs, m4_rs)
#Model comparison indicates that unrestricted model has a poorer fit to data (p< .001), so the unrestricted model is kept.

#final clm:
m4_clm <- sem(m4_urs, data = final_df_clean, estimator = "MLR")
summary(m4_clm, fit.measures = TRUE, standardized = TRUE)

#_____________ GRAPHS____________________

#-------------Plotting correlation matrix-----------------------------
# Function to add significance stars
add_stars <- function(p_values) {
  stars <- ifelse(p_values < 0.001, "***", ifelse(p_values < 0.01, "**", ifelse(p_values < 0.05, "*", "")))
  return(stars)
}

# Generate labels with stars
cor_labels <- matrix(paste0(round(corr_matrix, 2), add_stars(p_matrix)), nrow = nrow(corr_matrix))

# Create a data frame for plotting
plot_data <- data.frame(
  row = rep(rownames(corr_matrix), ncol(corr_matrix)),
  col = rep(colnames(corr_matrix), each = nrow(corr_matrix)),
  corr = as.vector(corr_matrix),
  p_value = as.vector(p_matrix),
  label = as.vector(cor_labels)
)

cor_vars <- final_df_clean[, c("parental_warmth_w1", "parental_warmth_w2", "parental_warmth_w3", "peer_support_w1", "peer_support_w2", "peer_support_w3", "emotion_w1", "emotion_w2", "emotion_w3", "self_control_w1", "self_control_w2", "self_control_w3")]

correlation_result <- corr.test(cor_vars, use = "pairwise.complete.obs")
correlation_result

corr_matrix <- correlation_result$r
p_matrix <- correlation_result$p

new_labels <- c("Parental\nWarmth\nW1", "Parental\nWarmth\nW2", "Parental\nWarmth\nW3",
                "Peer Support\nW1", "Peer Support\nW2", "Peer Support\nW3",
                "Emotion W1", "Emotion W2", "Emotion W3",
                "Self Control\nW1", "Self Control\nW2", "Self Control\nW3")

# Apply new labels to the correlation matrix and p-value matrix
dimnames(corr_matrix) <- list(new_labels, new_labels)
dimnames(p_matrix) <- list(new_labels, new_labels)

# Save the plot with specified format
png(filename = "../team_1_project/plots/correlation_matrix.png", width = 1000, height = 1000, units = "px")
par(mar = c(12, 12, 4, 2)) # Adjust margins to prevent label cutoff
corPlot(corr_matrix, upper = FALSE, numbers = TRUE, diag = FALSE, stars = TRUE, pval = p_matrix, 
        main = "Correlation Plot of All Variables Across the Three Waves", cex = 1.3, cex.axis = 1.3, xlas = 2)
dev.off()
#---------------------------CROSS LAG MODEL CORRELATIONS----------------------
# Extract standardized solutions from the final models
stand_m1_clm <- standardizedsolution(m1_clm)
stand_m2_clm <- standardizedsolution(m2_clm)
stand_m3_clm <- standardizedsolution(m3_clm)
stand_m4_clm <- standardizedsolution(m4_clm)

# Combine standardized solutions into one dataframe
stand_combined <- bind_rows(
  stand_m1_clm %>% mutate(model = "Parental Warmth - Emotional Symptoms"),
  stand_m2_clm %>% mutate(model = "Parental Warmth - Self-Control"),
  stand_m3_clm %>% mutate(model = "Peer Support - Emotional Symptoms"),
  stand_m4_clm %>% mutate(model = "Peer Support - Self-Control")
)

# Select relevant columns
stand_combined <- stand_combined %>%
  select(model, rhs, lhs, est.std, pvalue, op) %>%
  rename(Model = model, Predictor = rhs, Outcome = lhs, `Standardized Beta` = est.std, `P-Value` = pvalue, Type = op)

# Add significance stars based on p-value
stand_combined <- stand_combined %>%
  mutate(Significance = case_when(
    `P-Value` < 0.001 ~ "***",
    `P-Value` < 0.01 ~ "**",
    `P-Value` < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Set empty values for intercepts and specify type
stand_combined <- stand_combined %>%
  mutate(Predictor = ifelse(Type == "~1", "", Predictor),
         Type = case_when(
           Type == "~" ~ "Regression",
           Type == "~~" ~ "Covariance",
           Type == "~1" ~ "Intercept",
           TRUE ~ Type
         ))

# Filter to ensure exactly three covariances per model
filtered_covariances <- stand_combined %>%
  filter(Type == "Covariance") %>%
  group_by(Model) %>%
  filter(row_number() <= 3) %>%
  ungroup()

# Combine with regression and intercepts
stand_combined_filtered <- stand_combined %>%
  filter(Type != "Covariance") %>%
  bind_rows(filtered_covariances)

# Separate the data for parental warmth and peer support
parental_warmth_data <- stand_combined_filtered %>%
  filter(grepl("Parental Warmth", Model))

peer_support_data <- stand_combined_filtered %>%
  filter(grepl("Peer Support", Model))

# Function to format standardized beta values with 2 decimal places and stars
format_beta <- function(beta, sig) {
  formatted_beta <- sprintf("%.2f", beta)
  return(paste0(formatted_beta, sig))
}

# Format the standardized beta values
parental_warmth_data <- parental_warmth_data %>%
  mutate(`Standardized Beta` = mapply(format_beta, `Standardized Beta`, Significance)) %>%
  select(-Significance, -`P-Value`)

peer_support_data <- peer_support_data %>%
  mutate(`Standardized Beta` = mapply(format_beta, `Standardized Beta`, Significance)) %>%
  select(-Significance, -`P-Value`)

# Create the table for Parental Warmth
parental_warmth_table <- parental_warmth_data %>%
  arrange(Model, Type, Predictor, Outcome) %>%
  gt(groupname_col = "Model") %>%
  tab_header(
    title = "Standardized Beta Coefficients of Cross-Lagged Models",
    subtitle = "Parental Warmth: Emotional Symptoms and Self-Control"
  ) %>%
  cols_label(
    Type = "Type",
    Predictor = "Predictor",
    Outcome = "Outcome",
    `Standardized Beta` = "Standardized Beta"
  )

# Create the table for Peer Support
peer_support_table <- peer_support_data %>%
  arrange(Model, Type, Predictor, Outcome) %>%
  gt(groupname_col = "Model") %>%
  tab_header(
    title = "Standardized Beta Coefficients of Cross-Lagged Models",
    subtitle = "Peer Support: Emotional Symptoms and Self-Control"
  ) %>%
  cols_label(
    Type = "Type",
    Predictor = "Predictor",
    Outcome = "Outcome",
    `Standardized Beta` = "Standardized Beta"
  )
# Display the tables
parental_warmth_table
peer_support_table

gtsave(parental_warmth_table, "../team_1_project/plots/parental_warmth_table.png")
gtsave(peer_support_table, "../team_1_project/plots/peer_support_table.png")

#-------------------CROSS LAG MODELS--------------------

#visualisation matrix
layout_1 <- matrix(c(
  "parental_warmth_w1", "parental_warmth_w2", "parental_warmth_w3",
  "emotion_w1", "emotion_w2", "emotion_w3"
), nrow = 2, byrow = TRUE)

# Visualize the model with the custom layout
png(filename = "../team_1_project/plots/clm_1.png", width = 900, height = 900, units = "px")
clm_1_visual <- semPaths(m1_clm, whatLabels = "std",
         layout = layout_1,
         edge.label.cex = 1.2,
         curvePivot = TRUE,
         color = list(lat = "lightblue", man = "lightblue"),
         label.cex = 1.2,
         sizeMan = 13,
         sizeLat = 13,
         residuals = FALSE, # To hide residuals
         intercepts = FALSE, # To hide intercepts
         nCharNodes = 0, # To ensure full variable names are shown
         edge.label.position = 0.3) # Adjust edge label position
dev.off()

layout_2 <- matrix(c(
  "parental_warmth_w1", "parental_warmth_w2", "parental_warmth_w3",
  "self_control_w1", "self_control_w2", "self_control_w3"
), nrow = 2, byrow = TRUE)

png(filename = "../team_1_project/plots/clm_2.png", width = 900, height = 900, units = "px")
clm_2_visual <- semPaths(m2_clm, whatLabels = "std",
                  layout = layout_2,
                  edge.label.cex = 1.2,
                  curvePivot = TRUE,
                  color = list(lat = "lightblue", man = "lightblue"),
                  label.cex = 1.2,
                  sizeMan = 13,
                  sizeLat = 13,
                  residuals = FALSE, # To hide residuals
                  intercepts = FALSE, # To hide intercepts
                  nCharNodes = 0, # To ensure full variable names are shown
                  edge.label.position = 0.3) # Adjust edge label position
dev.off()

layout_3 <- matrix(c(
  "peer_support_w1", "peer_support_w2", "peer_support_w3",
  "emotion_w1", "emotion_w2", "emotion_w3"
), nrow = 2, byrow = TRUE)

# Visualize the model with the custom layout
png(filename = "../team_1_project/plots/clm_3.png", width = 900, height = 900, units = "px")
clm_3_visual <- semPaths(m3_clm, whatLabels = "std",
                  layout = layout_3,
                  edge.label.cex = 1.2,
                  curvePivot = TRUE,
                  color = list(lat = "lightblue", man = "lightblue"),
                  label.cex = 1.2,
                  sizeMan = 13,
                  sizeLat = 13,
                  residuals = FALSE, # To hide residuals
                  intercepts = FALSE, # To hide intercepts
                  nCharNodes = 0, # To ensure full variable names are shown
                  edge.label.position = 0.3) # Adjust edge label position
dev.off()

layout_4 <- matrix(c(
  "peer_support_w1", "peer_support_w2", "peer_support_w3",
  "self_control_w1", "self_control_w2", "self_control_w3"
), nrow = 2, byrow = TRUE)

png(filename = "../team_1_project/plots/clm_4.png", width = 900, height = 900, units = "px")
clm_4_visual <- semPaths(m4_clm, whatLabels = "std",
                  layout = layout_4,
                  edge.label.cex = 1.2,
                  curvePivot = TRUE,
                  color = list(lat = "lightblue", man = "lightblue"),
                  label.cex = 1.2,
                  sizeMan = 13,
                  sizeLat = 13,
                  residuals = FALSE, # To hide residuals
                  intercepts = FALSE, # To hide intercepts
                  nCharNodes = 0, # To ensure full variable names are shown
                  edge.label.position = 0.3) # Adjust edge label position
dev.off()

# Plot each clm object individually
plot(clm_1_visual)
plot(clm_2_visual)
plot(clm_3_visual)
plot(clm_4_visual)

