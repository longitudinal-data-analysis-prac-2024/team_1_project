library(haven)
library(foreign)
library(tidyverse)

df <- read.spss("q1_dataset.sav", to.data.frame = T)

#------------------ IV: Maternal Warmth-------------------------------------------
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
  #SDQ inattention: i) SDQ: inattention outcomes subscale dataframe
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

#1) self control  reverse code
self_control <- rv_vars(self_control, 6, c('C1_SControl2', 'C1_SControl3', 'C1_SControl4', 'C1_SControl5', 'C1_SControl7', 'C1_SControl9', 'C1_SControl10', 'C1_SControl12', 'C1_SControl13'))

self_control <- rv_vars(self_control, 6, c('C2_SControl2', 'C2_SControl3', 'C2_SControl4', 'C2_SControl5', 'C2_SControl7', 'C2_SControl9', 'C2_SControl10', 'C2_SControl12', 'C2_SControl13'))

self_control <- rv_vars(self_control, 6, c('C3_SControl2', 'C3_SControl3', 'C3_SControl4', 'C3_SControl5', 'C3_SControl7', 'C3_SControl9', 'C3_SControl10', 'C3_SControl12', 'C3_SControl13'))

# Calculate self-control composite score for Wave 1
self_control <- cl_df(13, 1, self_control, c('C1_SControl1', 'C1_SControl2', 'C1_SControl3', 'C1_SControl4', 'C1_SControl5', 'C1_SControl6','C1_SControl7', 'C1_SControl8','C1_SControl9', 'C1_SControl10', 'C1_SControl11','C1_SControl12', 'C1_SControl13')) %>%
  rename(self_control_w1 = mean_score)

# Calculate self-control composite score for Wave 2
self_control <- cl_df(13, 1, self_control, c('C2_SControl1', 'C2_SControl2', 'C2_SControl3', 'C2_SControl4', 'C2_SControl5', 'C2_SControl6','C2_SControl7', 'C2_SControl8','C2_SControl9', 'C2_SControl10', 'C2_SControl11','C2_SControl12', 'C2_SControl13')) %>%
  rename(self_control_w2 = mean_score)

# Calculate self-control composite score for Wave 3
self_control <- cl_df(13, 1, self_control, c('C3_SControl1', 'C3_SControl2', 'C3_SControl3', 'C3_SControl4', 'C3_SControl5', 'C3_SControl6','C3_SControl7', 'C3_SControl8','C3_SControl9', 'C3_SControl10', 'C3_SControl11','C3_SControl12', 'C3_SControl13')) %>%
  rename(self_control_w3 = mean_score)

# Create final_df with ID and composite scores
final_df <- df %>%
  select(ID, parental_warmth_w1, parental_warmth_w2, parental_warmth_w3) %>%
  left_join(select(loneliness_recode, ID, peer_support_w1, peer_support_w2, peer_support_w3), by = "ID") %>%
  left_join(select(sdq_emotion, ID, emotion_w1, emotion_w2, emotion_w3)) %>% 
  left_join(select(self_control, ID, self_control_w1, self_control_w2, self_control_w3), by = "ID")

final_df
<<<<<<< HEAD
=======

#--------------------- SEM: parental warmth vs emotional symptoms ----------------------
library(lavaan)

m1_urs <- "parental_warmth_w3 ~ 1 + emotion_w2 + parental_warmth_w2
          emotion_w3 ~ 1 + parental_warmth_w2 + emotion_w2
          parental_warmth_w2 ~ 1 + emotion_w1 + parental_warmth_w1
          emotion_w2 ~ 1 + parental_warmth_w1 + emotion_w1

          parental_warmth_w2 ~~ emotion_w2
          parental_warmth_w3 ~~ emotion_w3
          parental_warmth_w1 ~~ emotion_w1"

m1_urs <- sem(m1_urs, data = final_df)
summary(m1_urs)

m1_rs <- "parental_warmth_w3 ~ 1 + a*emotion_w2 + parental_warmth_w2
          emotion_w3 ~ 1 + a*parental_warmth_w2 + emotion_w2
          parental_warmth_w2 ~ 1 + emotion_w1 + parental_warmth_w1
          emotion_w2 ~ 1 + parental_warmth_w1 + emotion_w1

          parental_warmth_w2 ~~ emotion_w2
          parental_warmth_w3 ~~ emotion_w3
          parental_warmth_w1 ~~ emotion_w1"

m1_rs <- sem(m1_rs, data = final_df)

anova(m1_urs, m1_rs) 

#---------- SEM: parental warmth vs self-control -----------------
m2_urs <- "parental_warmth_w3 ~ 1 + self_control_w2 + parental_warmth_w2
          self_control_w3 ~ 1 + parental_warmth_w2 + self_control_w2
          parental_warmth_w2 ~ 1 + self_control_w1 + parental_warmth_w1
          self_control_w2 ~ 1 + parental_warmth_w1 + self_control_w1

          parental_warmth_w2 ~~ self_control_w2
          parental_warmth_w3 ~~ self_control_w3
          parental_warmth_w1 ~~ self_control_w1"

m2_urs <- sem(m2_urs, data = final_df)
summary(m2_urs)

m2_rs <- "parental_warmth_w3 ~ 1 + a*self_control_w2 + parental_warmth_w2
          self_control_w3 ~ 1 + a*parental_warmth_w2 + self_control_w2
          parental_warmth_w2 ~ 1 + self_control_w1 + parental_warmth_w1
          self_control_w2 ~ 1 + parental_warmth_w1 + self_control_w1

          parental_warmth_w2 ~~ self_control_w2
          parental_warmth_w3 ~~ self_control_w3
          parental_warmth_w1 ~~ self_control_w1"

m2_rs <- sem(m2_rs, data = final_df)

anova(m2_urs, m2_rs) 
>>>>>>> 82c63c977dd85b99ad103de33638f34e7f3a6fe1
