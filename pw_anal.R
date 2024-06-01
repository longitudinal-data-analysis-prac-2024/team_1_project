library(haven)
library(foreign)

df <- read.spss("q1_dataset.sav", to.data.frame = T)

library(tidyverse)

#i. the influence of parental warmth on emotional and behavioral regulation in first year secondary school 

#---------------------iv: parental warmth--------------------------------------------------------

#1) recode parental warmth and hostility (w1,2,3) into numeric 

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


#3) calculate parental warmth score for each child in each wave 

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

df <- df %>% rename(pw1 = mean_score)


for(ID in length(df$ID)){
  df <- cl_df(4,6,df,c('C2_PWHA2', 'C2_PWHA5', 'C2_PWHA6', 'C2_PWHA7', 'C2_PWHA9', 'C2_PWHA10'))
}

df <- df %>% rename(pw2 = mean_score)

for(ID in length(df$ID)){
  df <- cl_df(4,6,df,c('C3_PWHA2', 'C3_PWHA5', 'C3_PWHA6', 'C3_PWHA7', 'C3_PWHA9', 'C3_PWHA10'))
}

df <- df %>% rename(pw3 = mean_score)

  


#--------------dv1:emotional symptoms (calculated from the sdq)-----------------

recode_emo <- function(df, columns) {
  for (col in columns) {
    col_sym <- ensym(col) 
    df <- df %>%
      mutate(!!col_sym := recode(!!col_sym,
                                 "Not true" = 0,
                                 "Sort of true" = 1,
                                 "Certainly true" = 2
                                 
      ))
  }
  return(df)
}

df <- recode_emo(df, c('C1_B3', 'C1_B8', 'C1_B13', 'C1_B16', 'C1_B24'))
df <- recode_emo(df, c('C2_B3', 'C2_B8', 'C2_B13', 'C2_B16', 'C2_B24'))
df <- recode_emo(df, c('C3_B3', 'C3_B8', 'C3_B13', 'C3_B16', 'C3_B24'))

df <- df %>% mutate(emo_w1 = round(rowMeans(select(., c('C1_B3', 'C1_B8', 'C1_B13', 'C1_B16', 'C1_B24')), na.rm = T)*5))
df <- df %>% mutate(emo_w2 = round(rowMeans(select(., c('C2_B3', 'C2_B8', 'C2_B13', 'C2_B16', 'C2_B24')), na.rm = T)*5))
df <- df %>% mutate(emo_w3 = round(rowMeans(select(., c('C3_B3', 'C3_B8', 'C3_B13', 'C3_B16', 'C3_B24')), na.rm = T)*5))

#---------------dv2: behavioral regulation (hyperactivity + self-control) ----------------------------------------

