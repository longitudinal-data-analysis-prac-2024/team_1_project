library(haven)
library(foreign)
library(tidyverse)

dataset_1 <- read.spss("q1_dataset.sav", to.data.frame = T)
dataset_1

#ii. Peer support influence emotional and cognitive outcomes

#IV: 
# Peer Support variable - loneliness
peersupport <- as_tibble(dataset_1)

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

#code to change all NA to 0: loneliness_recode[is.na(loneliness_recode)] <- 0

print(loneliness_recode)

#ii. composite loneliness scores for peer support score:(higher score = more peer support)
#WAVE 1:
peersupport_w1 <- loneliness_recode %>% 
  mutate(
    peersupport_score_w1 = rowMeans(select(., C1_L1:C1_L16), na.rm = TRUE) * 16)

peersupport_w1 %>% 
  select(ID, peersupport_score_w1)

  #standardize wave 1 loneliness to match wave 2 & wave 3:
peersupport_w1match <- loneliness_recode %>% 
  mutate(
    peersupport_score_w1match = rowMeans(select(., C1_L1, C1_L2, C1_L6, C1_L7, C1_L11, C1_L14, C1_L15), na.rm = TRUE) * 7,
  )

peersupport_w1match %>% 
  select(ID, peersupport_score_w1match)

#WAVE 2: 
peersupport_w2 <- loneliness_recode %>% 
  mutate(
    peersupport_score_W2 = rowMeans(select(., C2_L1:C2_L15), na.rm = TRUE) * 7)

peersupport_w2 %>% 
  select(ID, peersupport_score_W2)

#WAVE 3:
peersupport_w3 <- loneliness_recode %>% 
  mutate(
    peersupport_score_W3 = rowMeans(select(., C3_L1:C3_L15), na.rm = TRUE) * 7)

peersupport_w3 %>% 
  select(ID, peersupport_score_W3)

#DV: 
# i) SDQ: emotional outcomes subscale dataframe
sdq_emotion <- as_tibble(dataset_1) %>% 
  select(ID, C1_B3, C1_B8, C1_B13, C1_B16, C1_B24, C2_B3, C2_B8, C2_B13, C2_B16, C2_B24, C3_B3, C3_B8, C3_B13, C3_B16, C3_B24) %>%
  unique() %>%
  arrange(ID) %>% 
  mutate(across(C1_B3:C3_B24, ~ recode(., "Not true" = 0, "Sort of true" = 1, "Certainly true" = 2))) %>%
  mutate(across(C1_B3:C3_B24, ~ as.numeric(.)))

print(sdq_emotion)

#ii. composite emotional SDQ scores:(higher score = more emotional problem)
#WAVE 1:
sdq_emotion_w1 <- sdq_emotion %>% 
  mutate(
    emotion_score_w1 = rowMeans(select(., C1_B3:C1_B24), na.rm = TRUE) * 5)

sdq_emotion_w1 %>% 
  select(ID, emotion_score_w1)

#WAVE 2:
sdq_emotion_w2 <- sdq_emotion %>% 
  mutate(
    emotion_score_w2 = rowMeans(select(., C2_B3:C2_B24), na.rm = TRUE) * 5)

sdq_emotion_w2 %>% 
  select(ID, emotion_score_w2)

sdq_emotion_w3<- sdq_emotion %>% 
  mutate(
    emotion_score_w3 = rowMeans(select(., C3_B3:C3_B24), na.rm = TRUE) * 5)

sdq_emotion_w3 %>% 
  select(ID, emotion_score_w3)

# ii) Brief Self-Control Scale (BSCS): cognitive outcomes

