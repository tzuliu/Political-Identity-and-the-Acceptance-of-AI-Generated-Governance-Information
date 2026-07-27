rm(list=ls(all=TRUE))
########################
####Loading packages####
########################
####Data science related
library(dplyr)
library(tidyr)
library(tibble)
####轉換regression結果為tidy
library(broom)
####rescale data
library(scales)
####處理string
library(stringr)
####For PCA 
library(factoextra)
####For Cornbach's alpha
library(psych)
####ggplot2 related
library(ggrepel)
####中文字型相關
library(showtext)
library(sysfonts)
library(ragg)
####讀取字體
font_add("taipei", regular = "/Users/tpliu/Desktop/UTaipei/國發會/知識圖譜範例/TaipeiSansTCBeta-Regular.ttf")
showtext_auto()  # 讓 ggplot 繪圖時使用 showtext

####################
####Loading data####
####################
ai <- readRDS("/Users/tpliu/Desktop/Projects/AI/Data/ai.rds")

################################################################
#### Excluding Independents and Non-Big 3 Party Identifiers ####
################################################################
ai <- ai %>% 
  filter(
    Q18 != "其他政黨（可於下方空格輸入政黨名稱）",
    Q22 != "其他政黨（可於下方空格輸入政黨名稱）",
    Q22 != "沒有任何政黨偏好"
  )

#####################################################
#### Coalacing Responses of the 4 Dimensions of  ####
#### Acceptance of Legislator Eval across Groups ####
#####################################################
ai <- ai %>%
  mutate(across(c(Q42_1, Q46_1, Q50_1, Q54_1, Q58_1),#Diemnsion 1
                ~ na_if(str_squish(as.character(.)), "")),
         consistent = coalesce(Q42_1, Q46_1, Q50_1, Q54_1, Q58_1)) %>%
  mutate(across(c(Q42_2, Q46_2, Q50_2, Q54_2, Q58_2),#Dimension 2
                ~ na_if(str_squish(as.character(.)), "")),
         correct = coalesce(Q42_2, Q46_2, Q50_2, Q54_2, Q58_2)) %>%
  mutate(across(c(Q42_3, Q46_3, Q50_3, Q54_3, Q58_3),#Dimension 3
                ~ na_if(str_squish(as.character(.)), "")),
         replacement = coalesce(Q42_3, Q46_3, Q50_3, Q54_3, Q58_3)) %>%
  mutate(across(c(Q42_4, Q46_4, Q50_4, Q54_4, Q58_4),#Dimension 4
                ~ na_if(str_squish(as.character(.)), "")),
         accept = coalesce(Q42_4, Q46_4, Q50_4, Q54_4, Q58_4)) 

########################################################################
#### New Dataframe of 4 Dimensions of Acceptance of Legislator Eval ####
########################################################################
legis <- ai %>%
  transmute(consistent = as.numeric(ai$consistent),
            correct = as.numeric(ai$correct),
            replacement = as.numeric(ai$replacement),
            accept = as.numeric(ai$accept))

#############
#### PCA ####
#############
pca_legis <- prcomp(legis, scale. = TRUE)
summary(pca_legis)#Proportion of Variance indicates that PC1 explains
                  #88.83% of total variance

##############################################################################
#### Taking the Scores on PC1 as the New Single Vairable for 4 Dimensions ####
##############################################################################
pc_legis_scores <- predict(pca_legis)[, "PC1"]
legis_row_mean <- rowMeans(legis, na.rm = TRUE)
if (cor(pc_legis_scores, legis_row_mean, use = "pairwise.complete.obs") < 0) {
  pc_legis_scores <- -pc_legis_scores
}#Make Sure thatr The Directions of Scores are Consistent with 4 Dimensions
ai <- ai %>%#Transferring New Scores into a 0-10 Scale
  mutate(legis_score = scales::rescale(pc_legis_scores, to = c(0, 10)))

########################################################
#### Coalacing Responses of the 4 Dimensions of     ####
#### Acceptance of Univ Material Eval across Groups ####
########################################################
###########################################################################
#### New Dataframe of 4 Dimensions of Acceptance of Univ Material Eval ####
###########################################################################
material <- ai %>%
  transmute(across(c(Q38_1, Q38_2, Q38_3, Q38_4
  ),
  ~ as.numeric(.)))

#############
#### PCA ####
#############
pca_material <- prcomp(material, scale. = TRUE)
summary(pca_material)#Proportion of Variance indicates that PC1 explains
                     #77.42% of total variance

##############################################################################
#### Taking the Scores on PC1 as the New Single Vairable for 4 Dimensions ####
##############################################################################
pc_material_scores <- predict(pca_material)[, "PC1"]
material_row_mean <- rowMeans(material, na.rm = TRUE)
if (cor(pc_material_scores, material_row_mean, use = "pairwise.complete.obs") < 0) {
  pc_material_scores <- -pc_material_scores
}#Make Sure thatr The Directions of Scores are Consistent with 4 Dimensions
ai <- ai %>%#Transferring New Scores into a 0-10 Scale
  mutate(material_score = scales::rescale(pc_material_scores, to = c(0, 10)))

#########################################################
#### Coalacing Responses of the 4 Dimensions of      ####
#### Acceptance of Univ Interview Eval across Groups ####
########################################################
############################################################################
#### New Dataframe of 4 Dimensions of Acceptance of Univ Interview Eval ####
############################################################################
interview <- ai %>%
  transmute(Q40_1 = as.numeric(ai$Q40_1),
            Q40_2 = as.numeric(ai$Q40_2),
            Q40_3 = as.numeric(ai$Q40_3),
            Q40_4 = as.numeric(ai$Q40_4))

#############
#### PCA ####
#############
pca_interview <- prcomp(interview, scale. = TRUE)
summary(pca_interview)#Proportion of Variance indicates that PC1 explains
                      #83.78% of total variance

##############################################################################
#### Taking the Scores on PC1 as the New Single Vairable for 4 Dimensions ####
##############################################################################
pc_interview_scores <- predict(pca_interview)[, "PC1"]
interview_row_mean <- rowMeans(interview, na.rm = TRUE)
if (cor(pc_interview_scores, interview_row_mean, use = "pairwise.complete.obs") < 0) {
  pc_interview_scores <- -pc_interview_scores
}#Make Sure thatr The Directions of Scores are Consistent with 4 Dimensions
ai <- ai %>%#Transferring New Scores into a 0-10 Scale
  mutate(interview_score = scales::rescale(pc_interview_scores, to = c(0, 10)))

#################################################
#### Output RDS with Scores as New Variables ####
#################################################
saveRDS(ai, file = "/Users/tpliu/Desktop/Projects/AI/Data/ai-analysis.rds")

