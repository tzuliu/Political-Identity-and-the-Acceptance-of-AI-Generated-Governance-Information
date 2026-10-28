rm(list=ls(all=TRUE))
########################
####Loading packages####
########################
####Data science related
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
####轉換regression結果為tidy
library(broom)
####rescale data
library(scales)
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

######################
#### Loading data ####
######################
ai <- readRDS("/Users/tpliu/Desktop/Projects/AI/Data/ai-analysis.rds")


####################################################
#### New Variable "group" for Assignment Groups ####
####################################################
ai_fisher <- ai %>%
  mutate(group = case_when(
    !is.na(Q42_1) ~ "control",
    !is.na(Q46_1) ~ "treatment1",
    !is.na(Q50_1) ~ "treatment2",
    !is.na(Q54_1) ~ "treatment3",
    !is.na(Q58_1) ~ "treatment4",
  )) 

##################################################################################
#### New Variable "category" for Copartisan or Not with Legislators and Media ####
##################################################################################
ai_fisher <- ai_fisher %>%
  mutate(
    category = case_when(
      # Copartisan with Media but not Legislators
      (Q18 == "中國國民黨" & group == "treatment1") | (Q18 == "臺灣民眾黨" & group == "treatment1") |
        (Q18 == "民主進步黨" & group == "treatment4") |
        (Q22 == "中國國民黨" & group == "treatment1") | (Q22 == "臺灣民眾黨" & group == "treatment1") |
        (Q22 == "民主進步黨" & group == "treatment4") ~ "MCLI",
      
      # Copartisan with Media and Legislators
      (Q18 == "中國國民黨" & group == "treatment2") | (Q18 == "臺灣民眾黨" & group == "treatment2") |
        (Q18 == "民主進步黨" & group == "treatment3") | 
        (Q22 == "中國國民黨" & group == "treatment2") | (Q22 == "臺灣民眾黨" & group == "treatment2") | 
        (Q22 == "民主進步黨" & group == "treatment3") ~ "MCLC",
      
      # Copartisan with Legislators but not Media
      (Q18 == "中國國民黨" & group == "treatment4") | (Q18 == "臺灣民眾黨" & group == "treatment4") |
        (Q18 == "民主進步黨" & group == "treatment1") | 
        (Q22 == "中國國民黨" & group == "treatment4") | (Q22 == "臺灣民眾黨" & group == "treatment4") | 
        (Q22 == "民主進步黨" & group == "treatment1") ~ "MILC",
      
      # Non-Copartisan with Media and Legislators
      (Q18 == "中國國民黨" & group == "treatment3") | (Q18 == "臺灣民眾黨" & group == "treatment3") |
        (Q18 == "民主進步黨" & group == "treatment2") | 
        (Q22 == "中國國民黨" & group == "treatment3") | (Q22 == "臺灣民眾黨" & group == "treatment3") | 
        (Q22 == "民主進步黨" & group == "treatment2") ~ "MILI",
      
      # control group
      (group == "control") ~ "control",
      
      # 其他未涵蓋 → NA
      TRUE ~ NA_character_
    )
  )


############################################
#### Rearranging the levels of category ####
############################################
ai_fisher <- ai_fisher %>%
  mutate(category = factor(
    category,
    levels = c("control", "MCLC", "MCLI", "MILC", "MILI")
  ))

##########################################
#### Function of Fisher's R to Z Test ####
##########################################
fisher_test <- function(r1, r2, n1, n2) {
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  
  se <- sqrt(1/(n1-3) + 1/(n2-3))
  z  <- (z1 - z2)/se
  
  p  <- 2*(1 - pnorm(abs(z)))
  
  return(c(r1 = r1, r2 = r2, z = z, p = p))
}

#####################################################
#### Retrieving Necessary Statistics for Testing ####
#####################################################
##########################################################################################
#### Correlation and N for Control's Univ Material Eval and Control's Legislator Eval ####
##########################################################################################
c_cor_mat <- cor.test(ai_fisher$material_score[ai_fisher$category == "control"], ai_fisher$legis_score[ai_fisher$category == "control"])
r_c_mat <- c_cor_mat$estimate#Correlation Coefficient
n_c_mat <- c_cor_mat$parameter+2#N

###########################################################################################
#### Correlation and N for Control's Univ Interview Eval and Control's Legislator Eval ####
###########################################################################################
c_cor_int <- cor.test(ai_fisher$interview_score[ai_fisher$category == "control"], ai_fisher$legis_score[ai_fisher$category == "control"])
r_c_int <- c_cor_int$estimate#Correlation Coefficient
n_c_int <- c_cor_int$parameter+2#N

######################################################################################################
#### Correlation and N for All Treatment's Univ Material Eval and All Treatment's Legislator Eval ####
######################################################################################################
t_cor_mat <- cor.test(ai_fisher$material_score[ai_fisher$category != "control"], ai_fisher$legis_score[ai_fisher$category != "control"])
r_t_mat <- t_cor_mat$estimate#Correlation Coefficient
n_t_mat <- t_cor_mat$parameter+2#N


#######################################################################################################
#### Correlation and N for All Treatment's Univ Interview Eval and All Treatment's Legislator Eval ####
#######################################################################################################
t_cor_int <- cor.test(ai_fisher$interview_score[ai_fisher$category != "control"], ai_fisher$legis_score[ai_fisher$category != "control"])
r_t_int <- t_cor_int$estimate
n_t_int <- t_cor_int$parameter+2

#########################################################################################
#### Correlation and N for Control's Univ Material Eval and Control's Interview Eval ####
#########################################################################################
c_cor_two <- cor.test(ai_fisher$material_score[ai_fisher$category == "control"], ai_fisher$interview_score[ai_fisher$category == "control"])
r_c_two <- c_cor_two$estimate#Correlation Coefficient
n_c_two <- c_cor_two$parameter+2#N

#####################################################################################################
#### Correlation and N for ALL Treatment's Univ Material Eval and All Treatment's Interview Eval ####
#####################################################################################################
c_tre_two <- cor.test(ai_fisher$material_score[ai_fisher$category != "control"], ai_fisher$interview_score[ai_fisher$category != "control"])
r_t_two <- c_tre_two$estimate#Correlation Coefficient
n_t_two <- c_tre_two$parameter+2#N

#########################
#### Testing Results ####
#########################
fisher_test(r_c_mat, r_t_mat, n_c_mat, n_t_mat)# Univ Material Eval vs Legislator Eval

fisher_test(r_c_int, r_t_int, n_c_int, n_t_int)# Univ Interview Eval vs Legislator Eval

fisher_test(r_c_two, r_t_two, n_c_two, n_t_two)# Univ Material Eval vs Univ Interview Eval

#fisher_test(r_c_two, r_c_mat, n_c_two, n_c_mat)
#fisher_test(r_c_two, r_c_int, n_c_two, n_c_int)
#fisher_test(r_c_mat, r_c_int, n_c_mat, n_c_mat)

#fisher_test(r_t_two, r_t_mat, n_t_two, n_t_mat)
#fisher_test(r_t_two, r_t_int, n_t_two, n_t_int)
#fisher_test(r_t_mat, r_t_int, n_t_mat, n_t_mat)













