rm(list=ls(all=TRUE))

##########################
#### Loading packages ####
##########################
#### Data science related
library(dplyr)
library(tidyr)
library(tibble)
library(broom)
####ggplot2 related
library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(ggrepel)

######################
#### Loading data ####
######################
ai <- readRDS("/Users/tpliu/Desktop/Projects/AI/Data/ai-analysis.rds")

####################################################
#### New Variable "group" for Assignment Groups ####
####################################################
ai_legis <- ai %>%
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
ai_legis <- ai_legis %>%
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
ai_legis <- ai_legis %>%
  mutate(category = factor(
    category,
    levels = c("control", "MCLC", "MCLI", "MILC", "MILI")
  ))

########################################
#### AIPW for Legislator Congruence ####
########################################

aipw_lc <- ai_legis %>%
  filter(!category %in% "control") %>%
  mutate(
    T = ifelse(category == "MCLC" | category == "MILC", 1, 0) 
  )

#########################
####  AIPW for LC  ####
#########################
################################
#### Proepnsity Score Model ####
################################
ps_model_lc <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                     family = binomial(link = "logit"), data = aipw_lc)
e_hat_lc <- predict(ps_model_lc, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_lc <- pmin(pmax(e_hat_lc, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_lc <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = aipw_lc)

##################################
#### Estimated Outcome for LC ####
##################################
df1_lc <- aipw_lc
df1_lc$T <- 1
m1_hat_lc <- predict(outcome_model_lc, newdata = df1_lc)

##################################
#### Estimated Outcome for LI ####
##################################
df0_lc <- aipw_lc
df0_lc$T <- 0
m0_hat_lc <- predict(outcome_model_lc, newdata = df0_lc)

############################################
#### AIPW Estimator of ATE for LC vs LI ####
############################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_lc <- (m1_hat_lc + aipw_lc$T * (aipw_lc$legis_score - m1_hat_lc) / e_hat_lc) -
  (m0_hat_lc + (1 - aipw_lc$T) * (aipw_lc$legis_score - m0_hat_lc) / (1 - e_hat_lc))

##########################
#### ATE for LC vs LI ####
##########################
ate_aipw_lc <- mean(phi_hat_lc)#ATE
se_aipw_lc  <- sd(phi_hat_lc) / sqrt(nrow(aipw_lc))#SE


###################################
#### AIPW for Media Congruence ####
###################################

aipw_mc <- ai_legis %>%
  filter(!category %in% "control") %>%
  mutate(
    T = ifelse(category == "MCLC" | category == "MCLI", 1, 0) 
  )

#######################
####  AIPW for MC  ####
#######################
################################
#### Proepnsity Score Model ####
################################
ps_model_mc <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                     family = binomial(link = "logit"), data = aipw_mc)
e_hat_mc <- predict(ps_model_mc, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_mc <- pmin(pmax(e_hat_mc, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_mc <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = aipw_mc)

##################################
#### Estimated Outcome for MC ####
##################################
df1_mc <- aipw_mc
df1_mc$T <- 1
m1_hat_mc <- predict(outcome_model_mc, newdata = df1_mc)

##################################
#### Estimated Outcome for MI ####
##################################
df0_mc <- aipw_mc
df0_mc$T <- 0
m0_hat_mc <- predict(outcome_model_mc, newdata = df0_mc)

############################################
#### AIPW Estimator of ATE for MC vs MI ####
############################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_mc <- (m1_hat_mc + aipw_mc$T * (aipw_mc$legis_score - m1_hat_mc) / e_hat_mc) -
  (m0_hat_mc + (1 - aipw_mc$T) * (aipw_mc$legis_score - m0_hat_mc) / (1 - e_hat_mc))

##########################
#### ATE for MC vs MI ####
##########################
ate_aipw_mc <- mean(phi_hat_mc)#ATE
se_aipw_mc  <- sd(phi_hat_mc) / sqrt(nrow(aipw_mc))#SE

###################################
#### Plotting the AIPW Results ####
###################################
#########################################
#### Creating Dataframe for Plotting ####
#########################################
aipw_c_result <- data.frame(
  Treatment = c("Legislator Cogruence", "Media Congruence"),
  Estimate = c(ate_aipw_lc, ate_aipw_mc),
  SE = c(se_aipw_lc, se_aipw_mc),
  LB = c(ate_aipw_lc - 1.96*se_aipw_lc, ate_aipw_mc - 1.96*se_aipw_mc),
  UB = c(ate_aipw_lc + 1.96*se_aipw_lc, ate_aipw_mc + 1.96*se_aipw_mc)
)

aipw_c_result <- aipw_c_result %>%
  mutate(
    Treatment_Label = case_when(
      Treatment == "Legislator Cogruence" ~ "Legislator Congruence \n vs \n Legislator Incongruence",
      Treatment == "Media Congruence" ~ "Media Congruence \n vs \n Media Incongruence"
    )
  )

aipw_c_result$Treatment_Label <- factor(
  aipw_c_result$Treatment_Label,
  levels = rev(c("Legislator Congruence \n vs \n Legislator Incongruence", 
                 "Media Congruence \n vs \n Media Incongruence"))
)

##################
#### Plotting ####
##################
p_con <- ggplot(aipw_c_result, aes(x = Estimate, y = Treatment_Label)) +
  geom_errorbar(aes(xmin = LB, xmax = UB), width = 0, linewidth = 1.2) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    #title = "Average Treatment Effects \n Legislator Congruence vs Legislator Incongruence
    #Media Congruence vs Media Incongruence",
    x = "ATE",
    y = NULL 
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold", size = 10, hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.text.y = element_text(
      hjust = 0.5,
      lineheight = 0.9,
      margin = margin(r = 10)
    )
  )

ggsave("/Users/tpliu/Desktop/Political-Identity-and-the-Acceptance-of-AI-Generated-Governance-Information/Plots/ate-con.pdf", p_con, width = 15, height = 10, units = "cm")



