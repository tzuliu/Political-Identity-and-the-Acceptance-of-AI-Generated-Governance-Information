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

#########################
####  AIPW for MCLC  ####
#########################
################################################
#### Creating Dataframe for Control vs MCLC ####
################################################
T1 <- ai_legis%>%
  filter(category %in% c("MCLC", "control")) %>%
  mutate(T = ifelse(category == "MCLC", 1, 0))

################################
#### Proepnsity Score Model ####
################################
ps_model_t1 <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                     family = binomial(link = "logit"), data = T1)
e_hat_t1 <- predict(ps_model_t1, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_t1 <- pmin(pmax(e_hat_t1, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_t1 <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = T1)

####################################
#### Estimated Outcome for MCLC ####
####################################
df1_t1 <- T1
df1_t1$T <- 1
m1_hat_t1 <- predict(outcome_model_t1, newdata = df1_t1)

#######################################
#### Estimated Outcome for Control ####
#######################################
df0_t1 <- T1
df0_t1$T <- 0
m0_hat_t1 <- predict(outcome_model_t1, newdata = df0_t1)

###################################################
#### AIPW Estimator of ATE for Control vs MCLC ####
###################################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_t1 <- (m1_hat_t1 + T1$T * (T1$legis_score - m1_hat_t1) / e_hat_t1) -
  (m0_hat_t1 + (1 - T1$T) * (T1$legis_score - m0_hat_t1) / (1 - e_hat_t1))

#################################
#### ATE for Control vs MCLC ####
#################################
ate_aipw_t1 <- mean(phi_hat_t1)#ATE
se_aipw_t1  <- sd(phi_hat_t1) / sqrt(nrow(T1))#SE

#########################
####  AIPW for MILI  ####
#########################
################################################
#### Creating Dataframe for Control vs MILI ####
################################################
T4 <- ai_legis%>%
  filter(category %in% c("MILI", "control")) %>%
  mutate(T = ifelse(category == "MILI", 1, 0))

################################
#### Proepnsity Score Model ####
################################
ps_model_t4 <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                   family = binomial(link = "logit"), data = T4)
e_hat_t4 <- predict(ps_model_t4, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_t4 <- pmin(pmax(e_hat_t4, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_t4 <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = T4)

####################################
#### Estimated Outcome for MILI ####
####################################
df1_t4 <- T4
df1_t4$T <- 1
m1_hat_t4 <- predict(outcome_model_t4, newdata = df1_t4)

#######################################
#### Estimated Outcome for Control ####
#######################################
df0_t4 <- T4
df0_t4$T <- 0
m0_hat_t4 <- predict(outcome_model_t4, newdata = df0_t4)

###################################################
#### AIPW Estimator of ATE for Control vs MILI ####
###################################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_t4 <- (m1_hat_t4 + T4$T * (T4$legis_score - m1_hat_t4) / e_hat_t4) -
  (m0_hat_t4 + (1 - T4$T) * (T4$legis_score - m0_hat_t4) / (1 - e_hat_t4))

#################################
#### ATE for Control vs MILI ####
#################################
ate_aipw_t4 <- mean(phi_hat_t4)#ATE
se_aipw_t4  <- sd(phi_hat_t4) / sqrt(nrow(T4))#SE

#########################
####  AIPW for MILC  ####
#########################
################################################
#### Creating Dataframe for Control vs MILC ####
################################################
T2 <- ai_legis%>%
  filter(category %in% c("MILC", "control")) %>%
  mutate(T = ifelse(category == "MILC", 1, 0))

################################
#### Proepnsity Score Model ####
################################
ps_model_t2 <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                   family = binomial(link = "logit"), data = T2)
e_hat_t2 <- predict(ps_model_t2, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_t2 <- pmin(pmax(e_hat_t2, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_t2 <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = T2)

####################################
#### Estimated Outcome for MILC ####
####################################
df1_t2 <- T2
df1_t2$T <- 1
m1_hat_t2 <- predict(outcome_model_t2, newdata = df1_t2)

#######################################
#### Estimated Outcome for Control ####
#######################################
df0_t2 <- T2
df0_t2$T <- 0
m0_hat_t2 <- predict(outcome_model_t2, newdata = df0_t2)

###################################################
#### AIPW Estimator of ATE for Control vs MILC ####
###################################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_t2 <- (m1_hat_t2 + T2$T * (T2$legis_score - m1_hat_t2) / e_hat_t2) -
  (m0_hat_t2 + (1 - T2$T) * (T2$legis_score - m0_hat_t2) / (1 - e_hat_t2))

#################################
#### ATE for Control vs MILC ####
#################################
ate_aipw_t2 <- mean(phi_hat_t2)#ATE
se_aipw_t2  <- sd(phi_hat_t2) / sqrt(nrow(T2))#SE

#########################
####  AIPW for MCLI  ####
#########################
################################################
#### Creating Dataframe for Control vs MCLI ####
################################################
T3 <- ai_legis%>%
  filter(category %in% c("MCLI", "control")) %>%
  mutate(T = ifelse(category == "MCLI", 1, 0))

################################
#### Proepnsity Score Model ####
################################
ps_model_t3 <- glm(T ~ Gender + Age + Edu + 
                     Income + lvuse + lvable, 
                   family = binomial(link = "logit"), data = T3)
e_hat_t3 <- predict(ps_model_t3, type = "response")# Retrieving Probability of besing Asdsigned to MCLC
e_hat_t3 <- pmin(pmax(e_hat_t3, 0.01), 0.99)# Restricting Probability between 0.01-0.09

#######################
#### Outcome Model ####
#######################
outcome_model_t3 <- lm(legis_score ~ T + Gender + Age + Edu + 
                         Income + lvuse + lvable, data = T3)

####################################
#### Estimated Outcome for MCLI ####
####################################
df1_t3 <- T3
df1_t3$T <- 1
m1_hat_t3 <- predict(outcome_model_t3, newdata = df1_t3)

#######################################
#### Estimated Outcome for Control ####
#######################################
df0_t3 <- T3
df0_t3$T <- 0
m0_hat_t3 <- predict(outcome_model_t3, newdata = df0_t3)

###################################################
#### AIPW Estimator of ATE for Control vs MCLI ####
###################################################
##########################################
#### Treatment Effect of Every Sample ####
##########################################
phi_hat_t3 <- (m1_hat_t3 + T3$T * (T3$legis_score - m1_hat_t3) / e_hat_t3) -
  (m0_hat_t3 + (1 - T3$T) * (T3$legis_score - m0_hat_t3) / (1 - e_hat_t3))

#################################
#### ATE for Control vs MCLI ####
#################################
ate_aipw_t3 <- mean(phi_hat_t3)#ATE
se_aipw_t3  <- sd(phi_hat_t3) / sqrt(nrow(T3))#SE

###################################
#### Plotting the AIPW Results ####
###################################
#########################################
#### Creating Dataframe for Plotting ####
#########################################

aipw_result <- data.frame(
  Treatment = c("T1", "T2", "T3", "T4"),
  Estimate = c(ate_aipw_t1, ate_aipw_t2, ate_aipw_t3, ate_aipw_t4),
  SE = c(se_aipw_t1, se_aipw_t2, se_aipw_t3, se_aipw_t4),
  LB = c(ate_aipw_t1 - 1.96*se_aipw_t1, ate_aipw_t2 - 1.96*se_aipw_t2, 
         ate_aipw_t3 - 1.96*se_aipw_t3, ate_aipw_t4 - 1.96*se_aipw_t4),
  UB = c(ate_aipw_t1 + 1.96*se_aipw_t1, ate_aipw_t2 + 1.96*se_aipw_t2, 
         ate_aipw_t3 + 1.96*se_aipw_t3, ate_aipw_t4 + 1.96*se_aipw_t4)
)

aipw_result <- aipw_result %>%
  mutate(
    Treatment_Label = case_when(
      Treatment == "T1" ~ "Media Congruent \n Legislator Congruent",
      Treatment == "T2" ~ "Media Incongruent \n Legislator Congruent",
      Treatment == "T3" ~ "Media Congruent \n Legislator Incongruent",
      Treatment == "T4" ~ "Media Incongruent \n Legislator Incongruent"
    ),
    Media = case_when(
      Treatment %in% c("T1", "T3") ~ "Media Congruent",
      Treatment %in% c("T2", "T4") ~ "Media Incongruent"
    ),
    Legislator = case_when(
      Treatment %in% c("T1", "T2") ~ "Legislator Congruent",
      Treatment %in% c("T3", "T4") ~ "Legislator Incongruent"
    )
  )

aipw_result$Treatment_Label <- factor(
  aipw_result$Treatment_Label,
  levels = rev(c("Media Congruent \n Legislator Congruent", 
                 "Media Incongruent \n Legislator Congruent",
                 "Media Congruent \n Legislator Incongruent",
                 "Media Incongruent \n Legislator Incongruent"))
)

##################
#### Plotting ####
##################

p <- ggplot(aipw_result, aes(x = Estimate, y = Treatment_Label,
                        color = Legislator,
                        shape = Media)) +
  geom_errorbar(aes(xmin = LB, xmax = UB), width = 0, linewidth = 1.2) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(limits = c(-2.5, 1.5)) +
  scale_color_manual(values = c("Legislator Congruent" = "black", "Legislator Incongruent" = "gray60")) +
  scale_shape_manual(values = c("Media Congruent" = 17, "Media Incongruent" = 16)) +
  labs(
    #title = "Average Treatment Effects of Every Treatment Group vs Control Group",
    x = "ATE",
    y = NULL,
    color = "Legislator Congruence", 
    shape = "Media Congruence"  
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

ggsave("/Users/tpliu/Desktop/Political-Identity-and-the-Acceptance-of-AI-Generated-Governance-Information/Plots/ate-all.pdf", p, width = 15, height = 10, units = "cm")





