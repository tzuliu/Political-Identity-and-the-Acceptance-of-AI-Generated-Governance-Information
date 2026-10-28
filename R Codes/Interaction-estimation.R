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
library(patchwork)

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

#####################################################################
#### Interaction Estimation for Legislator Congruence * AI Usage ####
#####################################################################
inter_lc <- ai_legis %>%
  filter(!category %in% "control") %>%
  mutate(
    T = ifelse(category == "MCLC" | category == "MILC", 1, 0) 
  )

inter_lc_reg <- lm(legis_score ~ T*lvuse + Gender + Age + Edu + 
                     Income + lvable, data = inter_lc)

########################################################################
#### Plotting Regression Lines for Legislator Congruence * AI Usage ####
########################################################################
pred_data_lc <- expand.grid(
  lvuse = seq(
    min(inter_lc$lvuse, na.rm = TRUE),
    max(inter_lc$lvuse, na.rm = TRUE),
    length.out = 100
  ),
  T = c(0, 1),
  Gender = levels(inter_lc$Gender)[1],
  Age = mean(inter_lc$Age, na.rm = TRUE),
  Edu = median(inter_lc$Edu, na.rm = TRUE),
  Income = median(inter_lc$Income, na.rm = TRUE),
  lvable = mean(inter_lc$lvable, na.rm = TRUE)
)

pred_lc <- predict(
  inter_lc_reg,
  newdata = pred_data_lc,
  interval = "confidence",
  level = 0.95
)

pred_data_lc <- cbind(
  pred_data_lc,
  Estimate = pred_lc[, "fit"],
  LB = pred_lc[, "lwr"],
  UB = pred_lc[, "upr"]
)

pred_data_lc <- pred_data_lc %>%
  mutate(
    Legislator_Congruence = factor(
      T,
      levels = c(1, 0),
      labels = c(
        "Congruence",
        "Incongruence"
      )
    )
  )

p_lc_int_use <- ggplot(pred_data_lc, aes(x = lvuse, y = Estimate, linetype  = Legislator_Congruence)) +
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = Legislator_Congruence), alpha = 0.16, color = NA) + 
  scale_fill_manual(values = c("Congruence" = "grey10","Incongruence" = "grey45")) + 
  geom_line(linewidth = 0.9) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(
    title = "Legislator Congruence",
    x = "Level of Generative AI Usage",
    y = "Predicted Acceptance of AI-Generated Evaluations",
    linetype = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 11
    ),
    plot.margin = margin(
      t = 10, r = 10, b = 10, l = 18
    ),
    legend.position = "bottom"
  )

################################################################
#### Interaction Estimation for Media Congruence * AI Usage ####
################################################################
inter_mc <- ai_legis %>%
  filter(!category %in% "control") %>%
  mutate(
    T = ifelse(category == "MCLC" | category == "MCLI", 1, 0) 
  )

inter_mc_reg <- lm(legis_score ~ T*lvuse + Gender + Age + Edu + 
                     Income + lvable, data = inter_mc)

########################################################################
#### Plotting Regression Lines for Legislator Congruence * AI Usage ####
########################################################################
pred_data_mc <- expand.grid(
  lvuse = seq(
    min(inter_mc$lvuse, na.rm = TRUE),
    max(inter_mc$lvuse, na.rm = TRUE),
    length.out = 100
  ),
  T = c(0, 1),
  Gender = levels(inter_mc$Gender)[1],
  Age = mean(inter_mc$Age, na.rm = TRUE),
  Edu = median(inter_mc$Edu, na.rm = TRUE),
  Income = median(inter_mc$Income, na.rm = TRUE),
  lvable = mean(inter_mc$lvable, na.rm = TRUE)
)

pred_mc <- predict(
  inter_mc_reg,
  newdata = pred_data_mc,
  interval = "confidence",
  level = 0.95
)

pred_data_mc <- cbind(
  pred_data_mc,
  Estimate = pred_mc[, "fit"],
  LB = pred_mc[, "lwr"],
  UB = pred_mc[, "upr"]
)

pred_data_mc <- pred_data_mc %>%
  mutate(
    Media_Congruence = factor(
      T,
      levels = c(1, 0),
      labels = c(
        "Congruence",
        "Incongruence"
      )
    )
  )

p_mc_int_use <- ggplot(pred_data_mc, aes(x = lvuse, y = Estimate, linetype  = Media_Congruence)) +
  geom_ribbon(aes(ymin = LB, ymax = UB, fill = Media_Congruence), 
              alpha = 0.16, color = NA) + 
  scale_fill_manual(values = c("Congruence" = "grey10","Incongruence" = "grey45")) + 
  geom_line(linewidth = 0.9) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(
    title = "Media Congruence",
    x = "Level of Generative AI Usage",
    y = NULL,
    linetype = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 11
    ),
    plot.margin = margin(
      t = 10, r = 10, b = 10, l = 18
    ),
    legend.position = "bottom"
  )

#########################
#### Combining Plots ####
#########################
p_int_two <- (p_lc_int_use | p_mc_int_use) +
  plot_annotation(
    #title = "Congruence and Generative AI Usage",
    theme = theme(plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 12
      )
    )
  ) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box.just = "center"
  )

ggsave("/Users/tpliu/Desktop/Political-Identity-and-the-Acceptance-of-AI-Generated-Governance-Information/Plots/con-use-int.pdf", p_int_two, width = 24, height = 15, units = "cm")



