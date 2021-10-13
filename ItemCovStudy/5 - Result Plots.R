#################################################
## PLOTTING FUNCTIONS FOR RESULTS
#################################################

#load needed packages
library(tidyverse)   #needed for data wrangling and plotting
library(ggExtra)     #needed for marginal plots

########################
## PLOT ITEM CHARACTERISTIC CURVES
########################

fixed <- as.data.frame(fixef(TwoPL_deptr, summary = FALSE))

temp <- fixed %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  group_by(Theta, Item) %>%
  summarise(ci = list(as.data.frame(posterior_summary(p)) %>% 
                        rename(p = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error) %>%
  mutate(Trial = rep(1:3, times = 10),
         Item  = str_remove(Item, "\\d$"))

fixed1 <- fixed
fixed1[, 13:22] <- fixed[, 13:22]+fixed[, 1]
fixed1[, 23:32] <- fixed[, 23:32]+fixed[, 2]
fixed1[, 45:54] <- fixed[, 45:54]+fixed[, 33]
fixed1[, 55:64] <- fixed[, 55:64]+fixed[, 34]

temp1 <- fixed1 %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  group_by(Theta, Item) %>%
  summarise(ci = list(as.data.frame(posterior_summary(p)) %>% 
                        rename(p = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error) %>%
  mutate(Trial = rep(1:3, times = 10),
         Item  = str_remove(Item, "\\d$"))

ICCpars <- rbind(temp, temp1)
ICCpars$Dependency <- rep(c("No Prior Recall", "Previous Recall"), each = 3000)
ICCpars <- ICCpars[, c(6, 2, 1, 3, 4:5, 7)]

tiff("TrialOneICCs.tiff", width = 6.5, height = 3, units = 'in', res = 1200)
ICCpars[ICCpars$Trial == 1 & ICCpars$Dependency == "No Prior Recall", ] %>%
  mutate(Item = factor(Item, levels = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine"))) %>%
  ggplot(aes(x = Theta, y = p)) +
  geom_line(size = 0.4) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(x = expression(theta~('ability on the logit scale')),
       y = expression(italic(p)(y==1))) +
  theme_bw() +
  theme(text=element_text(family="serif", face="bold", size=12))
dev.off()

tiff("TrialTwoICCs.tiff", width = 6.5, height = 3, units = 'in', res = 1200)
ICCpars[ICCpars$Trial == 2, ] %>%
  mutate(Item = factor(Item, levels = c("Ticket", "Cabin", "Butter", "Shore", "Engine", "Arm", "Queen", "Letter", "Pole", "Grass"))) %>%
  ggplot(aes(x = Theta, y = p, linetype = Dependency)) +
  geom_line(size = 0.4) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(x = expression(theta~('ability on the logit scale')),
       y = expression(italic(p)(y==1))) +
  theme_bw() +
  theme(text=element_text(family="serif", face="bold", size=12),
        legend.position = "bottom")
dev.off()

tiff("TrialThreeICCs.tiff", width = 6.5, height = 3, units = 'in', res = 1200)
ICCpars[ICCpars$Trial == 3, ] %>%
  mutate(Item = factor(Item, levels = c("Queen", "Grass", "Arm", "Cabin", "Pole", "Shore", "Butter", "Engine", "Ticket", "Letter"))) %>%
  ggplot(aes(x = Theta, y = p, linetype = Dependency)) +
  geom_line(size = 0.4) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(x = expression(theta~('ability on the logit scale')),
       y = expression(italic(p)(y==1))) +
  theme_bw() +
  theme(text=element_text(family="serif", face="bold", size=12),
        legend.position = "bottom")
dev.off()

########################
## PLOT ITEM INFORMATION CURVES
########################

temp <- fixed %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  mutate(i = p * (1 - p)) %>% 
  group_by(Theta, Item) %>% 
  summarise(ci = list(as.data.frame(posterior_summary(i)) %>% 
                        rename(i = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error) %>%
  mutate(Trial = rep(1:3, times = 10),
         Item  = str_remove(Item, "\\d$"))

temp1 <- fixed1 %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  mutate(i = p * (1 - p)) %>% 
  group_by(Theta, Item) %>% 
  summarise(ci = list(as.data.frame(posterior_summary(i)) %>% 
                        rename(i = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error) %>%
  mutate(Trial = rep(1:3, times = 10),
         Item  = str_remove(Item, "\\d$"))

IICpars <- rbind(temp, temp1)
IICpars$Dependency <- rep(c("No Prior Recall", "Previous Recall"), each = 3000)
IICpars <- IICpars[, c(6, 2, 1, 3, 4:5, 7)]

IICpars[IICpars$Trial == 1 & IICpars$Dependency == "No Prior Recall", ] %>%
  mutate(Item = factor(Item, levels = c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine"))) %>%
  ggplot(aes(x = Theta, y = i)) +
  geom_line(size = 1.05) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(title = "IICs for the 2PL",
       subtitle = "Each curve is based on the posterior median.", 
       x = expression(theta~('ability on the logit scale')),
       y = "Information") +
  theme_classic()

IICpars[IICpars$Trial == 2, ] %>%
  mutate(Item = factor(Item, levels = c("Ticket", "Cabin", "Butter", "Shore", "Engine", "Arm", "Queen", "Letter", "Pole", "Grass"))) %>%
  ggplot(aes(x = Theta, y = i, linetype = Dependency)) +
  geom_line(size = 1.05) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(title = "IICs for the 2PL",
       subtitle = "Each curve is based on the posterior median.", 
       x = expression(theta~('ability on the logit scale')),
       y = "Information") +
  theme_classic()
  
IICpars[IICpars$Trial == 3, ] %>%
  mutate(Item = factor(Item, levels = c("Queen", "Grass", "Arm", "Cabin", "Pole", "Shore", "Butter", "Engine", "Ticket", "Letter"))) %>%
  ggplot(aes(x = Theta, y = i, linetype = Dependency)) +
  geom_line(size = 1.05) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  facet_wrap(~ Item, ncol = 5) +
  labs(title = "IICs for the 2PL",
       subtitle = "Each curve is based on the posterior median.", 
       x = expression(theta~('ability on the logit scale')),
       y = "Information") +
  theme_classic()

########################
## TESTWISE INFORMATION CURVE
########################

temp <- fixed %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  mutate(i = p * (1 - p)) %>% 
  group_by(Theta, iter) %>% 
  summarise(sum_i = sum(i)) %>% 
  group_by(Theta) %>% 
  summarise(ci = list(as.data.frame(posterior_summary(sum_i)) %>% 
                        rename(i = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error)

temp1 <- fixed1 %>%
  select(contains("Item")) %>%
  mutate(iter = 1:n()) %>%
  pivot_longer(starts_with(c("beta_", "logalpha_"))) %>%
  mutate(Item   = str_remove(name, "^beta_Item|^logalpha_Item"),
         Parameter = ifelse(str_detect(name, "beta"), "Difficulty", "Discrimination")) %>%
  select(-name) %>%
  pivot_wider(names_from = Parameter, values_from = value) %>%
  expand(nesting(iter, Item, Difficulty, Discrimination),
         Theta = seq(-6, 6, length.out = 100)) %>%
  mutate(p = inv_logit_scaled(Difficulty + exp(Discrimination) * Theta)) %>%
  mutate(i = p * (1 - p)) %>% 
  group_by(Theta, iter) %>% 
  summarise(sum_i = sum(i)) %>% 
  group_by(Theta) %>% 
  summarise(ci = list(as.data.frame(posterior_summary(sum_i)) %>% 
                        rename(i = Estimate, ymin = Q2.5, ymax = Q97.5))) %>% 
  unnest(cols = c(ci)) %>%
  select(-Est.Error)

IICtest <- rbind(temp, temp1)
IICtest$Dependency <- rep(c("No Prior Recall", "Previous Recall"), each = 100)

IICtest %>%
  ggplot(aes(x = Theta, y = i, linetype = Dependency)) +
  geom_line(size = 1.05) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.25) +
  labs(title = "IIC for the 2PL",
       subtitle = "Curve is based on the posterior median.", 
       x = expression(theta~('ability on the logit scale')),
       y = "Information") +
  theme_classic()

########################
## PLOT RELIABILITY OF THE TEST
########################

theta <- as.data.frame(ranef(TwoPL_deptr)$ID)
var_theta <- as.numeric(VarCorr(TwoPL_deptr)[[1]][[1]][1])
reliability_est <- var_theta/(var_theta + theta[, 2]^2)
rel_dat <- data.frame(Theta = theta[, 1], Rxx = reliability_est)
rm(theta_est, var_theta, reliability_est) #keep environment tidy
ggplot(data = rel_dat, aes(x = Theta, y = Rxx)) +
  geom_smooth(color = "black", size = 1.10) +
  ylab("Reliability Estimate") +
  xlab("Person Ability Estimate") +
  theme_bw() +
  theme(text=element_text(family="Times", face="bold", size=12))


########################
## SCATTER PLOT OF RAW TO THETA SCORES
########################

df_wide <- reshape(TwoPL_deptr$data[, c("ID", "Item", "Resp")], direction = "wide", idvar = "ID", timevar = "Item")
df_wide <- as.data.frame(sapply(df_wide, function(x) as.numeric(x)-1))
df_wide$ID <- df_wide$ID+1
df_wide$RawScore <- rowSums(df_wide[, 2:31])

cor(df_wide$RawScore, theta$Estimate.theta_Intercept)
cor(df_wide$RawScore, theta$Estimate.theta_Intercept, method = "kendall")

ScatterDat <- as.data.frame(cbind(df_wide$RawScore, theta$Estimate.theta_Intercept))
colnames(ScatterDat) <- c("CERAD Immediate Recall Raw Score", "Latent Trait (Theta)")

p <- ggplot(ScatterDat, aes(x=ScatterDat[, 1], y=ScatterDat[, 2])) +
  geom_jitter(size = 0.8) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  labs(
    x = "CERAD Immediate Recall Raw Score",
    y = "Latent Trait (Theta)"
  ) +
  theme(text = element_text(family = "serif", size = 12))

tiff("CERADScoreScatterplot.tiff", width = 6.5, height = 3, units = 'in', res = 1200)
ggMarginal(p, type = "densigram")
dev.off()
