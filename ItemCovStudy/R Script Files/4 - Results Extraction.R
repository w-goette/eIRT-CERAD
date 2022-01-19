#################################################
## RESULTS EXTRACTION AND SUMMARIZATION
#################################################

#load needed libraries
library(brms)         #need post-hoc convenience functions
library(bayestestR)   #provides helpful summaries
library(tidyverse)    #needed for data wrangling

########################
## FIT STATISTIC FUNCTIONS
########################

#make functions for item and person fit testing (code provided by Burkner, 2020, online supplement - https://dx.doi.org/10.3390%2Fjintelligence8010005)
ll <- function(y, p) {
  y * log(p) + (1 - y) * log(1 - p)
}

fit_statistic <- function(model, criterion, group, ndraws = NULL) {
  group <- enquo(group)
  subset <- NULL
  if (!is.null(ndraws)) {
    subset <- sample(seq_len(ndraws(model)), ndraws) 
  }
  ppe <- posterior_epred(model, subset = subset) %>%
    t() %>%
    as.data.frame() %>%
    cbind(model$data) %>%
    gather("draw", "ppe", starts_with("V"))
  
  yrep <- posterior_predict(model, subset = subset) %>%
    t() %>%
    as.data.frame() %>%
    cbind(df_long$Resp) %>%
    gather("draw", "yrep", starts_with("V"))
  
  ppe %>%
    mutate(yrep = yrep$yrep) %>%
    mutate(
      crit = criterion(as.numeric(Resp)-1, ppe),
      crit_rep = criterion(yrep, ppe)
    ) %>%
    group_by(!!group, draw) %>%
    summarise(
      crit = sum(crit), 
      crit_rep = sum(crit_rep),
      crit_diff = crit_rep - crit
    ) %>%
    mutate(draw = as.numeric(sub("^V", "", draw))) %>%
    arrange(!!group, draw) %>%
    identity()
}

theme_hist <- function(...) {
  bayesplot::theme_default() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      ...
    )
}

########################
## COMBINE MODEL COMPARISONS
########################

#combine all the model comparisons
ModelComparisons <- list("LOOIC" = list("Comparison.1" = LooCom1, "Comparison.2" = LooCom2, "Comparison.3" = LooCom3, "Comparison.4" = LooCom4, "Comparison.5" = LooCom5),
                         "Weights"= list("Comparison.1" = ModWgt1, "Comparison.2" = ModWgt2, "Comparison.3" = ModWgt3, "Comparison.4" = ModWgt4, "Comparison.5" = ModWgt5))

rm(LooCom1, LooCom2, LooCom3, LooCom4, LooCom5,
   ModWgt1, ModWgt2, ModWgt3, ModWgt4, ModWgt5)

#save data for use in .rmd
saveRDS(ModelComparisons, "ModelComparisons.rds")

########################
## SUMMARIZE MODEL RESULTS
########################

describe_posterior(TwoPL_deptr, centrality = "median", ci = 0.95, ci_method = "hdi",
                   test = c("p_direction", "p_MAP"))
rope(TwoPL_deptr, ci = c(0.95, 1.00), ci_method = "HDI")

describe_posterior(TwoPL_itmex, centrality = "median", ci = 0.95, ci_method = "hdi",
                   test = c("p_direction", "p_MAP"))
rope(TwoPL_itmex, ci = c(0.95, 1.00), ci_method = "HDI")

########################
## SUMMARIZE ITEM COVARIATES
########################

#describe item covariates
item_cov <- data.frame(matrix(0, nrow = 10, ncol = 6, dimnames = list(c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine"),
                                                                      c("FreqSTX", "Concrete", "Diversity", "AoA", "BOI", "Phonemes"))))
item_cov$Item <- c("Butter", "Arm", "Shore", "Letter", "Queen", "Cabin", "Pole", "Ticket", "Grass", "Engine")
item_cov$FreqSTX <- ifelse(item_cov$Item == "Arm", 3.523, ifelse(item_cov$Item == "Butter", 3.018, ifelse(item_cov$Item == "Cabin", 3.001, ifelse(item_cov$Item == "Engine", 3.211, ifelse(item_cov$Item == "Grass", 2.933, ifelse(item_cov$Item == "Letter", 3.625, ifelse(item_cov$Item == "Pole", 2.808, ifelse(item_cov$Item == "Queen", 3.446, ifelse(item_cov$Item == "Shore", 3.006, 3.366))))))))) #log scale, frequency/1,000,000 words in SUBTLXus corpus
item_cov$Concrete <- ifelse(item_cov$Item == "Arm", 4.960, ifelse(item_cov$Item == "Butter", 4.900, ifelse(item_cov$Item == "Cabin", 4.920, ifelse(item_cov$Item == "Engine", 4.860, ifelse(item_cov$Item == "Grass", 4.930, ifelse(item_cov$Item == "Letter", 4.700, ifelse(item_cov$Item == "Pole", 4.660, ifelse(item_cov$Item == "Queen", 4.450, ifelse(item_cov$Item == "Shore", 4.790, 4.700)))))))))
item_cov$Diversity <- ifelse(item_cov$Item == "Arm", 1.657, ifelse(item_cov$Item == "Butter", 1.302, ifelse(item_cov$Item == "Cabin", 1.259, ifelse(item_cov$Item == "Engine", 1.334, ifelse(item_cov$Item == "Grass", 1.565, ifelse(item_cov$Item == "Letter", 1.639, ifelse(item_cov$Item == "Pole", 1.694, ifelse(item_cov$Item == "Queen", 1.544, ifelse(item_cov$Item == "Shore", 1.384, 1.658)))))))))
item_cov$AoA <- ifelse(item_cov$Item == "Arm", 3.260, ifelse(item_cov$Item == "Butter", 5.780, ifelse(item_cov$Item == "Cabin", 6.390, ifelse(item_cov$Item == "Engine", 6.280, ifelse(item_cov$Item == "Grass", 3.940, ifelse(item_cov$Item == "Letter", 4.740, ifelse(item_cov$Item == "Pole", 5.630, ifelse(item_cov$Item == "Queen", 4.420, ifelse(item_cov$Item == "Shore", 6.925, 5.320)))))))))
item_cov$BOI <- ifelse(item_cov$Item == "Arm", 6.478, ifelse(item_cov$Item == "Butter", 6.217, ifelse(item_cov$Item == "Cabin", 4.560, ifelse(item_cov$Item == "Engine", 5.333, ifelse(item_cov$Item == "Grass", 5.455, ifelse(item_cov$Item == "Letter", 5.259, ifelse(item_cov$Item == "Pole", 5.320, ifelse(item_cov$Item == "Queen", 4.083, ifelse(item_cov$Item == "Shore", 4.333, 5.333)))))))))
item_cov$Phonemes <- ifelse(item_cov$Item == "Arm", 3, ifelse(item_cov$Item == "Butter", 4, ifelse(item_cov$Item == "Cabin", 5, ifelse(item_cov$Item == "Engine", 5, ifelse(item_cov$Item == "Grass", 4, ifelse(item_cov$Item == "Letter", 4, ifelse(item_cov$Item == "Pole", 3, ifelse(item_cov$Item == "Queen", 4, ifelse(item_cov$Item == "Shore", 3, 5)))))))))
item_cov$Item <- NULL

item_cov %>%
  summarize_all(list(mean = mean, sd = sd, range = range))
#FreqSTX = 3.19 (0.28), 2.81-3.63
#Concrete = 4.79 (0.16), 4.45-4.96
#Diversity = 1.50 (0.17), 1.26-1.69
#AoA = 5.27 (1.17), 3.26-6.93
#BOI = 5.24 (0.76), 4.08-6.48
#Phonemes = 4.00 (0.82), 3.00-5.00

cor(item_cov, method = "kendall")
cor(item_cov, method = "spearman")

########################
## GET FIT STATISTICS
########################

#check item fit
item_fit <- fit_statistic(
  TwoPL_deptr, criterion = ll, group = Item,
  ndraws = 1000
) #run test statistic
item_diff <- item_fit %>%
  group_by(Item) %>%
  summarise(bp = mean(crit_diff > 0)) #get results from test statistic
item_fit %>% 
  ggplot(aes(crit_diff)) +
  geom_histogram() +
  facet_wrap("Item", scales = "free") +
  theme_hist()  +
  theme(text=element_text(family="Times", face="bold", size=12)) #plot test statistic results
saveRDS(item_fit, "ItemFit.rds")

#get person fit and summaries
person_pars <- ranef(TwoPL_deptr, summary = FALSE)$ID[, , "theta_Intercept"] #get latent ability estimates
person_sds <- apply(person_pars, 1, sd) #add standard deviations
person_pars <- person_pars %>%
  sweep(1, person_sds, "/") %>%
  posterior_summary() %>%
  as_tibble() %>%
  rownames_to_column(var = "person") %>%
  mutate(person = as.numeric(person)) #put together a table with estimate, SD, and credible intervals
person_pars %>%
  arrange(Estimate) %>%
  mutate(id2 = seq_len(n())) %>%
  ggplot(aes(id2, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (sorted after Estimate)") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  theme(text=element_text(family="Times", face="bold", size=12)) #plot table results with credible intervals shown
saveRDS(person_pars, "PersonEstimates.rds")

#check person fit
person_fit <- fit_statistic(
  TwoPL_deptr, criterion = ll, group = ID, 
  ndraws = 1000
) #run test statistic
person_diff <- person_fit %>%
  group_by(ID) %>%
  summarise(bp = mean(crit_diff > 0)) #get results from test statistic
person_max <- which.max(person_diff$bp) #find maximum statistic value
person_fit %>% 
  filter(ID == person_max) %>%
  ggplot(aes(crit_diff)) +
  geom_histogram() +
  theme_hist() +
  theme(text=element_text(family="Times", face="bold", size=12)) #show result from maximum statistic
sum(ifelse(person_diff[, 2] > .95, 1, 0)) #count number of person misfits using a Bayesian p-value of < .05
#51 > .95 or 4.18% of total sample
saveRDS(person_fit, "PersonFit.rds")
