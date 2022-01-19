#################################################
## IRT MODELING (MEASUREMENT + EXPLANATORY)
#################################################

#load needed packages
library(brms)   #runs models

#make sure chains run in parallel across multiple cores
options(mc.cores = parallel::detectCores())

#make results comparable across machines
SEED <- 1932392
set.seed(SEED)

########################
## MEASUREMENT MODEL
########################

#prior predictive checks
Rasch_priors_fixed <-
  prior("normal(0, 5)", class = "b") +
  prior("normal(0, 3)", class = "sd", group = "ID")  #weakly informative priors

TwoPL_priors_fixed <- 
  prior("normal(0, 5)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "ID", nlpar = "theta") #weakly informative priors

Rasch_prior <- brm(Resp ~ 0 + Item + (1 | ID),
                   data = df_long, family = bernoulli("logit"),
                   prior = Rasch_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "only",
                   file = "Fitted Models/1PL_prior_check")

plot(Rasch_prior) #wide coverage of possible values
pp_check(Rasch_prior, ndraws = 100, type = "bars")
pp_check(Rasch_prior, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(Rasch_prior, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))

TwoPL_prior <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 0 + Item,
                      logalpha ~ 0 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "only",
                   file = "Fitted Models/2PL_prior_check")

plot(TwoPL_prior) #wide coverage of possible values
pp_check(TwoPL_prior, ndraws = 100, type = "bars")
pp_check(TwoPL_prior, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_prior, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))

#save memory and space in the R environment
rm(Rasch_prior, TwoPL_prior)
gc()

#intercept only comparison of 1PL v. 2PL models
Rasch_inter <- brm(bf(Resp ~ 0 + Item + (1 | ID), decomp = "QR"),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = Rasch_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/1PL_intercept")

Rasch_inter <- add_criterion(Rasch_inter, criterion = "loo")

TwoPL_inter <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 0 + Item,
                      logalpha ~ 0 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_intercept")

TwoPL_inter <- add_criterion(TwoPL_inter, criterion = "loo")

LooCom1 <- loo_compare(Rasch_inter, TwoPL_inter)
ModWgt1 <- model_weights(Rasch_inter, TwoPL_inter, weights = "stacking")

pp_check(TwoPL_inter, ndraws = 100, type = "bars")
pp_check(TwoPL_inter, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_inter, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_inter)

#save memory and space in the R environment
rm(Rasch_inter)
gc()

#run multidimensional change model
TwoPL_chnge <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (-1 + Time1 + Time2 + Time3 | ID),
                      beta ~ 0 + Item,
                      logalpha ~ 0 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes", control = list(adapt_delta = 0.99),
                   file = "Fitted Models/2PL_changeModel")

TwoPL_chnge <- add_criterion(TwoPL_chnge, criterion = "loo")

pp_check(TwoPL_chnge, ndraws = 100, type = "bars")
pp_check(TwoPL_chnge, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_chnge, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_chnge)

#run growth/learning model
TwoPL_learn <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 + Time.n | ID) + (-1 + Trial1 | ID) + (-1 + Trial2 | ID) + (-1 + Trial3 | ID),
                      beta ~ 0 + Item,
                      logalpha ~ 0 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "no", control = list(adapt_delta = 0.99), #brms error occurs with sample_prior = "yes" (returns that 'prior_sd_ID' is duplicate value, old brms bug that can occur in non-linear models when including prior samples) -- prior samples not needed for this model anyway
                   file = "Fitted Models/2PL_growthModel")

TwoPL_learn <- add_criterion(TwoPL_learn, criterion = "loo")

pp_check(TwoPL_learn, ndraws = 100, type = "bars")
pp_check(TwoPL_learn, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_learn, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_learn)

#run multidimensional model that includes correlated trial scores
TwoPL_multi <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (-1 + Trial1 + Trial2 + Trial3 | ID),
                      beta ~ 0 + Item,
                      logalpha ~ 0 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes", control = list(adapt_delta = 0.95),
                   file = "Fitted Models/2PL_multidimensional")

TwoPL_multi <- add_criterion(TwoPL_multi, criterion = "loo")

pp_check(TwoPL_multi, ndraws = 100, type = "bars")
pp_check(TwoPL_multi, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_multi, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_multi)

#perform complete model comparisons for local independence assumption
LooCom2 <- loo_compare(TwoPL_inter, TwoPL_chnge, TwoPL_learn, TwoPL_multi, criterion = "loo")
ModWgt2 <- model_weights(TwoPL_inter, TwoPL_chnge, TwoPL_learn, TwoPL_multi, weights = "stacking")

#run local dependency model
TwoPL_depmd <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 0 + LocDep + Item,
                      logalpha ~ 0 + LocDep + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependencyModel")

TwoPL_depmd <- add_criterion(TwoPL_depmd, criterion = "loo")

LooCom3 <- loo_compare(TwoPL_inter, TwoPL_depmd)
ModWgt3 <- model_weights(TwoPL_inter, TwoPL_depmd, weights = "stacking")

pp_check(TwoPL_depmd, ndraws = 100, type = "bars")
pp_check(TwoPL_depmd, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_depmd, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_depmd)

#save memory and space in the R environment
rm(TwoPL_inter)
gc()

#run local dependency with unique effect per word
TwoPL_depun <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 0 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + Item,
                      logalpha ~ 0 + DepButter + DepArm + DepShore + DepLetter + DepQueen + DepCabin + DepPole + DepTicket + DepGrass + DepEngine + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependencyUniqueModel")

TwoPL_depun <- add_criterion(TwoPL_depun, criterion = "loo")

LooCom4 <- loo_compare(TwoPL_depmd, TwoPL_depun)
ModWgt4 <- model_weights(TwoPL_depmd, TwoPL_depun, weights = "stacking")

pp_check(TwoPL_depun, ndraws = 100, type = "bars")
pp_check(TwoPL_depun, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_depun, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_depun)

#save memory and space in the R environment
rm(TwoPL_depun)
gc()

#run local dependency with unique effect per word
TwoPL_deptr <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 0 + LocDep.1 + LocDep.2 + Item,
                      logalpha ~ 0 + LocDep.1 + LocDep.2 + Item),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors_fixed, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes",
                   file = "Fitted Models/2PL_dependencyTrialModel")

TwoPL_deptr <- add_criterion(TwoPL_deptr, criterion = "loo")

LooCom5 <- loo_compare(TwoPL_depmd, TwoPL_deptr)
ModWgt5 <- model_weights(TwoPL_depmd, TwoPL_deptr, weights = "stacking")

tiff("PPC_all.tiff", width = 4, height = 3, units = 'in', res = 1200)
pp_check(TwoPL_deptr, ndraws = 100, type = "bars") + theme(legend.position="bottom")
dev.off()

tiff("PPC_items.tiff", width = 10, height = 10, units = 'in', res = 1200)
pp_check(TwoPL_deptr, ndraws = 100, type = "bars_grouped", group = "Item") +
  facet_wrap(ncol = 5, "group", labeller = as_labeller(c("Butter1" = "Trial 1: Butter", "Arm1" = "Trial 1: Arm", "Shore1" = "Trial 1: Shore", "Letter1" = "Trial 1: Letter", "Queen1" = "Trial 1: Queen", "Cabin1" = "Trial 1: Cabin", "Pole1" = "Trial 1: Pole", "Ticket1" = "Trial 1: Ticket", "Grass1" = "Trial 1: Grass", "Engine1" = "Trial 1: Engine", "Ticket2" = "Trial 2: Ticket", "Cabin2" = "Trial 2: Cabin", "Butter2" = "Trial 2: Butter", "Shore2" = "Trial 2: Shore", "Engine2" = "Trial 2: Engine", "Arm2" = "Trial 2: Arm", "Queen2" = "Trial 2: Queen", "Letter2" = "Trial 2: Letter", "Pole2" = "Trial 2: Pole", "Grass2" = "Trial 2: Grass", "Queen3" = "Trial 3: Queen", "Grass3" = "Trial 3: Grass", "Arm3" = "Trial 3: Arm", "Cabin3" = "Trial 3: Cabin", "Pole3" = "Trial 3: Pole", "Shore3" = "Trial 3: Shore", "Butter3" = "Trial 3: Butter", "Engine3" = "Trial 3: Engine", "Ticket3" = "Trial 3: Ticket", "Letter3" = "Trial 3: Letter"))) + 
  theme(legend.position="bottom")
dev.off()

tiff("PPC_persons.tiff", width = 8, height = 5, units = 'in', res = 1200)
pp_check(TwoPL_deptr, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE)))) +
  theme(legend.position="bottom")
dev.off()

plot(TwoPL_deptr)

#save memory and space in the R environment
rm(TwoPL_depmd)
gc()

########################
## EXPLANATORY MODEL
########################

#new priors to account for transition to explanatory
TwoPL_priors <- 
  prior("normal(0, 2)", class = "b", nlpar = "beta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "ID", nlpar = "theta") #weakly informative priors

#examine explanatory covariates
TwoPL_itmex <- brm(bf(Resp ~ beta + exp(logalpha) * theta,
                      nl = TRUE, decomp = "QR",
                      theta ~ 0 + (1 | ID),
                      beta ~ 1 + Time.c + poly(ItemPos, 2) + FreqSTX + Concrete + Diversity + AoA + BOI + Phonemes,
                      logalpha ~ 1 + Time.c + poly(ItemPos, 2) + FreqSTX + Concrete + Diversity + AoA + BOI + Phonemes),
                   data = df_long, family = brmsfamily("bernoulli", link = "logit"),
                   prior = TwoPL_priors, seed = SEED, iter = 3000, warmup = 1000,
                   sample_prior = "yes", 
                   file = "Fitted Models/2PL_itemCovariates")

pp_check(TwoPL_itmex, ndraws = 100, type = "bars")
pp_check(TwoPL_itmex, ndraws = 100, type = "bars_grouped", group = "Item")
pp_check(TwoPL_itmex, ndraws = 100, type = "bars_grouped", group = "ID", 
         newdata = subset(df_long, df_long$ID %in% as.factor(sample.int(n = 1219, size = 12, replace = FALSE))))
plot(TwoPL_itmex)
plot(conditional_effects(TwoPL_itmex), ask = FALSE)

#save memory and space in the R environment
rm(TwoPL_itmex)
gc()
