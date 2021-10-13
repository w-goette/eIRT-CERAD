#################################################
## MISSING DATA IMPUTATION
#################################################

#load in needed packages
library(mice)       #multiple imputation mechanism
library(brms)       #fits multivariate regressions
library(BayesLCA)   #performs latent class analysis

#make sure chains run in parallel across multiple cores
options(mc.cores = parallel::detectCores())

#make results comparable across machines
SEED <- 1932392
set.seed(SEED)

#compute the regression-based inclusion criterion
Missingness <- function(x) {
  sum(is.na(x)) / length(x) * 100
} #function computes percent of missing data
mean(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                       "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                       "Race", "Education", "MaternalEdu", "PaternalEdu")], 2, Missingness)) #average % missing data = 1.81
max(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                      "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                      "Race", "Education", "MaternalEdu", "PaternalEdu")], 2, Missingness)) #highest observed % missing data (by column) = 13.10
max(apply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                      "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                      "Race", "Education", "MaternalEdu", "PaternalEdu")], 1, Missingness)) #highest observed % missing data (by row) = 46.15

sapply(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                   "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                   "Race", "Education", "MaternalEdu", "PaternalEdu")], function(x) Missingness(x)) #show how many missing points there are in each column

df_imp <- mice(df_resp[, c("MMSE", "Animals", "CERADdel", "LMIM", "CPIM", "SDMT", "CPDM", "LMDM", "LMrcg", "TMTA", "TMTB", "CERADimm", "CERADdisc",
                           "Ravens", "UrbanRural", "Age", "TotalAssets", "FinancialWealth", "TotalIncome", "PovertyRatioInst", "Gender", "Ethnicity",
                           "Race", "Education", "MaternalEdu", "PaternalEdu", "Impaired")], m = 5, maxit = 30, seed = SEED, print = FALSE) #defaults make sense for data
saveRDS(df_imp, "Data_imputed.rds")

plot(df_imp) #good mixing overall of the chains
densityplot(df_imp) #results are plausible, support that those missing test data were generally more impaired, no apparent bias in the demographic variables

df_imp <- complete(df_imp, action = "all")
df_imps <- as.list(1:5)
for(i in 1:5) {
  df_imps[[i]] <- df_imp[[i]]
}

CogRegression <- brm_multiple(data = df_imps, family = student(), seed = SEED,
                              bf(mvbind(MMSE, Animals, CERADdel, LMIM, CPIM, SDMT, CPDM, LMDM, LMrcg, TMTA, TMTB, CERADimm, CERADdisc) ~
                                   Ravens + UrbanRural + Age + TotalAssets + FinancialWealth + TotalIncome + PovertyRatioInst + Gender + Ethnicity +
                                   Race + Education + MaternalEdu + PaternalEdu + Impaired, decomp = "QR"),
                              prior = c(prior("normal(0, 1)", class = "b", resp = "MMSE"),
                                        prior("student_t(3, 27, 11.5)", class = "Intercept", resp = "MMSE"),
                                        prior("normal(0, 1)", class = "b", resp = "Animals"),
                                        prior("student_t(3, 16, 20)", class = "Intercept", resp = "Animals"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADdel"),
                                        prior("student_t(3, 5, 8)", class = "Intercept", resp = "CERADdel"),
                                        prior("normal(0, 1)", class = "b", resp = "LMIM"),
                                        prior("student_t(3, 10, 15)", class = "Intercept", resp = "LMIM"),
                                        prior("normal(0, 1)", class = "b", resp = "CPIM"),
                                        prior("student_t(3, 8, 7)", class = "Intercept", resp = "CPIM"),
                                        prior("normal(0, 1)", class = "b", resp = "SDMT"),
                                        prior("student_t(3, 33, 37)", class = "Intercept", resp = "SDMT"),
                                        prior("normal(0, 1)", class = "b", resp = "CPDM"),
                                        prior("student_t(3, 6, 10)", class = "Intercept", resp = "CPDM"),
                                        prior("normal(0, 1)", class = "b", resp = "LMDM"),
                                        prior("student_t(3, 7.5, 16)", class = "Intercept", resp = "LMDM"),
                                        prior("normal(0, 1)", class = "b", resp = "LMrcg"),
                                        prior("student_t(3, 10, 8)", class = "Intercept", resp = "LMrcg"),
                                        prior("normal(0, 1)", class = "b", resp = "TMTA"),
                                        prior("student_t(3, 56, 122)", class = "Intercept", resp = "TMTA"),
                                        prior("normal(0, 1)", class = "b", resp = "TMTB"),
                                        prior("student_t(3, 142, 228)", class = "Intercept", resp = "TMTB"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADimm"),
                                        prior("student_t(3, 18, 16)", class = "Intercept", resp = "CERADimm"),
                                        prior("normal(0, 1)", class = "b", resp = "CERADdisc"),
                                        prior("student_t(3, 9, 7)", class = "Intercept", resp = "CERADdisc"),
                                        prior("gamma(2, 0.1)", class = "nu"),
                                        prior("student_t(3, 0, 10)", class = "Intercept"),
                                        prior("normal(0, 1)", class = "b")),
                              file = "Fitted Models/cognitiveDiagnosisRegression")

CogRegression <- add_criterion(CogRegression, criterion = "bayes_R2") #add R2 estimate

pred <- array(0, dim = c(20000, 3167, 13))
for(i in 1:5) {
  pred[(1+((i-1)*4000)):(i*4000), 1:3167, 1:13] <- predictive_error(CogRegression, newdata = df_imps[[i]], subset = (1+((i-1)*4000)):(i*4000))
}

df_resp$MMSE_resid <- colMeans(pred[, , 1])/sd(pred[, , 1])
df_resp$Animals_resid <- colMeans(pred[, , 2])/sd(pred[, , 2])
df_resp$CERADdel_resid <- colMeans(pred[, , 3])/sd(pred[, , 3])
df_resp$LMIM_resid <- colMeans(pred[, , 4])/sd(pred[, , 4])
df_resp$CPIM_resid <- colMeans(pred[, , 5])/sd(pred[, , 5])
df_resp$SDMT_resid <- colMeans(pred[, , 6])/sd(pred[, , 6])
df_resp$CPDM_resid <- colMeans(pred[, , 7])/sd(pred[, , 7])
df_resp$LMDM_resid <- colMeans(pred[, , 8])/sd(pred[, , 8])
df_resp$LMrcg_resid <- colMeans(pred[, , 9])/sd(pred[, , 9])
df_resp$TMTA_resid <- colMeans(pred[, , 10])/sd(pred[, , 10])
df_resp$TMTB_resid <- colMeans(pred[, , 11])/sd(pred[, , 11])
df_resp$CERADimm_resid <- colMeans(pred[, , 12])/sd(pred[, , 12])
df_resp$CERADdisc_resid <- colMeans(pred[, , 13])/sd(pred[, , 13])

df_resp$MMSE_cut <- ifelse(df_resp$MMSE_resid < -1.0, 1, 0)
df_resp$Animals_cut <- ifelse(df_resp$Animals_resid < -1.0, 1, 0)
df_resp$CERADdel_cut <- ifelse(df_resp$CERADdel_resid < -1.0, 1, 0)
df_resp$LMIM_cut <- ifelse(df_resp$LMIM_resid < -1.0, 1, 0)
df_resp$CPIM_cut <- ifelse(df_resp$CPIM_resid < -1.0, 1, 0)
df_resp$SDMT_cut <- ifelse(df_resp$SDMT_resid < -1.0, 1, 0)
df_resp$CPDM_cut <- ifelse(df_resp$CPDM_resid < -1.0, 1, 0)
df_resp$LMDM_cut <- ifelse(df_resp$LMDM_resid < -1.0, 1, 0)
df_resp$LMrcg_cut <- ifelse(df_resp$LMrcg_resid < -1.0, 1, 0)
df_resp$TMTA_cut <- ifelse(df_resp$TMTA_resid < -1.0, 1, 0)
df_resp$TMTB_cut <- ifelse(df_resp$TMTB_resid < -1.0, 1, 0)
df_resp$CERADimm_cut <- ifelse(df_resp$CERADimm_resid < -1.0, 1, 0)
df_resp$CERADdisc_cut <- ifelse(df_resp$CERADdisc_resid < -1.0, 1, 0)

#run latent class analysis on cut scores
LCAres <- blca.vb(df_resp[, 121:133], 10, delta = 1/10)
df_resp$Group <- as.factor(max.col(Zscore(df_resp[, 121:133], LCAres)))
df_resp$Impaired <- as.factor(ifelse(df_resp$Impaired == "Yes", "Yes", ifelse(df_resp$Group != "1", "Yes", "No")))
saveRDS(LCAres, "LatentClassModel.rds")

#tidy up the data
df_resp <- df_resp[, -c(108:134)]
saveRDS(df_resp, "Data_wide.rds")

#keep only non-impaired samples for estimates
df_long$Impaired <- melt(df_resp[, c(1:30, 44)], measure.vars = 1:30)[, 1]
df_long <- subset(df_long, df_long$Impaired == "No")
df_long$ID <- rep(1:nrow(subset(df_resp, df_resp$Impaired == "No")), 30)
saveRDS(df_long, "Data_long.rds")

#tidy the environment
rm(pred, LCAres, df_imps, CogRegression)
