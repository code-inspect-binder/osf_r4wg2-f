# =========================================
# This script analyses data of Study 1 
# =========================================

# load packages
library(haven)
library(psych)
library(dplyr)
library(car)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(ggfortify)
library(lm.beta)
library(reghelper)
library(stats)
library(Hmisc)
library(pwr)
library(interactions)
library(ggExtra)

# read in data
data <- read_sav("/Users/maria/Documents/projects/_collabs/anticip_status/data/ELSA_Study1.sav")
data <- as.data.frame(data)


# ------------------
# Data wrangling
# ------------------

# recode values for missing data (-9, -1) in whole dataset as NA
data <- data %>% mutate_all(~na_if(., -9))
data <- data %>% mutate_all(~na_if(., -1))

# age
# -----

# recode values of 99 as missing
data$indager.w2[data$indager.w2 == 99] <- NA
data$age <- data$indager.w2

# sex
# -------

# recode variable
data$sex[data$indsex.w2 == 2] <- 0 # female
data$sex[data$indsex.w2 == 1] <- 1

# status change
# ----------------

# dummy code staqtus change variable
data$sc.los <- ifelse(data$scladdc.w3 == 3, 1, 0)
data$sc.win <- ifelse(data$scladdc.w3 == 1, 1, 0)

# how many people status change?
table(data$scladdc.w3, useNA = "always")

# belief in future opportunity
# -------------------------------

# recode future items so that higher values indicate higher optimism
data$scqolc.w3r <- 5-data$scqolc.w3
data$scqolr.w3r <- 5-data$scqolr.w3
data$scqols.w3r <- 5-data$scqols.w3

# calculate mean future items
data$fb <- rowMeans(data[,c("scqolc.w3r", "scqolr.w3r",  "scqols.w3r")], na.rm = T) 

# life satisfaction
# ------------------

# recode life satisfaction (ls) items so that higher values indicate higher ls
data$sclifea.w2r <- 8-data$sclifea.w2
data$sclifeb.w2r <- 8-data$sclifeb.w2
data$sclifec.w2r <- 8-data$sclifec.w2
data$sclifed.w2r <- 8-data$sclifed.w2
data$sclifee.w2r <- 8-data$sclifee.w2

data$sclifea.w3r <- 8-data$sclifea.w3
data$sclifeb.w3r <- 8-data$sclifeb.w3
data$sclifec.w3r <- 8-data$sclifec.w3
data$sclifed.w3r <- 8-data$sclifed.w3
data$sclifee.w3r <- 8-data$sclifee.w3

# calculate mean score on life satistfaction scale
data$ls.2 <- rowMeans(data[,c("sclifea.w2r", "sclifeb.w2r",  "sclifec.w2r", 
                              "sclifed.w2r", "sclifee.w2r")], na.rm = T) # wave 2

data$ls.3 <- rowMeans(data[,c("sclifea.w3r", "sclifeb.w3r",  "sclifec.w3r", 
                              "sclifed.w3r", "sclifee.w3r")], na.rm = T) # wave 3

# participant exclusion I
# ------------------------

# exclude participants who have missing data 
data <- subset(data, !is.na(scladdc.w3)) 
data <- subset(data, !is.na(fb)) 
data <- subset(data, !is.na(ls.2)) 
data <- subset(data, !is.na(ls.3)) 

# create data set including those with status gain but not maintenance for suppl. analyses
data.2 <- subset(data, scladdc.w3 == 1 | scladdc.w3 == 3) 

# exclude participants who have status win
data <- subset(data, sc.win == 0) 

# ------------------------
# Descriptive statistics
# ------------------------

# descriptives M and SD for Table 1
psych::describe(data[c("age", "sex", "SES", "sc.los", "fb", "ls.2", "ls.3")])
table(data$sex)
prop.table(table(data$sex))

# correltations for Table 1
rcorr(as.matrix(data[c("age", "sex", "SES", "sc.los", "fb", "ls.2", "ls.3")]))


# ----------------------
# Moderated regression 
# ----------------------

# center variables
data$ls.2c <- c(scale(data$ls.2,center = T, scale = F))
data$fb.c  <- c(scale(data$fb,  center = T, scale = F))
data$age.c <- c(scale(data$age, center = T, scale = F))

# define interaction variable
data$fb_sc.los <- data$sc.los*data$fb.c
data$fb_sc.win <- data$sc.win*data$fb.c

# model without covariates
# -------------------------------

# definde and fit model
m1 <- ls.3 ~ fb.c + sc.los + fb.c*sc.los + ls.2c
fit1 <- lm(m1, data = data)
summary(fit1)

# get confidence intervals 
confint(fit1) 

# get standardised regression coefficients
lm.beta(fit1)

# calculate simple slopes
simple_slopes(fit1)

# check model
autoplot(fit1, which = 1:6, ncol = 3, label.size = 3)

# model with covariates
# ----------------------------

# define and fit model
m2 <- ls.3 ~ fb.c + sc.los + fb.c*sc.los + age.c + sex + SES + ls.2c
fit2 <- lm(m2, data = data)
summary(fit2)

# get confidence intervals
confint(fit2) 

# get standardised regression coefficients
lm.beta(fit2)

# check model
autoplot(fit2, which = 1:6, ncol = 3, label.size = 3)


# -----------------------------
# Plots Moderated regression
# -----------------------------

# get numbers for pfuture beliefs
psych::describe(data$fb.c)
 
# plot results 
interact_plot(fit1, pred = fb.c, modx = sc.los, #partial.residuals = TRUE,
              interval = TRUE,
              int.width = 0.95,
              modx.values = c( 0, 1), 
              colors = c("#f0b45b", "#e4797f"), 
              modx.labels = c("No change", "Loss"), 
              legend.main = "Status loss") +
  labs(y = "Predicted life satisfaction", x = "Belief in future opportunity") +
  coord_cartesian(ylim = c(1,7)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(1,2,3,4,5,6,7)) +
  scale_x_continuous(expand = c(0.05, 0.05), breaks = c(-2.14,-1.14,-0.14, 0.86), labels = c(1,2,3,4)) +
  theme_classic(base_size = 15) +
  theme(
    axis.title = element_text(colour = "black", size = 19, margin = margin(t = 0, r = 15, b = 0, l = 0)),         
    axis.text.x  = element_text(colour = "black", size = 19, margin = margin(t = 15, r = 0, b = 0, l = 0)), 
    axis.text.y  = element_text(colour = "black", size = 19), 
    legend.position = c(0.8,0.15), 
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 17), 
    legend.key.width = unit(1,"cm")
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# save plot
ggsave("plot1.pdf", width = 7, height = 6)


# --------------------------------------
# Supplementary analyses: Status gain
# --------------------------------------

# center variables
data.2$ls.2c <- c(scale(data.2$ls.2,center = T, scale = F))
data.2$fb.c  <- c(scale(data.2$fb,  center = T, scale = F))
data.2$age.c <- c(scale(data.2$age, center = T, scale = F))

# define interaction variable
data.2$fb_sc.win <- data.2$sc.win*data.2$fb.c

# model without covariates
# -------------------------------

# definde and fit model
m1 <- ls.3 ~ fb.c + sc.win + fb.c*sc.win + ls.2c
fit1 <- lm(m1, data = data.2)
summary(fit1)

# get confidence intervals 
confint(fit1) 

# get standardised regression coefficients
lm.beta(fit1)


# model with covariats
# ----------------------------

# define and fit model
m2 <- ls.3 ~ fb.c + sc.win + fb.c*sc.win + age.c + sex + SES + ls.2c
fit2 <- lm(m2, data = data.2)
summary(fit2)

# get confidence intervals
confint(fit2) 

# get standardised regression coefficients
lm.beta(fit2)


# -----------------------------------------
# Supplementary analyses: Power analyses
# -----------------------------------------

# 1% of variance explained by adding status loss (assuming total variance explained 60%)
# f2 = 0.05/(1-0.6) (see https://www.statmethods.net/stats/power.html)
pwr.f2.test(u = 2, f2 = 0.025, sig.level = 0.025, power = 0.90) 

# 5% of variance explained by adding status loss (assuming total variance explained 60%)
# f2 = 0.01/(1-0.6) (see https://www.statmethods.net/stats/power.html)
pwr.f2.test(u = 2, f2 = 0.125, sig.level = 0.025, power = 0.90) 



