# model for cb count vs. sugar concentration

# load packages
install.packages("glmmTMB")
install.packages("DHARMa")
install.packages("emmeans")
install.packages("scales")

library(glmmTMB)     # For GLMMs including zero-inflated and hurdle models
library(DHARMa)      # For residual diagnostics of mixed models
library(emmeans)     # For estimated marginal means and contrasts
library(tidyverse)
library(dplyr)  # data wrangling - conflict with stats (filter(); lag())
library(ggplot2)    # For plotting
library(scales)   # label_comma()
library(cowplot)

# load data
ExEnd <- read.csv("../data/EndDayDissection.csv")   # end day experimental bees
str(ExEnd)
# Transform multiple specific columns to factors 
ExEnd <- ExEnd %>%
  mutate(across(c(box,  # for filtering
  group, sex, colony    # for possible random effects in mixed models
  ), as.factor))
# ??? sugar_conc as factor / integer??? depend on analysis
# ExEnd$sugar_conc <- as.factor(ExEnd$sugar_conc) 

# check 0% individuals
sugar_0 <- ExEnd[ExEnd$sugar_conc == 0, ]
print(sugar_0)
# remove 0% sugar_conc controls, not relavent to analysis
ExEnd <- ExEnd[ExEnd$sugar_conc != 0, ]

# subset by boxes
bA <- ExEnd[ExEnd$box == "A", ]
str(bA)
bB <- ExEnd[ExEnd$box == "B", ]
str(bB)

# hurdle fit
# CBLoadPerB is count (multiples of 27000), so use negative binomial to handle overdispersion (better than Poisson)

# if too few non-0 counts, the estimate will be unstable, check first
table(bA$sugar_conc, bA$CBLoadPerB > 0) # few 0 counts, try other model rather than Hurdle
2/51 # percentage
table(bB$sugar_conc, bB$CBLoadPerB > 0) # > 1/2 are 0 counts, try Hurdle (code deleted, needs rerun for binomial model of 0 vs. non-0???)
(19+17+7)/80 # percentage
# check sex distribution of infected vs. non-infected bees (box A all females)
table(bB$sex, bB$CBLoadPerB > 0)

## GLMM ANALYSIS — SPORE LOADS IN INFECTED BEES

# Filter to infected bees only
infected_data <- ExEnd[ExEnd$CBLoadPerB > 0, ]
str(infected_data)

# subset by box
iA <- infected_data[infected_data$box == "A", ]
iB <- infected_data[infected_data$box == "B", ]

# summaries central tendency
# all sample, for box A
bA %>%
  group_by(as.factor(sugar_conc)) %>%
  summarise(
    mean   = mean(CBLoadPerB, na.rm = TRUE),
    median = median(CBLoadPerB, na.rm = TRUE),
    mode   = as.numeric(names(sort(table(CBLoadPerB), decreasing = TRUE)[1]))
  )

# infected (exclude 0 counts)
infected_data %>%
  group_by(box,as.factor(sugar_conc)) %>%
  summarise(
    mean   = mean(CBLoadPerB, na.rm = TRUE),
    median = median(CBLoadPerB, na.rm = TRUE),
    mode   = as.numeric(names(sort(table(CBLoadPerB), decreasing = TRUE)[1]))
  )

### box A (inoculated)
# suagr concentration as int or factor??? (as int seems fit better based on diagnostics, also more biologically meaningful?)
## GLM(selected for report), nbinom2 as data family (non-linear variance increase with mean)
glm_A_cat <- glmmTMB(
  CBLoadPerB ~ as.factor(sugar_conc)*colony,
  data = bA,
  family = nbinom2()
)
summary(glm_A_cat)

glm_A <- glmmTMB(
  CBLoadPerB ~ sugar_conc+colony,
  data = bA,
  family = nbinom2()
)
summary(glm_A)

AIC(glm_A, glm_A_int) # cat AIC smaller

  # With only 2 colonies, the random effect is essentially unidentifiable 
  # need at minimum 5–6 groups for reliable random effect estimation.
  # so use fixed effect to account for colony nested variation

## model diagnostics
# simulate residuals (DHARMa)
sim_A <- simulateResiduals(glm_A, n = 1000)

# main plot (left = QQ plot, right = residual vs fitted)
# QQ plot: points should fall on the diagonal (good)
# Residual vs fitted: should have no patterns, no funnel shape 
# KS test: if p > 0.05, no significant deviation
plot(sim_A)

# test for over-dispersion
# - p > 0.05 → NO evidence of overdispersion (good).
# - p < 0.05 → model underestimates variance; may need:
testDispersion(sim_A) 

# check zero-inflation
# - p > 0.05 → no evidence of zero-inflation (good).
# - p < 0.05 → consider adding a zero-inflation component:
#       model_infected <- glmmTMB(..., ziformula = ~1)
testZeroInflation(sim_A)

# Outlier test
# - p > 0.05 → no evidence of problematic outliers.
# - p < 0.05 → investigate specific data points; may
#   indicate measurement issues or missing predictors.
testOutliers(sim_A)

### plot fo A (raw cb count points + box + model estimate mean)
# estimate marginal mean from model
emm_A <- emmeans(
  glm_A,
  ~ sugar_conc,
  at = list(sugar_conc = c(10, 35, 60)), # estimate at my treatment values
  type = "response"   # returns mean spore load and 95% CI on response scale
)

# Convert to tidy data frame:
pred_A <- as.data.frame(emm_A)
pred_A 
# Contains:
# Diet, response, SE, asymp.LCL, asymp.UCL

# make factor sugar concentration for box plot
bA$sugar_con_f <- as.factor(bA$sugar_conc)
str(bA)

# plot A (scale the y axis or not???)
p_cbA <- ggplot(bA, aes(
  x = sugar_con_f,
  y = CBLoadPerB
)) +

  # scale y axis by log 10
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +

  # Raw-data boxplots
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.6,
    fill = "grey85",
    color = "black"
  ) +

  # Jittered raw data points, colour by colony?
  geom_jitter(
    aes(color = colony),
    width = 0.15,
    alpha = 0.6,
    size = 2
  ) +

  # model estimated mean (black diamond)
  geom_point(
    data = pred_A,
    inherit.aes = FALSE,
    aes(
      x = as.factor(sugar_conc),
      y = response
    ),
    size = 5,
    shape = 18,           # black diamond
    alpha = 0.8
  ) +

  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "", # leave some empty upper space
    x = "Sucrose concentration (w/w %)",
    y = expression(italic(C.bombi)~cells~per~bee),
    color = "Colony"
  ) +

  # formatting & text size
  theme_cowplot(12) +
  theme(
    axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 12.5),
    axis.text.y = element_text(size = 12.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12.5)
  )

# check plot
p_cbA

# save figure
ggsave(
  "../results/cb_A.pdf",
  p_cbA,
  width    = 6,    # inches
  height   = 6,     # inches
  dpi      = 300    # high resolution
)


### box B - hurdle model (manually fit the 2 parts separately, 
  # for faster fitting & diagnostics)
# 1. binomial GLM (0 / non-0, CB load)
# assign a column of cb presence / absence (1/0) 
bB$present <- as.integer(bB$CBLoadPerB > 0)

bi_B <- glmmTMB(present ~ sugar_conc + colony,
            family = binomial,
            data = bB)

summary(bi_B)

# 2. GLM for infected only 
# (sugar cat / int???)
glm_B_cat <- glmmTMB(
  CBLoadPerB ~ as.factor(sugar_conc)*colony,
  data = iB,
  family = nbinom2()
  )
summary(glm_B_cat)

glm_B <- glmmTMB(
  CBLoadPerB ~ sugar_conc + colony,
  data = iB,
  family = nbinom2()
  )
summary(glm_B)
AIC(glm_B, glm_B_int) # cat AIC smaller


## model diagnostics (DHARMa), similar to box A's
# 1. binomial part
sim_biB <- simulateResiduals(bi_B, n = 1000)
plot(sim_biB) # good fit, KS p = 0.579, dispersion p = 0.79, outlier test: p = 1.0
testDispersion(sim_biB) # p > 0.05, good

# 2. non-0 infected part
sim_B <- simulateResiduals(glm_B, n = 1000)
plot(sim_B) # good fit of p values, good QQ plot, stil minor issue with right-hand plot
testDispersion(sim_B) # p >0.05, good
testOutliers(sim_B) # no outliers

### plot fo B 
## 1. 0 vs. non-0 (binomial part)
emm_biB <- emmeans(
  bi_B,
  ~ sugar_conc,
  at = list(sugar_conc = seq(10, 60, length.out = 100)), # estimate at my treatment range
  type = "response"   # returns mean spore load and 95% CI on 0-1 scale
)

# Convert to tidy data frame:
pred_biB <- as.data.frame(emm_biB)
pred_biB

# plot B binomial
p_biB <- ggplot(bB, aes(
  x = sugar_conc,
  y = present
)) +

  # confidenve interval
  geom_ribbon(
    data = pred_biB,
    inherit.aes = FALSE,
    aes(
      x = sugar_conc, 
      ymin = asymp.LCL, 
      ymax = asymp.UCL),
    alpha = 0.15) +

  # (remove) Jittered raw data points, colour by colony?
  #geom_jitter(
  #  aes(color = colony),
  #  width = 2,
  #  height = 0.05,
  #  alpha = 0.6,
  #  size = 2
  # ) +

  # model estimated mean (black diamond)
  geom_line(
    data = pred_biB,
    inherit.aes = FALSE,
    aes(
      x = sugar_conc,
      y = prob
    ),
    linewidth = 1
    # alpha = 0.8
  ) +

  # axis lables
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(
    title = "A", # leave some empty upper space
    x = "Sucrose concentration (w/w %)",
    y = expression(Probability~of~italic(C.bombi)~presence),
    color = "Colony"
  ) +

  # formatting & text size
  theme_cowplot(12) +
  theme(
    y.lim = c(0, 1),
    x.lim = c(10, 60),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 12.5),
    axis.text.y = element_text(size = 12.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12.5)
  )

# check plot
p_biB

# save
ggsave(
  "../results/cb_biB_NoRaw.pdf",
  p_biB,
  width    = 6,    # inches
  height   = 6,     # inches
  dpi      = 300    # high resolution
)


## 2. raw cb (infected) count points + box + model estimate mean)
# ??? problem: emm not really fitting, too much colony variation, too few data points available
# ??? plan: don't do test / model fitting? show a discriptive scatter plot only
# estimate marginal mean from model (separate colonies)
emm_B <- emmeans(
  glm_B,
  ~ sugar_conc,
  at = list(sugar_conc = c(10, 35, 60)), # estimate at my treatment values
  type = "response"   # returns mean spore load and 95% CI on response scale
)

# Convert to tidy data frame:
pred_B <- as.data.frame(emm_B)
pred_B 
# Contains:
# Diet, response, SE, asymp.LCL, asymp.UCL

# make factor sugar concentration for box plot
iB$sugar_con_f <- as.factor(iB$sugar_conc)
str(iB)

# plot B (infected)
p_infB <- ggplot(iB, aes(
  x = sugar_con_f,
  y = CBLoadPerB
)) +

  # scale y axis by log 10
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +

  # Raw-data boxplots
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.6,
    fill = "grey85",
    color = "black"
  ) +

  # Jittered raw data points, colour by colony?
  geom_jitter(
    aes(color = colony),
    width = 0.15,
    alpha = 0.6,
    size = 2
  ) +

  # model estimated mean (black diamond)
  geom_point(
    data = pred_B,
    inherit.aes = FALSE,
    aes(
      x = as.factor(sugar_conc),
      y = response
    ),
    size = 5,
    shape = 18,           # black diamond
    alpha = 0.8
  ) +

  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "B", # can be empty to leave some empty upper space
    x = "Sucrose concentration (w/w %)",
    y = italic(C.bombi)~cells~per~bee,
    color = "Colony"
  ) +

  # formatting & text size
  theme_cowplot(12) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text.x = element_text(size = 12.5),
    axis.text.y = element_text(size = 12.5),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12.5)
  )

# check plot
p_infB

# save figure
ggsave(
  "../results/cb_infB_scatter.pdf",
  p_cbB,
  width    = 6,    # inches
  height   = 6,     # inches
  dpi      = 300    # high resolution
)

# combine 2 plots side-to-side
p_cbB <- plot_grid(p_biB, p_infB, ncol = 2)
p_cbB
# save
ggsave(
  filename = "../results/cb_B.pdf",
  plot     = p_cbB,
  width    = 12,    # inches
  height   = 6,     # inches
  dpi      = 300    # high resolution
)


###### filed attempts, not used in thesis
### box plot of cb count, grouped by sugar concentration & boxes
# convert data structure for plotting
box_ExEnd <- ExEnd %>%
  mutate(sugar_conc = factor(sugar_conc))  # categorical sugar concentration

# plot
ggplot(box_ExEnd, aes(x = sugar_conc, y = CBLoadPerB, fill = box)) +
  geom_boxplot() +
  scale_y_log10(labels = label_comma()) +
  labs(
    title = "CB Load per Bee by Sugar Concentration and Box",
    x = "Sugar Concentration (w/w %)",
    y = expression(CB~load~per~B~(log[10]~  scale))
  ) +
  scale_fill_manual(
    values = c("A" = "#E53935", "B" = "#2196F3"),
    labels = c("A" = "Innoculated", "B" = "Susceptible")
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )   

### glm fit & raw data points
# model predicted values
pred_A <- data.frame(
  sugar_conc = seq(min(iA$sugar_conc), max(iA$sugar_conc), length.out = 300)
)
# predict() returns a matrix; column 1 = E[Y | Y > 0]
pA <- predict(nb2_A_glm, newdata = pred_A, type = "response")
pred_A$fit <- if (is.matrix(pA)) pA[, 1] else pA

# repeat for box B
pred_B <- data.frame(
  sugar_conc = seq(min(iB$sugar_conc), max(iB$sugar_conc), length.out = 300)
)
pB <- predict(nb2_B_glm, newdata = pred_B, type = "response")
pred_B$fit <- if (is.matrix(pB)) pB[, 1] else pB

# combine data frames for plotting
raw_AB <- rbind(
  data.frame(sugar_conc = iA$sugar_conc, CBLoadPerB = iA$CBLoadPerB, box = "iA"),
  data.frame(sugar_conc = iB$sugar_conc, CBLoadPerB = iB$CBLoadPerB, box = "iB")
)
pred_AB <- rbind(
  data.frame(sugar_conc = pred_A$sugar_conc, fit = pred_A$fit, box = "iA"),
  data.frame(sugar_conc = pred_B$sugar_conc, fit = pred_B$fit, box = "iB")
)
# color palette 
pal <- c(iA = "#E53935",   # innoculated = red
         iB = "#2196F3")   # susceptible = blue

# plot raw data and model-predicted means
ggplot() +

  # --- raw points (jitter because sugar_conc is discrete) --------------------
  geom_jitter(data  = raw_AB,
              aes(x = sugar_conc, y = CBLoadPerB, colour = box),
              width = 0.8, height = 0,
              alpha = 0.60, size  = 2.5) +

  # --- model-predicted mean lines -------------------------------------------
  geom_line(data      = pred_AB,
            aes(x     = sugar_conc, y = fit, colour = box),
            linewidth = 1.3) +

  # --- axes -----------------------------------------------------------------
  scale_y_log10(labels = label_comma()) +
  scale_x_continuous(breaks = sort(unique(c(iA$sugar_conc, iB$sugar_conc)))) +

  # --- shared colour legend --------------------------------------------------
  scale_colour_manual(
    values = pal,
    labels = c(iA = "Innoculated", iB = "Susceptible")
  ) +

  labs(
    title    = "Negative Binomial GLM",
    x        = "Sugar concentration (w/w %)",
    y        = expression(CB~load~per~B~(log[10]~scale))
  ) +

  theme_bw(base_size = 13) +
  theme(
    legend.position  = "top",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )