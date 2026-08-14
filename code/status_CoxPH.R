# try Cox proportional hazards model with survival package

# load packages
install.packages("survminer")
install.packages("patchwork")

library(survival) # for cox proportional hazards model
library(dplyr) # data manipulation
library(survminer)   # visualize survival (Kaplan-Meier plots)
library(ggplot2)     # visualization
library(patchwork)      # combine multiple plots

# load data
ExData <- read.csv("../data/ExStatus.csv")
str(ExData)
# Transform multiple specific columns to factors (CPH gives different results for factor vs numeric)
ExData <- ExData %>%
  mutate(across(c(group, box, sex, colony), as.factor)) # choice: sugar_con as factor / number
# remove 0% sugar_conc controls, not relavent to analysis
ExData <- ExData[ExData$sugar_conc != 0, ]

# subset by boxes
bA <- ExData[ExData$box == "A", ]
bB <- ExData[ExData$box == "B", ]

# check 0 count percentage, see if analyse cb counts directly -> no
bB %>%
  group_by(sugar_conc) %>%
  summarise(sampleN = length(CBLoadPerB), mean = mean(CBLoadPerB), variance = var(CBLoadPerB),
  zeroCount_Percent = length(CBLoadPerB[CBLoadPerB == 0])/length(CBLoadPerB) * 100)  
  # variance much larger than mean, 0 count % = 30~68
# box A
bA %>%
  group_by(sugar_conc) %>%
  summarise(sampleN = length(CBLoadPerB), mean = mean(CBLoadPerB), variance = var(CBLoadPerB), 
  zeroCount_Percent = length(CBLoadPerB[CBLoadPerB == 0])/length(CBLoadPerB) * 100)  
  # variance much larger than mean, 0 count % = 7~30

# Fit Cox PH model (~sugar + colony)
# box A

# Check PH assumption 
## use fixed effect model
CPHa_test <- coxph(Surv(exp_day, status) ~ sugar_conc + strata(colony), # trata controls colony
                     data = bA)
summary(CPHa_test)
cox.zph(CPHa_test)

## use log-log plot -> parallel, assumption not violated
fit <- survfit(Surv(exp_day, status) ~ colony, data = bA)
ggsurvplot(fit, fun = "cloglog",
           xlab = "log(time)",
           ylab = "log(-log(Survival))",
           title = "Log-Log Plot by Colony")

## (abandoned) stratified is better than frailty as too few colonies (n=2) for random effects estimation
# frailty model -> can't test with cox.zph
CPHa <- coxph(Surv(exp_day, status) ~ sugar_conc + frailty(colony), # frailty treat colony as a random effect
                     data = bA)
summary(CPHa)
    # (numerical sugar_conc) HR=0.99<1
    # trend: higher sugar concentration slightly decreases death risk. 
    # no significance(p = 0.1-0.2)



#box B (death n = 2, too few for cox PH, no need)


### not used (all
CPHall <- coxph(Surv(exp_day, status) ~ sugar_conc + box, data = ExData)
summary(CPHall)
cox.zph(CPHall)
    # p > 0.05, PH assumption holds
# all, box as factor alone
CPHbox <- coxph(Surv(exp_day, status) ~ box, data = ExData)
summary(CPHbox)
    # similar results to effect of box
    # box B had lower death risk than A (HR=0.07<1, p<0.001) 
# cb load
    # ~ sugar_conc + box + CBLoadPerB not applicable because one group (box B 10%) has 0 death, which is not allowed in coxph model
CPHcb <- coxph(Surv(exp_day, status) ~ box + CBLoadPerB, data = ExData)
summary(CPHcb)
cox.zph(CPHcb) # no vialation
    # cb has no effect (HR = 1, p=0.91), similar in seperate box analysis
### ) not used

# visualize survival: Kaplan-Meier plot (fit + plot)
# Colors
sugar_palette <- c(
    "sugar_conc=10"    = "#ffa600", 
    "sugar_conc=35"    = "#a27829", 
    "sugar_conc=60"    = "#442c00"
)

# fit curves for plotting
kmA <- survfit(Surv(exp_day, status) ~ sugar_conc, data = bA)   # for box A alone, ~sugar_conc
kmB <- survfit(Surv(exp_day, status) ~ sugar_conc, data = bB)   # for box B

#### plot 2 boxes separately?
### box A
KMA <- ggsurvplot(
  kmA, 
  data = bA,
  # risk.table = TRUE,       # Adds a "Number at Risk" table at the bottom
  xlab = "Time (Days)",    # X-axis label
  ylab = "Proportion of survived",
  title = "A",
  legend.title = "Sucrose concentration (w/w)",
  xlim = c(7, 13),          # Forces X-axis to start at 7 and end at 13
  ylim = c(0.3, 1.0),
  break.time.by = 1,         # Puts a tick mark at every day (7, 8, 9, 10...)
  censor = FALSE
)

# Modify KMA$plot (the ggplot object inside ggsurvplot)
KMA$plot <- KMA$plot +
  guides(colour = guide_legend(nrow = 3)) +
  theme(
    # plot title
    plot.title = element_text(size = 16, face = "bold"),  # adjust size as needed
    # Legend inside panel, slightly right of y-axis
    legend.position = c(0.1, 0.30),          # adjust horizontally as needed
    legend.justification = c("left", "top"),
    legend.background = element_rect(
      fill = alpha("white", 0.75),
      color = NA),
    legend.key.width = unit(0.5, "cm"),   # increase horizontal length
    legend.key.height = unit(0.6, "cm"),  # optional: give some vertical padding
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12.5)
  )+
  scale_color_manual(
    values = sugar_palette,
    labels = c(
      "sugar_conc=10" = "10%",
      "sugar_conc=35" = "35%",
      "sugar_conc=60" = "60%"
    )
  ) 

KMA

### box B
KMB <- ggsurvplot(
  kmB, 
  data = bB,
  # pval = FALSE,             # Adds p-value from log-rank test?
  # risk.table = TRUE,       # Adds a "Number at Risk" table at the bottom
  xlab = "Time (Days)",    # X-axis label
  ylab = "Proportion of survived",
  title = "B",
  xlim = c(7, 13),          # Forces X-axis to start at 7 and end at 13
  ylim = c(0.3, 1.0),
  legend.title = "Sucrose concentration (w/w)",
  break.time.by = 1,         # Puts a tick mark at every day (7, 8, 9, 10...)
  censor = FALSE
)
# small parts modify
KMB$plot <- KMB$plot +
  guides(colour = guide_legend(nrow = 3)) +
  theme(
    # plot title
    plot.title = element_text(size = 16, face = "bold"),  # adjust size as needed
    # Legend inside panel, slightly right of y-axis
    legend.position = c(0.1, 0.30),          # adjust horizontally as needed
    legend.justification = c("left", "top"),
    legend.background = element_rect(
      fill = alpha("white", 0.75),
      color = NA),
    legend.key.width = unit(0.5, "cm"),   # increase horizontal length
    legend.key.height = unit(0.6, "cm"),  # optional: give some vertical padding
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12.5)
  )+
  scale_color_manual(
    values = sugar_palette, # colour for the subsets
    labels = c(
      "sugar_conc=10" = "10%",
      "sugar_conc=35" = "35%",
      "sugar_conc=60" = "60%"
    )
  ) 

KMB

# combine 2 plots side-to-side
KMAB <- arrange_ggsurvplots(list(KMA, KMB), ncol = 2, nrow = 1)
# save
ggsave(
  filename = "../results/KM_seperated.pdf",
  plot     = KMAB,
  width    = 12,    # inches
  height   = 5,     # inches
  dpi      = 300    # high resolution
)


#### (abandoned) plot both box into 1 chart? 
# Extract the survival curve data from both fits
kmA_data <- surv_summary(kmA, data = bA)
kmB_data <- surv_summary(kmB, data = bB)

# Add a box identifier
kmA_data$box <- "A"
kmB_data$box <- "B"

# Combine
km_all <- rbind(kmA_data, kmB_data)

# create a variable interaction table for colour and linetype mapping
km_all$group <- interaction(km_all$sugar_conc, km_all$box)

# plot
KM_shared <- ggplot(km_all, aes(x = time, y = surv, 
                   colour = strata, 
                   linetype = box)) +
  geom_step() +
  scale_color_manual(
    values = sugar_palette,
    labels = c("10%", "35%", "60%"),
    name   = "Sucrose concentration (w/w)"
  ) +
  scale_linetype_manual(
    values = c("A" = "solid", "B" = "dashed"),
    labels = c("Inoculated", "Susceptible"),
    name   = "Bee types"
  ) +
  coord_cartesian(xlim = c(7, 13), ylim = c(0.3, 1.0)) +
  scale_x_continuous(breaks = seq(7, 13, by = 1)) +
  labs(
    x     = "Time (Days)",
    y     = "Proportion survived (%)",
  ) +
  theme_classic() +
  theme(
    plot.title   = element_text(size = 16, face = "bold"),
    legend.position      = c(0.01, 0.30),
    legend.justification = c("left", "top"),
    legend.background    = element_rect(fill = alpha("white", 0.75), color = NA),
    legend.text          = element_text(size = 12.5)
  ) +
  guides(
    colour   = guide_legend(nrow = 3),
    linetype = guide_legend(nrow = 2)
  )
KM_shared
