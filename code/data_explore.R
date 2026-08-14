# to explore data ("EndDayDissection.csv")

# load packages
library(ggplot2) # to plot
library(dplyr) # to use the pipe operator %>%
library(patchwork)  # to combine multiple plots into one figure

# load data
cb_count <- read.csv("../data/EndDayDissection.csv")
# Transform multiple specific columns to factors
cb_count <- cb_count %>%
  mutate(across(c(date_d_m, sugar_conc, group, box, sex), as.factor))
str(cb_count)

# select the susceptible box only (box B)
cb_countB <- cb_count[cb_count$box == "B", ]
str(cb_countB)
# select box A (inoculated) only
cb_countA <- cb_count[cb_count$box == "A", ]
str(cb_countA)

## check discriptive stats
# box B
cb_countB %>%
  group_by(sugar_conc) %>%
  summarise(sampleN = length(conc), mean = mean(conc), variance = var(conc),
  zeroCount = length(conc[conc == 0])/length(conc) * 100)  # variance much larger than mean

# box A
cb_countA %>%
  group_by(sugar_conc) %>%
  summarise(sampleN = length(conc), mean = mean(conc), variance = var(conc), 
  zeroCount = length(conc[conc == 0])/length(conc) * 100)  # variance much larger than mean

# sex
cb_count %>%
  group_by(sex) %>%
  summarise(number_of_individuals = length(sex))

## plot check
# the number of samples per sugar concentration-----------------------------------------
# box A
p1 <- ggplot(cb_countA, aes(x = sugar_conc)) +
  geom_bar(width = 0.6) +
  stat_count(
    aes(label = after_stat(count)),  # auto-computed count as label
    geom  = "text",
    vjust = -0.5,                    # position slightly ABOVE the bar
    size  = 5,
    fontface = "bold"
  ) +
  labs(title = "Box inoculated", x = "Sugar concentration (w/w %)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
# -> all 3 groups varies, higher sugar % more samples

# box B
p2 <- ggplot(cb_countB, aes(x = sugar_conc)) +
  geom_bar(width = 0.6) +
  stat_count(
    aes(label = after_stat(count)),  # auto-computed count as label
    geom  = "text",
    vjust = -0.5,                    # position slightly ABOVE the bar
    size  = 5,
    fontface = "bold"
  ) +
  labs(title = "Box susceptible", x = "Sugar concentration (w/w %)", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")
# -> 10% have 2 more samples than others.

# save both plots as one figure
combined <- p1 + p2 +
  plot_annotation(
    title    = "Number of samples per sugar concentration",
    theme    = theme(
      plot.title    = element_text(size = 16, face = "bold"),
    )
  )
ggsave(
  filename = "../sandbox/sample_size.pdf",
  plot     = combined,
  width    = 12,    # inches
  height   = 5,     # inches
  dpi      = 300    # high resolution
)

# plot & save B cb conc vs. sugar % ----------------------------------------------
pdf("../sandbox/cb_sugar_EndDayBoxB.pdf", width = 6, height = 6)
plot(cb_countB$conc ~ cb_countB$train, xlab = "Sugar %", ylab = "Crithidia concentration (cells/ul)")
dev.off()
# plot & save A cb conc vs. sugar % (box A)
pdf("../sandbox/cb_sugar_EndDayBoxA.pdf", width = 8, height = 6)
plot(cb_countA$conc ~ cb_countA$train, xlab = "Sugar %", ylab = "Crithidia concentration (cells/ul)")
dev.off()

# percentage of 0 cb count in each sugar group----------------------------------------
per0_10 <- 100*length(cb_countB$conc[cb_countB$train == "10" & cb_countB$conc == 0])/length(cb_countB$conc[cb_countB$train == "10"])
per0_35 <- 100*length(cb_countB$conc[cb_countB$train == "35" & cb_countB$conc == 0])/length(cb_countB$conc[cb_countB$train == "35"])
per0_60 <- 100*length(cb_countB$conc[cb_countB$train == "60" & cb_countB$conc == 0])/length(cb_countB$conc[cb_countB$train == "60"])
per0N_10 <- 100-per0_10
per0N_35 <- 100-per0_35
per0N_60 <- 100-per0_60
# combine to one data frame & add more columns for plotting
sugar_CBper0 <- data.frame(sugar = c(10, 10, 35, 35, 60, 60), 
                         per = c(per0_10, per0N_10, per0_35, per0N_35, per0_60, per0N_60),
                         yn0 = c("0", "Non-0", "0", "Non-0", "0", "Non-0"))

# plot percentage of 0 cb count vs. sugar %
pdf("../sandbox/0cb_sugar_EndDayBoxB.pdf", width = 8, height = 6)

ggplot(data = sugar_CBper0, aes(x = sugar, y = per, fill = yn0)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(per, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Percentage of 0 C.bombi count vs. sugar %",
       x = "Sugar concentration (w/w %)", y = "Percentage (%)", fill = "C.bombi count") +
  theme_minimal()

dev.off()

