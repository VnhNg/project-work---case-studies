library(ggplot2)
citation("ggplot2")
library(car)
citation("car")
library(readr)
library(tidyr)
library(ARTool)
toBibtex(citation("ARTool"))
library(effectsize)
citation("effectsize")

chandler <- read_csv("C:/Users/DELL/OneDrive/Máy tính/FS_ss2025/Project3/Chandler.csv")
chandler <- chandler[chandler$DROP==0,]
head(chandler)

# Define the anchor values
anchor <- list(
  pen = c(4, 3.998),
  Proteindrink = c(10, 9.8),
  lebron = c(0.5, 0.498),
  slidy = c(40, 39.75),
  Cheese = c(5, 4.85),
  Figurine = c(50, 49),
  TV = c(5000, 4998),
  beachhouse = c(800000, 799800),
  number = c(10000, 9989)
)

# Create anchor_diff columns
for (name in names(anchor)) {
  new_col_name <- paste0(name, "_diff")
  chandler[new_col_name] <- ifelse(chandler$Anchortype == "round", 
                                   (anchor[[name]][1] - chandler[[name]]) * 100 /anchor[[name]][1], 
                                   (anchor[[name]][2] - chandler[[name]]) * 100 /anchor[[name]][2])
}

# Check NA values
sum(apply(is.na(chandler[, names(anchor)]), 1, any))
chandler[apply(is.na(chandler[, names(anchor)]), 1, any),c("Participant",names(anchor))]

# Check negative values in diff
diff_cols <- paste0(names(anchor), "_diff")
sum(apply(chandler[, diff_cols]<0, 1, any, na.rm = TRUE))
chandler[apply(chandler[, diff_cols]<0, 1, any, na.rm = TRUE),c("Participant",diff_cols)]

# Neue mittlere Abweichung (nur positive Werte zählen, negative Wert = NA)
for (col in diff_cols) {
  chandler[[col]][chandler[[col]] < 0] <- NA
}
chandler$mean_diff <- rowMeans(chandler[, diff_cols], na.rm = TRUE)

# Check Normality for each group
ggplot(chandler[chandler$Condition==1,], aes(sample = mean_diff)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Empirische Quantile"
  ) +
  theme(
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  ) 
shapiro.test(chandler$mean_diff[chandler$Condition==1])
hist(chandler$mean_diff[chandler$Condition==1], breaks = 15)

ggplot(chandler[chandler$Condition==2,], aes(sample = mean_diff)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Empirische Quantile"
  ) +
  theme(
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  ) 
shapiro.test(chandler$mean_diff[chandler$Condition==2])
hist(chandler$mean_diff[chandler$Condition==2], breaks = 15)

ggplot(chandler[chandler$Condition==3,], aes(sample = mean_diff)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Empirische Quantile"
  ) +
  theme(
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  ) 
shapiro.test(chandler$mean_diff[chandler$Condition==3])
hist(chandler$mean_diff[chandler$Condition==3], breaks = 15)

ggplot(chandler[chandler$Condition==4,], aes(sample = mean_diff)) + 
  stat_qq() + 
  stat_qq_line() +
  labs(
    x = "Theoretische Quantile",
    y = "Empirische Quantile"
  ) +
  theme(
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  ) 
shapiro.test(chandler$mean_diff[chandler$Condition==4]) 
hist(chandler$mean_diff[chandler$Condition==4], breaks = 15)

# Check Homogeneity of variance
leveneTest(chandler$mean_diff, chandler$Condition)

chandler$Anchortype <- factor(chandler$Anchortype)
chandler$magnitude <- factor(chandler$magnitude)

# Führe die robuste zweifaktorielle ANOVA aus
art <- art(mean_diff ~ Anchortype * magnitude, data = chandler)
anova(art)


# Klassisches ANOVA-Modell berechnen
mod <- aov(mean_diff ~ Anchortype * magnitude, data = chandler)

eta_squared(mod, partial = TRUE)
1618.340/(1618.340+11471.980)
16901.745 /(16901.745 +11471.980)
6.898 /(6.898 +11471.980)
# Boxplot: count pro Visit und Gruppe
ggplot(chandler, aes(x = magnitude, y = mean_diff, fill = Anchortype)) + 
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Motivation",
    y = "MRD in %",
    fill = "Ankertyp"
  ) +
  scale_fill_manual(
    values = c("round" = "firebrick", "precise" = "forestgreen"),
    labels = c("round" = "rund", "precise" = "präzise")
  ) +
  scale_x_discrete(labels = c("0" = "klein", "1" = "größ")) +
  theme(
    legend.position = c(0.92, 0.15),  
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 12),    
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 12) 
  )


# Descriptive statistics by condition
library(dplyr)

# Overall descriptives
summary(chandler$mean_diff)

# By condition
chandler %>%
  group_by(Condition) %>%
  summarise(
    n = n(),
    min = min(mean_diff, na.rm = TRUE),
    median = median(mean_diff, na.rm = TRUE),
    mean = mean(mean_diff, na.rm = TRUE),
    max = max(mean_diff, na.rm = TRUE),
    sd = sd(mean_diff, na.rm = TRUE),
    IQR = quantile(mean_diff, 0.75, na.rm = TRUE) - quantile(mean_diff, 0.25, na.rm = TRUE)
  )

# By factors
chandler %>%
  group_by(Anchortype, magnitude) %>%
  summarise(
    n = n(),
    mean = mean(mean_diff, na.rm = TRUE),
    median = median(mean_diff, na.rm = TRUE),
    sd = sd(mean_diff, na.rm = TRUE),
    .groups = 'drop'
  )



