library(tidyverse)  
library(ggplot2)
library(psych)

df <- read.csv("C:/Users/DELL/OneDrive/Máy tính/FS_ss2025/Project2/epilepsy.csv", sep=";")
df_wide <- df %>%
  pivot_wider(names_from = visit, values_from = count, names_prefix = "count_")
head(df_wide)

df_wide_clean <- df_wide %>%
  group_by(patient) %>%
  summarise(
    Age  = first(na.omit(Age)),
    Base = first(na.omit(Base)),
    Trt  = first(na.omit(Trt)),
    count_1 = first(na.omit(count_1)),
    count_2 = first(na.omit(count_2)),
    count_3 = first(na.omit(count_3)),
    count_4 = first(na.omit(count_4)),
    count_mean = mean(c(count_1, count_2, count_3, count_4), na.rm = TRUE),
    .groups = "drop"
  )

pla <- df_wide_clean[df_wide_clean$Trt == 0, c("Age", "Base", "count_1", "count_2", "count_3", "count_4", "count_mean")]
describe(pla)
summary(pla)
round(apply(pla, 2, IQR),2)

pro <- df_wide_clean[df_wide_clean$Trt == 1, c("Age", "Base", "count_1", "count_2", "count_3", "count_4", "count_mean")]
describe(pro)
summary(pro)
round(apply(pro, 2, IQR),2)

#2
# Histogramm gemeinsam Baseline 
ggplot(df_wide_clean, aes(x = Base)) +
  geom_histogram(binwidth = 20, fill = "lightblue", color = "white", boundary = 0, closed = "left") +
  scale_x_continuous(
    breaks = seq(0, max(df_wide$Base), by = 20),
    expand = c(0, 0)
  ) +
  labs(
    x = "Anzahl der Krämpfe innerhalb der Baseline",
    y = "Absolute Häufigkeit"
  ) +
  theme(
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  )


# QQ-Plot gemeinsam Baseline
ggplot(df_wide_clean, aes(sample = Base)) + 
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

#3

# Boxplot Baseline nach Gruppe
ggplot(df_wide_clean, aes(x = factor(Trt), y = Base, fill = factor(Trt))) +
  geom_boxplot() +
  labs(x = "Gruppe", y = "Baseline-Krampfanzahl") +
  scale_fill_manual(values = c("firebrick", "forestgreen"), 
                    labels = c("Placebo", "Progabid")) +
  theme(
    legend.position = "none",  
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)   
  ) +
  scale_x_discrete(labels = c("Placebo", "Progabid")) 


# Boxplot Alter nach Gruppe
ggplot(df_wide_clean, aes(x = factor(Trt), y = Age, fill = factor(Trt))) +
  geom_boxplot() +
  labs(x = "Gruppe", y = "Alter") +
  scale_fill_manual(values = c("firebrick", "forestgreen"), 
                    labels = c("Placebo", "Progabid")) +
  theme(
    legend.position = "none",  
    axis.title.x = element_text(size = 25),  
    axis.title.y = element_text(size = 25),  
    axis.text.x = element_text(size = 20),  
    axis.text.y = element_text(size = 20)  
  ) +
  scale_x_discrete(labels = c("Placebo", "Progabid")) 


#4
df <- df %>%
  mutate(
    Trt = factor(Trt, labels = c("Placebo", "Progabid")),
    visit = factor(visit)  
  )

# Boxplot: count pro Visit und Gruppe
ggplot(df, aes(x = visit, y = count, fill = Trt)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "Klinikbesuch",
    y = "Anzahl der Krämpfe",
    fill = "Gruppe"
  ) +
  scale_fill_manual(values = c("firebrick", "forestgreen")) +
  theme(
    legend.position = c(0.9, 0.85),  
    legend.background = element_rect(fill = "white", color = "black"),  
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 12),    
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12) 
  )


#7
# count1
ggplot(pla, aes(sample = count_1)) + 
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

ggplot(pro, aes(sample = count_1)) + 
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

# count2
ggplot(pla, aes(sample = count_2)) + 
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

ggplot(pro, aes(sample = count_2)) + 
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

#count3
ggplot(pla, aes(sample = count_3)) + 
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

ggplot(pro, aes(sample = count_3)) + 
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
# count4
ggplot(pla, aes(sample = count_4)) + 
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

ggplot(pro, aes(sample = count_4)) + 
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


wilcox.test(pla$count_1, pro$count_1, alternative = 'greater')
cliff.delta(count_1 ~ Trt, data = df_wide_clean)

wilcox.test(pla$count_2, pro$count_2, alternative = 'greater')
cliff.delta(count_2 ~ Trt, data = df_wide_clean)

wilcox.test(pla$count_3, pro$count_3, alternative = 'greater')
cliff.delta(count_3 ~ Trt, data = df_wide_clean)

wilcox.test(pla$count_4, pro$count_4, alternative = 'greater')
cliff.delta(count_4 ~ Trt, data = df_wide_clean)

wilcox.test(pla$count_mean, pro$count_mean, alternative = 'greater')
cliff.delta(count_mean ~ Trt, data = df_wide_clean)

library(effsize)
cliff.delta(count_1 ~ Trt, data = df_wide_clean)
