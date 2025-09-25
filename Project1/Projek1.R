library(ggplot2)
library(gridExtra)
library(GGally)
library(psych)


crime_data <- read.csv("C:/Users/DELL/OneDrive/Máy tính/FS_ss2025/Project1/Data.txt", sep="")


str(crime_data)
summary(crime_data)

# 1. Data preparation

sum(is.na(crime_data))

# Create derived variables (per capita metrics)
crime_data$ausgaben_per_capita <- crime_data$Ausgaben / crime_data$Einwohner * 100
crime_data$anwaelte_per_capita <- crime_data$Anwaelte / crime_data$Einwohner * 100
crime_data$staatsdiener_per_capita <- crime_data$Staatsdiener / crime_data$Einwohner * 100
crime_data$beobachtung_per_capita <- crime_data$Beobachtung / crime_data$Einwohner * 100
crime_data$Verbrechen <- crime_data$Verbrechen 
crime_data$population_density <- crime_data$Einwohner / crime_data$Flaeche 

cd <- crime_data[,c("ausgaben_per_capita", "Verbrechen", "anwaelte_per_capita", 
              "staatsdiener_per_capita", "beobachtung_per_capita", "population_density")]
describe(cd)
round(apply(cd, 2, IQR),2)

# 2. Outlier detection


box_A <- ggplot(crime_data, aes(x = ausgaben_per_capita)) + 
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Nomierte Ausgaben (in $1.000)") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.3, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

box_B <- ggplot(crime_data, aes(x = Verbrechen)) + 
  geom_boxplot(fill = "darkred", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Verbrechensrate") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0.2, 0.2, 0.3, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

box_C <- ggplot(crime_data, aes(x = anwaelte_per_capita)) + 
  geom_boxplot(fill = "lightgreen", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Anwalt-Dichte") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0.2, 0.2, 0.3, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

box_D <- ggplot(crime_data, aes(x = staatsdiener_per_capita)) + 
  geom_boxplot(fill = "darkviolet", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Staatsdiener-Dichte") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0.2, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

box_E <- ggplot(crime_data, aes(x = beobachtung_per_capita)) + 
  geom_boxplot(fill = "gold", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Kriminalistische Beobachtungsdichte") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0.2, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

box_F <- ggplot(crime_data, aes(x = population_density)) + 
  geom_boxplot(fill = "darkblue", color = "black", outlier.color = "red", outlier.shape = 8) +
  labs(x = "Bevölkerungsdichte pro km²") +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.margin = unit(c(0.3, 0.2, 0.2, 0.2), "cm"),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size=30),)

grid.arrange(box_A, box_B, box_C, box_D, box_E, box_F, nrow = 2, ncol = 3, padding = unit(1, "mm"))

# Identify outliers for spending per capita
identify_outliers <- function(data, column_name) {
  
  quartiles <- quantile(data[[column_name]], probs=c(0.25, 0.75))
  iqr <- quartiles[2] - quartiles[1]
  
  
  lower_bound <- quartiles[1] - 1.5 * iqr
  upper_bound <- quartiles[2] + 1.5 * iqr
  
 
  outliers <- data[data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, ]
  
  print(paste("Outliers in", column_name, ":"))
  print(outliers[[1]])  # Assuming the first column contains the 'Staat' column
  

  return(data[data$Staat %in% outliers$Staat, ])
}
outlier_data <- identify_outliers(crime_data, "ausgaben_per_capita")
outlier_data <- identify_outliers(crime_data, "Verbrechen")
outlier_data <- identify_outliers(crime_data, "anwaelte_per_capita")
outlier_data <- identify_outliers(crime_data, "staatsdiener_per_capita")
outlier_data <- identify_outliers(crime_data, "beobachtung_per_capita")
outlier_data <- identify_outliers(crime_data, "population_density")


crime_data1 <- crime_data[!crime_data$Staat %in% c("DC"),]
# Create individual histograms with customizations
hist_A <- ggplot(crime_data1, aes(x = ausgaben_per_capita)) + 
  geom_histogram(bins = 14, fill = "lightblue", color = "black") +
  labs(x = "Nomierte Ausgaben (in $1.000)", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)

hist_B <- ggplot(crime_data1, aes(x = Verbrechen)) + 
  geom_histogram(bins = 7, fill = "darkred", color = "black") +
  labs(x = "Verbrechensrate", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)

hist_C <- ggplot(crime_data1, aes(x = anwaelte_per_capita)) + 
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
  labs(x = "Anwalt-Dichte", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)

hist_D <- ggplot(crime_data1, aes(x = staatsdiener_per_capita)) + 
  geom_histogram(bins = 10, fill = "darkviolet", color = "black") +
  labs(x = "Staatsdiener-Dichte", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=16),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)

hist_E <- ggplot(crime_data1, aes(x = beobachtung_per_capita)) + 
  geom_histogram(bins = 10, fill = "gold", color = "black") +
  labs(x = "K. Beobachtungsdichte", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)

hist_F <- ggplot(crime_data1, aes(x = population_density)) + 
  geom_histogram(bins = 10, fill = "darkblue", color = "black") +
  labs(x = "Bevölkerungsdichte pro km²", y = "Anzahl der Staaten") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size=12),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size=12),)


grid.arrange(hist_A, hist_B, hist_C, hist_D, hist_E, hist_F, nrow = 2, ncol = 3)


# 3. Exploratory Analysis


colnames(cd) <- c("Ausgaben", "Verbrechensrate", "Anwaltsdicht", "Staatdienersdichte", "Beobachtungsdichte", "Bevölkerungsdichte")
ggpairs(cd,
        upper = list(continuous = GGally::wrap(ggally_cor, stars = F)),
        diag = 'blank')

crime_data2 <- crime_data[!crime_data$Staat %in% c("DC","AK"),]
cd2 <- crime_data2[,c("ausgaben_per_capita", "Verbrechen", "anwaelte_per_capita", 
                    "staatsdiener_per_capita", "beobachtung_per_capita", "population_density")]
colnames(cd2) <- c("Ausgaben", "Verbrechensrate", "Anwaltsdicht", "Staatdienersdichte", "Beobachtungsdichte", "Bevölkerungsdichte")
ggpairs(cd2,
        upper = list(continuous = GGally::wrap(ggally_cor, stars = F)),
        diag = 'blank')






