library(ggplot2)
library(patchwork)
library(dplyr)

ESS11 <- read.csv("C:/Users/DELL/OneDrive/Máy tính/FS_ss2025/Project5/ESS11.csv", sep=";")
length(unique(ESS11$cntry))
#1
ESS11$vacc19_bin <- ifelse(ESS11$vacc19 == 1, 1,
                           ifelse(ESS11$vacc19 == 2, 0, NA))
#2
# Überblick über Vertrauensvariablen
trst_vars <- c("trstprl", "trstlgl", "trstplc", "trstplt", "trstprt")
ESS11$trst_mean <- rowMeans(ESS11[, trst_vars], na.rm = TRUE)

#3
targets <- c("AT", "BE", "CZ", "DE", "FR") 
land_namen <- c(
  "DE" = "Deutschland",
  "AT" = "Österreich",
  "BE" = "Belgien",
  "FR" = "Frankreich",
  "CZ" = "Tschechien"
)
model_vars <- c("vacc19_bin", "agea", "eisced", "hinctnta",
                "respc19a", "trst_mean", "netusoft", "gndr",
                "hhmmb", "domicil", "maritalb")

ESS11_EU <- ESS11[ESS11$cntry %in% targets,]
ESS11_EU_clean <- ESS11_EU[complete.cases(ESS11_EU[, model_vars]), ]

# Tabelle: Absolute Häufigkeiten nach Impfstatus und Land
table_counts <- ESS11_EU_clean %>%
  filter(cntry %in% names(land_namen)) %>%
  mutate(cntry = as.character(cntry),
         Land = land_namen[cntry],
         Impfstatus = ifelse(vacc19_bin == 1, "Geimpft", "Nicht geimpft")) %>%
  count(Land, Impfstatus) %>%
  tidyr::pivot_wider(names_from = Impfstatus, values_from = n, values_fill = 0) %>%
  arrange(Land)
print(table_counts)


# Balkendiagramm 
impf_counts <- ESS11_EU_clean %>%
  filter(cntry %in% names(land_namen)) %>%
  group_by(cntry, vacc19_bin) %>%
  summarise(Anzahl = n(), .groups = "drop") %>%
  mutate(Impfstatus = ifelse(vacc19_bin == 1, "Geimpft", "Nicht geimpft"),
         Land = land_namen[cntry])

impf_counts$Land <- factor(impf_counts$Land, levels = c("Belgien", "Deutschland", "Frankreich", "Österreich"))
impf_counts$Impfstatus <- factor(impf_counts$Impfstatus, levels = c("Geimpft", "Nicht geimpft"))
ggplot(impf_counts, aes(x = Anzahl, y = Land, fill = Impfstatus)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  scale_fill_manual(values = c("Geimpft" = "darkgreen", "Nicht geimpft" = "darkred")) +
  scale_y_discrete(limits = rev(levels(impf_counts$Land))) + 
  labs(x = "Anzahl Personen", y = NULL, fill = "Impfstatus"
       ) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size = 12),       
    legend.title = element_text(size = 14),      
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )

#4
# Farbpalette für Impfstatus
impf_colors <- c("0" = "darkred", "1" = "darkgreen")

# Tabellen für kategoriale Variablen
cat_vars <- c("eisced", "hinctnta", "respc19a", "netusoft", "gndr", "domicil")
lapply(cat_vars, function(var) {
  print(var)
  table(ESS11_EU_clean[[var]], ESS11_EU_clean$vacc19_bin)
})

# Boxplots für numerische Variablen
num_vars <- c("agea", "trst_mean", "hhmmb")

make_boxplot <- function(var, ylab) {
  ggplot(ESS11_EU_clean, aes(x = factor(vacc19_bin), y = .data[[var]], fill = factor(vacc19_bin))) +
    geom_boxplot() +
    scale_fill_manual(values = impf_colors, guide = FALSE) +
    scale_x_discrete(labels = c("0" = "Nicht geimpft", "1" = "Geimpft")) +
    labs(x = "Impfstatus", y = ylab) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 30),
      axis.text = element_text(size = 20)
    )
}

p1 <- make_boxplot("agea", "Alter"); p1
p2 <- make_boxplot("trst_mean", "Vertrauensindex"); p2
p3 <- make_boxplot("hhmmb", "Haushaltsgröße"); p3
p1 + p2 + p3


# Scatterplot overlay
plot_data <- ESS11_EU_clean %>%
  group_by(agea, trst_mean) %>%
  mutate(overlap = n_distinct(vacc19_bin) > 1) %>%
  ungroup() %>%
  mutate(
    punkt_typ = case_when(
      overlap ~ "Überlappung",
      vacc19_bin == 1 ~ "Geimpft",
      vacc19_bin == 0 ~ "Nicht geimpft"
    )
  )

farben <- c("Geimpft" = "darkgreen", "Nicht geimpft" = "darkred", "Überlappung" = "black")
formen <- c("Geimpft" = 8, "Nicht geimpft" = 8, "Überlappung" = 19)

ggplot(plot_data, aes(x = agea, y = trst_mean)) +
  geom_point(aes(color = punkt_typ, shape = punkt_typ), size = 2.5, alpha = 0.7) +
  scale_color_manual(values = farben, guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = formen, guide = guide_legend(title = NULL)) +
  labs(
    x = "Alter",
    y = "Vertrauensindex"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = "white", color = "gray60"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
  )



#7
str(ESS11_EU_clean[model_vars])
ESS11_EU_clean[cat_vars] <- lapply(ESS11_EU_clean[cat_vars], factor)

# Modellformulierung
model <- glm(vacc19_bin ~ agea + eisced + hinctnta + respc19a +
               trst_mean + netusoft + gndr + hhmmb + domicil,
             data = ESS11_EU_clean,
             family = binomial(link = "logit")) 

# stepwise
n <- nrow(ESS11_EU_clean)
bic_model <- step(model, direction = "both", k = log(n), trace = 10)  
BIC(bic_model)
summary(bic_model)

# interpretation



# Confusion Matrix mit caret
threshold <- 0.5

predicted_probs <- predict(best_model, type = "response")
predicted_class <- ifelse(predicted_probs >= threshold, 1, 0)

library(caret)
citation("caret")
confusionMatrix(factor(predicted_class),
                factor(ESS11_EU_clean$vacc19_bin),
                positive = "1")

  

# all combinations
library(MuMIn)
citation("MuMIn")
options(na.action = "na.fail")  # wichtig für dredge
model_set <- dredge(model, rank = "BIC")  # oder rank = "BIC"

# top5
head(model_set, 5)

best_model <- get.models(model_set, 1)[[1]]
summary(best_model)

# VIF - multicolinearity
library(car)
vif(bic_model)
vif(best_model)

# scatterplot - perfekte seperation
plot_data <- ESS11_EU_clean %>%
  group_by(agea, trst_mean) %>%
  mutate(overlap = n_distinct(vacc19_bin) > 1) %>%
  ungroup() %>%
  mutate(
    punkt_typ = case_when(
      overlap ~ "Überlappung",
      vacc19_bin == 1 ~ "Geimpft",
      vacc19_bin == 0 ~ "Nicht geimpft"
    )
  )

farben <- c("Geimpft" = "darkgreen", "Nicht geimpft" = "darkred", "Überlappung" = "black")
formen <- c("Geimpft" = 8, "Nicht geimpft" = 8, "Überlappung" = 19)

ggplot(plot_data, aes(x = agea, y = trst_mean)) +
  geom_point(aes(color = punkt_typ, shape = punkt_typ), size = 2.5, alpha = 0.7) +
  scale_color_manual(values = farben, guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = formen, guide = guide_legend(title = NULL)) +
  labs(
    x = "Alter",
    y = "Vertrauensindex"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_rect(fill = "white", color = "gray60"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
  )



