# Pakete laden
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(effects)

# CSV-Datei einlesen (achte auf korrekte Pfadangabe)
df <- read.csv2("C:/Users/DELL/OneDrive/Máy tính/FS_ss2025/Project4/lebenserwartung.csv")
head(df)
str(df)
summary(df)

# Neue Variablen pro Kopf berechnen
df <- df %>%
  mutate(
    leistungsempfaenger_quote = leistungsempfaenger_innen / bevoelkerung,
    ha_Insgesamt_pro_kopf = ha_Insgesamt / bevoelkerung,
    ha_Verkehr_pro_kopf = ha_Verkehr / bevoelkerung,
    ha_Siedlung_pro_kopf = ha_Siedlung / bevoelkerung
  )

# Standardisierte Variablen
standard_vars <- c(
  "gisd_score",
  "leistungsempfaenger_quote",
  "ha_Siedlung_pro_kopf"
)
df_std <- df %>%
  mutate(across(all_of(standard_vars), scale))

# Histogramm
wichtige_variablen <- c(
  "Female_50.", "Male_50.",
  "gisd_score",
  "bevoelkerung",
  "leistungsempfaenger_quote",
  "ha_Insgesamt_pro_kopf",
  "ha_Verkehr_pro_kopf",
  "ha_Siedlung_pro_kopf"
)
plots <- lapply(wichtige_variablen, function(var) {
  ggplot(df, aes_string(x = var)) +
    geom_histogram(bins = 10, fill = "steelblue", color = "black") +
    theme_minimal() +
    ggtitle(var)
})
grid.arrange(grobs = plots, ncol = 4)

# 3
library(tidyr)

# Daten in Long-Format umwandeln für beide Zielvariablen
df_long <- df %>%
  select(Female_50., Male_50., all_of(standard_vars)) %>%
  pivot_longer(cols = all_of(standard_vars), names_to = "Praediktor", values_to = "Wert")

label_names <- c(
  gisd_score = "GISD-Score",
  leistungsempfaenger_quote = "LEA",
  ha_Siedlung_pro_kopf = "SFK in Hektar"
)


# Plot für Female_50.
p_female <- ggplot(df_long, aes(x = Wert, y = Female_50.)) +
  geom_point(alpha = 0.6, colour = "red") +
  geom_smooth(
    method = "loess",
    method.args = list(span = 1, degree = 1, family = "gaussian"),
    se = FALSE,
    colour = "black"
  ) +
  facet_wrap(
    ~Praediktor,
    scales = "free_x",
    labeller = as_labeller(label_names)
  ) +
  labs(
    y = "Lebenserwartung (Frauen)",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14)
  )
p_female


# Plot für Male_50.
p_male <- ggplot(df_long, aes(x = Wert, y = Male_50.)) +
  geom_point(alpha = 0.6, colour = "steelblue") +
  geom_smooth(
    method = "loess",
    method.args = list(span = 1, degree = 1, family = "gaussian"),
    se = FALSE,
    colour = "black"
  ) +
  facet_wrap(
    ~Praediktor,
    scales = "free_x",
    labeller = as_labeller(label_names)
  ) +
  labs(
    y = "Lebenserwartung (Männer)",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14)
  )
p_male

# 4
library(car)
# Modelle für Frauen
mod_f1 <- lm(Female_50. ~ gisd_score, data = df_std)
mod_f2 <- lm(Female_50. ~ gisd_score + ha_Siedlung_pro_kopf, data = df_std)
mod_f3 <- lm(Female_50. ~ gisd_score + ha_Siedlung_pro_kopf + leistungsempfaenger_quote, data = df_std)

summary(mod_f3)
vif(mod_f3)
summary(mod_f2)
vif(mod_f2)

# plot f3
resid_f3_df <- data.frame(
  Fitted_f3 = fitted(mod_f3),
  Residuals_f3 = resid(mod_f3),
  Standardized_f3 = rstandard(mod_f3)
)
p_f3_resid <- ggplot(resid_f3_df, aes(x = Fitted_f3, y = Residuals_f3)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # LOESS-Kurve
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuen vs. Geschätzte Werte",
    x = "Geschätzte Werte",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  # Textgröße erhöhen
# Q-Q Plot Daten
qq_f3 <- qqnorm(rstandard(mod_f3), plot.it = FALSE)
qq_f3_df <- data.frame(
  Theoretical_f3 = qq_f3$x,
  Sample_f3 = qq_f3$y
)
# Q-Q Plot für mod_f3
p_f3_qq <- ggplot(qq_f3_df, aes(x = Theoretical_f3, y = Sample_f3)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Q-Q-Plot",
    x = "Theoretische Qualtile",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  
grid.arrange(p_f3_resid, p_f3_qq, ncol = 2)


# Daten für ggplot vorbereiten – für mod_f2 (Frauenmodell 2 Prädiktoren)
resid_f2_df <- data.frame(
  Fitted_f2 = fitted(mod_f2),
  Residuals_f2 = resid(mod_f2),
  Standardized_f2 = rstandard(mod_f2)
)
p_f2_resid <- ggplot(resid_f2_df, aes(x = Fitted_f2, y = Residuals_f2)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # LOESS-Kurve
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuen vs. Geschätzte Werte",
    x = "Geschätzte Werte",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  # Textgröße erhöhen
# Q-Q Plot Daten
qq_f2 <- qqnorm(rstandard(mod_f2), plot.it = FALSE)
qq_f2_df <- data.frame(
  Theoretical_f2 = qq_f2$x,
  Sample_f2 = qq_f2$y
)
# Q-Q Plot für mod_f2
p_f2_qq <- ggplot(qq_f2_df, aes(x = Theoretical_f2, y = Sample_f2)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Q-Q-Plot",
    x = "Theoretische Qualtile",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  
grid.arrange(p_f2_resid, p_f2_qq, ncol = 2)

# Modelle für Männer
mod_m1 <- lm(Male_50. ~ gisd_score, data = df_std)
mod_m2 <- lm(Male_50. ~ gisd_score + ha_Siedlung_pro_kopf, data = df_std)
mod_m3 <- lm(Male_50. ~ gisd_score + ha_Siedlung_pro_kopf + leistungsempfaenger_quote, data = df_std)

summary(mod_m3)
vif(mod_m3)
summary(mod_m2)
vif(mod_m2)

# plot m3
resid_m3_df <- data.frame(
  Fitted_m3 = fitted(mod_m3),
  Residuals_m3 = resid(mod_m3),
  Standardized_m3 = rstandard(mod_m3)
)
p_m3_resid <- ggplot(resid_m3_df, aes(x = Fitted_m3, y = Residuals_m3)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # LOESS-Kurve
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuen vs. Geschätzte Werte",
    x = "Geschätzte Werte",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  # Textgröße erhöhen
# Q-Q Plot Daten
qq_m3 <- qqnorm(rstandard(mod_m3), plot.it = FALSE)
qq_m3_df <- data.frame(
  Theoretical_m3 = qq_m3$x,
  Sample_m3 = qq_m3$y
)
# Q-Q Plot für mod_m3
p_m3_qq <- ggplot(qq_m3_df, aes(x = Theoretical_m3, y = Sample_m3)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Q-Q-Plot",
    x = "Theoretische Qualtile",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  
grid.arrange(p_m3_resid, p_m3_qq, ncol = 2)


# Daten für ggplot vorbereiten – für mod_m2 (Frauenmodell 2 Prädiktoren)
resid_m2_df <- data.frame(
  Fitted_m2 = fitted(mod_m2),
  Residuals_m2 = resid(mod_m2),
  Standardized_m2 = rstandard(mod_m2)
)
p_m2_resid <- ggplot(resid_m2_df, aes(x = Fitted_m2, y = Residuals_m2)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # LOESS-Kurve
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuen vs. Geschätzte Werte",
    x = "Geschätzte Werte",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  # Textgröße erhöhen
# Q-Q Plot Daten
qq_m2 <- qqnorm(rstandard(mod_m2), plot.it = FALSE)
qq_m2_df <- data.frame(
  Theoretical_m2 = qq_m2$x,
  Sample_m2 = qq_m2$y
)
# Q-Q Plot für mod_m2
p_m2_qq <- ggplot(qq_m2_df, aes(x = Theoretical_m2, y = Sample_m2)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Q-Q-Plot",
    x = "Theoretische Qualtile",
    y = "Residuen"
  ) +
  theme_minimal(base_size = 14)  
grid.arrange(p_m2_resid, p_m2_qq, ncol = 2)


# 7
# Effektplot: Einfluss des GISD auf Female_50.
plot(allEffects(mod_f2), main = "Effekte auf Lebenserwartung (Frauen)")

# Effektplot: Einfluss des GISD auf Male_50.
plot(allEffects(mod_m2), main = "Effekte auf Lebenserwartung (Männer)")

# 8
# Frauenmodell mit Interaktion
mod_f_inter <- lm(Female_50. ~ gisd_score * ha_Siedlung_pro_kopf, data = df_std)
summary(mod_f_inter)

# Männermodell mit Interaktion
mod_m_inter <- lm(Male_50. ~ gisd_score * ha_Siedlung_pro_kopf, data = df_std)
summary(mod_m_inter)

# Effekt der Interaktion für Frauen
plot(effect("gisd_score:ha_Siedlung_pro_kopf", mod_f_inter),
     main = "Interaktion: GISD × Siedlungsfläche (Frauen)",
     multiline = TRUE)


# Effekt der Interaktion für Männer
plot(effect("gisd_score:ha_Siedlung_pro_kopf", mod_m_inter),
     main = "Interaktion: GISD × Siedlungsfläche (Männer)",
     multiline = TRUE)

par(mfrow = c(2, 3))
plot(mod_f2, which = 1)
plot(mod_f2, which = 2)
plot(mod_f2, which = 3)
plot(mod_f2, which = 4)
plot(mod_f2, which = 5)
plot(mod_f2, which = 6)

plot(mod_m2, which = 1)
plot(mod_m2, which = 2)
plot(mod_m2, which = 3)
plot(mod_m2, which = 4)
plot(mod_m2, which = 5)
plot(mod_m2, which = 6)


#9 
# Cook m2
cook_df <- data.frame(
  Beobachtung = 1:nrow(df_std),
  CookD = cooks.distance(mod_f2)
)
threshold <- 4 / (nrow(df_std) - length(coef(mod_f2)))  
ggplot(cook_df, aes(x = Beobachtung, y = CookD)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_hline(yintercept = threshold, color = "black", linetype = "dashed") +
  geom_text(
    data = subset(cook_df, CookD > threshold),
    aes(label = Beobachtung),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Einfluss einzelner Beobachtungen (Frauen)",
    x = "Beobachtung",
    y = "Cooks Distanz"
  ) +
  theme_minimal(base_size = 14)


# Cook m2
cook_df <- data.frame(
  Beobachtung = 1:nrow(df_std),
  CookD = cooks.distance(mod_m2)
)
threshold <- 4 / (nrow(df_std) - length(coef(mod_m2)))  
ggplot(cook_df, aes(x = Beobachtung, y = CookD)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = threshold, color = "black", linetype = "dashed") +
  geom_text(
    data = subset(cook_df, CookD > threshold),
    aes(label = Beobachtung),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Cook's Distance – Einfluss einzelner Beobachtungen (Männer)",
    x = "Beobachtung",
    y = "Cooks Distanz"
  ) +
  theme_minimal(base_size = 14)










