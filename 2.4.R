library(dplyr)
library(ggplot2)

country <- readRDS("newlife-bmi.rds")
bmi <- read.csv("ObesityDataSet.csv")

colnames(df)

df <- country[c(1,5,6,7,8,9,10,11,54:74)]
df_clean <- df %>% 
  distinct(Country.Code, .keep_all = TRUE)

df_clean <- na.omit(df_clean)

duplicates <- df_clean[duplicated(df_clean$Country.Code), ]
str(df_clean)

df_clean$AllOW <- as.numeric(df_clean$AllOW)
df_clean$MalesOverweight.BMI.25.29.9kg.m.. <- as.numeric(df_clean$MalesOverweight.BMI.25.29.9kg.m..)
df_clean$MalesObesity.BMI..30kg.m.. <- as.numeric(df_clean$MalesObesity.BMI..30kg.m..)
df_clean$FemalesOverweight.BMI.25.29.9kg.m.. <- as.numeric(df_clean$FemalesOverweight.BMI.25.29.9kg.m..)
df_clean$FemalesObesity.BMI..30kg.m.. <- as.numeric(df_clean$FemalesObesity.BMI..30kg.m..)

df_clean <- df_clean %>% 
  mutate(ObesityRate = rowSums(across(c(AllOW, AllOB)), na.rm = TRUE)) %>% 
  mutate(FemaleObesityRate = rowSums(across(c(FemalesOverweight.BMI.25.29.9kg.m.., FemalesObesity.BMI..30kg.m..)), na.rm = TRUE)) %>% 
  mutate(MaleObesityRate = rowSums(across(c(MalesOverweight.BMI.25.29.9kg.m.., MalesObesity.BMI..30kg.m..)), na.rm = TRUE))

summary(df_clean$X2020)
summary(df_clean$ObesityRate)
summary(df_clean$FemaleObesityRate)
summary(df_clean$MaleObesityRate)

  # Korrelation mellem life expectancy og andel overvægtige
cor(df_clean$X2020, df_clean$ObesityRate, use = "complete.obs")

  # Scatterplot
plot(df_clean$ObesityRate, df_clean$X2020, 
     main = "Sammenhæng mellem andel overvægtige og life expectancy",
     xlab = "Andel overvægtige (%)",
     ylab = "Life Expectancy",
     pch = 16, col = "blue")
abline(lm(X2020 ~ ObesityRate, data = df_clean), col = "red", lwd = 2)

  # gns. obesity rate pr forventet levealder
plot_data <- df_clean %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(ObesityRate = mean(ObesityRate, na.rm = TRUE))

ggplot(plot_data, aes(x = factor(X2020), y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gennemsnitlig Obesity Rate (BMI >25) pr. Levealder",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

model <- lm(X2020 ~ ObesityRate, data = df_clean)
# Se resultaterne
summary(model)

  # undersøg bmi for USA datasæt
bmi <- bmi %>% 
  mutate(BMI = Weight / (Height^2)) %>% 
  mutate(BMI = round(BMI,1))

summary(bmi$BMI)
hist(bmi$BMI, main = "BMI-fordeling", xlab = "BMI", col = "steelblue")
meanBMI <- mean(bmi$BMI > 25, na.rm = TRUE)
summary(df_clean$ObesityRate)

# gns. obesity rate pr forventet levealder - kvinder
plot_data_female <- df_clean %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(FemaleObesityRate = mean(FemaleObesityRate, na.rm = TRUE))

ggplot(plot_data_female, aes(x = factor(X2020), y = FemaleObesityRate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gennemsnitlig Obesity Rate (BMI >25) pr. Levealder for kvinder",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

# gns. obesity rate pr forventet levealder - mænd
plot_data_male <- df_clean %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(MaleObesityRate = mean(MaleObesityRate, na.rm = TRUE))

ggplot(plot_data_male, aes(x = factor(X2020), y = MaleObesityRate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gennemsnitlig Obesity Rate (BMI >25) pr. Levealder for mænd",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

# gns. obesity rate pr forventet levealder - AllOW
plot_data_OW <- df_clean %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(allOW = mean(AllOW, na.rm = TRUE))

ggplot(plot_data_OW, aes(x = factor(X2020), y = allOW)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gennemsnitlig BMI (25-29.9) pr. Levealder",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

# gns. obesity rate pr forventet levealder - AllOB
plot_data_OB <- df_clean %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(allOB = mean(AllOB, na.rm = TRUE))

ggplot(plot_data_OB, aes(x = factor(X2020), y = allOB)) +
  geom_col(fill = "steelblue") +
  labs(title = "Gennemsnitlig BMI (>30) pr. Levealder",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

africaCC <- c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", 
                  "COM", "COD", "COG", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", 
                  "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", 
                  "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "RWA", 
                  "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", "TZA", "TGO", 
                  "TUN", "UGA", "ZMB", "ZWE")

# Filtrering af rækker, hvor Country.Code matcher en af koderne
africa_bmi <- df_clean %>% 
  filter(Country.Code %in% africaCC)
  
# gns. obesity rate pr forventet levealder - afrika
plot_data_africa <- africa_bmi %>%
  group_by(X2020) %>%
  mutate(X2020 = round(X2020)) %>% 
  summarise(ObesityRate = mean(ObesityRate, na.rm = TRUE))

ggplot(plot_data_africa, aes(x = factor(X2020), y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  labs(title = "",
       x = "Forventet levealder",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal()

global_avg_obesityRate <- mean(df_clean$ObesityRate, na.rm = T)

ggplot(africa_bmi, aes(x = Country, y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = global_avg_obesityRate, color = "orange", linetype = "dashed", size = 1) +
  annotate("text", x = 5, y = global_avg_obesityRate + 1, label = "Global gennemsnitlig obesity rate", color = "orange", size = 5, fontface = "bold") +
  labs(title = "Obesity Rate i Afrika",
       x = "Country",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_30_wealth <- c("LUX", "SGP", "IRL", "NOR", "QAT", "ARE", "CHE", "USA", "DNK", 
                   "NLD", "BRN", "ISL", "AUT", "BEL", "SWE", "DEU", "AUS", "BHR", 
                   "SAU", "FIN", "CAN", "KWT", "MLT", "FRA", "GBR", "NZL", "ITA", 
                   "KOR", "SVN", "CZE")

wealth_bmi <- df_clean %>% 
  filter(Country.Code %in% top_30_wealth)

ggplot(wealth_bmi, aes(x = Country, y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = global_avg_obesityRate, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 5, y = global_avg_obesityRate + 1, label = "Global gennemsnitlig obesity rate", color = "red", size = 4, fontface = "bold") +
  labs(title = "Obesity Rate i de 30 rigeste baseret på BNP",
       x = "Country",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_20_obesity <- df_clean %>%
  arrange(desc(ObesityRate)) %>%  # Sorter i faldende rækkefølge
  slice_head(n = 20)

top_20CC <- top_20_obesity$Country.Code
top_20CC

ggplot(top_20_obesity, aes(x = Country, y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = global_avg_obesityRate, color = "orange", linetype = "dashed", size = 1) +
  annotate("text", x = 5, y = global_avg_obesityRate + 1, label = "Global gennemsnitlig obesity rate", color = "orange", size = 5, fontface = "bold") +
  labs(title = "Top 20 lande med højeste Obesity Rate",
       x = "Country",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(top_20_obesity, aes(x = Country, y = ObesityRate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = global_avg_obesityRate, color = "orange", linetype = "dashed", size = 1) +
  annotate("label", x = 5, y = global_avg_obesityRate + 1, 
           label = "Global gennemsnitlig obesity rate", 
           color = "orange", fill = "white", size = 5, fontface = "bold") +
  labs(title = "Top 20 lande med højeste Obesity Rate",
       x = "Country",
       y = "Gennemsnitlig Obesity Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

