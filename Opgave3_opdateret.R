# opg.1 – Illustration af spørgsmålet

library(data.table)
library(ggplot2)
library(ordinal)
library(tidyverse)
library(RMariaDB)
library(lubridate)

# normalt read.csv virker ikke godt med store csv-file, derfor anvend her package data.table.
# funktion "fread" er samme som read.csv men hurtigere, alle control i read.csv som nrows, colclasses, sep blive håndtere automatisk
# encoding "Latin-1" kan håndtere dansk bogstaver æ, ø, å

df <- fread("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv", encoding = "Latin-1")

names(df)[names(df) == "Branchekode primær"] <- "BRANCHEKODE"

branchkode <- fread("Dansk-Branchekode-2007-(DB07)-v3-2014.csv", 
                    encoding = "Latin-1")

# gruppering Branche


df <- merge(df, branchkode[, c("BRANCHEKODE", "GRP10HOVEDAFDELING")], 
            by = "BRANCHEKODE", 
            all.x = TRUE)

# lave graf med branchen

df_b <- df %>%
  mutate(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget gode" ~ 1,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode" ~ 0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ 0,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlig" ~ -0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget dårlige" ~ -1
  ))

df_branchen <- df_b %>%
  group_by(`GRP10HOVEDAFDELING`) %>%
  summarise(nettotal = mean(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`, na.rm = TRUE))



df_branchen <- df_branchen %>%
  mutate(Branche = case_when(
    GRP10HOVEDAFDELING == "Bygge og anlæg" ~ "Bygge/anlæg",
    GRP10HOVEDAFDELING == "Ejendomshandel og udlejning" ~ "Ejendom",
    GRP10HOVEDAFDELING == "Erhvervsservice" ~ "Service",
    GRP10HOVEDAFDELING == "Handel og transport" ~ "Handel/trans",
    GRP10HOVEDAFDELING == "Industri, rŒstofindvinding og forsyningsvirksomhed" ~ "Industri",
    GRP10HOVEDAFDELING == "Information og kommunikation" ~ "Info/kom",
    GRP10HOVEDAFDELING == "Kultur, fritid og anden service" ~ "Kultur",
    GRP10HOVEDAFDELING == "Landbrug, skovbrug og fiskeri" ~ "Landbrug",
    GRP10HOVEDAFDELING == "Offentlig administration, undervisning og sundhed" ~ "Offentlig adm",
    TRUE ~ GRP10HOVEDAFDELING
  ))

df_branchen <- df_branchen[-9,]

df_branchen$Branche = factor(df_branchen$Branche,
                             levels = c("Bygge og anl¾g",      
                                        "Kultur",              
                                        "Landbrug",            
                                        "Industri",            
                                        "Handel/trans",         
                                        "Info/kom",            
                                        "Service",              
                                        "Ejendom"             
                             ))       

ggplot(df_branchen, aes(x = `Branche`, y = nettotal)) +
  geom_bar(stat = "identity", aes(fill = `Branche`)) +
  geom_text(aes(label = round(nettotal, 2)), vjust = -0.5) +  
  labs(title = "Ejendomshandel og udlejnings branchen synes selv\nde har størest muglihed for lån",
       x = "Antal branchen",
       y = "Nettotal",
       fill = "Antal branchen") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size = 12))


# for lav en grafisk illustration af fordelingen af svarene på spørgsmålet skal vi have en frekvense table
# der findes både dårlige og dårlig under kolonen
df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` <- gsub("Dårlige", "Dårlig", df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

svar_fordeling <- table(df[,1])
svar_df <- data.frame(Svar = names(svar_fordeling), Antal = as.numeric(svar_fordeling))

svar_df <- svar_df[-6,]

svar_df$Svar = factor(svar_df$Svar,
                      levels = c("Meget dårlige",              
                                 "Dårlig",            
                                 "Neutrale",            
                                 "Gode",
                                 "Meget gode"
                      )) 

svar_df$pct <- round(svar_df$Antal / sum(svar_df$Antal) * 100, digits = 1)

ggplot(svar_df, aes(x = Svar, y = pct)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(pct, 2)), position = position_dodge(width = 0.9), vjust = -0.5) + 
  theme_minimal() +
  labs(title = "Flest folk synes der er Gode mulighed for at låne penge til sin virksomhed",
       x = "Svarmulighed",
       y = "pct") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 10))

# for uddybe opg.1 mht. Baums artikler
# vi vil gerne se hvad har folk svaret ift. størrelse af virksomhed og det gør vi ved deler virksomhedens størrelse med antal ansatte og svarmuligheder med nettotal
df1.1 <- df %>%
  mutate(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget gode" ~ 1,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode" ~ 0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ 0,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlig" ~ -0.5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget dårlige" ~ -1
  )) %>%
  mutate(`Antal ansatte Cvr-nr.` = case_when(
    `Antal ansatte Cvr-nr.` <= 20 ~ "1-20 ansatte",
    `Antal ansatte Cvr-nr.` <= 50 ~ "21-50 ansatte",
    `Antal ansatte Cvr-nr.` <= 100 ~ "51-100 ansatte",
    `Antal ansatte Cvr-nr.` <= 400 ~ "100-400 ansatte",
    `Antal ansatte Cvr-nr.` > 400 ~ "Over 400 ansatte"
  ))

df_nettotal <- df1.1 %>%
  group_by(`Antal ansatte Cvr-nr.`) %>%
  summarise(nettotal = mean(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`, na.rm = TRUE))

# omdanne til facotr med levels for specifik rækkefølge
df_nettotal$`Antal ansatte Cvr-nr.` <- factor(df_nettotal$`Antal ansatte Cvr-nr.`, 
                                              levels = c("1-20 ansatte", 
                                                         "21-50 ansatte", 
                                                         "51-100 ansatte", 
                                                         "100-400 ansatte",
                                                         "Over 400 ansatte"))
# lave søjle diagram
ggplot(df_nettotal, aes(x = `Antal ansatte Cvr-nr.`, y = nettotal)) +
  geom_bar(stat = "identity", aes(fill = `Antal ansatte Cvr-nr.`)) +
  geom_text(aes(label = round(nettotal, 2)), vjust = -0.5) +  
  labs(title = "De største virksomheder synes bedst om finansieringsklimaet",
       x = "Antal ansatte",
       y = "Nettotal",
       fill = "Antal ansatte") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(size = 12))

# Nu vil vi gerne se med økonomisk perpektiv siden antallet af medarbejdere er ikke alene afgørende lånemulighederne
# Vi ved virksomhed med højt indtjening på aktiver er nemmere til at låne penge. Og jo robuste virksomheder er jo bedre adgang til lån
# Derfortil det næste graf kigge vi på afkastningsgrad og soliditetsgrad
# Vi skal første have gennemsnit af afkastningsgrad og soliditetsgrad fordi kolonner er blev delt fra 2016-2021

df_virksomhed_gns <- df %>%
  rowwise() %>%
  mutate(
    # Beregn gennemsnit soliditet per virksomhed
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    # Beregn gennemsnit afkast per virksomhed
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE)
  ) %>%
  ungroup()

# Nu deler vi den ift. svar muligheder

df_svar_gns <- df_virksomhed_gns %>%
  mutate(
    svar_kategori = case_when(
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Meget dårlige", "Dårlige") ~ "Meget dårlige/Dårlige",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ "Neutrale",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode"
    )
  ) %>%
  group_by(svar_kategori) %>%
  summarise(
    Soliditetsgrad = mean(gns_soliditet, na.rm = TRUE),
    Afkastningsgrad = mean(gns_afkast, na.rm = TRUE)
  )

df_svar_gns <- df_svar_gns[-4,]

# Nu laver vi graf
df_long <- df_svar_gns %>%
  pivot_longer(
    cols = c(Soliditetsgrad, Afkastningsgrad),
    names_to = "type",
    values_to = "værdi"
  )

df_long$svar_kategorii = factor(df_long$svar_kategori, 
                                levels = c( "Meget dårlige/Dårlige",
                                            "Neutrale",
                                            "Gode/Meget gode"
                                ))


ggplot(df_long, aes(x = type, y = værdi, fill = svar_kategorii)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(værdi, 2)), position = position_dodge(width = 0.9),
            vjust = -0.5) +  
  labs(title = "Virksomheder med et højere soliditetsgrad synes\nbedre om finansieringsklimaet",
       y = "Pct.",
       x = "",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 5))

# opg.2 – Cumulative Link Models

df_CLM <- df %>%
  mutate(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` = case_when(
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget gode" ~ 5,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Gode" ~ 4,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ 3,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Dårlig" ~ 2,
    `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Meget dårlige" ~ 1
  )) %>%
  filter(!is.na(`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)) %>%
  rowwise() %>%
  mutate(
    # Beregn gennemsnit soliditet per virksomhed
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    # Beregn gennemsnit afkast per virksomhed
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE),
    # Beregn gennemsnit balance per virksomhed
    gns_balance = mean(c(`Balance 2021 (1.000 kr)`,
                         `Balance 2020 (1.000 kr)`,
                         `Balance 2019 (1.000 kr)`,
                         `Balance 2018 (1.000 kr)`,
                         `Balance 2017 (1.000 kr)`,
                         `Balance 2016 (1.000 kr)`),
                       na.rm = TRUE),
    
    gns_likviditet = mean(c(`Likviditetsgrad 2021 (%)`,
                            `Likviditetsgrad 2020 (%)`,
                            `Likviditetsgrad 2019 (%)`,
                            `Likviditetsgrad 2018 (%)`,
                            `Likviditetsgrad 2017 (%)`,
                            `Likviditetsgrad 2016 (%)`),
                          na.rm = TRUE),
    alder = year(Sys.Date()) - year(dmy(Etableringsdato))
  ) %>%
  ungroup



df_CLM$gns_balance <- log(df_CLM$gns_balance)

df_CLM$lånmuligheder <- factor(df_CLM$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`,
                               levels = c("1", "2", "3", "4", "5"),
                               labels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode"),
                               ordered = TRUE)

df_CLM_clean <- df_CLM %>%
  mutate(across(c(gns_soliditet, gns_afkast, gns_balance), 
                ~scale(.) %>% as.vector))

test_clm1 <- clm(lånmuligheder~ gns_soliditet + 
                   gns_afkast + 
                   gns_balance +
                   gns_likviditet +
                   alder,
                 data = df_CLM, link = "logit")
summary(test_clm1)

model_clm1 <- clm(lånmuligheder~ gns_soliditet + 
                    gns_afkast + 
                    gns_balance +
                    gns_likviditet +
                    alder,
                  data = df_CLM, link = "logit")

summary(model_clm1)

model_clm2 <- clm(lånmuligheder~gns_balance, data = df_CLM, link = "logit")

summary(model_clm2)

# opg.3

# Først laver vi grunddata
df_3 <- df %>%
  rowwise() %>%
  mutate(
    gns_soliditet = mean(c(`Soliditetsgrad 2021 (%)`,
                           `Soliditetsgrad 2020 (%)`, 
                           `Soliditetsgrad 2019 (%)`,
                           `Soliditetsgrad 2018 (%)`,
                           `Soliditetsgrad 2017 (%)`,
                           `Soliditetsgrad 2016 (%)`),
                         na.rm = TRUE),
    gns_afkast = mean(c(`Afkastningsgrad 2021 (%)`,
                        `Afkastningsgrad 2020 (%)`,
                        `Afkastningsgrad 2019 (%)`,
                        `Afkastningsgrad 2018 (%)`,
                        `Afkastningsgrad 2017 (%)`,
                        `Afkastningsgrad 2016 (%)`),
                      na.rm = TRUE),
    # Del balance med 1000 for at få samme skala
    gns_balance = mean(c(`Balance 2021 (1.000 kr)`,
                         `Balance 2020 (1.000 kr)`,
                         `Balance 2019 (1.000 kr)`,
                         `Balance 2018 (1.000 kr)`,
                         `Balance 2017 (1.000 kr)`,
                         `Balance 2016 (1.000 kr)`),
                       na.rm = TRUE)
  ) %>%
  ungroup()

# Gruppering og kategorisering
df_3g <- df_3 %>%
  mutate(
    svar_kategori = case_when(
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Meget dårlige", "Dårlige") ~ "Meget dårlige/Dårlige",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` == "Neutrale" ~ "Neutrale",
      `Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)` %in% c("Gode", "Meget gode") ~ "Gode/Meget gode"
    )
  ) %>%
  filter(!is.na(svar_kategori)) %>%  # Fjern NA værdier
  group_by(svar_kategori) %>%
  summarise(
    Balance = mean(gns_balance, na.rm = TRUE)
  )

df_3g$Balance <- log(df_3g$Balance)

# Omstrukturering til long format
df_3long <- df_3g %>%
  pivot_longer(
    cols = c(gns_soliditet, gns_afkast, gns_balance),
    names_to = "type",
    values_to = "værdi"
  )

# Sæt korrekt rækkefølge på faktor levels
df_3g$svar_kategori <- factor(df_3g$svar_kategori,
                              levels = c("Meget dårlige/Dårlige",
                                         "Neutrale",
                                         "Gode/Meget gode"))

# Plot
ggplot(df_3g, aes(x = svar_kategori, y = Balance, fill = svar_kategori)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Balance, 2)), vjust = -0.5) +  
  labs(title = "Virksomheder med et højere balance\nsynes bedre om finansieringsklimaet",
       y = "log(balance)",
       x = "",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(0, 14),
                     breaks = seq(0, 14, by = 2))