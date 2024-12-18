library(rvest)
library(xml2)
library(httr)
library(RMariaDB)
library(dplyr)
library(lubridate)
library(logr)

log_path <- sprintf("/tmp/Airmeas_%s.log", format(Sys.time(), "%Y%m%d"))

log_open(log_path)

log_print(paste(Sys.time(),"Nu køre script"))

base_url <- "https://envs2.au.dk"

log_print(paste(Sys.time(),"Begynde med webscrape HCAB"))

# HCAB
initial_res_hcab <- GET(paste0(base_url, "/Luftdata/Presentation/table/Copenhagen/HCAB"))
initial_html_hcab <- read_html(initial_res_hcab)

# Få token
token <- initial_html_hcab %>%
  html_element("#__AjaxAntiForgeryForm input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Lav POST request for hovedtabellen
hcab_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Parse responset
Hcab_html <- content(hcab_table_res, "text")
Hcab_data <- read_html(Hcab_html) %>%
  html_table() %>%
  .[[1]]  # Tag første tabel

log_print(paste(Sys.time(),"Succesfuldt webscrape HCAB"))


log_print(paste(Sys.time(),"Begynde med webscrape Anholt"))

# Anholt
initial_res_anholt <- GET(paste0(base_url, "/Luftdata/Presentation/table/Rural/ANHO"))
initial_html_anholt <- read_html(initial_res_anholt)


# Lav POST request for hovedtabellen
anholt_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/ANHO"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Parse responset
anholt_html <- content(anholt_table_res, "text")
Anholt_data <- read_html(anholt_html) %>%
  html_table() %>%
  .[[1]]  # Tag første tabel

log_print(paste(Sys.time(),"Succesfuldt webscrape Anholt"))

log_print(paste(Sys.time(),"Begynde med webscrape Aarhus"))

# Aarhus
initial_res_aarhus <- GET(paste0(base_url, "/Luftdata/Presentation/table/Aarhus/AARH3"))
initial_html_aarhus <- read_html(initial_res_aarhus)

aarhus_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list("__RequestVerificationToken" = token),
  encode = "form"
)

aarhus_html <- content(aarhus_table_res, "text")
aarhus_data <- read_html(aarhus_html) %>%
  html_table() %>%
  .[[1]]

log_print(paste(Sys.time(),"Succesfuldt webscrape Aarhus"))

log_print(paste(Sys.time(),"Begynde med webscrape Risoe"))

# Risø
initial_res_risoe <- GET(paste0(base_url, "/Luftdata/Presentation/table/Rural/RISOE"))
initial_html_risoe <- read_html(initial_res_risoe)

risoe_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/RISOE"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded"
  ),
  body = list("__RequestVerificationToken" = token),
  encode = "form"
)

risoe_html <- content(risoe_table_res, "text")

risoe_data <- read_html(risoe_html) %>%
  html_table() %>%
  .[[1]]

log_print(paste(Sys.time(),"Succesfuldt webscrape Risoe"))


Hcab_data <- Hcab_data %>%
  mutate(`Målt (starttid)` = dmy_hm(`Målt (starttid)`))

aarhus_data <- aarhus_data %>%
  mutate(`Målt (starttid)` = dmy_hm(`Målt (starttid)`))

Anholt_data <- Anholt_data %>%
  mutate(`Målt (starttid)` = dmy_hm(`Målt (starttid)`))

risoe_data <- risoe_data %>%
  mutate(`Målt (starttid)` = dmy_hm(`Målt (starttid)`))

# sql
conw <- dbConnect(MariaDB(),
                  dbname = "luftdata",
                  host = "13.61.15.156",
                  port = 3306,
                  user = "dalremote",
                  password = "Jernæblevej16tv!"
)

dbWriteTable(conw,"Hcab",Hcab_data, append = F)
dbWriteTable(conw,"Anholt",Anholt_data, append = F)
dbWriteTable(conw,"AArhus",aarhus_data, append = F)
dbWriteTable(conw,"Risoe",risoe_data, append = F)


log_print(paste(Sys.time(),"Hente databaser fra SQL"))

ny_Hcab <- dbReadTable(conw, "Hcab")
ny_Anholt <- dbReadTable(conw, "Anholt")
ny_AArhus <- dbReadTable(conw, "AArhus")
ny_Risoe <- dbReadTable(conw, "Risoe")

ld_hcab <- max(ny_Hcab$Målt..starttid.)
ld_aarhus <- max(ny_AArhus$Målt..starttid.)
ld_anholt <- max(ny_Anholt$Målt..starttid.)
ld_risoe <- max(ny_Risoe$Målt..starttid.)


log_print(paste(Sys.time(),"Filtere nyeste dataer"))

nd_Hcab <- Hcab_data %>% filter(`Målt (starttid)` > ld_hcab)
nd_AArhus <- aarhus_data %>% filter(`Målt (starttid)` > ld_aarhus)
nd_Anholt <- Anholt_data %>% filter(`Målt (starttid)` > ld_anholt)
nd_Risoe <- risoe_data %>% filter(`Målt (starttid)` > ld_risoe)


log_print(paste(Sys.time(),"Tilføje filtered dataer til SQL databser"))

dbWriteTable(conw,"Hcab",nd_Hcab, append = T)
dbWriteTable(conw,"Anholt",nd_Anholt, append = T)
dbWriteTable(conw,"AArhus",nd_AArhus, append = T)
dbWriteTable(conw,"Risoe",nd_Risoe, append = T)

log_close()
