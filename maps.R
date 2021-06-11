library(readxl)
library(tidyverse)
library(sf)
library(plotly)
library(viridis)

apt_data <- read_excel("apt_data.xlsx", 
                       col_types = c("numeric", 
                                     "text", "numeric", "numeric", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "numeric", "numeric",
                                     "numeric", "numeric", "skip", "numeric",
                                     "numeric", "numeric", "numeric", "skip",
                                     "numeric", "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "text", "text", "text",
                                     "date", "skip", "text", "skip"))

apt_data <- apt_data %>%
  filter(!note_brevi == "Elimina")

#read sf data for E-R region
emilia_romagna <- read_sf(dsn = Sys.getenv("SF_PATH"),
                          layer = "Com01012019_WGS84")

emilia_romagna <- emilia_romagna %>% 
  arrange(COMUNE)

emilia_romagna$comune <- toupper(emilia_romagna$COMUNE)

#data cleaning
comune_er <- unique(emilia_romagna$comune)
comune_apt <- unique(apt_data$comune)

which(!comune_apt %in% comune_er)
#43  94 110 113 132 134 138 220

apt_data$comune <- str_replace_all(apt_data$comune, 
                                   c("REGGIO EMILIA" = "REGGIO NELL'EMILIA",
                                     "LESIGNANO DE'BAGNI" = "LESIGNANO DE' BAGNI",
                                     "ZIBELLO" = "POLESINE ZIBELLO",
                                     "CARPANETO P.NO" = "CARPANETO PIACENTINO",
                                     "BORGONOVO VT" = "BORGONOVO VAL TIDONE",
                                     "ZIANO P.NO" = "ZIANO PIACENTINO",
                                     "CASTELNUOVO RAGONE" = "CASTELNUOVO RANGONE",
                                     "SAN GIOVANNI MARIGNANO" = "SAN GIOVANNI IN MARIGNANO"))

#number of obs by municipality
count_cm <- apt_data %>% 
  count(comune)

colnames(count_cm)[2] <- "count"

merged <- left_join(emilia_romagna, count_cm, by = "comune")

merged$count_map <- merged$count

merged <- merged %>%
  mutate(count_map = coalesce(count_map, 0))

merged$count_map[merged$count_map >0 &  merged$count_map <6] <- 1
merged$count_map[merged$count_map >5 & merged$count_map <26] <- 2
merged$count_map[merged$count_map >25 & merged$count_map <76] <- 3
merged$count_map[merged$count_map > 75] <- 4

merged$count_map <- as.factor(merged$count_map)

#sum by municipality
cap <- apt_data %>% 
  group_by(comune) %>% 
  summarise(cap_tot = sum(cap_tot_2021, na.rm = TRUE))

merged <- left_join(merged, cap, by = "comune")

merged$cap_tot[merged$cap_tot >0 &  merged$cap_tot <101] <- 1
merged$cap_tot[merged$cap_tot >100 & merged$cap_tot <251] <- 2
merged$cap_tot[merged$cap_tot >250 & merged$cap_tot <1001] <- 3
merged$cap_tot[merged$cap_tot >1000 &  merged$cap_tot <10001] <- 4
merged$cap_tot[merged$cap_tot >10000 & merged$cap_tot <40001] <- 5
merged$cap_tot[merged$cap_tot >40000] <- 6

merged$cap_tot <- as.factor(merged$cap_tot)

#map 1: number of obs by municipality
pal_bl <- c("#FFFFFF","#BFBFFF", "#8080FF", "#4040FF", "#000080")

m_count <- ggplot(data = merged) +
  geom_sf(aes(fill = count_map, text = count)) +
  scale_fill_manual(values = pal_bl,
                    labels = c("No strutture", 
                               "1 - 5", 
                               "6 - 25", 
                               "26 - 75", 
                               ">75")) +
  labs(fill = "Numero di strutture", 
       title = "Totale strutture per comune") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  coord_sf()

m_count

m_count_plotly <- ggplotly(m, tooltip = "text")
m_count_plotly

#map 2: sum by municipality
pal_rd <- c("#FFFFFF","#FFCCCC", "#FF8C8C", "#FF5959", "#FF2626", "#AA0000", "#660000")

m_cap <- ggplot(data = merged2) +
  geom_sf(aes(fill = cap_tot, text = cap_tot, group = comune)) +
  scale_fill_manual(values = pal_rd,
                    labels = c("No posti", 
                               "1 - 100", 
                               "101 - 250", 
                               "251 - 1.000", 
                               "1.001 - 10.000", 
                               "10.001 - 40.000", 
                               ">40.000")) +
  labs(fill = "Capienza totale", 
       title = "Capienza totale per comune") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  coord_sf()

m_cap

m_cap_plotly <- ggplotly(m_cap, tooltip = c("text", "group"))
m_cap_plotly

#map 3: point map
apt_data_point <- apt_data %>% 
  arrange(cap_tot_2021) %>%
  mutate(name = factor(comune, unique(comune))) %>%
  mutate(cap_tot_2021 = cap_tot_2021/100)

my_breaks <- c(1, 10, 85, 150, 300, 1000, 5000, 11000)

m_point <- ggplot(data = emilia_romagna) +
  geom_sf(fill = "grey90", alpha = 0.3) +
  geom_point(data = apt_data, aes(x = lon, y = lat, text = nome, color = cap_tot_2021, 
                                  size = cap_tot_2021, alpha = cap_tot_2021), stroke = FALSE) +
  scale_size_continuous(name = "Posti a sedere", range = c(3,20), breaks = my_breaks) +
  scale_alpha_continuous(name = "Posti a sedere", trans = "log", range = c(0.1, .9), breaks = my_breaks) +
  scale_color_viridis(name = "Posti a sedere", option = "magma", trans = "log", breaks = my_breaks) +
  guides(colour = guide_legend()) +
  ggtitle("Strutture congressuali in Emilia Romagna") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 5, size = 20),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) +
  coord_sf()

m_point

m_point_plotly <- ggplotly(m_point, tooltip = "text")
m_point_plotly