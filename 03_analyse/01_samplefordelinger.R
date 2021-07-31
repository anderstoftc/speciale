pacman::p_load(cregg, 
               dplyr,
               stringr,
               ggplot2,
               cregg,
               estimatr,
               grid,
               emmeans,
               openxlsx, 
               tidyverse,
               gridExtra,
               xtable)

# Load themes and colors 
source("06_analyse/epinion_style.R")
source("06_analyse/epinion_color.R")

#### DATA Resp ####
#udregner besvarelsesprocent
#Åbner sample
sample <- readRDS("03_SampleManagement/data/sample/Sample_fortrolig_20210518.rds")

sample <- sample %>%
  dplyr::filter(!SendGroup %in% c("Test", "Test2"))

#Åbner kategorisering af køn
sample_koen <- read.xlsx("03_SampleManagement/data/Kategorisering-af-køn-færdig.xlsx")

sample_koen <- sample_koen %>% mutate_if(is.character, str_trim)

### 3.1 Email -----------------------
#### Sæt til lower case
sample_koen$Email <- str_to_lower(sample_koen$Email)

#Joiner køn på
sample_all <- left_join(sample_koen, sample, by ="Email")

#vælger kun relevante variable
sample_all <- sample_all %>%
  dplyr::select(Id, Koen_kat =Koen, Parti, Kommune)

#Åbner besvarelsesdata
survey_besva <- read_sav("P10080.sav") %>% as_factor()

#Joiner de to:
df_bes <- left_join(sample_all, survey_besva, by ="Id")

#Laver variabel på, hvordan sample ser ud ift. population
df_bes <- df_bes %>%
  dplyr::mutate(besvaret = case_when(Respondent_Serial > 0 ~ "Besvaret",
                                     TRUE ~ "Ikke besvaret",)) %>%
  dplyr::select(Parti, Kommune, Koen_kat, besvaret) 

parti_sample <- df_bes %>%
  dplyr::group_by(Parti) %>%
  tally() %>%
  mutate(kvote = "I sample",
         sample.share = n/sum(n)) 

parti_bes <- df_bes %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Parti) %>%
  tally() %>%
  mutate(kvote = "Besvaret",
         besv.share = n/sum(n)) 

parti_in_sample <- left_join(parti_sample, parti_bes, by="Parti")
parti_in_sample <- parti_in_sample %>%
  mutate(forskel = sample.share - besv.share,
         forskel_pct_point=  percent(forskel, accuracy = 0.1L ),
         sample.share=  percent(sample.share, accuracy = 0.1L ),
         besv.share=  percent(besv.share, accuracy = 0.1L )) %>%
  dplyr::select(Parti, sample.share, besv.share, forskel_pct_point) %>%
  arrange(desc(sample.share))

koen_sample <- df_bes %>%
  filter(!is.na(Koen_kat)) %>%
  dplyr::group_by(Koen_kat) %>%
  tally() %>%
  mutate(kvote = "I sample",
         sample.share = n/sum(n)) 

koen_bes <- df_bes %>%
  filter(!is.na(Koen_kat)) %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Koen_kat) %>%
  tally() %>%
  mutate(kvote = "Besvaret",
         besv.share = n/sum(n)) 

koen_in_sample <- left_join(koen_sample, koen_bes, by="Koen_kat")
koen_in_sample <- koen_in_sample %>%
  mutate(forskel = sample.share - besv.share,
         forskel_pct_point=  percent(forskel, accuracy = 0.1L ),
         sample.share=  percent(sample.share, accuracy = 0.1L ),
         besv.share=  percent(besv.share, accuracy = 0.1L ),
         Parti = Koen_kat) %>%
  dplyr::select(Parti, sample.share, besv.share, forskel_pct_point) %>%
  arrange(desc(sample.share))

### antal i population vs. i sample - på blok
table(df_bes$Parti)

df_bes <- df_bes %>%
  mutate(Blok = ifelse(Parti=='Socialdemokratiet'|Parti=='Radikale Venstre'|Parti=='Socialistisk Folkeparti'|Parti=='Enhedslisten'|Parti=='Alternativet', "Rod Blok",
                       ifelse(Parti=='Konservative'|Parti=='Nye Borgerlige'|Parti=='Liberal Alliance'|Parti=='Dansk Folkeparti'|Parti=='Venstre'|Parti=='Kristendemokraterne', "Blaa Blok", "Ukodet"))) 
#%>%
#mutate(Ind_krit = ifelse(Parti=='B: Radikale Venstre'|Parti=='F: Socialistisk Folkeparti'|Parti=='Ø: Enhedslisten'|Parti=='Å: Alternativet', "Lempelig udlaendingepolitik",
#                        ifelse(Parti=='A: Socialdemokratiet'|Parti=='C: Konservative'|Parti=='D: Nye Borgerlige'|Parti=='I: Liberal Alliance'|Parti=='O: Dansk Folkeparti'|Parti=='V: Venstre'|Parti=='K: Kristendemokraterne'|Parti=='Andet, angiv venligst:', "Stram udlaendingepolitik", "Ukodet"))) %>%

table(df_bes$Blok, df_bes$besvaret)

blok_sample <- df_bes %>%
  filter(!is.na(Blok)) %>%
  dplyr::group_by(Blok) %>%
  tally() %>%
  mutate(kvote = "I sample",
         sample.share = n/sum(n)) 

blok_bes <- df_bes %>%
  filter(!is.na(Blok)) %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Blok) %>%
  tally() %>%
  mutate(kvote = "Besvaret",
         besv.share = n/sum(n)) 

blok_in_sample <- left_join(blok_sample, blok_bes, by="Blok")
blok_in_sample <- blok_in_sample %>%
  mutate(forskel = besv.share - sample.share ,
         forskel_pct_point=  percent(forskel, accuracy = 0.1L ),
         sample.share=  percent(sample.share, accuracy = 0.1L ),
         besv.share=  percent(besv.share, accuracy = 0.1L ),
         Parti = Blok) %>%
  dplyr::select(Parti, sample.share, besv.share, forskel_pct_point) %>%
  arrange(desc(sample.share))

all_in_sample <- bind_rows(koen_in_sample, parti_in_sample, blok_in_sample)

xtable(all_in_sample)

# hvor mange opgør de i gennemsnit i hvert parti
parti_antal_komu <- df_bes %>%
  #filter(!is.na(Blok)) %>%
  dplyr::group_by(Parti, Kommune) %>%
  tally() %>%
  dplyr::group_by(Parti) %>%
  dplyr::summarise(gns_antal = mean(n))

blok_antal_komu <- df_bes %>%
  #filter(!is.na(Blok)) %>%
  dplyr::group_by(Blok, Kommune) %>%
  tally() %>%
  dplyr::group_by(Blok) %>%
  dplyr::summarise(gns_antal = mean(n))

##### BESVARELSESPROCENT
bes_parti <- df_bes %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Parti) %>%
  dplyr::tally() %>%
  mutate(kvote = "Besvaret",
         sample.share = n/sum(n))

sample_parti <- df_bes %>%
  dplyr::group_by(Parti) %>%
  dplyr::tally() %>%
  mutate(kvote = "I alt",
         sample.share = n/sum(n))

parti <- left_join(bes_parti, sample_parti, by="Parti")

parti <- parti %>%
  mutate(besva_pro = n.x / n.y,
         besva_pro2 = percent(besva_pro, accuracy = 0.1L ),
         Antal_sample = n.y,
         Antal_besva = n.x) %>%
  dplyr::select(Parti, Antal_sample, Antal_besva, besva_pro2) %>%
  arrange(desc(besva_pro2))


all <- df_bes %>%
  dplyr::group_by(besvaret) %>%
  tally() %>%
  mutate(Parti = "I alt", 
         besva_pro = n/sum(n),
         besva_pro2 = percent(besva_pro, accuracy = 0.1L ),
         Antal_besva = n,
         Antal_sample = sum(n)) %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::select(Parti, Antal_besva, Antal_sample, besva_pro2)


bes_koen <- df_bes %>%
  filter(!is.na(Koen_kat)) %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Koen_kat) %>%
  dplyr::tally() %>%
  mutate(kvote = "Besvaret",
         sample.share = n/sum(n))

sample_koen <- df_bes %>%
  filter(!is.na(Koen_kat)) %>%
  dplyr::group_by(Koen_kat) %>%
  dplyr::tally() %>%
  mutate(kvote = "I alt",
         sample.share = n/sum(n))


koen <- left_join(bes_koen, sample_koen, by="Koen_kat")

koen <- koen %>%
  mutate(besva_pro = n.x / n.y,
         besva_pro2 = percent(besva_pro, accuracy = 0.1L ),
         Antal_sample = n.y,
         Antal_besva = n.x,
         Parti = Koen_kat) %>%
  dplyr::select(Parti, Antal_sample, Antal_besva, besva_pro2) %>%
  arrange(desc(besva_pro2))

all <- rbind(all, koen, parti)

xtable(all)



## Laver fordelinger på kommune besvarelser
df_bes_kom <- df_bes %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Kommune) %>%
  tally() %>%
  arrange(desc(n))

df_bes_kom$Kommune <- dplyr::recode(df_bes_kom$Kommune, Vesthimmerland = "Vesthimmerlands", Nordfyn = "Nordfyns")

map <- mapDK::mapDK(values = "n", id = "Kommune", data = df_bes_kom) +
  scale_fill_continuous(low="grey83", high="grey25", name = "Antal besvarelser") +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    text=element_text(family="Times New Roman")
  )

map

ggsave(filename = "06_Analyse/02_results/01_samplefordelinger/fordeling_resp_kort.png",
       plot = map, 
       width = 8, 
       height = 8, 
       dpi = 320)

ggsave(filename = "06_Analyse/02_results/01_samplefordelinger/fordeling_resp_kort.eps",
       plot = map, 
       width = 8, 
       height = 8, 
       dpi = 320)


d <- df_bes %>%
  filter(besvaret %in% "Besvaret") %>%
  dplyr::group_by(Kommune) %>%
  tally() %>%
  arrange(desc(n))

### Bruger resp data til yderligere fordelinger
survey_df <- readRDS("05_data/data_weighted_final_resp.rds")
d <- survey_df %>% 
  dplyr::group_by(dato) %>%
  tally()

ggplot(d, aes(x = dato, y = n)) + 
  geom_bar(stat="identity") +
  theme_classic()

#Merger andelen af indvandrere i kommunen på
sum(is.na(survey_df$Kommune))

mean_koen <- survey_df %>%
  dplyr::filter(!Koen %in% "Hovedmail/Ukendt") %>%
  dplyr::group_by(Koen) %>%
  dplyr::summarise(grp.mean = mean(Alder, na.rm=TRUE),
                   median_alder = median(Alder, na.rm=TRUE))

p1 <- survey_df %>%
  filter(!Koen %in% "Hovedmail/Ukendt") %>%
ggplot(., aes(x=Alder, color = Koen, fill=Koen)) +
  geom_density(alpha=0.6) +
  geom_vline(data=mean_koen, aes(xintercept=grp.mean, color=Koen),
             linetype="dashed") +
  labs(title="Alder på tværs af køn",x="Alder i år", y = "Densitet") +
  scale_color_grey() +
  scale_fill_grey() + 
  ylim(0, 0.0475) +
  theme_classic() +
  theme(legend.position = "top", text=element_text(family="Times New Roman"))+
  labs(fill = "", color = "")

p1

mean_medl <- survey_df %>%
  dplyr::filter(!Koen %in% "Hovedmail/Ukendt") %>%
  filter(!is.na(Q2_b1_num)) %>%
  dplyr::group_by(Koen) %>%
  summarise(mean_medlsk = mean(Q2_b1_num),
            median_medlsk = median(Q2_b1_num))

mean_medl_part <- survey_df %>%
  filter(!is.na(Q2_b1_num)) %>%
  dplyr::group_by(Parti) %>%
  dplyr::summarise(mean_medlsk = mean(Q2_b1_num),
                   median_medlsk = median(Q2_b1_num)) 

group_medl <- survey_df %>%
  filter(!is.na(Q2_b1_num)) %>%
  dplyr::group_by(Q2_b1_num) %>%
  tally() %>%
  summarise(pro = n/sum(n))

p2 <- survey_df %>%
  dplyr::filter(!Koen %in% "Hovedmail/Ukendt") %>%
  ggplot(., aes(x=Q2_b1_num-1, color = Koen, fill=Koen)) +
  geom_density(alpha=0.6) +
  geom_vline(data=mean_medl, aes(xintercept=mean_medlsk, color=Koen),
             linetype="dashed") +
  labs(title="Antal års medlemskab på tværs af køn",x="Års medlemskab", y = "Densitet", ) +
  theme_classic() +
  scale_color_grey() +
  scale_fill_grey() + 
  ylim(0, 0.0475) +
  theme(legend.position = "top", text=element_text(family="Times New Roman"))+
  labs(fill = "", color = "")

p2

plot <- plot_grid(p2, p1, ncol=2, rel_widths =c(1/2, 1/2))

plot

ggsave(filename = "06_Analyse/02_results/01_samplefordelinger/fordeling_resp.png",
       plot = plot, 
       width = 10, 
       height = 5, 
       dpi = 320)

position_andel <- survey_df %>%
  dplyr::group_by(Position) %>%
  tally() %>%
  mutate(kvote = "I sample",
         sample.share = n/sum(n),
         sample.share2=  percent(sample.share, accuracy = 0.1L )) %>%
  select(Position, n, sample.share2)

xtable(position_andel)





