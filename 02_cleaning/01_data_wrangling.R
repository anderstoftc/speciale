
#læser design fil, hvor mulige conjoint kombinationer fremgår med ID
profilegenskaber_df<-read.xlsx("02_Opsætning/Design.file.And.Labelling.NEW.Conditional_beskaef.xlsx")

# Load conjoint-svarfil
conjoint_output_df<-read.csv(file="05_data/10080_CBC_data.csv", header=T)

# Fjern dubli
conjoint_output_df<-conjoint_output_df %>%
  arrange(desc(sys_StartTime))

#Vælg relevante variable
head(conjoint_output_df)
conjoint_output_df <- conjoint_output_df %>%
  select("Id",
         "sys_CBCVersion_CBCStudies",
         "CBCStudies_Random1",
         "CBCStudies_Random2",
         "CBCStudies_Random3",
         "CBCStudies_Random4",
         "CBCStudies_Random5",
         "CBCStudies_Random6",
         "CBCStudies_Random7",
         "CBCStudies_Random8",
         starts_with("rating"))

## 2.1 Tilføj labels til designfil
profilegenskaber_df$Feat01.Navn<-factor(profilegenskaber_df$`Att.01.-.Navn`, 
                                        levels=c("1","2", "3", "4"),
                                        labels=c("Kvindelig majoritet",
                                                 "Mandlig majoritet",
                                                 "Kvindelig minoritet",
                                                 "Mandlig minoritet"))

profilegenskaber_df$Feat01a.Koen<-factor(profilegenskaber_df$`Att.01.-.Navn`, 
                                         levels=c("1","2", "3", "4"),
                                         labels=c("Kvinde",
                                                  "Mand",
                                                  "Kvinde",
                                                  "Mand"))

profilegenskaber_df$Feat01b.Etnicitet<-factor(profilegenskaber_df$`Att.01.-.Navn`, 
                                              levels=c("1","2", "3", "4"),
                                              labels=c("Majoritet",
                                                       "Majoritet",
                                                       "Minoritet",
                                                       "Minoritet"))

profilegenskaber_df$Feat02.Alder<-factor(profilegenskaber_df$`Att.02.-.Alder`, 
                                         levels=c("1","2", "3", "4", "5", "6"),
                                         labels=c("28 år",
                                                  "35 år",
                                                  "42 år",
                                                  "49 år",
                                                  "56 år",
                                                  "63 år"))

profilegenskaber_df$Feat03.civilstatus<-factor(profilegenskaber_df$`Att.03.-.Civilstatus`, 
                                               levels=c("1","2", "3", "4"),
                                               labels=c("Gift og har to børn",
                                                        "Bor med sin kæreste",
                                                        "Bor alene", 
                                                        "Gift og har ingen børn"))

profilegenskaber_df$Feat04a.beskaeftigelse<-factor(profilegenskaber_df$`Att.04.-.Beskæftigelse`, 
                                                   levels=c("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"),
                                                   labels=c('SOSU Assistent',
                                                            'Receptionist',
                                                            'Laborant',
                                                            'Socialrådgiver',
                                                            'Sygeplejerske ',
                                                            'Bibliotekar ',
                                                            'Pædagog',
                                                            'Psykolog',
                                                            'Håndværker',
                                                            'Elektriker',
                                                            'Fængselsbetjent',
                                                            'Postbud',
                                                            'Politibetjent',
                                                            'Bankrådgiver',
                                                            'Advokat',
                                                            'Leder i privat virksomhed'))

profilegenskaber_df$Feat04b.beskaeftigelse_koen<-factor(profilegenskaber_df$`Att.04.-.Beskæftigelse`, 
                                                        levels=c("1","2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"),
                                                        labels=c('Lavtuddannet kvindefag',
                                                                 'Lavtuddannet kvindefag',
                                                                 'Lavtuddannet kvindefag',
                                                                 'Lavtuddannet kvindefag',
                                                                 'Højtuddannet kvindefag',
                                                                 'Højtuddannet kvindefag',
                                                                 'Højtuddannet kvindefag',
                                                                 'Højtuddannet kvindefag',
                                                                 'Lavtuddannet mandefag',
                                                                 'Lavtuddannet mandefag',
                                                                 'Lavtuddannet mandefag',
                                                                 'Lavtuddannet mandefag',
                                                                 'Højtuddannet mandefag',
                                                                 'Højtuddannet mandefag',
                                                                 'Højtuddannet mandefag',
                                                                 'Højtuddannet mandefag'))


profilegenskaber_df$Feat05.politisk_erfaring<-factor(profilegenskaber_df$`Att.05.-.Politisk.erfaring`, 
                                                     levels=c("1","2", "3", "4"),
                                                     labels=c('Ingen politisk erfaring',
                                                              'Aktivt medlem af lokalforeningen',
                                                              'Opstillet ved seneste kommunalvalg ',
                                                              'Medlem af kommunalbestyrelsen'))

profilegenskaber_df$Feat06.partimedlemskab<-factor(profilegenskaber_df$`Att.06.-.Partimedlemskab`, 
                                                   levels=c("1","2", "3", "4", "5"),
                                                   labels=c('Nyligt medlem',
                                                            'Medlem i 1 år',
                                                            'Medlem i 3 år',
                                                            'Medlem i 6 år',
                                                            'Medlem i 10 år'))

# Renser datasaettet
profilegenskaber_df<-profilegenskaber_df %>%
  select(Version, Task, Concept, starts_with("Feat"))

# 2. Klargoer data til merge
#Gør designfilen klar til at merge
profilegenskaber_indeks<-profilegenskaber_df %>%
  mutate(Indeks = paste(profilegenskaber_df$Version, 
                        profilegenskaber_df$Task, 
                        profilegenskaber_df$Concept, 
                        sep="_")) %>% 
  select(-Version, -Task, -Concept)

# ID'er, der har completed surveyet vælges - dette skyldes testobservationer
# Loader survey_data med relevante ID-variable
survey_df<-read_sav("P10080.sav") %>% as_factor()

#Laver et mønster for de rigtige ID'er
IDsVECTOR <- as.character(str_trim(survey_df$CBC_Id))

#Vælger de conjoint-data, som har ID-match i surveydata
conjoint_output_df<-conjoint_output_df %>%
  filter(Id %in% IDsVECTOR)

## 3. Gør choicemål tidy
#Vælg choice
conjoint_choice_df <- conjoint_output_df %>%
  select("Id",
         "CBCStudies_Random1",
         "CBCStudies_Random2",
         "CBCStudies_Random3",
         "CBCStudies_Random4",
         "CBCStudies_Random5",
         "CBCStudies_Random6",
         "CBCStudies_Random7",
         "CBCStudies_Random8",
         starts_with("rating"))

#Lav variabel om, så der for hver profil er et choice (dummy 1 og 0)
#Er profil 1 valgt
conjoint_choice_df$profil_1<-1
conjoint_choice_df$profil_1[conjoint_choice_df$CBCStudies_Random1==2]<-0

#Er profil 2 valgt
conjoint_choice_df$profil_2<-1
conjoint_choice_df$profil_2[conjoint_choice_df$CBCStudies_Random1!=2]<-0

#Tjek
table(conjoint_choice_df$profil_1,conjoint_choice_df$profil_2)

#Er profil 3 valgt
conjoint_choice_df$profil_3<-1
conjoint_choice_df$profil_3[conjoint_choice_df$CBCStudies_Random2==2]<-0

#Er profil 4 valgt
conjoint_choice_df$profil_4<-1
conjoint_choice_df$profil_4[conjoint_choice_df$CBCStudies_Random2!=2]<-0

#Tjek
table(conjoint_choice_df$profil_3,conjoint_choice_df$profil_4)

#Er profil 5 valgt
conjoint_choice_df$profil_5<-1
conjoint_choice_df$profil_5[conjoint_choice_df$CBCStudies_Random3==2]<-0

#Er profil 6 valgt
conjoint_choice_df$profil_6<-1
conjoint_choice_df$profil_6[conjoint_choice_df$CBCStudies_Random3!=2]<-0

#Tjek
table(conjoint_choice_df$profil_5,conjoint_choice_df$profil_6)

#Er profil 7 valgt
conjoint_choice_df$profil_7<-1
conjoint_choice_df$profil_7[conjoint_choice_df$CBCStudies_Random4==2]<-0

#Er profil 8 valgt
conjoint_choice_df$profil_8<-1
conjoint_choice_df$profil_8[conjoint_choice_df$CBCStudies_Random4!=2]<-0

#Tjek
table(conjoint_choice_df$profil_7,conjoint_choice_df$profil_8)

#Er profil 9 valgt
conjoint_choice_df$profil_9<-1
conjoint_choice_df$profil_9[conjoint_choice_df$CBCStudies_Random5==2]<-0

#Er profil 10 valgt
conjoint_choice_df$profil_10<-1
conjoint_choice_df$profil_10[conjoint_choice_df$CBCStudies_Random5!=2]<-0

#Tjek
table(conjoint_choice_df$profil_9,conjoint_choice_df$profil_10)

#Er profil 11 valgt
conjoint_choice_df$profil_11<-1
conjoint_choice_df$profil_11[conjoint_choice_df$CBCStudies_Random6==2]<-0

#Er profil 12 valgt
conjoint_choice_df$profil_12<-1
conjoint_choice_df$profil_12[conjoint_choice_df$CBCStudies_Random6!=2]<-0

#Tjek
table(conjoint_choice_df$profil_11,conjoint_choice_df$profil_12)

#Er profil 13 valgt
conjoint_choice_df$profil_13<-1
conjoint_choice_df$profil_13[conjoint_choice_df$CBCStudies_Random7==2]<-0

#Er profil 14 valgt
conjoint_choice_df$profil_14<-1
conjoint_choice_df$profil_14[conjoint_choice_df$CBCStudies_Random7!=2]<-0

#Tjek
table(conjoint_choice_df$profil_13,conjoint_choice_df$profil_14)

#Er profil 15 valgt
conjoint_choice_df$profil_15<-1
conjoint_choice_df$profil_15[conjoint_choice_df$CBCStudies_Random8==2]<-0

#Er profil 16 valgt
conjoint_choice_df$profil_16<-1
conjoint_choice_df$profil_16[conjoint_choice_df$CBCStudies_Random8!=2]<-0

#Tjek
table(conjoint_choice_df$profil_15,conjoint_choice_df$profil_16)


#Vælg de relevante variable
conjoint_choice_long <- conjoint_choice_df %>%
  select("Id",
         "profil_1",
         "profil_2",
         "profil_3",
         "profil_4",
         "profil_5",
         "profil_6",
         "profil_7",
         "profil_8",
         "profil_9",
         "profil_10",
         "profil_11",
         "profil_12",
         "profil_13",
         "profil_14",
         "profil_15",
         "profil_16")

conjoint_choice_tidy<-gather(conjoint_choice_long, 
                             key="resp", 
                             value="preferred_profile",
                             profil_1:profil_16) %>%
  arrange(Id)

#Lav en liste over hvilket respondentId, der har fået hvilke design
resp_indeks_df <- conjoint_output_df %>%
  select("Id",
         "sys_CBCVersion_CBCStudies")

#Tilføj en variabel for hvilket designspor, der hører til hver Id
conjoint_choice_tidy<-left_join(conjoint_choice_tidy, resp_indeks_df, by="Id") # Kan du huske hvad resp_indeks_df er? Kan det ersattes med "profilegenskaber df?

#Tilføj variabel for task
conjoint_choice_tidy$Task<-1

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_3"]<-2
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_4"]<-2

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_5"]<-3
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_6"]<-3

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_7"]<-4
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_8"]<-4

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_9"]<-5
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_10"]<-5

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_11"]<-6   
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_12"]<-6  

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_13"]<-7    
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_14"]<-7  

conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_15"]<-8   
conjoint_choice_tidy$Task[conjoint_choice_tidy$resp=="profil_16"]<-8  


#Tilføj variabel for concept
conjoint_choice_tidy$Concept<-1
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_2"]<-2
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_4"]<-2
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_6"]<-2
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_8"]<-2
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_10"]<-2
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_12"]<-2 
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_14"]<-2 
conjoint_choice_tidy$Concept[conjoint_choice_tidy$resp=="profil_16"]<-2 

#Lav indeksvariabel til at merge med design
conjoint_choice_final <- conjoint_choice_tidy %>%
  mutate(Indeks = paste(conjoint_choice_tidy$sys_CBCVersion_CBCStudies, 
                        conjoint_choice_tidy$Task, 
                        conjoint_choice_tidy$Concept, 
                        sep="_")) %>%
  select(Id, preferred_profile, Indeks)

# Merger vi choice-outputtet med designfilen
conjoint_choice_features_final <- left_join(conjoint_choice_final, profilegenskaber_indeks, by="Indeks")

table(as.numeric(survey_df$Q2_b1), survey_df$Q2_b1)

table(as.numeric(survey_df$bagg2_b1), survey_df$bagg2_b1)

#definerer funktion til at bestemme rækkefølge passer ift. case_when


survey_merge <- survey_df %>%
  select(Id=CBC_Id, 
         Foed_aar = bagg2_b1,
         Kommune = bagg3_b1,
         Koen = bagg1,
         Position = Q_Screen,
         Parti = Q_party,
         Q2_b1, 
         Q3, 
         Q4_1_resp, 
         Q4_3_resp, 
         Q4_4_resp, 
         bagg2_b1, 
         bagg4, 
         bagg5) %>%
  mutate(Q2_b1_num = as.numeric(Q2_b1)) %>%
  mutate(Alder = 2020-as.numeric(as.character(Foed_aar))) %>%
  mutate(Blok = ifelse(Parti=='A: Socialdemokratiet'|Parti=='B: Radikale Venstre'|Parti=='F: Socialistisk Folkeparti'|Parti=='Ø: Enhedslisten'|Parti=='Å: Alternativet', "Rod Blok",
                       ifelse(Parti=='C: Konservative'|Parti=='D: Nye Borgerlige'|Parti=='I: Liberal Alliance'|Parti=='O: Dansk Folkeparti'|Parti=='V: Venstre'|Parti=='K: Kristendemokraterne'|Parti=='Andet, angiv venligst:', "Blaa Blok", "Ukodet"))) %>%
  mutate(Ind_krit = ifelse(Parti=='B: Radikale Venstre'|Parti=='F: Socialistisk Folkeparti'|Parti=='Ø: Enhedslisten'|Parti=='Å: Alternativet', "Lempelig udlaendingepolitik",
                       ifelse(Parti=='A: Socialdemokratiet'|Parti=='C: Konservative'|Parti=='D: Nye Borgerlige'|Parti=='I: Liberal Alliance'|Parti=='O: Dansk Folkeparti'|Parti=='V: Venstre'|Parti=='K: Kristendemokraterne'|Parti=='Andet, angiv venligst:', "Stram udlaendingepolitik", "Ukodet"))) %>%
  mutate(medlemskab = case_when(Q2_b1_num<11 ~ "Under 10 år",
                                Q2_b1_num>=11 & Q2_b1_num<=26 ~ "10-25 år",
                                Q2_b1_num>26 ~ "Over 25 år")) %>%
  mutate(alder_kat = case_when(Alder<50 ~ "Under 50 år",
                               Alder>=50 & Alder<=65 ~ "50-60 år",
                               Alder>65 ~ "Over 65 år"))

#vender medlemskab korrekt
survey_merge$medlemskab <- factor(survey_merge$medlemskab)
survey_merge$medlemskab <- relevel(survey_merge$medlemskab, "Under 10 år")
table(survey_merge$medlemskab)

survey_merge$alder_kat <- factor(survey_merge$alder_kat)
survey_merge$alder_kat <- relevel(survey_merge$alder_kat, "Under 50 år")
table(survey_merge$alder_kat)

table(survey_merge$Position, survey_merge$Parti)

#Merger andelen af indvandrere i kommunen på
andel_ind <- read.xlsx("05_data/andel_ind.xlsx")

survey_merge <- left_join(survey_merge, andel_ind, by="Kommune")

sum(is.na(survey_merge$Kommune))

conjoint_choice_features_final$Id <- as.character(conjoint_choice_features_final$Id)

# Og så merger vi choice-outputter med survey-filen
conjoint_df_final <- left_join(conjoint_choice_features_final, survey_merge, by="Id")

# Og gemmer som Rds
saveRDS(conjoint_df_final, file="05_data/conjoint_choice.Rds")


