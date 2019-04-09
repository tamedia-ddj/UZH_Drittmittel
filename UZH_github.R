library(stringr) # --> für str_c
library(dplyr) # --> count()


## Pfad ####

# Hier den Pfad zu den lokalen Daten angeben
url_base <- ""


## Einlesen der Liste der Professoren ####
# Diese Liste wurde aus dem Vorlesungsverzeichnis der UZH exportiert. 
# Einzelnen Professoren wurden noch von Hand hinzugefügth

url_profs_ergänzt <- str_c(url_base, "/profs_ergänzt.csv")
df_profs_ergänzt <- read.csv(url_profs_ergänzt, sep = ";", encoding = "UTF-8")


## Transparenzlisten der UZH einlesen #### 

## Liste mit Spenden/Drittgelder
# Diese Lite wurde von https://www.uzh.ch/de/about/basics/transparency/grants/list.html exportiert
# Spalte "Anonym" wurde von Hand hinzugefügt
url_drittZH <- str_c(url_base, "/uni_drittmittel.csv")
df_dritt <- read.table(url_drittZH, encoding = "UTF-8", header = TRUE, sep = ";", quote = '"', check.names = TRUE)
df_dritt$Geldgebende <- as.character(df_dritt$Geldgebende)
colnames(df_dritt)[1] <- "Empfangende"
df_dritt$Geldgebende[df_dritt$Anonym == "WAHR"] <- "Anonym"


# Währungsumrechnung
# Kurs vom 08.04.2019 gemäss google.com
df_currency <- data.frame(Währung = c("CHF","EUR","USD", "GBP"))
df_currency$factor <- c(1, 1.12, 1, 1.3)
df_dritt <- merge(df_dritt, df_currency, by = "Währung")
df_dritt$Betrag_CHF <- df_dritt$Gesamtbetrag * df_dritt$factor


## Liste mit gestifteten Professuren
# Diese Lite wurde von https://www.uzh.ch/cmsssl/de/about/basics/transparency/endowed-professorships/list.html exportiert
# Spalte "Faktor" wurde von Hand hinzugefügt, basierend auf der Spalte "Dauer"
# Bei den UBS Professuren wurde 10 Jahre eingesetzt gemäss Auskunft der UZH
url_gestiftetZH <- str_c(url_base, "/uni_gestiftet.csv")
df_gestiftet <- read.table(url_gestiftetZH, encoding = "UTF-8", header = TRUE, sep = ";", quote = '"', check.names = FALSE)
df_gestiftet$Förderer <- as.character(df_gestiftet$Förderer)
colnames(df_gestiftet)[1] <- "Stiftungsprofessur"
df_gestiftet$Ertrag_2018_CHF <- df_gestiftet$Förderbetrag/df_gestiftet$Faktor


## Daten weiterverarbeiten ####

# Namen auf Nachnamen reduzieren
df_dritt$LastN <- word(df_dritt$Empfangende, start = 1, sep = ",")
df_gestiftet$LastN <- word(df_gestiftet$Inhaber, start = 1, sep = ",")

# alles konsistent in einen Dataframe packen
df1 <- df_dritt[, c(3, 7, 2, 11, 10)]
df2 <- df_gestiftet[, c(4, 9, 2, 10, 9)]
colnames(df2) <- colnames(df1)
df <- rbind(df1, df2)


## Einheitliche Namen für gleich Spender festlegen
df$Geldgebende[grepl("roche", df$Geldgebende, ignore.case = TRUE)] <- "Roche"
df$Geldgebende[grepl("mercator", df$Geldgebende, ignore.case = TRUE)] <- "Mercator Stiftung"
df$Geldgebende[grepl("lnnosuisse", df$Geldgebende, ignore.case = TRUE)] <- "lnnosuisse"
df$Geldgebende[grepl("NOMIS", df$Geldgebende, ignore.case = TRUE)] <- "NOMIS Stiftung"
df$Geldgebende[grepl("Synapsis", df$Geldgebende, ignore.case = TRUE)] <- "Synapsis"
df$Geldgebende[grepl("European Space Agency", df$Geldgebende, ignore.case = TRUE)] <- "ESA"


## Betrag 2018 pro Empfangendem ####
df_ag_Empfänger <- aggregate(df$Ertrag_2018_CHF, by=list(name_ganz = df$Empfangende), FUN=sum, drop = FALSE)
df_ag_Empfänger$count <- count(df, Empfangende)$n
df_ag_Empfänger <- df_ag_Empfänger[order(-df_ag_Empfänger$x),] 


## Professor und Fakultät aus der Liste zuweisen ####
# Die Spalte "Betrag" umfasst in diesem DF bei den gestifteten Professuren Betrag für das Jahr 2018
# und bei den Spenden die neu dazugekommene Geldmenge im 2018 
# --> Dies ist der Betrag welcher im Artikel als Proxi für die Drittmittelverteilung verwendet wird
colnames(df)[3] <- "name_ganz"
df_master <- merge(x = df, y = df_profs_ergänzt, by = "name_ganz", all.x = FALSE)
df_master = subset(df_master, select = -c(Var.8, Lehrveranstaltungen, LastN.y, Var.1, X.U.FEFF.))
url_master <- str_c(url_base, "/Daten", "/master.csv")
write.csv(df_master, url_master)

## Aggregieren pro Fakultät, Geldgebe oder Empfänger ####
## aggregiert pro Fakultät
df_ag_Fakultät <- aggregate(df_master$Betrag_CHF, by=list(Fakultät = df_master$Fakultät), FUN = sum)
df_ag_Fakultät$count <- count(df_master, Fakultät)$n

## aggregiert pro Geldgeber
df_ag_Geldgeber <- aggregate(df_master$Betrag_CHF, by=list(Geldgeber = df_master$Geldgebende), FUN = sum)
df_ag_Geldgeber$count <- count(df_master, Geldgebende)$n


## aggregiert pro Empfänger
df_ag_Empfänger <- aggregate(df_master$Betrag_CHF, by=list(Empfänger = df_master$name_ganz), FUN = sum)
df_ag_Empfänger$count <- count(df_master, name_ganz)$n



