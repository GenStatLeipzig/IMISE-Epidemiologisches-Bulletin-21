rm(list = ls())
require(toolboxH)
require(ggplot2)
require(ggthemes)
require(stringr)
require(here)
require(lubridate)
require(plotly)
require(patchwork)
initializeSkript()

datum = as_date("2022-01-31") #today())-3
datum

loaded1 = load(here("data/b21_s11_1_inputobjekte_simu.RData"))
loaded1
plotenddate = "2022-03-15"
plotenddateTOT = "2022-04-15"

## KIT DAten ---

cumdead = fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Deaths_Germany.csv", encoding = "UTF-8")
cumdead
inccase = fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Cases_Germany.csv", encoding = "UTF-8")
inccase

# data.table(namex = unique(inccase$location_name), numix = unique(inccase$location)) %>% ccc
setnames(inccase, c('date','location_name', "value"), c('DateRep','CountryExp',"NewConfCases"))
setnames(cumdead, c('date','location_name', "value"), c('DateRep','CountryExp',"AllDeaths"))

inccase[, DateRep := as_date(DateRep)]
cumdead[, DateRep := as_date(DateRep)]

inccase[, id := paste(DateRep,location )]
cumdead[, id := paste(DateRep,location )]

qlist1 = venn2(inccase$id, cumdead$id)

inccase[,AllDeaths := cumdead[match_hk(inccase$id, cumdead$id), AllDeaths]]



ggplot(inccase, aes(DateRep, NewConfCases, col = CountryExp)) + geom_line() + scale_y_log10()
ggplot(inccase, aes(DateRep, AllDeaths, col = CountryExp)) + geom_line()



# add KH data ----

all_icu4 = fread("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-tagesdaten.csv")
all_icu4
all_icu4[,DateRep := as_date(date)]
stopifnot(nrow(all_icu4[is.na(gemeindeschluessel)==T])==0)

kreise = read_excel2(here("data/04-kreise.xlsx"),2, skip = 3)
bl = kreise[str_length(`Schlüssel-nummer`)==2]
bl[,`Schlüssel-nummer`:= as.numeric(`Schlüssel-nummer`)]
all_icu4[,bundesland2 := bl[match_hk(all_icu4$bundesland, bl$`Schlüssel-nummer`),`Regionale Bezeichnung`]]
all_icu4[,.N,bundesland2]

kreise2 = kreise[str_length(`Schlüssel-nummer`)==5]
kreise2[,kreisnr:= as.numeric(`Schlüssel-nummer`)]


all_icu4[,kreis := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Kreisfreie Stadt`]]
all_icu4[,einwohner := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Bevölkerung2)`]]
all_icu4[, betten_gesamt := betten_frei+betten_belegt]

#` # plotte`
all_icu4[,.(betten_gesamt=sum(betten_gesamt)), DateRep] %>%
  ggplot( aes(DateRep, betten_gesamt)) + geom_point() + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_x_date(labels = date_format("%m-%d"),date_breaks = "3 days" )


all_icu4[,einwohner:= as.numeric(einwohner)]

all_icu4[,frei_typ:= "jetzt frei"]

stopifnot(all_icu4[is.na(betten_gesamt)] %>% nrow(.)==0)# , betten_gesamt:= betten_belegt+betten_frei]

all_icu4_bl = all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_invasiv_beatmet), #update naming 1.4.21
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .(bundesland, DateRep, bundesland2,frei_typ)]

all_icu4_ger= all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_invasiv_beatmet), # update 1.4.21
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .( DateRep, frei_typ)]




all_icu4_ger[, level:= "Land"]
all_icu4_bl[, level:= "Bundesland"]
all_icu4[, level:= "Kreis"]

all_icu4_ger[, CountryExp := 'Deutschland']
all_icu4_bl[, CountryExp := bundesland2]
all_icu4[, CountryExp := kreis]


all_icu4_ger[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug ## ab 18.4.2020 nichtmehr upscaled, weillaut verordnung fast alle melden u

all_icu4_bl[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug
all_icu4[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug


(ggplot(all_icu4_bl, aes(DateRep,  `COVID-19 aktuell in Behandlung_upscaled` , col = CountryExp)) + geom_point() + geom_line() + geom_vline(xintercept = as_date('20-04-16'), lty = 2)) %>% ggplotly()
## 
icu_sachsen = all_icu4_bl[CountryExp=="Sachsen"]

# Normastation ----
khcheck = read_excel2(here("data/Covid_20220211.xlsx"),"COVID Normal", skip = 1) # https://www.dkgev.de/fileadmin/default/Mediapool/Corona/11.02.2022/Covid_20220211.xlsx from https://www.dkgev.de/dkg/coronavirus-fakten-und-infos/aktuelle-bettenbelegung/?s=09
khcheck # veraltet



observed_data2 = inccase[CountryExp =="Free State of Saxony"]
observed_data2[, covid_inICU_upscaled := icu_sachsen[match_hk(observed_data2$DateRep %>% as.character(), icu_sachsen$DateRep%>% as.character()),`COVID-19 aktuell in Behandlung_upscaled`]]

# observed_data[,mean:= NewConfCases]
# observed_data2 = observed_data[DateRep >=as_date("2021-11-15") & Altersgruppe=="all"]
# 
# ggplot(observed_data2, aes(DateRep, NewConfCases7, col = Altersgruppe )) + geom_line()
# 
#               
# 
normalstation = read_excel2(here("data/sachsen_kh_normalstation.xlsx"))
normalstation

observed_data2[,belegung_normalstation := normalstation[match_hk(observed_data2$DateRep %>% as.character(), normalstation$Datum %>% as.character()),Hospitalisiert]]
observed_data2[, icu_plus_normal_belegung := belegung_normalstation + covid_inICU_upscaled ]
# 
# observed_data2[, .( DateRep,belegung_normalstation , covid_inICU_upscaled, icu_plus_normal_belegung) ]
# 

# WriteXLS_hk("hospitalized4marcin", here("results/b21_s5_2_hospitalized_saxony.xlsx"))
# 
observed_data2 = observed_data2[DateRep>= min(testpositives$DateRep)]
setorder(observed_data2, DateRep)
observed_data2[, NewConfCases7 := frollmean(NewConfCases, 7, na.rm = T, align = "center"), .(CountryExp) ]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
mycolors = c(gg_color_hue(4)[c(4,3,2)], "black")

p_posit= ggplot(testpositives[DateRep<=as_date(plotenddate)], aes(DateRep, mean, col = scenario2, fill = scenario2)) +
  theme_minimal(base_size = 16) + 
  ylab("Berichtete tageweise Testpositive") +
  scale_x_date(label = date_format(format ='%d.%b' ))+
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.2, colour = NA)+
  geom_line(lwd = 1, alpha = 0.8) +
  geom_line(data= observed_data2[,. (DateRep, mean = NewConfCases, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4) + 
  geom_line(data= observed_data2[,. (DateRep, mean = NewConfCases7 , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4, lty = 1) +
  ggtitle(unique(testpositives$variable)) +
  
  
  geom_line(data= observed_data2[DateRep>=as_date("2022-02-01"),. (DateRep, mean = NewConfCases, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4, col = "red") + 
  geom_line(data= observed_data2[DateRep>=as_date("2022-02-01"),. (DateRep, mean = NewConfCases7 , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4, lty = 1, col = "red") +
  
  
  labs(color = "Szenario:", fill = "Szenario:") + 
  scale_color_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+ 
  scale_fill_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+
  xlab("")
p_posit


# death ----



p_totekumul= ggplot(totekumul[DateRep<=as_date(plotenddateTOT)], aes(DateRep, mean, col = scenario2, fill = scenario2)) +
  theme_minimal(base_size = 16) + 
  ylab("Berichtete kumul. Verstorbene") +
  scale_x_date(label = date_format(format ='%d.%b' ))+
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.2, colour = NA)+
  geom_line(lwd = 1, alpha = 0.8) +
  geom_line(data= observed_data2[DateRep >as_date("2021-12-15"),. (DateRep, mean = AllDeaths , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4) + 
  
  geom_line(data= observed_data2[DateRep >as_date("2022-02-01"),. (DateRep, mean = AllDeaths , scenario2 = "Berichtet")] , lwd = 1, alpha = 1, col = "red") + 
  
  
  ggtitle(unique(totekumul$variable)) +
  labs(color = "Szenario:", fill = "Szenario:")+ 
  scale_color_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+ 
  scale_fill_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+
  xlab("")
p_totekumul

# hosp ----



p_bettenbelegung= ggplot(bettenbelegung[DateRep<=as_date(plotenddate)], aes(DateRep, mean, col = scenario2, fill = scenario2)) +  
  theme_minimal(base_size = 16) + 
  scale_x_date(label = date_format(format ='%d.%b' ))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.2, colour = NA)+
  geom_line(lwd = 1, alpha = 0.8) +
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_line(data= observed_data2[,. (DateRep, mean = icu_plus_normal_belegung, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4) + ggtitle("Bettenbelegung") +
  
  geom_line(data= observed_data2[DateRep >as_date("2022-02-01"),. (DateRep, mean = icu_plus_normal_belegung, scenario2 = "Berichtet")] , lwd = 1, alpha = 1, col = "red") + ggtitle("Bettenbelegung") +
  
  
  ylab("Bettenbelegung (ITS & Normalstation)")+
  labs(color = "Szenario:", fill = "Szenario:")+ 
  scale_color_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+ 
  scale_fill_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet")) +
  xlab("")
p_bettenbelegung
ggplotly(p_bettenbelegung)
# Abb 10 haupt bulletin----
p_posit +guides(fill = 'none', col = "none") + p_bettenbelegung + p_totekumul + plot_layout(nrow = 1,guides = 'collect') & theme(legend.position = 'top')



jpeg(here("results/b21git_s01_1_scenarion_A_B_Cv2.jpeg"),width = 15,6, res = 150, quality= 100, units = "in")
p_posit +guides(fill = 'none', col = "none") + p_bettenbelegung + p_totekumul + plot_layout(nrow = 1,guides = 'collect') & theme(legend.position = 'top')

dev.off()

