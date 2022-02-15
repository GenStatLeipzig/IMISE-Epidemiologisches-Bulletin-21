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

# observed_data = fread(here(paste0("data/042_datint_ecdc_saxony_",datum,"_v5_agestrat.txt")))
# observed_data[,DateRep:= as_date(DateRep)]
# observed_data[,mean:= NewConfCases]
# observed_data2 = observed_data[DateRep >=as_date("2021-11-15") & Altersgruppe=="all"]
# 
# ggplot(observed_data2, aes(DateRep, NewConfCases7, col = Altersgruppe )) + geom_line()
# 
#               
# 
# normalstation = read_excel2(here("data/sachsen_kh_normalstation.xlsx"))
# normalstation
# 
# observed_data2[,belegung_normalstation := normalstation[match_hk(observed_data2$DateRep %>% as.character(), normalstation$Datum %>% as.character()),Hospitalisiert]]
# observed_data2[, icu_plus_normal_belegung := belegung_normalstation + covid_inICU_upscaled ]
# 
# observed_data2[, .( DateRep,belegung_normalstation , covid_inICU_upscaled, icu_plus_normal_belegung) ]
# 

# WriteXLS_hk("hospitalized4marcin", here("results/b21_s5_2_hospitalized_saxony.xlsx"))


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
  ylab("Bettenbelegung (ITS & Normalstation)")+
  labs(color = "Szenario:", fill = "Szenario:")+ 
  scale_color_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet"))+ 
  scale_fill_manual(values = mycolors, breaks  = c( "Pessimistisch","Balanciert", "Optimistisch", "Berichtet")) +
  xlab("")
p_bettenbelegung

# Abb 10 haupt bulletin----
p_posit +guides(fill = 'none', col = "none") + p_bettenbelegung + p_totekumul + plot_layout(nrow = 1,guides = 'collect') & theme(legend.position = 'top')


