#United Nations Data// World Bank
#install.packages("WDI")

library(WDI)
country<-c('DZ','AO','BJ',
           'BW','BF','BI',
           'CV','CM','CF','TD',
           'KM','CG','CD','CI',
           'DJ','EG','GQ','ER','ET',
           'GA','GM','GH','GN',
           'GW','KE','LS',
           'LR','LY','MG',
           'MW','ML','MR',
           'MU','MA','MZ',
           'NA','NE','NG',
           'RW','ST','SN',
           'SC','SL','SO',
           'ZA','SS','SD',
           'SZ','TZ','TG',
           'TN','UG','ZM','ZW')
indicator<-c('EG.ELC.ACCS.ZS',
             'EG.ELC.PROD.KH',
             'EG.ELC.RNEW.KH',
             'IC.GE.NUM           ',
             'EG.ELC.HOUS.ZS',
             'IC.BUS.EASE.XQ')
headings<-c("iso",
            "country",
            "year",
            "Access to Electricity",
            "Electricity Production",
            "Electricity production from renewable sources",
            "Procedures required to connect to electricity",
            "Household electrification rate")
try({wdi_data<-WDI(indicator=indicator, country=country, start=2017, end=2017)},silent=TRUE)
#names(wdi_data)<-headings
write.csv(wdi_data,"~/Desktop/wdi_data.csv")

ecokraft<-read.csv("~/OneDrive/Eco-Kraft/AAM_CR_CSV.csv")
summary(lm(ecokraft$Rank~ecokraft$Pop.Elec+ecokraft$Pop.Density+ecokraft$GDP.PC+ecokraft$Ann..GDP.Growth+ecokraft$Corruption+ecokraft$Unemployment+ecokraft$Inflation+ecokraft$Mobile+ecokraft$Internet+ecokraft$Elec.Consump+ecokraft$ClimateScope+ecokraft$HDI+ecokraft$Country.Risk))