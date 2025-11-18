library(readxl)
pacman::p_load(haven, foreign, skimr, reshape, plyr, dplyr, readstata13, stringr, skimr, xaringan, srvyr, lme4, broom, texreg, scales, plotly, desctable, descriptr, forcats, GGally, MplusAutomation, semPlot, stargazer, polycor, psych, corrplot, ggplot2, relimp, GGally, sjPlot, correlation, semPlot, gridExtra, tidyr)
load("/home/felipe/Dropbox/Proyecto Gamson ICCS/gamson_variables de interes.RData")
load("/Users/felipesanchez/Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/gamson_variables de interes.RData")

load("/home/felipe/Dropbox/Proyecto Gamson ICCS/data/Gamson.Rdata")
load("/Users/felipesanchez/Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/data/Gamson.Rdata")


pib <- read_excel("Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/pib.xls", 
                  sheet = "ICCS 2022")
vdem2022 <- read_excel("Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/data/vdem2022.xlsx")
vdem2022<- vdem2022[!duplicated(vdem2022), ]


gamson= gamson %>%
  rename_all(tolower)
iccs<-gamson
iccs[iccs==9998]<-NA

pib<-pib[c("Country Code", "2022")]
colnames(pib)<-c("country","pib")
gamson<-merge(gamson, pib, by="country")

names(gamson)
names(iccs)

gamson<-gamson[c("country" , "idpop"      ,"idgrade"    , "is4g02"     ,"is4g07" , "is4g09"    , "is4g20a"   , "is4g20b"    ,"is4g20d",   
                  "is4g20e","is4g20f","is4g20g" , "is4g20h" , "is4g20i" , "is4g22a"   , "is4g22b"   ,  "is4g22e"    ,"is4g22f", "is4g22h" ,   "is4g22i" ,   "is4g27a"   , "is4g27c",   
                  "is4g27e","is4g27f","is4g29a" , "is4g29b" , "is4g29c" , "is4g29d"   , "is4g29e"   ,  "is4g29f"    ,"is4g29g", "is4g30a" ,   "is4g30b" ,   "is4g30c"   , "is4g30d",   
                  "is4g31a","is4g31b","is4g31c" , "is4g31d" , "is4g31e" , "is4g31f"   , "is4g31g"   ,  "is4g31h"    ,"is4g31i", "is4g31j" ,   "is4g31k" ,   "is4g31l"   , "is4g31m",   
                  "is4g31n","s_hisei","s_hisced", "s_citsoc", "s_compart",   "s_elecpart" ,  "s_polpart", "s_legact", "s_illact", "s_engdm", 
                 "s_citeff" , "s_intrust") ]

iccs<-gamson

iccs<-merge(iccs, vdem2022, by="country")

iccs$sex[iccs$is4g02==1]<-0
iccs$sex[iccs$is4g02==2]<-1

table(iccs$sex)

iccs$dissidents[iccs$gamson=="e.dissidents"]<-1
iccs$dissidents[iccs$gamson!="e.dissidents"]<-0

iccs$assured[iccs$gamson=="d.assured"]<-1
iccs$assured[iccs$gamson!="d.assured"]<-0

iccs$subordinates[iccs$gamson=="c.subordinates"]<-1
iccs$subordinates[iccs$gamson!="c.subordinates"]<-0

iccs$alienated[iccs$gamson=="b.alienated"]<-1
iccs$alienated[iccs$gamson!="b.alienated"]<-0

iccs$moderates[iccs$gamson=="a.moderates"]<-1
iccs$moderates[iccs$gamson!="a.moderates"]<-0


m1<-lm(s_legact~gamson, data = iccs)
m2<-lm(s_illact~gamson, data = iccs)
m3<-lm(s_engdm~gamson, data = iccs)

texreg::screenreg(l=list(m1, m2, m3))

#plot_model(m1, type = "pred", terms = "gamson")+theme_bw()
#plot_model(m2, type = "pred", terms = "gamson")+theme_bw()
#plot_model(m3, type = "pred", terms = "gamson")+theme_bw()
iccs$pib1000<-(iccs$pib)/100000000
summary(iccs$pib1000)

m1 <- lme4::lmer(s_legact~ gamson + s_hisced + sex + s_illact + (1 + gamson | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ gamson + s_hisced + sex + s_legact + (1 + gamson | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ gamson + s_hisced + sex + (1  | country), data = iccs, REML=FALSE)
plot_model(m1, type = "re", vline.color = "black")+theme_bw()
ggsave("/Users/felipesanchez/Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/ISA_2025/ramdon_legal.jpeg", 
       units="cm", width=11, height=10, dpi=600)

plot_model(m2, type = "re", vline.color = "black")+theme_bw()
ggsave("/Users/felipesanchez/Library/CloudStorage/Dropbox/Proyecto Gamson ICCS/ISA_2025/ramdon_ilegal.jpeg", 
       units="cm", width=11, height=10, dpi=600)
plot_model(m3, type = "re")


m1 <- lme4::lmer(s_legact~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_libdem + s_illact + (1  | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_libdem + s_legact + (1  | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_libdem + (1  | country), data = iccs, REML=FALSE)
texreg::screenreg(l=list(m1, m2))

plot_model(m2, type = "re")


m1 <- lme4::lmer(s_legact~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_partipdem + s_illact + (1 | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_partipdem + s_legact + (1 | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ dissidents + assured + alienated + subordinates + s_hisced + sex + v2x_partipdem + (1 + dissidents | country), data = iccs, REML=FALSE)

m1 <- lme4::lmer(s_legact~ dissidents + assured + alienated + subordinates + s_hisced + sex + polarization + (1 | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ dissidents + assured + alienated + subordinates + s_hisced + sex + polarization + (1 | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ dissidents + assured + alienated + subordinates + s_hisced + sex + polarization + (1 | country), data = iccs, REML=FALSE)

m1 <- lme4::lmer(s_legact~ gamson*s_hisced + sex + teaching_freedom + (1 | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ gamson*s_hisced + sex + teaching_freedom + (1 | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ gamson*s_hisced + sex + teaching_freedom + (1 | country), data = iccs, REML=FALSE)

m1 <- lme4::lmer(s_legact~ gamson +  s_hisced + sex + p_assembly + (1  | country), data = iccs, REML=FALSE)
m2 <- lme4::lmer(s_illact~ gamson +  s_hisced + sex + p_assembly + (1  | country), data = iccs, REML=FALSE)
m3 <- lme4::lmer(s_engdm ~ gamson +  s_hisced + sex + p_assembly + (1  | country), data = iccs, REML=FALSE)

anova(m2,m1)
texreg::screenreg(l=list(m1, m2,m3))

m1a <- lme4::lmer(s_legact~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
m2a <- lme4::lmer(s_illact~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
m3a <- lme4::lmer(s_engdm ~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
texreg::screenreg(l=list(m1a, m2a,m3a))

plot_model(m2a, type="int")

m1b <- lme4::lmer(s_legact~ dissidents + alienated + subordinates + s_hisced + sex + assured*v2x_libdem + (1 + assured | country), data = iccs, REML=FALSE)
m2b <- lme4::lmer(s_illact~ dissidents + alienated + subordinates + s_hisced + sex + assured*v2x_libdem + (1 + assured | country), data = iccs, REML=FALSE)
m3b <- lme4::lmer(s_engdm ~ dissidents + alienated + subordinates + s_hisced + sex + assured*v2x_libdem + (1 + assured | country), data = iccs, REML=FALSE)
texreg::screenreg(l=list(m1b, m2b,m3b))
plot_model(m1b, type="pred", terms = c("v2x_libdem","assured"))

m1c <- lme4::lmer(s_legact~ s_hisced + sex + gamson*v2x_partipdem + (1 + gamson | country), data = iccs, REML=FALSE)
m2c <- lme4::lmer(s_illact~ s_hisced + sex + gamson*v2x_partipdem + (1 + gamson | country), data = iccs, REML=FALSE)
m3c <- lme4::lmer(s_engdm ~ s_hisced + sex + gamson*v2x_partipdem + (1 + gamson | country), data = iccs, REML=FALSE)
texreg::screenreg(l=list(m1c, m2c,m3c))
plot_model(m1c, type="int")
plot_model(m2c, type="int")

plot_model(m2c, type = "pred", terms = c("v2x_partipdem","gamson"), axis.lim = list(c(0.2,.8),c(0,80)))
plot_model(m1c, type = "pred", terms = c("v2x_partipdem","gamson"), axis.lim = list(c(0.2,.8),c(0,80)))


m1b <- lme4::lmer(s_legact~   alienated + subordinates + s_hisced + sex + assured + s_illact + dissidents*v2x_partipdem + (1 + dissidents | country), data = iccs, REML=FALSE)
m2b <- lme4::lmer(s_illact~   alienated + subordinates + s_hisced + sex + assured + s_legact + dissidents*v2x_partipdem + (1 + dissidents | country), data = iccs, REML=FALSE)
m3b <- lme4::lmer(s_engdm ~   alienated + subordinates + s_hisced + sex + assured + dissidents*v2x_partipdem + (1 + dissidents | country), data = iccs, REML=FALSE)
plot_model(m1b, type="pred", terms = c("v2x_partipdem","dissidents"))

m1a <- lme4::lmer(s_legact~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
m2a <- lme4::lmer(s_illact~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
m3a <- lme4::lmer(s_engdm ~ dissidents + alienated + subordinates + s_hisced + sex + assured*teaching_freedom + (1 + assured | country), data = iccs, REML=FALSE)
texreg::screenreg(l=list(m1a, m2a,m3a))

