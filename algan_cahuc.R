library(countrycode)
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)
library(wbstats)
library(ggplot2)
devtools::install_github("xmarquez/democracyData")
library(democracyData)
polity <-download_polity_annual()
setwd('~/documents/algan_cahuc')
data <- read.csv('algan_cahuc.csv', header=F, stringsAsFactors=F)
names(data) <- c('country', 'trust')
data$continent<-as.factor(countrycode(data$country,origin='country.name', destination = 'continent'))
data$iso2c<-as.factor(countrycode(data$country,origin='country.name', destination = 'iso2c'))

data$iso3c<-countrycode(data$country,origin='country.name', destination = 'iso3c')
data %>% na.omit() -> data
data %>% group_by(continent) %>% summarize(mean_trust = round(mean(trust))) %>% kable() %>%kable_styling()
View(data)

wbsearch(pattern = "ease") %>% kable() %>% kable_styling()
wb(country = "all",indicator='GV.TI.SCOR.IDX', mrv=2)%>% kable() %>% kable_styling()
1polity%>% kable() %>% kable_styling()
polity_recent <- polity %>% 
  group_by(scode) %>%
  filter(year == max(year)) %>%mutate(iso3c=countrycode(scode,origin='p4c', destination = 'iso3c')) %>%omit.na()
data %>%inner_join( polity_recent, by='iso3c') -> data
ggplot(data, aes(x=trust, y=polity2.x, color=continent, shape=continent,size=3)) + geom_point() +geom_text(label=data$country, position = position_nudge(y = 1, x=1))+geom_jitter(width=2, height=1)+ggtitle(label='Polity IV vs. Trust')

cpi<-read.csv('cpi.csv',sep='\t')
cpi %>% kable() %>% kable_styling()
data %>%inner_join( cpi, by='iso3c') -> data
ggplot(data, aes(x=trust, y=cpi2018),size=3) + geom_point() + geom_smooth(method='lm')+geom_text(label=data$country.x, position = position_nudge(y = 1, x=1), size=3.5)+ ggtitle("CPI 2018 vs. Trust") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

ease <-read.csv('ease.csv',sep='\t')
ease$iso3c<-countrycode(ease$country,origin='country.name', destination = 'iso3c')
data %>%inner_join( ease, by='iso3c') -> data
lm_eqn <- function(df){
  y<-data$ease
  x<-data$trust
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
ggplot(data, aes(x=trust, y=ease),size=3) + geom_point() + geom_smooth(method='lm')+geom_text(label=data$country.x, position = position_nudge(y = 1, x=1), size=3.5)+ ggtitle("Ease of Doing Business vs. Trust") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ geom_text(x = 25, y = 300, label = lm_eqn(data), parse = TRUE)

