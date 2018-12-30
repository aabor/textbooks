library("PerformanceAnalytics")
library("quantmod")
library(dplyr)

Sys.setlocale("LC_TIME", "C")
# environment in which to store data 
data <- new.env()

# set dates
date.start <- "1992-01-01"
date.end <- "2016-12-31"

# set tickers
# tickers <- c("FEDFUNDS", "GDPPOT", "DGS10")

# https://fred.stlouisfed.org/categories              
# 
# Real Broad Effective Exchange Rate for Russia© (RBRUBIS)
# Immediate Rates: Less than 24 Hours: Central Bank Rates for the Russian Federation© (IRSTCB01RUM156N)
# 3-Month or 90-day Rates and Yields: Interbank Rates for the Russian Federation© (IR3TIB01RUM156N)
# 
# 
# Consumer Price Index: All Items for Russian Federation© (RUSCPIALLMINMEI)
# Consumer Price Index: Total All Goods Less Food for the Russian Federation© (CPGDLF01RUM661N)
# Domestic Producer Prices Index: Manufacturing for Russian Federation© (RUSPPDMMINMEI)
# 
# 
# 
# M0 for the Russian Federation© (MANMM102RUM189N)
# M1 for the Russian Federation© (MANMM101RUA189N)
# M2 for the Russian Federation© (MABMM201RUM189N)
# M3, Alternate Definition 4 for Russian Federation© (MAM3A4RUM189N)
# 
# Total Reserves excluding Gold for Russian Federation© (TRESEGRUM052N)
# 
# Registered Unemployment Rate for the Russian Federation© (LMUNRRTTRUM156S)
# Registered Unemployment Level for the Russian Federation© (LMUNRLTTRUM647N)
# Monthly Earnings: All Activities for the Russian Federation© (LCEATT03RUM664N)
# 
# Production of Total Industry in Russian Federation© (RUSPROINDMISMEI)
# Ratio of Exports to Imports for the Russian Federation© (XTEITT01RUM156N)
# OECD based Recession Indicators for Russian Federation from the Peak through the Trough (RUSRECM)
# Current Price Gross Domestic Product in Russian Federation© (RUSGDPNQDSMEI)
# M3, Alternate Definition 4 for Russian Federation© (MAM3A4RUM189N)

tickers<-c("RBRUBIS", "IRSTCB01RUM156N", "IR3TIB01RUM156N", 
           "RUSCPIALLMINMEI", "CPGDLF01RUM661N", "RUSPPDMMINMEI", 
           "MANMM102RUM189S", "MANMM101RUM189S", "MABMM201RUM189S", 
           "MABMM301RUM189S", "TRESEGRUM052N", "LMUNRRTTRUM156S", 
           "LMUNRLTTRUM647N", "LCEATT03RUM664N", "RUSPROINDMISMEI", 
           "XTEITT01RUM156N", "RUSRECM", "RUSGDPNQDSMEI","MAM3A4RUM189N")
# import data from FRED database
getSymbols( tickers
            , src = "FRED"  # needed!
            , from = date.start  # ignored
            , to = date.end  # ignored
            , env = data
            , adjust = TRUE
)
head(data$FEDFUNDS)
tail(data$RBRUBIS)


#OECD based Recession Indicators for Russian Federation 
#from the Peak through the Trough 
#rearrange data to fit to chart.TimeSeries plot
plot(data$RUSRECM)
head(data$RUSRECM)
data$RUSRECM<-data$RUSRECM[-1]
start <- index(data$RUSRECM[which(diff(data$RUSRECM)==1)])
end   <- index(data$RUSRECM[which(diff(data$RUSRECM)==-1)-1])
class(start)
head(start)
head(end)
glimpse(start)
length(start)
length(end)
end[5]<-"2016-08-01"
rus.reccesion.df <- data.frame(start=start, end=end)
rus.reccesion.df <- subset(rus.reccesion.df, start >= min(data$RBRUBIS))
rus.reccesion.df

#merge two columns of dates in one in the format "1997-09-01/1998-10-01"
merge_dates<-function(x,c1,c2) paste(c1,c2,sep="/")
ru.cycles.dates<-apply(rus.reccesion.df, 1:2, merge_dates
                       ,c1=rus.reccesion.df$start, c2=rus.reccesion.df$end)
with(rus.reccesion.df,
     ru.cycles.dates<-paste(start, end, sep="/"))

#use recession data from Russia in all charts
ru.cycles.dates
class(ru.cycles.dates)

# plot monetary aggregates
# subset time series in list
l<-list(data$MANMM102RUM189S, 
       data$MANMM101RUM189S,
       data$MABMM201RUM189S,
       data$MABMM301RUM189S)
length(l)
head(l[[1]])
# create xts object
m<-do.call("merge",l)
# convert to trillions of roubles
m<-m/1000000000000
head(m)
tail(m)
# create filename with path to output folder
filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "money",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
     , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(m
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Агрегаты денежной массы в России с 1995 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:4
                 ,lwd=1:4
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="трлн.руб.") # adds titles to the axes
# create legend
legend_place<-"bottomright"
legend(x=legend_place, # places a legend at the appropriate place
       c("M0","M1","M2","M3"), # puts text in the legend
       lty=1:4, # gives the legend appropriate symbols (lines)
       ,col=1:4
       ,lwd=1:4) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Банк России (ЦБ РФ), Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

# Interest rates
l<-list(data$IRSTCB01RUM156N, 
        data$IR3TIB01RUM156N)
length(l)
head(l[[1]])
# create xts object
m<-do.call("merge",l)
# convert to trillions of roubles
head(m)
tail(m)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "interest_rates_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(m
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Процентные ставки по кредитам Банка России с 1995 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:4
                 ,lwd=1:4
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="%") # adds titles to the axes
# create legend
legend_place<-"topright"
legend(x=legend_place, # places a legend at the appropriate place
       c("сроком до 24 часов",
         "сроком 90 дней"), # puts text in the legend
       lty=1:4, # gives the legend appropriate symbols (lines)
       ,col=1:4
       ,lwd=1:4) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Банк России (ЦБ РФ), Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()


# Price indexes
l<-list(data$RUSCPIALLMINMEI, 
        data$CPGDLF01RUM661N*100,
        data$RUSPPDMMINMEI)
length(l)
head(l[[1]])
# create xts object
m<-do.call("merge",l)
# convert to trillions of roubles
head(m)
tail(m)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "price_indexes_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(m
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Индексы цен в России с 1995 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="") # adds titles to the axes
# create legend
legend_place<-"bottomright"
legend(x=legend_place, # places a legend at the appropriate place
       c("Индекс потребительских цен (ИПЦ)",
         "ИПЦ за искл. продуктов питания",
         "Индекс цен производителей"), # puts text in the legend
       cex=1, pch=1, pt.cex = 1,
       lty=1:3, # gives the legend appropriate symbols (lines)
       ,col=1:3
       ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Банк России (ЦБ РФ), Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

# Total reserves in Russia
head(data$TRESEGRUM052N)
data$TRESEGRUM052N<-data$TRESEGRUM052N/1000000000
filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "total_reserves_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$TRESEGRUM052N
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Международные резервы России, за исключением золота с 1993 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="млрд. долл. США") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Банк России (ЦБ РФ), Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()


# Registered Unemployment Rate for the Russian Federation
head(data$LMUNRRTTRUM156S)
tail(data$LMUNRRTTRUM156S)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "unemployment_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$LMUNRRTTRUM156S
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Зарегистрированный уровень безработицы в России с 1991 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="%") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

#Monthly Earnings: All Activities for the Russian Federation
head(data$LCEATT03RUM664N)
tail(data$LCEATT03RUM664N)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "salaries_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$LCEATT03RUM664N
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Заработная плата в России с 1996 по 2016 гг. (данные без снятой сезонности)"
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="руб.") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

# Production of Total Industry in Russian Federation© (RUSPROINDMISMEI)
head(data$RUSPROINDMISMEI)
tail(data$RUSPROINDMISMEI)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "production_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$RUSPROINDMISMEI
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Индекс промышленного производства в России с 1993 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()


# Ratio of Exports to Imports for the Russian Federation© (XTEITT01RUM156N)
head(data$XTEITT01RUM156N)
tail(data$XTEITT01RUM156N)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "exp_imp_ratio_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$XTEITT01RUM156N
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Отношение экспорта к импорту в России с 1991 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()


# Current Price Gross Domestic Product in Russian Federation© blns of roubles
head(data$RUSGDPNQDSMEI)
tail(data$RUSGDPNQDSMEI)

data$RUSGDPNQDSMEI<-data$RUSGDPNQDSMEI/1000
filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "gdp_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(data$RUSGDPNQDSMEI
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Валовой внутрненний продукт России с 2003 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="трлн. руб.") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

# Уровень монетизации экономики
# Отношение М2 к ВВП
# convert all data in trln of Russian roubles
m2<-data$MABMM201RUM189S/1000000000000
m.gdp<-m2/data$RUSGDPNQDSMEI
head(m2)
head(data$RUSGDPNQDSMEI)

filename<-paste("C:\\Users\\AlexBor\\Google Диск\\Мои учебники\\ДКБ\\LaTeXDoc\\img\\",
                "m2_gdp_Russia",".jpeg",sep='')
filename
# create graphical file
jpeg( filename = filename
      , width = 18, height = 12, units = 'cm', res = 600)
# create layout to draw additional text lines 
layout(matrix(c(1,1,2,2), 2, 2, byrow = T), heights = c(4,1))
# draw chart
chart.TimeSeries(m.gdp
                 ,period.areas=ru.cycles.dates
                 ,period.color="gray"
                 ,main="Уровень монетизации экономики (М2/ВВП) России с 2003 по 2016 гг."
                 ,minor.ticks=F
                 ,lty=1:4
                 ,col=1:3
                 ,lwd=1:3
                 ,date.format.in = "%m-%Y"
                 ,date.format="%Y"
                 ,xlab="Год",ylab="") # adds titles to the axes
# create legend
# legend_place<-"bottomright"
# legend(x=legend_place, # places a legend at the appropriate place
#        c("Индекс потребительских цен (ИПЦ)",
#          "ИПЦ за искл. продуктов питания",
#          "Индекс цен производителей"), # puts text in the legend
#        cex=1, pch=1, pt.cex = 1,
#        lty=1:3, # gives the legend appropriate symbols (lines)
#        ,col=1:3
#        ,lwd=1:3) # gives the legend lines the correct color and width

# add additional text lines
mtext("Затемненные области показывают периоды рецессий в России.", 
      side=1,
      line=4,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Источник: Росстат, Organisation for Economic Co-operation and Development (OECD). 2016.", 
      side=1,
      line=5,
      adj = 0,
      font=3, 
      cex=0.6)
mtext("Расчеты и рисунок автора.", 
      side=1,
      line=6,
      adj = 0,
      font=3, 
      cex=0.6)
# close graphic device
dev.off()

