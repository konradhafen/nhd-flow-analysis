# Do setup ----------------------------------------------------------------

rm(list=ls())
options(scipen=999)
library(tidyverse)

setwd("E:/konrad/Projects/usgs/nhd-flow-estimates/wrk_data/analysis/csv")
fn = "gages_strord.csv"

ingages <- as.data.frame(read_csv(fn))
fn = "flowline_strord.csv"
innhd <- as.data.frame(read_csv(fn))
innhd <- innhd[innhd$StreamOrde > 0,]
fn = "catchments_17_strord.csv"
incat <- as.data.frame(read_csv(fn))
incat <- incat[incat$StreamOrde > 0,]

# Histograms --------------------------------------------------------------

hist(ingages$so_StreamO, freq=F, breaks = seq(1, max(ingages$so_StreamO), by=1))
hist(innhd$StreamOrde, freq=F, breaks=seq(1, max(innhd$StreamOrde), by=1))

ingages$barcolor <- as.character(ingages$so_StreamO)
ggplot(ingages, aes(x=so_StreamO, y=..density..)) +
  geom_histogram(aes(color=as.character(so_StreamO)), alpha=0.0, binwidth=1)

# Stacked histogram figure ------------------------------------------------

ingages$barcolor <- as.character(ingages$so_StreamO)
innhd$barcolor <- as.character(innhd$StreamOrde)
colors <- c('#00d1ec', '#00399d', '#00734d', '#02ac00', '#58c800', '#aee500', '#fffa00', '#ffa700', '#ff5300')
ggplot(NULL) +
  geom_histogram(data=ingages, aes(x=so_StreamO, y=..density..), binwidth=1, fill=colors, color='black') +
  geom_histogram(data=innhd, aes(x=StreamOrde, y=..density..), binwidth=1, color='red', alpha=0.0) +
  labs(x='Stream order', y='Proportion') +
  scale_x_continuous(breaks=seq(1, 9, 1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=16, face='bold'), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0)))
ggsave("C:/Users/khafe/Downloads/strord_gages.png", plot = last_plot(), width = 8, height = 6, units = "in", bg = "transparent")


# Catchment area figure ---------------------------------------------------

tmpdat <- incat[c('StreamOrde', 'AreaSqKM')]
#incat$barcolor <- as.character(incat$StreamOrde)
aggdat <- aggregate(tmpdat$AreaSqKM, b=list(tmpdat$StreamOrde), FUN=sum)
rm(tmpdat)
aggdat$sqkm1000 <- aggdat$x/1000
aggdat$per <- (aggdat$x/sum(aggdat$x))*100.0
colors <- c('#00d1ec', '#00399d', '#00734d', '#02ac00', '#58c800', '#aee500', '#fffa00', '#ffa700', '#ff5300')
ggplot(data=aggdat, aes(x=Group.1, y=per)) +
  geom_bar(stat='identity', fill=colors) +
  labs(x='Stream order', y='Percent area') +
  scale_x_continuous(breaks=seq(1, 9, 1)) +
  scale_y_continuous(breaks=seq(0, 60, 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), axis.line = element_line(colour = "black"),
        axis.title=element_text(size=16, face='bold'), axis.text=element_text(size=12), 
        plot.background = element_rect(fill = "transparent", color = NA), 
        axis.title.y = element_text(margin=margin(t=0, r=20, b=0, l=0)),
        axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0)))
ggsave("C:/Users/khafe/Downloads/strord_catchments_perArea.png", plot = last_plot(), width = 8, height = 6, units = "in", bg = "transparent")

  