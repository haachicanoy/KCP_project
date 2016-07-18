# images to publish
# H. Achicanoy
# CIAT, 2016

# Historical data
allWrap2 <- as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), mean(Average)))
allWrap2$std <- as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), sd(Average)))[,ncol(as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), sd(Average))))]
names(allWrap2) <- c('Index', 'Season', 'Mean', 'Std')

# Future data
wrapFutClimInd_median2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), mean(Median)))
wrapFutClimInd_median2$std <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), sd(Median)))[,ncol(as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), mean(Median))))]
names(wrapFutClimInd_median2) <- c('Index', 'RCP', 'Season', 'Mean', 'Std')

tana_river <- allWrap2
tana_river$RCP <- 'Historical'
tana_river <- rbind(tana_river, wrapFutClimInd_median2)

tana_river2 <- tana_river[tana_river$Index=='P_95'|tana_river$Index=='NDWS',]
tana_river2 <- tana_river2[tana_river2$RCP=='Historical'|tana_river2$RCP=='rcp26'|tana_river2$RCP=='rcp85',]
tana_river2$RCP <- gsub(pattern='rcp', replacement='RCP ', tana_river2$RCP)
tana_river2$Index <- as.character(tana_river2$Index)
tana_river2$Index <- gsub(pattern='NDWS', replacement='Drought', tana_river2$Index)
tana_river2$Index <- gsub(pattern='P_95', replacement='Floods', tana_river2$Index)

library(ggplot2)

limits <- aes(ymax=Mean+Std, ymin=Mean-Std)

p <- ggplot(tana_river2, aes(fill=Season, y=Mean, x=RCP)) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
dodge <- position_dodge(width=0.9)
p <- p + facet_grid(Index ~ Season, scales="free")
p <- p + geom_errorbar(limits, position=dodge, width=0.25)
p <- p + xlab('') + ylab('')
p <- p + theme_bw()
ggsave(filename='D:/ToBackup/Modelling/_results/tana_river_cc_droughts_floods.png', plot=p, width=6, height=6)
