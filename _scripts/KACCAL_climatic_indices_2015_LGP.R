# Calculating climatic indices by counties in Kenya - Historical information first and second season. Determine GROWING SEASON
# KACCAL project
# H. Achicanoy
# CIAT, 2016

source('/mnt/workspace_cluster_8/Kenya_KACCAL/scripts/KACCAL_calc_risk_indices_modified.R')

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)
library(compiler)

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

years_analysis <- paste('y', 1981:2005, sep='')
inputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables'
outputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical'

seasonList <- c('first', 'second')

calc_climIndices <- function(county='Siaya', season='first'){
  
  cat('\n\n\n*** Processing:', gsub(pattern=' ', replacement='_', county, fixed=TRUE), 'county ***\n\n')
  countyDir <- paste(inputDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), sep='')
  
  if(dir.exists(countyDir))
  {
    indexes <- paste(outputDir, '/', season, '_season/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_', season, '_season_2015.RData', sep='')
    
    # @@@ if(!file.exists(indexes)){
    
    cat('Loading raster mask for:', gsub(pattern=' ', replacement='_', county), '\n')
    countyMask <- raster(paste("/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/", gsub(pattern=' ', replacement='_', county), "_base.tif", sep=""))
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Soil data\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/soil/soil_data.RData', sep=''))
    soil <- soil_data_county; rm(soil_data_county)
    soil <- soil[,c("cellID","lon.x","lat.x","id_coarse","rdepth","d.25","d.100","d.225","d.450","d.800","d.1500","soilcp")]
    names(soil)[2:3] <- c('lon','lat')
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    ### CLIMATIC INDICES to calculate
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    # 10. GPSV: Stability in start of season
    # 11. LGPM: Length of growing season
    # 12. LGPV: Variability of LGP
    cat('*** 10. Processing watbal_wrapper function for each cell\n')
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Solar radiation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/dswrf/dswrf.RData', sep=''))
    dswrfAll <- ch2014_year; rm(ch2014_year)
    dswrfAll <- dswrfAll[years_analysis]
    dswrfAll <- lapply(1:length(dswrfAll), function(i){z <- as.data.frame(dswrfAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrfAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(dswrfAll) <- years_analysis
    dswrfAll <- reshape::merge_recurse(dswrfAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Daily precipitation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/prec/prec.RData', sep=''))
    precAll <- chirps_year; rm(chirps_year)
    precAll <- precAll[years_analysis]
    precAll <- lapply(1:length(precAll), function(i){z <- as.data.frame(precAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(precAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(precAll) <- years_analysis
    precAll <- reshape::merge_recurse(precAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Maximum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/tmax/tmax.RData', sep=''))
    tmaxAll <- ch2014_year; rm(ch2014_year)
    tmaxAll <- tmaxAll[years_analysis]
    tmaxAll <- lapply(1:length(tmaxAll), function(i){z <- as.data.frame(tmaxAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmaxAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tmaxAll) <- years_analysis
    tmaxAll <- reshape::merge_recurse(tmaxAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Minimum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/tmin/tmin.RData', sep=''))
    tminAll <- ch2014_year; rm(ch2014_year)
    tminAll <- tminAll[years_analysis]
    tminAll <- lapply(1:length(tminAll), function(i){z <- as.data.frame(tminAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tminAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tminAll) <- years_analysis
    tminAll <- reshape::merge_recurse(tminAll)
    
    ncells <- length(Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID'])))
    pixelList <- Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID']))
    
    # Created function
    library(tidyr)
    library(lubridate)
    NDWSProcess <- function(j){ # By pixel
      
      daysList <- Reduce(intersect, list(colnames(tmaxAll[,-c(1:3)]), colnames(tminAll[,-c(1:3)]),
                                         colnames(precAll[,-c(1:3)]), colnames(dswrfAll[,-c(1:3)])))
      
      out_all <- soil[which(soil$cellID==pixelList[j]), c('cellID', 'lon', 'lat')]
      out_all <- do.call("rbind", replicate(length(daysList), out_all, simplify=FALSE))
      out_all$SRAD <- as.numeric(dswrfAll[which(dswrfAll$cellID==pixelList[j]), match(daysList, colnames(dswrfAll))])
      out_all$TMIN <- as.numeric(tminAll[which(tminAll$cellID==pixelList[j]), match(daysList, colnames(tminAll))])
      out_all$TMAX <- as.numeric(tmaxAll[which(tmaxAll$cellID==pixelList[j]), match(daysList, colnames(tmaxAll))])
      out_all$RAIN <- as.numeric(precAll[which(precAll$cellID==pixelList[j]), match(daysList, colnames(precAll))])
      rownames(out_all) <- daysList
      
      soilcp <- soil[which(soil$cellID==pixelList[j]), 'soilcp']
      
      watbal_loc <- watbal_wrapper(out_all=out_all, soilcp=soilcp) # If we need more indexes are here
      watbal_loc$TAV <- (watbal_loc$TMIN + watbal_loc$TMAX)/2
      watbal_loc <- watbal_loc[,c('cellID', 'lon', 'lat', 'TAV', 'ERATIO')]
      watbal_loc$GDAY <- ifelse(watbal_loc$TAV >= 6 & watbal_loc$ERATIO >= 0.35, yes=1, no=0)
      
      ### CONDITIONS TO HAVE IN ACCOUNT
      # Length of growing season per year
      # Start: 5-consecutive growing days.
      # End: 12-consecutive non-growing days.
      
      # Run process by year
      lgp_year_pixel <- lapply(1:length(years_analysis), function(k){
        
        # Subsetting by year
        watbal_year <- watbal_loc[year(rownames(watbal_loc))==gsub(pattern='y', replacement='', years_analysis[k]),]
        
        # Calculate sequences of growing and non-growing days within year
        runsDF <- rle(watbal_year$GDAY)
        runsDF <- data.frame(Lengths=runsDF$lengths, Condition=runsDF$values)
        
        # Identify start and extension of each growing season during year
        LGP <- 0; LGP_seq <- 0
        for(i in 1:nrow(runsDF)){
          if(runsDF$Lengths[i] >= 5 & runsDF$Condition[i] == 1){
            LGP <- LGP + 1
            LGP_seq <- c(LGP_seq, LGP)
            LGP <- 0
          } else {
            if(LGP_seq[length(LGP_seq)]==1){
              if(runsDF$Lengths[i] >= 12 & runsDF$Condition[i] == 0){
                LGP <- 0
                LGP_seq <- c(LGP_seq, LGP)
              } else {
                LGP <- LGP + 1
                LGP_seq <- c(LGP_seq, LGP)
                LGP <- 0
              }
            } else {
              LGP <- 0
              LGP_seq <- c(LGP_seq, LGP)
            }
          }
        }
        LGP_seq <- c(LGP_seq, LGP)
        LGP_seq <- LGP_seq[-c(1, length(LGP_seq))]
        runsDF$gSeason <- LGP_seq; rm(i, LGP, LGP_seq)
        LGP_seq <- as.list(split(which(runsDF$gSeason==1), cumsum(c(TRUE, diff(which(runsDF$gSeason==1))!=1))))
        
        # Calculate start date and extension of each growing season by year and pixel
        growingSeason <- lapply(1:length(LGP_seq), function(g){
          
          LGP_ini <- sum(runsDF$Lengths[1:(min(LGP_seq[[g]])-1)]) + 1
          LGP <- sum(runsDF$Lengths[LGP_seq[[g]]])
          results <- data.frame(cellID=pixelList[j], year=gsub(pattern='y', replacement='', years_analysis[k]), gSeason=g, SLGP=LGP_ini, LGP=LGP)
          return(results)
          
        })
        growingSeason <- do.call(rbind, growingSeason)
        if(nrow(growingSeason)>2){
          growingSeason <- growingSeason[rank(-growingSeason$LGP) %in% 1:2,]
          growingSeason$gSeason <- rank(growingSeason$SLGP)
          growingSeason <- growingSeason[order(growingSeason$gSeason),]
        }
        return(growingSeason)
      })
      lgp_year_pixel <- do.call(rbind, lgp_year_pixel)
      
      ##################################################################
      # Verify start and ends of growing seasons by year
      ##################################################################
      # test3 <- watbal_loc[year(rownames(watbal_loc))==1985,]
      # test4 <- lgp_year_pixel[lgp_year_pixel$year==1985,]
      # par(mfrow=c(1,2)); plot(test3$ERATIO, ty='l'); plot(test3$RAIN, ty='l')
      # abline(v=test4$SLGP, col=2)
      
      # Start of growing season vs growing season
      # png('/home/hachicanoy/gSeason_vs_SLGP.png', width=8, height=8, pointsize=30, res=300, units='in')
      # plot(lgp_year_pixel$gSeason, lgp_year_pixel$SLGP, pch=20, col=lgp_year_pixel$gSeason, xlab='Growing season', ylab='Start of growing season (day of year)')
      # dev.off()
      # png('/home/hachicanoy/gSeason_vs_LGP.png', width=8, height=8, pointsize=30, res=300, units='in')
      # plot(lgp_year_pixel$gSeason, lgp_year_pixel$LGP, pch=20, col=lgp_year_pixel$gSeason, xlab='Growing season', ylab='Length of growing season (day of year)')
      # dev.off()
      ##################################################################
      
      SLGP <- lgp_year_pixel[,c('cellID', 'year', 'gSeason', 'SLGP')] %>% spread(year, SLGP)
      LGP  <- lgp_year_pixel[,c('cellID', 'year', 'gSeason', 'LGP')] %>% spread(year, LGP)
      matrices <- list(SLGP=SLGP, LGP=LGP)
      return(matrices)
    }
    library(compiler)
    NDWSProcessCMP <- cmpfun(NDWSProcess)
    
    # Running process for all pixel list
    library(parallel)
    NDWS <- mclapply(1:ncells, FUN=NDWSProcessCMP, mc.cores=20)
    NDWS <- do.call(rbind, NDWS)
    
    # Save all indexes
    clim_indexes <- list(TMEAN=TMEAN, GDD_1=GDD_1, GDD_2=GDD_2, ND_t35=ND_t35,
                         TOTRAIN=TOTRAIN, CDD=CDD, P5D=P5D, P_95=P_95, NDWS=NDWS)
    
    # Save according to season
    if(season == 'first'){
      seasonDir <- paste(outputDir, '/first_season', sep='')
      if(!dir.exists(seasonDir)){dir.create(seasonDir)}
      save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_first_season_2015.RData', sep=''))
    } else {
      if(season == 'second'){
        seasonDir <- paste(outputDir, '/second_season', sep='')
        if(!dir.exists(seasonDir)){dir.create(seasonDir)}
        save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_second_season_2015.RData', sep=''))
      }
    }
    
    # @@@ } else {
    # @@@   cat('Process has been done before. It is not necessary recalculate.\n')
    # @@@ }
    
  } else {
    cat('Process failed\n')
  }
  
  return(cat('Process done\n'))
  
}
# calc_climIndices_v <- Vectorize(calc_climIndices, vectorize.args='county')

countyList <- countyList[6,]

library(parallel)
mclapply(1:length(seasonList), function(j){
  lapply(1:length(countyList$County), function(i){
    calc_climIndices(county=countyList$County[i], season=seasonList[[j]])
  })
}, mc.cores=2)
