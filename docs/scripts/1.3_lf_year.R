#############################################################################################################################
## details for a year                        ################################################################################
#############################################################################################################################

# weight landed for S52 license samples
y <- 2022

sam <- lf[lf$year==2022,]
sam <-  unique(sam[sam$source==91,c('year','sample.id','month','wcatch','date','nafo','nbpc','source','gear','prov')])
nrow(sam)                               # wo two from Jerry

sam[is.na(sam$wcatch),'wcatch'] <- 300 # temporary
sum(sam$wcatch)

#ddply(sam,c('nafo'),summarise,sum(wcatch))

catch.sam <- ddply(sam,c('year','month','nafo','gear','prov'),summarise,catch=sum(wcatch)/1000)

catch.sam$country <- 'Canada'
catch.sam$source <- 'S52'
names(catch.sam)[which(names(catch.sam)=='gear')] <-'gear.cat'
