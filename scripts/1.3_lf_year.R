#############################################################################################################################
## details for a year                        ################################################################################
#############################################################################################################################
lfR <- paste0(dir.rdat,"lf.Rdata")
load(lfR)
# weight landed for S52 license samples
y <- 2022

sam <- lf[lf$year>=y,]
sam <-  unique(sam[sam$source==91,c('year','sample.id','month','wcatch','date','nafo','nbpc','source','gear','prov')])
nrow(sam)                               # wo two from Jerry

sam[is.na(sam$wcatch),'wcatch'] <- 300 # temporary
sum(sam$wcatch)

#ddply(sam,c('nafo'),summarise,sum(wcatch))

catch.sam <- ddply(sam,c('year','month','nafo','gear','prov'),summarise,catch=sum(wcatch)/1000)

catch.sam$country <- 'Canada'
catch.sam$source <- 'S52'
names(catch.sam)[which(names(catch.sam)=='gear')] <-'gear.cat'

catch.n <-  sam %>%  group_by(year, month, nafo) %>%  tally() %>%  pivot_wider(names_from=nafo, values_from=n)

catch.n<- catch.n %>% group_by(year, month) %>% 
mutate(total = purrr::pmap_dbl(list(`4T`,`4S`,`4W`,`4X`,`3K`,`3L`,`4R`), sum, na.rm = TRUE))
    
total = catch.n  %>% group_by(year) %>%  summarise_all(sum, na.rm=T) %>%  mutate(month="total")

catch.ntable<- bind_rows(catch.n %>% mutate(month=as.character(month)), total)

catch.ntable<- catch.ntable %>%  mutate_at(vars(`4T`,  `4S`,  `4W` , `4X` , `3K`,  `3L` , `4R`, total), ~ replace(., is.na(.), 0))

catch.ntable$country <- 'Canada'
catch.ntable$source <- 'S52'

write.csv(catch.ntable, paste0("../../csv/",max(my.year), "/Nlf_nafo.csv"), row.names=F)

