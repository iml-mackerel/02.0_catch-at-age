#############################################################################################################################
## PLOT all length distributions by year     ################################################################################
## goal: get a sense of the data             ################################################################################
#############################################################################################################################

# wd
wdimg <- 'img/bio/'
dir.create(wdimg,recursive = T,showWarnings = F)

# plots
y <- sort(unique(bio$year))
dummy <- lapply(y, function(x){
    d <- bio[bio$year==x &!is.na(bio$length),]
    labs <- ddply(d,c('sample.id'),summarise,nafo=nafo[1],date=date[1],gear=gear.full[1],source=source.full[1],n=length(length),nages=length(agef[!is.na(agef)]))
    p <- ggplot(d,aes(x=length))+
        geom_histogram(binwidth = 5,aes(fill=subsample))+
        facet_wrap(~sample.id,scale='free_y',ncol = 10)+                  
        geom_text(data=labs,aes(label=nafo,x=Inf,y=Inf),hjust=1,vjust=1,size=2)+
        geom_text(data=labs,aes(label=date,x=Inf,y=Inf),hjust=1,vjust=2,size=2)+
        geom_text(data=labs,aes(label=gear,x=Inf,y=Inf),hjust=1,vjust=3,size=2)+
        geom_text(data=labs,aes(label=source,x=Inf,y=Inf),hjust=1,vjust=4,size=2)+
        geom_text(data=labs,aes(label=n,x=Inf,y=Inf),hjust=1,vjust=5,size=2)+
        geom_text(data=labs,aes(label=nages,x=Inf,y=Inf),hjust=1,vjust=6,size=2)
    
    n <- ceiling(length(unique(bio[bio$year==x,'sample.id']))/8)
    ggsave(paste0(wdimg,'bio_',x,'.png'),p,width = unit(18,'cm'),height = unit(n,'cm'))
})

