#############################################################################################################################
## PLOT all length distributions by year     ################################################################################
## goal: get a sense of the data             ################################################################################
#############################################################################################################################

# wd
wdimg <- paste0('../../img/',tail(my.year,1),'/lf/')
dir.create(wdimg,recursive = T,showWarnings = F)

# plots
y <- sort(unique(lf$year))
dummy <- lapply(y, function(x){
    d <- lf[lf$year==x &!is.na(lf$length),]
    labs <- ddply(d,c('sample.id'),summarise,nafo=nafo[1],date=date[1],gear=gear.full[1],source=source.full[1],n=sum(n))
    p <- ggplot(d,aes(x=length,y=n))+
        geom_bar(stat='identity',aes(fill=subsample))+
        facet_wrap(~sample.id,scale='free_y',ncol = 10)+                  
        geom_text(data=labs,aes(label=nafo,x=Inf,y=Inf),hjust=1,vjust=1,size=2)+
        geom_text(data=labs,aes(label=date,x=Inf,y=Inf),hjust=1,vjust=2,size=2)+
        geom_text(data=labs,aes(label=gear,x=Inf,y=Inf),hjust=1,vjust=3,size=2)+
        geom_text(data=labs,aes(label=source,x=Inf,y=Inf),hjust=1,vjust=4,size=2)+
        geom_text(data=labs,aes(label=n,x=Inf,y=Inf),hjust=1,vjust=5,size=2)
    
    n <- ceiling(length(unique(lf[lf$year==x,'sample.id']))/8)
    ggsave(paste0(wdimg,'lf_',x,'.png'),p,width = unit(18,'cm'),height = unit(n,'cm'))
})
