# Read data

pigeons_to_jags <- function() {
  
  pigeons <- read.csv('data/matching_by_session.csv')
  pigeons <- pigeons[-which(pigeons$n_reinf_right == 0
                            | pigeons$n_reinf_left  == 0
                            | pigeons$n_resp_right  == 0
                            | pigeons$n_resp_left   == 0 ),]
  pigeons <- pigeons[!pigeons$dynamic_env,]
  
  sessions <- NA
  for(i in 1:nrow(pigeons)){
    sessions[i] <- as.numeric(strsplit(pigeons$session[i],split='s')[[1]][2])
  }
  
  birds <- as.numeric(as.factor(pigeons$bird))
  
  list(pigeons = pigeons,
       sessions = sessions,
       birds = birds)
}

bird_colors <- c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02')

plot_marginal <- function(node, nds){
  if(node=='alpha'){
    marginal = nds$alpha
  }
  else if(node=='beta'){
    marginal = nds$beta
  }
  plot(NULL, xlim=c(-2,2), ylim=c(0,1.5), axes=F, ann=F)
  abline(v=0, lty='dashed')
  abline(h=.25)
  par(cex.axis=1.5)
  axis(1)
  mtext(node,1,cex=2,line=3)
  for(brd in 1:6){
    hist(marginal[,brd],breaks=20,plot=F)->ht
    lines(ht$mids,ht$density,lwd=4,col=bird_colors[brd])
  }
}
