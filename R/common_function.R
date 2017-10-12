get_histo = function(Data,col_plot="pval",breaks=0.03,titre){
  
  Gain = round(mean(as.numeric(as.character(Data$overyielding)))*100,2)
  Mean=mean(as.numeric(as.character(Data$overyielding)))
  Positif = round(length(Data$overyielding[as.numeric(as.character(Data$overyielding))>0])*100/length(Data$overyielding),2)
  Negatif = round(length(Data$overyielding[as.numeric(as.character(Data$overyielding))<0])*100/length(Data$overyielding),2)
  
  B= ifelse(abs(max(as.numeric(as.character(Data$overyielding)))) > abs(min(as.numeric(as.character(Data$overyielding)))),max(as.numeric(as.character(Data$overyielding))),min(as.numeric(as.character(Data$overyielding))))
  
  
  pval= NULL
  for (i in 1:nrow(Data)){
    if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){pval = c(pval,"Significatif à 0.01")}
    if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){pval = c(pval,"Significatif à 0.05")}
    if (as.numeric(as.character(Data[i,"pvalue"])) > 0.05){pval = c(pval,"Non significatif (pvalue >0.05)")}
  }
  
  if(col_plot == "year"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(year))) + scale_fill_discrete(name = "Année")
  }else if(col_plot == "melange"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(melange))) + scale_fill_discrete(name = "Mélange")
  }else if(col_plot == "location"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(location))) + scale_fill_discrete(name = "Paysan")
  }else if(col_plot == "modalite"){
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(mod))) + scale_fill_discrete(name = "Modalité")
  }else{
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(pval)))  + scale_fill_discrete(name = "Significativité")
  }
  
  if(shapiro.test(as.numeric(as.character(Data$overyielding)))$p.value <0.05){
    Signif = wilcox.test(as.numeric(as.character(Data$overyielding)),mu=0)$p.value
  }else{
    Signif = t.test(as.numeric(as.character(Data$overyielding)),mu=0)$p.value
  }
  sign = get_stars(Signif)
  
  Mean = round(mean(as.numeric(as.character(Data$overyielding))),3)
  p = p + geom_histogram(breaks=seq(1.5*min(as.numeric(as.character(Data$overyielding))),1.5*max(as.numeric(as.character(Data$overyielding))),breaks), alpha=0.6, color="black")
  p = p + geom_vline(xintercept = Mean, size = 1.2, color="red") + geom_vline(xintercept = 0,  linetype = "dotted")
  p = p + labs(x=paste("Différence normalisée entre les mélanges et 
	                         la moyenne de leurs composantes pour ",titre,sep=""), y="Nombre de mélanges")
  p = p + labs(title = paste(titre,":","Gain moyen =",Mean*100,"% (",sign,");
        ","Cas positifs :",Positif,"%",";","Cas négatifs :",Negatif,"%",sep=" "))
  
  return(list("plot"=p,"pval"=Signif))
}

get_stars = function(res) {
  stars = findInterval(res, c(0, 0.001, 0.01, 0.05, 0.1))
  stars[is.na(stars)]=5
  codes = c("***" , "**","*", ".", " ")
  return(codes[stars])
}

# get duplicated data in 1 table
dupl_table = function(tab){
  tab = lapply(tab,function(x){return(cbind(x[[1]],x[[2]]))})
  TAB = NULL
  for (i in 1:length(tab)){TAB = rbind(TAB,tab[[i]])}
  
  attributes(tab)$invert=FALSE
  return(tab)
}





