# 0. help -----------------------------------------------------------------
#' Study of the intra-population selection within the mixture project
#' 
#' @param 
#' 
#' @return 
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{}}
#' 
analyse.selection = function(res_model, data_version, variable, person, nom, empile)
{
  
  data=unique(data_version[,c("year","location","germplasm","group","type","modalite")])
  data=data[1:(nrow(data)/2),]
  data$vrac = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[1]])}),",",data$location,":",data$year,"]",sep="")
  data$bouquet = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[3]])}),",",data$location,":",data$year,"]",sep="")
  
 compare = function(x){
   # x : nom du vrac et nom du bouquet. La fonction retourne la moyenne de chacune des chaines ainsi que la comparaison des 2
   MCMC = res_model[[variable]]$model.outputs$MCMC
   vrac=MCMC[,colnames(MCMC) %in% x["vrac"]] 
   bouquet=MCMC[,colnames(MCMC) %in% x["bouquet"]]
   
   Result = as.data.frame(cbind(vrac,bouquet))
   colnames(Result) = c("mu[vrac]","mu[bouquet]")
   Mpvalue = comp.parameters(Result, parameter = "mu", type = 1)
   return(c(mean(vrac),mean(bouquet),Mpvalue[1,2]))
 }
   result = apply(data,1,FUN=compare)
   result = t(result)
   colnames(result) = c("MoyenneVrac","MoyenneBouquet","pvalue")
   Data=cbind(data,result)
   Data$overyielding = Data$MoyenneBouquet/Data$MoyenneVrac-1
   Data=Data[Data$modalite != "",]
   pval= NULL
   for (i in 1:nrow(Data)){
     if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){pval = c(pval,"Significatif à 0.01")}
     if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){pval = c(pval,"Significatif à 0.05")}
     if (as.numeric(as.character(Data[i,"pvalue"])) > 0.05){pval = c(pval,"Non significatif (pvalue >0.05)")}
   }
  Mean=mean(Data$overyielding)
  Data$pval = pval
  
  mel = Data[Data$type %in% "Mélange", "overyielding"]
 
  
  # Histogram
  From= min(Data$overyielding)-0.2*abs(min(Data$overyielding))
  To = max(Data$overyielding) + 0.2*max(Data$overyielding)
  By = (max(Data$overyielding)-min(Data$overyielding))/12
  if(nrow(Data) ==1){p = ggplot(ylim = c(0,1))
  }else{ 
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=factor(pval, levels = c("Non significatif (pvalue >0.05)","Significatif à 0.05","Significatif à 0.01")))) +
    geom_histogram(breaks = seq(From,To,By), color = "black")
    }
  if (empile==TRUE){p = p + facet_wrap( ~ modalite, ncol =1)}
  if (length(mel) > 0 & length(mel) < nrow(Data) & empile == FALSE) {p = p + annotate("text",label = rep("*",length(mel)),x=mel,y=0, size=10)}
  p = p + ggtitle(paste(person, variable,nom,  sep=" : "))
  if (empile == FALSE) {
    p = p + geom_vline(xintercept = Mean, size = 1.2, color="red")
    p = p + geom_text(x=Mean,y=-0.03,label=paste("Gain moyen =",round((Mean)*100,2),"%",sep=" "), size=5)
  }
  p = p + labs(x=paste("rapport Bouquet de sélection / Vrac pour ",variable,sep=""), y="Nombre de couple Bouquet - Vrac") 
  p = p + geom_vline(xintercept = 0,  linetype = "dotted")
  p = p + scale_fill_discrete(name = "Significativité")

 # p = p +annotate("text",label = "* : Sélection(s) dans le mélange",x=(max(Data$overyielding)-0.5),y=3)
  
  return(list("histo" = p, "tab" = Data))
}
