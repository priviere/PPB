# 0. help -----------------------------------------------------------------
#' Study of the intra-population selection within the mixture project
#' 
#' @param res_model Results from \code{\link{PPBstats::MC}
#' 
#' @param data_version Result from \code{\link{shinemas2R::get.data}} with query.type = "data-S" with add columns type ("Composante" or "Mélange") and modalite which is the discriminating factor when empile = TRUE
#' 
#' @param variable The variable on which the analysis is done. Must be one of the elements of res_model
#' 
#' @param person Element of the plot title
#' 
#' @param empile If TRUE, separate the graphic in x plots depending on the x levels of the data_version's "modalite" column 
#' 
#' @param nom A name to put on graphic title
#' 
#' @return A list containing the plot and the data frame containing the data
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{}}
#' 
analyse.selection = function(res_model, data_version, variable, person, empile, nom, language)
{
  
  data=unique(data_version[,c("year","location","germplasm","group","type","modalite")])
 # data=data[1:(nrow(data)/2),]
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
     if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){
       if(language == "english"){pval = c(pval,"Component, significant at 0.01")}else{pval = c(pval,"Composante, significatif à 0.01")}}
     if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){
       if(language == "english"){pval = c(pval,"Component, significant at 0.05")}else{pval = c(pval,"Composante, significatif à 0.05")}}
     if (Data[i,"type"] == "Composante" & as.numeric(as.character(Data[i,"pvalue"])) > 0.05){
       if(language == "english"){pval = c(pval,"Component, not significant (pvalue > 0.05)")}else{pval = c(pval,"Composante, non significatif (pvalue > 0.05)")}}
     if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){
       if(language == "english"){pval = c(pval,"Mixture, significant at 0.01")}else{pval = c(pval,"Mélange, significatif à 0.01")}}
     if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){
       if(language == "english"){pval = c(pval,"Mixture, significant at 0.05")}else{pval = c(pval,"Mélange, significatif à 0.05")}}
     if (Data[i,"type"] == "Mélange" & as.numeric(as.character(Data[i,"pvalue"])) > 0.05){
       if(language == "english"){pval = c(pval,"Mixture, not significant (pvalue > 0.05)")}else{pval = c(pval,"Mélange, non significatif (pvalue > 0.05)")}}
   }
  Mean=unlist(lapply(levels(as.factor(Data$modalite)),function(x){mean(Data[Data$modalite %in% x,"overyielding"])}))
  names(Mean) = levels(as.factor(Data$modalite))
  Data$pval = pval
  
  mel = Data[Data$type %in% "Mélange", "overyielding"]
  names(mel) = Data[Data$type %in% "Mélange", "germplasm"]
  Data$modalite = unlist(lapply(Data$modalite, function(x){paste(x,ifelse(language=="english","- Mean gain: "," - Gain moyen : "),round((Mean[x])*100,2)," % (n = ",nrow(Data[Data$modalite %in% x,])," )", sep="")}))

  # Histogram
  From= min(Data$overyielding)-0.2*abs(min(Data$overyielding))
  To = max(Data$overyielding) + 0.2*max(Data$overyielding)
  By = (max(Data$overyielding)-min(Data$overyielding))/12
  if (language == "english"){factoL = c("Component, not significant (pvalue > 0.05)","Component, significant at 0.05","Component, significant at 0.01",
                                        "Mixture, not significant (pvalue > 0.05)","Mixture, significant at 0.05","Mixture, significant at 0.01")
  }else{factoL = c("Composante, non significatif (pvalue > 0.05)","Composante, significatif à 0.05","Composante, significatif à 0.01",
                                                       "Mélange, non significatif (pvalue > 0.05)","Mélange, significatif à 0.05","Mélange, significatif à 0.01")}
  if(nrow(Data) ==1){p = ggplot(ylim = c(0,1))
  
  }else{ 
    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=factor(pval, levels = factoL))) +
    geom_histogram(breaks = seq(From,To,By), color = "black")
  }
  
  if (empile==TRUE){ p = p + facet_wrap( ~ modalite, ncol =1, scales="free_y") + theme(strip.text.x = element_text(size=9))} 
  p = p + ggtitle(paste(person, variable, nom, sep=" : "))
  if (empile == FALSE) {
    p = p + geom_vline(xintercept = mean(Mean), size = 1.2, color="red")
   # p = p + geom_text(x=mean(Mean),y=-0.03,label=paste("Gain moyen =",round((mean(Mean))*100,2),"%",sep=" "), size=5)
  }
  p = p + labs(x=ifelse(language == "english",paste("Normalised difference between selected and non-selected bulk, ",variable,sep=""),paste("Différence normalisée entre bouquet de sélection et vrac, ",variable,sep="")),
               y=ifelse(language == "english","Number of comparisons selected vs non-selected bulk","Nombre de couples Bouquet - Vrac")) 
  p = p + geom_vline(xintercept = 0,  linetype = "dotted")
  p = p + scale_fill_discrete(name = "")
  p = p + theme(legend.text = element_text(size = 7), axis.title = element_text(size = 10))
  
  if (empile==F){p = p + annotate("text",label = c(paste("n :",nrow(Data),sep=" "),paste(ifelse(language == "english","Mean gain =","Gain moyen ="),round((mean(Mean))*100,2),"%",sep=" ")),x=(max(Data$overyielding)-0.2*max(Data$overyielding)),y=c(nrow(Data)/5,(nrow(Data)/5-nrow(Data)/50)))}

 # p = p +annotate("text",label = "* : Sélection(s) dans le mélange",x=(max(Data$overyielding)-0.5),y=3)
  
  return(list("histo" = p, "tab" = Data))
}