# 0. help -----------------------------------------------------------------
#' Study of the intra-population selection within the mixture project
#' 
#' @param donnees Results from \code{\link{PPBstats::MC} (list) or from \code{\link{shinemas2R::get.data} (data frame)
#' 
#' @param data_version Result from \code{\link{shinemas2R::get.data}} with query.type = "data-S" with add columns type ("Composante" or "Mélange") and modalite which is the discriminating factor when empile = TRUE
#' 
#' @param variable The variable on which the analysis is done. Must be one of the elements of donnees
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
analyse.selection = function(donnees, data_version, variable, person, empile=FALSE, nom="", language="english")
{
  
  data=unique(data_version[,c("year","location","germplasm","group","type","modalite")])
  data$vrac = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[1]])}),",",data$location,":",data$year,"]",sep="")
  data$bouquet = paste("mu[",sapply(strsplit(as.character(data$group)," | "),function(x){return(x[[3]])}),",",data$location,":",data$year,"]",sep="")

 compare_model = function(x){
   # x : nom du vrac et nom du bouquet. La fonction retourne la moyenne de chacune des chaines ainsi que la comparaison des 2
   MCMC = donnees[[variable]]$model.outputs$MCMC
   vrac=MCMC[,colnames(MCMC) %in% x["vrac"]] 
   bouquet=MCMC[,colnames(MCMC) %in% x["bouquet"]]
   
   Result = as.data.frame(cbind(vrac,bouquet))
   colnames(Result) = c("mu[vrac]","mu[bouquet]")
   Mpvalue = comp.parameters(Result, parameter = "mu", type = 1)
   return(c(mean(vrac),mean(bouquet),Mpvalue[1,2]))
 }
 
 chi2 = function(x){
   # x: nom du vrac et du bouquet
   if (class(x) == "data.frame"){Mat = donnees[as.character(donnees$expe_name) %in% x[,"group"],]}else{Mat = donnees[as.character(donnees$expe_name) %in% x["group"],]}
   # add one since color, awns and curve can be 0 and then problems when calculating overyielding
   vrac=as.numeric(na.omit(Mat[grep("vrac",Mat$sl_statut),variable]))+1
   bouquet = as.numeric(na.omit(Mat[grep("bouquet",Mat$sl_statut),variable]))+1
   if( length(levels(as.factor(na.omit(vrac)))) ==1 | length(levels(as.factor(na.omit(bouquet)))) ==1){
     return(c(mean(na.omit(as.numeric(vrac))-1),mean(na.omit(as.numeric(bouquet))-1),NA))
   }
#   if(length(vrac) != length(bouquet)){if(length(vrac) > length(bouquet)){bouquet=c(bouquet,rep(NA,(length(vrac)-length(bouquet))))}else{vrac=c(vrac,rep(NA,(length(bouquet)-length(vrac))))}}
    # Test non paramétrique U de Wilcoxon-Mann-Whitney pour données semi_quantitatives
    WMW = wilcox.test(as.numeric(c(vrac,bouquet)) ~ c(rep("vrac",length(vrac)),rep("bouquet",length(bouquet))))
  # m = as.data.frame(cbind(c(vrac, bouquet),c(rep("vrac",length(vrac)), rep("bouquet", length(bouquet)))))
#   hist(as.numeric(vrac))
#   hist(as.numeric(bouquet))
  # var.test(as.numeric(vrac[,variable]),as.numeric(bouquet[,variable]))
   
   return(c(mean(na.omit(as.numeric(vrac))),mean(na.omit(as.numeric(bouquet))),WMW$p.value))
 }

#1. If the data was analyzed using bayesian model -----------
if (class(donnees) == "list"){
  if (!(variable %in% names(donnees))){stop("Variable must be one of donnees's names")}
  result = apply(data,1,FUN=compare_model)
}
   
#2. If the data was not analyzed using the bayesian model: semi-quantitative data such as awns, color, curve --> use chi2 test to compare selection vs bulk-----------
if(class(donnees) == "data.frame"){
  if (!(variable %in% names(donnees))){stop("Variable must be one of donnees's names")}
  
  result = apply(data,1,FUN=chi2)
}
 
 result = t(result)
 colnames(result) = c("MoyenneVrac","MoyenneBouquet","pvalue")
 Data=cbind(data,result)
 Data$overyielding = Data$MoyenneBouquet/Data$MoyenneVrac-1
 Data=Data[Data$modalite != "",]
 pval= NULL
 Data[is.na(Data$pvalue),"pvalue"] = 1
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
 
  
#3. Histogram ---------
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
  }
  p = p + labs(x=ifelse(language == "english",paste("Normalised difference between selected and non-selected bulk, ",variable,sep=""),paste("Différence normalisée entre bouquet de sélection et vrac, ",variable,sep="")),
               y=ifelse(language == "english","Number of comparisons selected vs non-selected bulk","Nombre de couples Bouquet - Vrac")) 
  p = p + geom_vline(xintercept = 0,  linetype = "dotted")
  p = p + scale_fill_discrete(name = "")
  p = p + theme(legend.text = element_text(size = 7), axis.title = element_text(size = 10))
  
  if (empile==F){p = p + annotate("text",label = c(paste("n :",nrow(Data),sep=" "),paste(ifelse(language == "english","Mean gain =","Gain moyen ="),round((mean(Mean))*100,2),"%",sep=" ")),x=(max(Data$overyielding)-0.2*max(Data$overyielding)),y=c(nrow(Data)/5,(nrow(Data)/5-nrow(Data)/50)))}

  
  return(list("histo" = p, "tab" = Data))
}
