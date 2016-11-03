# 0. help -----------------------------------------------------------------
#' Barplot for the comparison of the mixture vs its components & the mean of its components
#' 
#' @param res_model output from the \code{PPBstats::analyse.outputs} function
#' 
#' @param farmers_data output from the \code{shinemas2R::get.data} function, with argument query type == "data.mixture.1"
#' 
#' @param variable the variable to study
#' 
#' @param plot.type the type of plot wanted. Can be "comp.in.farm" to compare the mixtures to each of its components in a farm ; "mixVScomp" to compare all mixtures to all components ; "mix.comp.distribution" ; "mix.gain.distribution" to plot the distribution of the difference between mixtures and the mean of its components
#'
#' @param person if plot.type = "comp.in.farm", the farmers you want the analysis done to
#' 
#' @param nb_parameters_per_plot the number of parameters per plot
#
#' @return A list containing, for each environment and mixture, the barplots ("bp") and the tables ("Tab")
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{shinemas2R::get.data}}
#' 

ggplot_mixture1 = function(res_model, melanges_PPB_mixture, variable, year, plot.type = "comp.in.farm", person, nb_parameters_per_plot = 8) 
{
	melanges_PPB_mixture=melanges_PPB_mixture$data
add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 

# 1. Compare, for each mixture in each farm, the mixture to its components and the mean of these components ----
if ( plot.type == "comp.in.farm" | plot.type == "mix.comp.distribution"| plot.type == "mix.gain.distribution") {
	  # Séparer par environnement
	d_env = plyr:::splitter_d(melanges_PPB_mixture, .(location))
	if (plot.type == "comp.in.farm"){d_env=list(d_env[[grep(person,names(d_env))]])}
	
	  # Par environnement, on sépare par mélange pour ensuite faire les graphs
	d_env_b = lapply(d_env, function(x){
	    # une table par mélange
	   mix = plyr:::splitter_d(x, .(mixture_id))
	   MIX = list()
	   for (i in 1:(length(mix)-1)) {
	   	Mel = mix[[i]]
	   	Comp = mix[[length(mix)]] 
	    MIX = c(MIX,list(rbind(Mel, Comp[Comp$expe %in% Mel$mixture_id,])))
	   }
	
	   # récupérer les données (MCMC) pour chaque mixture et les splitter
	   mix_split = lapply(MIX , function(y) {
	       noms = as.data.frame(unique(y$germplasm_son))
	       noms$type = c("Mélange",rep("Composante",(nrow(noms)-1)))
	       colnames(noms)[1] = "germplasm"
	       
	       mcmc = get_result_model(res_model, y, type_result = "MCMC", variable, param = "mu", year = year)
	       Mel = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$type == "Mélange"),"germplasm"]]

	       if (length(Mel) > 0) {
	         Comp = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$type == "Composante"),"germplasm"]]
	         if (ncol(Comp) < nrow(y)/2){missingComp = TRUE}else{missingComp=FALSE}
	         MeanComp = apply(Comp, 1, mean)
	         M = cbind(Mel, MeanComp, Comp)
	         attributes(M)$model = "model1"
	         colnames(M)[colnames(M) %in% "MeanComp"] = paste("mu[","MoyenneComposantes",",",unique(y$location),":",year,"]",sep="")
	         colnames(M)[colnames(M) %in% "Mel"] = paste("mu[", noms[noms$type %in% "Mélange","germplasm"],",",unique(y$location),":",year,"]",sep="")
	         comp.mu = get.mean.comparisons(M, "mu", get.at.least.X.groups = 1)
	         
	         C=comp.mu$Mpvalue[[1]]
	         A=C[which(rownames(C) == paste("mu[","MoyenneComposantes",",",unique(y$location),":",year,"]",sep="")), which(colnames(C) == paste("mu[",noms[which(noms$type == "Mélange"),"germplasm"],",",unique(y$location),":",year,"]",sep=""))]
	         if(A == 0){A = C[which(rownames(C) == paste("mu[",noms[which(noms$type == "Mélange"),"germplasm"],",",unique(y$location),":",year,"]",sep="")), which(colnames(C) == paste("mu[","MoyenneComposantes",",",unique(y$location),":",year,"]",sep=""))]}

	         comp.mu=comp.mu$mean.comparisons
	         comp.mu$germplasm = unlist(rm_between(comp.mu$parameter, "[", ",", extract=TRUE))
	         comp.mu$pval=A
	         
	         type = NULL
	         for (i in 1:nrow(comp.mu)) { 
	           a = noms[noms$germplasm %in% comp.mu[i,"germplasm"],"type"]
	           if (!is.null(a)) {type = c(type, a)}
	           if (comp.mu[i,"germplasm"] == "MoyenneComposantes") { type = c(type, "MoyenneComposantes")}
	         }
	         
	         Data = cbind(comp.mu, type)
	         Data = arrange(Data, median)
	         Data$max = max(Data$median, na.rm = TRUE)
	         
	         if (plot.type == "comp.in.farm") {
	           Data$split = add_split_col(Data, nb_parameters_per_plot)
	           Data_split = plyr:::splitter_d(Data, .(split))
	           
	           # faire le graph pour chaque split
	           bp = lapply(Data_split , function(z){return(barplot.mixture1(z))})
	           
	           return(list("barplot"= bp, "Tab" = Data))
	         }
	         if ((plot.type == "mix.comp.distribution" | plot.type == "mix.gain.distribution") & missingComp == FALSE) {
	           return(list("barplot" = NULL, "Tab" = Data))
	         }

	       } else {
	         warning("No data for the mixture")}
	        return(list("barplot"= NULL, "Tab" = NULL))
	     }) # end lapply(y)
	   return(mix_split)
	}) # end lapply(x)
	names(d_env_b) = names(d_env)
	if(plot.type == "comp.in.farm") {return(d_env_b)}
}

Nul = TRUE
for (i in 1:length(d_env_b)){
  for (j in 1:length(d_env_b[[i]])){
    for (k in 1:length(d_env_b[[i]][[j]])){
      if(!is.null(d_env_b[[i]][[j]][[k]])){Nul = FALSE}
    }
  }
}

# 2. Compare the effect of being a mixture vs the effect of being a component -----
# Normalement ça marche
if ( plot.type == "mixVScomp") {
	
	melange = grep("son",melanges_PPB_mixture$sl_statut)
	
	Mélanges = melanges_PPB_mixture[melange,]
	Composantes = melanges_PPB_mixture[-melange,]
	
	# Récupérer les résultats du modèle
	Result = lapply(list(Mélanges,Composantes), function(x){
		mcmc = get_result_model(res_model, x, type_result = "MCMC", variable, param = "mu", year = "2016")
			
		#concaténuer les chaines
		if (ncol(mcmc)>1) {
			MCMC = NULL
			for( i in 1:nrow(mcmc)) { MCMC = c(MCMC, mean(as.matrix(mcmc[i,])))}
		}else{
			MCMC = mcmc
		}
		return(MCMC)
	})
		
	Result = as.data.frame(cbind(Result[[1]],Result[[2]]))
	colnames(Result) = c("mu[Mélanges]","mu[Composantes]")
		
	# Comparer les MCMC des mélanges et des composantes
	Mpvalue = comp.parameters(Result, parameter = "mu", type = 1)
	Comparison = get.significant.groups(Mpvalue,Result, alpha = 0.05)

	Data = arrange(Comparison, median)
	Data$max = max(Data$median, na.rm = TRUE)
	Data$type = ex_bracket(Data$parameter)
	Data$germplasm = unlist(ex_bracket(Data$parameter))
	Data$max = max(Data$median, na.rm = TRUE)
	# graphique mélanges vs composantes
	bp = barplot.mixture1(Data)
	return(list("bp"=bp, "Tab" = Data))
}

# 3.  Sur le réseau, comparer la distribution des mélanges à celles de la moins bonne et la meilleure composante pour chaque mélange ----------
# Normalement ça marche, pas testé sur données
if (plot.type == "mix.comp.distribution" | plot.type == "mix.gain.distribution") {
    if(Nul == FALSE){
      Distrib = lapply(d_env_b, function(x){
        Worst = Best = Mel = MeanComp = NULL
        
        Mat = lapply(x, function(y) {
          Tab=y$Tab
          Data = Tab[Tab$type %in% "Composante",]
          if (!is.null(Data)){
            Worst = Data[which(Data$median == min(Data$median, na.rm = TRUE)),"median"]
            Best = Data[which(Data$median == max(Data$median, na.rm = TRUE)),"median"]
            Mel = Tab[Tab$type %in% "Mélange","median"]
            MeanComp = Tab[Tab$type %in% "MoyenneComposantes","median"]
            pval = Data$pval
           
            # get significance
            grepMel = grep("Mélange",Tab$type)
            grepMeanComp = grep("MoyenneComposantes",Tab$type)

            M = cbind(c(Worst,MeanComp,Mel,Best),c("1.moins bonne","2.moyenne composantes","3.mélange","4.meilleure"),pval[1:4])
            M=as.data.frame(M)

            colnames(M) = c("median","Type","pval")
            rownames(M) = c("Worst","MoyenneComposantes","Mélange","Best")
          }else{
            M = NULL
          }
          return(list("plot" = NULL, "tab"= M))
        })
        return(Mat)
      })
      
      if (plot.type == "mix.comp.distribution"){
        toPlot=NULL
        D = lapply(Distrib,function(x) {
          return(lapply(x,function(y){return(y$tab)}))
        })
        for ( i in 1:length(D)) {
          for (j in 1:length(D[[i]])){
            if(!is.null(D[[i]][[j]])){toPlot=rbind(toPlot,cbind(as.matrix(D[[i]][[j]]),rep(names(D)[i], 4),rep(paste("Paysan",i," Mélange",j,sep=""),4)))}
          }
        }
        
        colnames(toPlot) = c("Moyenne","Type","Paysan","Group")
        rownames(toPlot)=NULL
        toPlot=as.data.frame(toPlot)
        toPlot$Moyenne = as.numeric(as.character(toPlot$Moyenne))
        
        p = ggplot(toPlot, aes(x=Type,y=Moyenne,color = Group, shape=Group), xlab=variable)
        p = p +labs(x="", y=paste("Valeur du ",variable,sep=""))
        p = p + stat_summary(fun.y=mean,geom="point",color="black",shape="x",size=4.5)
        p = p + geom_jitter(position=position_jitter(0), cex=3)
        p = p + scale_shape_manual(values = seq(1,nrow(toPlot)/4,1))
        p = p + theme(legend.position="none")
        return(p)
      }

    }else{
      return(NULL)
    }

	}


# 4. Sur le réseau, distribution du gain des mélanges par rapport aux composantes ----------
	if (plot.type == "mix.gain.distribution") {
	  if(Nul==FALSE){
	    Histo = lapply(Distrib,function(x){
	      return(lapply(x, function(y){
	        z=y$tab
	        if(!is.null(z)){
	          diff = as.numeric(as.character(z["Mélange","median"]))/as.numeric(as.character(z["MoyenneComposantes","median"]))-1
	          
	          return(c(unlist(diff),as.numeric(as.character(z[1,"pval"]))))
	      }}))
	    })
	    
	    
	    Data = cbind(
	                 unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(melange[[1]])}))})),
	                 unlist(lapply(Histo,function(paysan){return(lapply(paysan, function(melange){return(melange[[2]])}))}))
	                 )
	    Data=cbind(rownames(Data),Data)
	    Data=as.data.frame(Data)
	    colnames(Data) = c("Paysan","overyielding","pvalue")
	    Gain = round(mean(as.numeric(as.character(Data$overyielding)))*100,2)
	    Mean=mean(as.numeric(as.character(Data$overyielding)))
	    
	    pval= NULL
	    for (i in 1:nrow(Data)){
	      if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.01){pval = c(pval,"significant at 0.01")}
	      if (as.numeric(as.character(Data[i,"pvalue"])) <= 0.05 & as.numeric(as.character(Data[i,"pvalue"])) > 0.01 ){pval = c(pval,"significant at 0.05")}
	      if (as.numeric(as.character(Data[i,"pvalue"])) > 0.05){pval = c(pval,"not significant (pvalue >0.05)")}
	    }
	    
	    p =  ggplot(data=Data,aes(as.numeric(as.character(overyielding)),fill=as.factor(pval))) + geom_histogram(breaks=seq(-0.2,0.4,0.04), alpha=0.6)
	    p = p + geom_vline(xintercept = Mean, size = 1.2, color="red") 
	    p = p + labs(x=paste("rapport Mélange/Moyenne des composantes pour ",variable,sep=""), y="Nombre de mélanges")
	    p = p + geom_text(x=Mean,y=-0.1,label=paste("Gain moyen =",Gain,"%",sep=" "), size=5)
	    p = p + geom_vline(xintercept = 0,  linetype = "dotted")

	   
	    return(p)
	  }else{
	    return(NULL)
	    }
	  
}



} # end function
