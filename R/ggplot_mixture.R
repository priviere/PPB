# 0. help -----------------------------------------------------------------
#' Barplot for the comparison of the mixture vs its components & the mean of its components
#' 
#' @param res_model output from the \code{PPBstats::analyse.outputs} function
#' 
#' @param farmers_data output from the \code{shinemas2R::get.data} function, with argument query type == "data.mixture.1"
#' 
#' @param variable the variable to study
#' 
#' @param plot.type the type of plot wanted. Can be "comp.in.farm" to compare the mixtures to each of its components in a farm ; 
#' 																									"mixVScomp" to compare all mixtures to all components ;
#' 																									"mix.comp.distribution" 
#' 																									"mix.gain.distribution" to plot the distribution of the difference between mixtures and the mean of its components
#'
#' @param nb_parameters_per_plot the number of parameters per plot
#
#' @return A list containing, for each environment and mixture, the barplots ("bp") and the tables ("Tab")
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{shinemas2R::get.data}}
#' 

ggplot_mixture1 = function(res_model, melanges_PPB_mixture, variable, plot.type = "comp.in.farm", nb_parameters_per_plot = 8) 
{
  
	melanges_PPB_mixture=melanges_PPB_mixture$data
add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 

# 1. Compare, for each mixture in each farm, the mixture to its components and the mean of these components ----
if ( plot.type == "comp.in.farm" | plot.type == "mix.comp.distribution") {
	  # Séparer par environnement
	d_env = plyr:::splitter_d(melanges_PPB_mixture, .(location))
	  	
	  # Par environnement, on sépare par mélange pour ensuite faire les graphs
	d_env_b = lapply(d_env, function(x){
	    # une table par mélange
	   mix = plyr:::splitter_d(x, .(mixture_id))
	   print(x)
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
	       
	       mcmc = get_result_model(res_model, y, type_result = "MCMC", variable, param = "mu", year = "2016")
	
	       Mel = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$type == "Mélange"),"germplasm"]]
	       
	       if (length(Mel) > 0) {
	         Comp = mcmc[,unlist(rm_between(colnames(mcmc), "[", ",", extract=TRUE)) %in% noms[which(noms$type == "Composante"),"germplasm"]]
	         
	         MeanComp = apply(Comp, 1, mean)
	         M = cbind(Mel, MeanComp, Comp)
	         attributes(M)$model = "model1"
	         colnames(M)[colnames(M) %in% "MeanComp"] = paste("mu[","MoyenneComposantes",",",unique(y$location),":",unique(y$year),"]",sep="")
	         colnames(M)[colnames(M) %in% "Mel"] = paste("mu[", noms[noms$type %in% "Mélange","germplasm"],",",unique(y$location),":",unique(y$year),"]",sep="")
	         comp.mu = get.mean.comparisons(M, "mu", get.at.least.X.groups = 1)
	         comp.mu=comp.mu$mean.comparisons
	         comp.mu$germplasm = unlist(rm_between(comp.mu$parameter, "[", ",", extract=TRUE))
	         
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
	           bp = lapply(Data_split , barplot.mixture1(z) )
	           
	           return(list("barplot"= bp, "Tab" = Data))
	        }

	       } else {
	         warning("No data for the mixture")}
	        return(list("barplot"= NULL, "Tab" = NULL))
	      
	       if (plot.type == "mix.comp.distribution") {
	       	return(list("barplot" = NULL, "Tab" = Data))
	       }
	       
	     }) # end lapply(y)
	   return(mix_split)
	}) # end lapply(x)
	names(d_env_b) = names(d_env)
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
	
	# graphique mélanges vs composantes
	bp = barplot.mixture1(Data)
	return(list("bp"=b, "Tab" = Data))
}

# 3.  Sur le réseau, comparer la distribution des mélanges à celles de la moins bonne et la meilleure composante pour chaque mélange ----------
# Normalement ça marche, pas testé sur données
if (plot.type == "mix.comp.distribution" | plot.type == "mix.gain.distribution") {
	Distrib = lapply(d_env_b, function(x){
		Worst = Best = Mel = MeanComp = NULL
		
		Mat = lapply(x, function(y) {
			Data = y$tab[y$tab$type %in% "Composante",]
			Worst = Data[which(Data$median == min(Data$median, na.rm = TRUE)),"median"]
			Best = Data[which(Data$median == max(Data$median, na.rm = TRUE)),"median"]
			Mel = y$tab[y$tab$type %in% "Mélange","median"]
			MeanComp = y$tab[y$tab$type %in% "MoyenneComposantes","median"]
			return( cbind(c(Worst,MeanComp,Mel,Best),c("1.moins bonne","2.moyenne composantes","3.mélange","4.meilleure")))
		})
		names(Mat) = names(x)
		toPlot=NULL
		for (i in 1:length(Mat)) {toPlot=rbind(toPlot,cbind(as.matrix(Mat[[i]]),rep(names(Mat)[i], 4)))}
		colnames(toPlot) = c("Moyenne","Type","Paysan")
		
		p = ggplot2.stripchart(data=as.data.frame(toPlot), xName='Type',yName='Moyenne',groupName = "Paysan",xtickLabelRotation=45)
		
		return(list("plot" = p, "tab"= Mat))
	})
	if (plot.type == "mix.comp.distribution") { return(Distrib)}
	
	
	# 4. Sur le réseau, distribution du gain des mélanges par rapport aux composantes ----------
	# Pas travaillé...
	if (plot.type == "mix.gain.distribution") {
	  Histo = lapply(Distrib,function(x){
	    x$diff = x[,"Mélange"]/x[,"MoyenneComposantes"]
	  })
	  p= ggplot2.histogram(data = subsynth, xName='overyielding', groupName='significativity', alpha = 0.5, position="stack", axisLine=c(0, "solid", "black"))
	  return(list("plot" = p, "tab" = Histo))
	}

	
}


# functions -----
barplot.mixture1 = function(x) {
    
	p = ggplot(x, aes(x = reorder(parameter, median), y = median, fill=unlist(x$type))) + geom_bar(stat = "identity")+ theme(legend.title = element_blank())
	
  # ajouter les groupes de significativité
	p = p + geom_text(data = x, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
	p = p + ggtitle(paste(x[1, "environment"], "\n alpha = ", x[1, "alpha"], "; alpha correction :", x[1, "alpha.correction"])) + ylab("")
			
	# pivoter légende axe abscisses
	p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, x[1,"max"])
	

	return(p)
} # end barplot.mix



} # end function