get_interaction_cycle <- function(data,
																	vec_variables,
																	equal.ylim = TRUE, 
																	nb_parameters_per_plot_in.col = NULL
																	)
# Permet de récupérer le graphique de l'évolution des notes au cours du cycle de culture.
# Adapté des fonction get.ggplot et get.ggplot_plot.it du package shinemas2R 
# car pour l'intégrer au package il faudrait modifier toutes les fonctions pour pouvoir prendre un facteur autre que l'année, le germplasme ou la personne
{
 library(reshape2)
	d=data_all$data$data
	d=d[,grep(paste(c(paste("^son$",sep=""),paste("^",vec_variables,"$",sep="")),collapse="|"),colnames(d))]
	d$son = unlist(lapply(as.character(d$son),function(x){strsplit(x,"_")[[1]][1]}))
	d=d[!is.na(d[,2:ncol(d)]),]
	d=d[!is.na(d$son),]
	
  a = grep(paste(paste("^",vec_variables,"$",sep=""),collapse="|"),colnames(d))
  var_not_in_data = setdiff(vec_variables,colnames(d)[a])
  if(length(var_not_in_data)!=2){
    if(length(var_not_in_data)==1){vec_variables = vec_variables[-grep(var_not_in_data,vec_variables)]}
    variable= gsub("^([^---]*)---.*$", "\\1",	vec_variables)
    colnames(d)[a] =variable
    
  	q=melt(d,id.vars="son",measure.vars=variable)
  	q=q[order(q$son),]
  	if(!is.null(nb_parameters_per_plot_in.col)){
  		ns = unique(q$son)
  		s = rep(c(1:length(ns)), each = nb_parameters_per_plot_in.col)[1:length(ns)]
  		names(s) = ns
  		q$split_in.col = s[q$son]
  	} else { q$split_in.col = rep(1, nrow(q)) }
  	q$split =paste("son", q$split_in.col, sep = "-")
  	if(equal.ylim){ # Set ylim for all ggplots
  		y.lim = max(q$value)
  	}
  	q =  plyr:::splitter_d(q, .(split))
  	
  	p = lapply(q,function(data){
  		p = ggplot(data, aes(y = value, x = factor(variable), colour = factor(son), group = factor(son))) 
  		if(equal.ylim){p = p + coord_cartesian(ylim = c(0.5,as.numeric(y.lim)))}
  		p = p + stat_summary(fun.y = mean, geom = "line") + stat_summary(fun.y = mean, geom = "point") 
  		p = p + labs(x="Evolution des notes",y="notes globales")
  		p = p + labs(color="population")
  		return(p)
  	})
  }else{p=NULL}
  return(p)
}

