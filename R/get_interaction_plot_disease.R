get_interaction_plot_disease <- function(tab, nb_parameter_per_plot,vec_variables)
{
  
  add_split_col = function(x, each){ rep(c(1:nrow(x)), each = each)[1:nrow(x)] } 
  
  if(!is.null(tab$duplicated_infos)){ 
    dup = lapply(tab$duplicated_infos,function(x){return(cbind(x[[1]],"block"=1,x[[2]]))})
    Dupl=NULL
    Data_duplicated_groups = NULL
    for (i in 1:length(dup)){
      Data_duplicated_groups = rbind(Data_duplicated_groups,c(paste("Groupe",i,sep=" "),unlist(as.character(dup[[i]][,1]))))
      dup[[i]][,1] = paste("Groupe",i,sep=" ")
      Dupl = rbind(Dupl,dup[[i]])
    }
    colnames(Dupl)[1] = "germplasm"
  }else{Dupl=NULL}
  no_Dupl=NULL
  for (i in 1:length(tab$not_duplicated_infos)){no_Dupl = rbind(no_Dupl,tab$not_duplicated_infos[[i]])}
  
  Tab = rbind(Dupl,no_Dupl)
  
  dates = unique(unlist(sapply(Tab[,3:ncol(Tab)],function(x){unique(x[!is.na(x)])})))
  lev = dates[sort.list(as.POSIXct(strptime(dates,"%d/%m/%y")))]
  
  Tab$split = add_split_col(Tab,nb_parameter_per_plot)
  Tab = plyr:::splitter_d(Tab, .(split))
    
  p <- lapply(Tab,function(x){
    x = melt(x,id.vars="germplasm",measure.vars = vec_variables[-grep(paste(setdiff(vec_variables,colnames(x)[3:ncol(x)]),collapse="|"),vec_variables)])
    x=x[!is.na(x$value),]
    x$variable = unlist(lapply(as.character(x$variable),function(x){strsplit(x,"_")[[1]][1]}))
    x$variable=unlist(lapply(x$variable,function(y){
      if(y=="sain"){return(1)}
      if(y=="malade"){return(2)}
      if(y=="tres"){return(3)}
    }))
    x$value = factor(x$value,levels=lev)
    p = ggplot(data=x,aes(x=value,y=variable, colour = germplasm, group=germplasm))# + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") 
    p = p + labs(x="date",y="note maladie")
    p = p + scale_y_continuous(breaks = c(1,2,3))
    p = p + geom_jitter(inherit.aes=TRUE,width=0.1,height=0)
    return(list("plot"=p,"Data"=x))
  })
  return(list("plot" = p, "Data_groups"=Data_duplicated_groups))
  
}