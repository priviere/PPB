get_heatmap <- function(data,
												vec_variables,
												nb_parameters_per_plot = NULL
                        )

# Permet de récupérer une heatmap pour les données non moulinées par le modèle
{
  d=data$data$data
  a = grep(paste(paste("^",vec_variables,"$",sep=""),collapse="|"),colnames(d))
  HM = cbind(d[,c("son","son_year")],d[,a])
  colnames(HM)[3] = "var"
  HM=HM[!(HM$var == "NA"),]
  HM$var = factor(HM$var)
  
  if(vec_variables == "verse---verse"){HM$var = factor(HM$var,levels(HM$var)[c(3,5,4,2,1)])}
  
  HM$son = unlist(lapply(as.character(HM$son),function(x){strsplit(x,"_")[[1]][1]}))
  split = rep(seq(1,100,1),nb_parameters_per_plot)
  split = split[order(split)]
  split=split[1:length(unique(HM$son))]
  names(split)=unique(HM$son)
  HM$split = split[HM$son]
  to_plot =  plyr:::splitter_d(HM, .(split))
  
  p = lapply(to_plot,function(data){
    p = ggplot(data=data,aes(x=son_year,y=son,fill=var)) + geom_tile() + scale_fill_manual(drop=FALSE,values =c("chartreuse4","seagreen2","palegreen1","coral2","red3"))
    p = p + labs(x = "année", y="populations", fill=paste("note",vec_variables,sep=" "))
  })


  return(p)
}

