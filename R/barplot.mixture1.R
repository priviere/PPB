# 0. help -----------------------------------------------------------------
#' Generates barplots for mixtures
#' 
#' @param x data frame containing the name of the populations, the median of the character, the type ("Mixture","Component",...)
#
#' @return The boxplot
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{PPBformations::ggplot_mixture1}}
#' 

barplot.mixture1 = function(x) {
  
  p = ggplot(x, aes(x = reorder(germplasm, median), y = median, fill=unlist(x$type))) + geom_bar(stat = "identity")+ theme(legend.title = element_blank())
  
  # ajouter les groupes de significativité
  p = p + geom_text(data = x, aes(x = reorder(germplasm, median), y = median/2, label = groups), angle = 90, color = "white")
  p = p + ggtitle(paste(x[1, "environment"], "\n alpha = ", x[1, "alpha"], "; alpha correction :", x[1, "alpha.correction"])) + ylab("")
  
  # pivoter légende axe abscisses
  p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, x[1,"max"])
  
  
  return(p)
}