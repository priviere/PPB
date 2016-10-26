# 0. help -----------------------------------------------------------------
#' Select parts of models results from PPBstats::analyse.outputs (eigher parameters comparisons or MCMC).
#' 
#' @param res_model output from the \code{PPBstats::analyse.outputs} function
#' 
#' @param data output from the \code{shinemas2R::get.data} function
#' 
#' @param type_result the results wanted : parameters comparison ("comp.mu") or MCMC ("MCMC")
#
#' @param variable the variable
#
#' @param param if type_result is "MCMC", the parameter wanted ("mu","beta","sigma" if model 1 is used, "alpha","beta","theta")
#' 
#' @param year the year wanted
#' 
#' @return The data with translated data
#' 
#' @author Gaelle Van Frank
#' 
#' @seealso \code{\link{PPBstats::analyse.outputs}}, \code{\link{shinemas2R::get.data}}
#' 
#' 
get_result_model = function(res_model, data, type_result = "comp.mu", variable, param = NULL, year = NULL) 
{

## marche pour le modèle 1 --> adapter aux sorties du modèle 2 !	
	
# 1. Check parameters -------------
	if ( is.null(year)) {stop("A year is needed.")}
	if ( type_result %in% c("comp.mu","MCMC") == FALSE ) {stop("Type_result must be comp.mu or MCMC." )}
	if ( type_result == "comp.mu" & param %in% c(NULL, "mu") == FALSE ) {stop("If type_result == comp.mu then param must be NULL or mu." )}
	if ( "comp.mu" %in% names(res_model[[variable]]) ) {modele = 1}
	
# 2. Get ID ------------------
	if ( "data" %in% names(data) == TRUE ) {data = data$data}
		
	noms=unique(data$son)
	noms = unique(gsub("^([^_]*_[^_]*_[^_]*)_.*$", "\\1", noms))
	
	germplasm = gsub("^([^_]*)_.*$", "\\1", noms)
	env = gsub("[^._]*_([^_]*)_.*$","\\1", noms)
	block = data$block
	

	if (type_result == "comp.mu") {	comp.mu = res_model[[variable]]$comp.mu ; param = unlist(strsplit(as.character(comp.mu$parameter)[1], "\\["))[1]	}
	
	if (param == "mu") {	ID = as.data.frame(cbind(as.character(noms),paste(param,"[",paste(germplasm,paste(env,year,sep=":"), sep=","),"]",sep=""))) }
	if (param == "beta") {	ID = unique(as.data.frame(cbind(paste("[",paste(paste(env,year,sep=":"), block, sep=","),"]",sep=""),paste(param,"[",paste(paste(env,year,sep=":"), block, sep=","),"]",sep="")))) }
	if (param == "sigma") {	ID = unique(as.data.frame(cbind(paste(param,"[",paste(env,year,sep=":"),"]",sep=""),paste(param,"[",paste(env,year,sep=":"),"]",sep="")))) }
	if (!is.null(ID)) { colnames(ID) = c("ID","parameter")}
	
# 3. Get model results -------------
	if (type_result == "comp.mu") {	
		D=merge(comp.mu,ID, by="parameter")
		D$entry = sub(paste(param, "\\[", sep=""), "", sapply(D$parameter, function(x){unlist(strsplit(as.character(x), ","))[1]}))
		D$environment =  sub("\\]", "", sapply(D$parameter, function(x){unlist(strsplit(as.character(x), ","))[2]}))
		D$location = sapply(D$environment, function(x){unlist(strsplit(as.character(x), ":"))[1]})
		D$year = sapply(D$environment, function(x){unlist(strsplit(as.character(x), ":"))[2]})
	}
	if (type_result == "MCMC") {	
		MCMC = res_model[[variable]]$model.outputs$MCMC
		D=MCMC[,colnames(MCMC) %in% ID$parameter] 
#		colnames(D) = ID[ID$parameter %in% colnames(MCMC),1]
		if (is.null(ncol(D)) & !is.null(D)) { D = as.data.frame(matrix(D,ncol=1))} #; colnames(D) = ID[ID %in% colnames(MCMC)]}
	}


return(D)
}