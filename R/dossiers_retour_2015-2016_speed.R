# 0. help -----------------------------------------------------------------
#' Script to analyse data from SHiNeMaS using shinemas2R and PPBstats
#' 
#' @author Pierre RIvière, Gaëlle Van Frank
#' 
#' @seealso \code{\link{shinemas2R::get.data}}, \code{\link{PPBstats::MC}}, \code{\link{PPBstats::FWH}}
#' 

setwd("/home/deap/gvanfrank/these/R/dossiers_retour/dossiers_retour_2015-2016")
source("info.connexion.R")
vec_fonctions = dir("/home/deap/gvanfrank/these/R/dossiers_retour/dossiers_retour_2015-2016/fonctions")
for (fon in vec_fonctions) { source(paste("./fonctions/", fon, sep = "")) }

# PPBstats 
vec_fonctions = dir("/home/deap/gvanfrank/these/R/packages_Pierre/PPBstats/R")
for (fon in vec_fonctions) { source(paste("/home/deap/gvanfrank/these/R/packages_Pierre/PPBstats/R/", fon, sep = "")) }

# shinemas2R
vec_fonctions = dir("/home/deap/gvanfrank/these/R/packages_Pierre/shinemas2R/R")
for (fon in vec_fonctions) { source(paste("/home/deap/gvanfrank/these/R/packages_Pierre/shinemas2R/R/", fon, sep = "")) }

# shinemas2R
library("RPostgreSQL")
library("network")
library("maps")
library("xtable")
library("plyr")
library("dplyr")
library("grid")
library("ggplot2")
library("RColorBrewer")
#library("sna")
library("scales")
#library("tidyr")
library("DBI")
library("RPostgreSQL")

# PPBstats
library("rjags")
library("FactoMineR")
library("ggplot2")
library("gridExtra")
library("parallel")


year = "2016"


mag = function(d){
  # tkw
  if( length(grep("tkw---tkw", colnames(d$data))) > 0 ) {
    a = as.numeric(d$data$"tkw---tkw")
    a[which(a>70)] = NA
    d$data$"tkw---tkw" = a
  }
  
  # protein
  if( length(grep("protein---protein", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"protein---protein"))
    a[which(a>20)] = NA
    d$data$"protein---protein" = a
  }

  # spike_weight
  if( length(grep("spike_weight---spike_weight", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"spike_weight---spike_weight"))
    a[which(a>10)] = NA
    d$data$"spike_weight---spike_weight" = a
  }

  # plant_height
  if( length(grep("plant_height---plant_height", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"plant_height---plant_height"))
    a[which(a==0)] = NA # mesure en m
    a[which(a<2)] = a[which(a<2)] * 1000 # mesure en m
    a[which(a>2 & a<500)] = NA # valeurs bizarres
    d$data$"plant_height---plant_height" = a
  }
  
  # LLSD
  if( length(grep("LLSD---LLSD", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"LLSD---LLSD"))
    a[which(a < -10)] = NA
    a[which(a > 500)] = NA
    d$data$"LLSD---LLSD" = a
  }
  
  
    # nbr_kernels_ind_corrected---nbr.epillets_ind_corrected
  if( length(grep("nbr_kernels_ind_corrected---nbr_kernels_ind_corrected", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"nbr_kernels_ind_corrected---nbr_kernels_ind_corrected"))
    a[which(a <= 0)] = NA
    a[which(a > 70)] = NA
    d$data$"nbr_kernels_ind_corrected---nbr_kernels_ind_corrected" = a
  }
 
      #  spike_length---spike_length_F
  if( length(grep("spike_length---spike_length_F", colnames(d$data))) > 0 ) {
    a = as.numeric(as.character(d$data$"spike_length---spike_length_F"))
    a[which(a <= 0)] = NA
    a[which(a > 250)] = NA
    d$data$"spike_length---spike_length_F" = a
  }
  
  return(d)
}


# 1. Statistical analysis on all data ----- 

vec_variables="tkw"
#vec_variables = c("tkw", "protein", "spike_weight", "plant_height","spike_length",
#"nbr_kernels_ind_corrected","LLSD")

# 1.1. Get the data and format it for PPBstats -----
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")
print("1.1. get data")
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")

data = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
query.type = "data-classic", filter.on = "father-son", data.type ="relation" ,variable.in=vec_variables
)

data$data = mag(data$data)
data = translate.data(data, list_translation)
data_stats = format.data(data, format = "PPBstats", fuse_g_and_s = TRUE)
data_stats[,vec_variables] = gsub(",",".",data_stats[,vec_variables])

#vec_variables = c("poids.de.mille.grains---poids.de.mille.grain", 
#				"proteine---proteine", 
#				"poids.de.l.epi---poids.de.l.epi", 
#				"hauteur---hauteur")
				
#vec_variables_trad = c("poids.de.mille.grains", "taux.de.proteine", "poids.de.l.epi", "hauteur")

vec_variables = "poids.de.mille.grains---poids.de.mille.grains"
vec_variables_trad = "poids.de.mille.grains"

# 1.2. model1 ----------
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")
print("1.2. model 1")
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")

fun_model1 = function(variable, data_stats) {
	out.model1 = MC(data = data_stats, variable = variable, return.epsilon = TRUE, nb_iterations = 15000) # , nb_iterations = 1000)
	model.outputs = analyse.outputs(out.model1) # si var: ça bug, que ok si var = NULL
	comp.mu = get.mean.comparisons(model.outputs$MCMC, "mu", get.at.least.X.groups = 2)
	return(list("model.outputs" = model.outputs, "comp.mu" = comp.mu))
}

res_model1 = mclapply(vec_variables, fun_model1, data_stats, mc.cores = length(vec_variables))
names(res_model1) = vec_variables_trad

# 1.3. model 2 ----------
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")
print("1.3. model 2")
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")

fun_model2 = function(variable, data_stats){
	out.model2 = FWH(data = data_stats, variable = variable, return.epsilon = TRUE, nb_iterations = 20000) # , nb_iterations = 1000)
	model.outputs = analyse.outputs(out.model2)

	para_ok = colnames(model.outputs$MCMC)
	test_a = length(grep("alpha\\[", para_ok )) > 0
	test_b = length(grep("beta\\[", para_ok )) > 0
	test_t = length(grep("theta\\[", para_ok )) > 0
	
	if( test_a ) { comp.alpha = get.mean.comparisons(model.outputs$MCMC, "alpha", get.at.least.X.groups = 2) } else { comp.alpha = NULL }
	if( test_b ) { comp.beta = get.mean.comparisons(model.outputs$MCMC, "beta", type = 2, threshold = 1) } else { comp.beta = NULL }
	if( test_t ) { comp.theta = get.mean.comparisons(model.outputs$MCMC, "theta", get.at.least.X.groups = 2) } else { comp.theta = NULL }
	
	comp.par = list("comp.alpha" = comp.alpha, "comp.beta" = comp.beta, "comp.theta" = comp.theta)

	if( test_a & test_b & test_t) { pp = predict.the.past(model.outputs, output.format = "summary") } else { pp = NULL }
	predict.past = pp
	return(list("model.outputs" = model.outputs, "comp.par" = comp.par, "predict.past" = predict.past))
}

res_model2 = mclapply(vec_variables, fun_model2, data_stats, mc.cores = length(vec_variables))
names(res_model2) = vec_variables_trad


# 2. Network data ----------
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")
print("2. Network data")
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")

data_network_year = get.data(db_user = db_user, db_host = db_host, db_name = db_name, 
db_password = db_password, query.type = "network", filter.on = "son", 
year.in = year)

vec_person = sort(as.character(unique(data_network_year$data$network.info$person)))

# 3. farmers'data ---------
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")
print("3. farmers'data")
print("-------------------------------------")
print("-------------------------------------")
print("-------------------------------------")

get_data_farmers = function(person){
print(person)
  # Toutes les données
  data_all = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
  query.type = "data-classic", person.in = person, filter.on = "father-son", data.type ="relation")
  
  data_all = mag(data_all)
   if ( class(data_all$data) == "list" ) {
	data_all = translate.data(data_all, list_translation)
	attributes(data_all)$shinemas2R.object = "data-classic"
  }else{
	data_all = NULL
  }
  
  # Données 2016 --------
  data_year = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
			query.type = "data-classic", person.in = person, year.in = year, filter.on = "son", data.type ="relation")
			
  data_year = mag(data_year)
  if ( class(data_year$data) == "list" ) {
	data_year = translate.data(data_year, list_translation)
	attributes(data_year)$shinemas2R.object = "data-classic"
  }else{
	data_year = NULL
  }
  
  # Différentiel de sélection
  data_S_year = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
  query.type = "data-S", person.in = person, year.in = year, filter.on = "father-son", data.type ="relation")
  
  data_S_year = mag(data_S_year)
  
  if (!is.null(data_S_year$data) & !is.null(attributes(data_S_year$data)$shinemas2R.object)) {
	data_S_year = translate.data(data_S_year, list_translation)
	attributes(data_S_year)$shinemas2R.object = "data-S"
  }
  
  # Réponse à la sélection
  data_SR_year = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
  query.type = "data-SR", person.in = person, year.in = year, filter.on = "father-son", data.type ="relation")
  
  data_SR_year = mag(data_SR_year)
  
  if (!is.null(data_SR_year$data) & !is.null(attributes(data_SR_year$data)$shinemas2R.object)) {
	data_SR_year = translate.data(data_SR_year, list_translation)
	attributes(data_SR_year)$shinemas2R.object = "data-SR"
  }
  
   #Données essai mélange
  mixtures = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password,
                                  query.type = "data-mixture-1", # query for mixtures
                                  person.in = person, # person to keep
                                  filter.on = "father-son", # filters on father AND son
                                  data.type = "relation",  # data linked to relation between seed-lots
                                  project.in="PPB-Mélange"
                                  )
                                  
  if (!is.null(mixtures$data)) {                                
	  mixtures$data$germplasm_son = gsub("^([^_]*)_.*$", "\\1", mixtures$data$son) 
	  mixtures$data$germplasm_father = gsub("^([^_]*)_.*$", "\\1", mixtures$data$father)
	  mixtures$data$year = gsub("^.*_([^_]*)_.*$","\\1",mixtures$data$son)
	  mixtures$data$location = gsub("[^._]*_([^_]*)_.*$","\\1",mixtures$data$son)
  }
  

  out = list("data_all" = data_all, "data_year" = data_year, "data_S_year" = data_S_year, "data_SR_year" = data_SR_year, "data_PPB_mixture" = mixtures)
  return(out)
}


out_farmers_data = mclapply(vec_person, get_data_farmers, mc.cores = 16)
names(out_farmers_data) = vec_person

out_from_speed = list("year" = year, "vec_person" = vec_person, "res_model1" = res_model1, "res_model2" = res_model2, "data_network_year" = data_network_year, "out_farmers_data" = out_farmers_data, "list_translation" = list_translation)

save(out_from_speed, file = "out_from_speed.RData")




