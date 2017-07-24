# 0. help -----------------------------------------------------------------
#' Function to analyse data from SHiNeMaS using R packages \code{shinemas2R} and \code{PPBstats} regarding template_feedback_folder_1
#' 
#' @param info_db used by shinemas2R::get.data() : a list with the following element to connect to the data base:
#' \itemize{
#' \item db_user	 user name of SHiNeMaS
#' \item db_host	 IP address of the computer where SHiNeMaS is. If local host db_host = "127.0.0.1"
#' \item db_name	 name of the data base
#' \item db_password	your password to login. If no password is needed, put ""
#' }
#' 
#' @param year the year of the feedback folder
#' 
#' @param vec_variables the variables to analyse
#' 
#' @param mc.cores	The number of cores used for parallelisation of the computing
#' 
#' @author Pierre Rivière, Gaëlle Van Frank
#' 
analyse_feedback_folder_1 = function(
  info_db,
  year = "2016",
  vec_variables,
  mc.cores
)
  # go ----------
{
  db_user = info_db$db_user
  db_host = info_db$db_host
  db_name = info_db$db_name
  db_password = info_db$db_password
  
  # list_translation --------
  list_translation = list(
    c("post_winter_global", "note.globale.hiver"),
    c("spring_global", "note.globale.printemps"),
    c("summer_global", "note.globale.ete"),
    c("topography", "topographie"),
    c("sowing_practices", "pratiques.semis"),
    c("battance", "battance"),
    c("micro_field_area", "surface.micro.parcelle"),
    c("sowing_qualitative_date", "date.semis"),
    c("field_area", "surface.champs"),
    c("soil_type_2", "type.de.sol.info2"),
    c("soil_type_1", "type.de.sol.info1"),
    c("field_info", "info.sur.le.champs"),
    c("space_between_micro_field", "espace.entre.micro.parcelles"),
    c("sowing_density", "densite.semis"),
    c("field_name", "nom.champ"),
    c("ploughing_before_sowing", "labour.avant.semis"),
    c("drainage", "drainage"),
    c("previous_culture", "précédent.cultural"),
    c("sowing_special_remarks", "remarques.automne"),
    
    c("sowing_notice_topography_2","notice.topographie.semis.2"),
    c("sowing_notice_sowing_practices","notice.pratiques.semis"),
    c("sowing_notice_battance","notice.battance"),
    c("sowing_notice_micro_field_area","notice.surface.micro.parcelle"),
    c("qualitative_date","date"),
    c("sowing_notice_field_area","notice.surface.champ"),
    c("sowing_notice_soil_type_2","notice.type.sol.2"),
    c("sowing_notice_soil_type_1","notice.type.sol.1"),
    c("sowing_notice_field_info","notice.info.champ"),
    c("sowing_notice_density","notice.densite.semis"),
    c("sowing_notice_field_name","notice nom.champ"),
    c("sowing_notice_ploughing_before_sowing","notice.labour.avant.semis"),
    c("sowing_notice_drainage","notice.drainage"),
    c("sowing_notice_previous_culture","notice.précédent.cultural"),
    
    c("autumn_rainfall", "pluies.automne"),
    c("autumn_temperature", "températures.automne"),
    c("autumn_climatic_field_notes", "notes.sur.le.climat.automne"),
    c("autumn_climatic_accident", "accidents.climatiques.automne"),
    
    c("post_winter_observation_date", "date.observation.hiver"),
    c("post_winter_reprise", "reprise"),
    c("post_winter_leaves_attitude", "attitude des feuilles"),
    c("post_winter_port_au_tallage", "port.au.tallage"),
    c("post_winter_density", "densité"),
    c("post_winter_notes", "commentaires.hiver"),
    
    c("post_winter_rainfall", "pluies.hiver"),
    c("post_winter_temperature", "températures.hiver"),
    c("post_winter_climatic_accident", "accidents.climatiques.hiver"),
    c("post_winter_climatic_notes", "notes.sur.le.climat.hiver"),
    c("post_winter_field_accident", "accidents.dans.le.champ.hiver"),
    c("post_winter_field_notes", "notes.sur.le.champ.hiver"),
    
    c("spring_observation_date", "date.observation.printemps"),
    c("spring_tallage", "tallage"),
    c("spring_vigueur", "vigueur"),
    c("spring_color", "couleur.printemps"),
    c("spring_self_propagating", "adventices.printemps"),
    c("heading_date_100", "date.épiaison_60-100"),
    c("spring_notes_diseases", "notes.maladies"), 
    c("heading", "remarques.printemps"),
    c("spring_heading_note", "note.épiaison"),
    c("spring_notes", "commentaires.printemps"),
    
    c("tallage","tallage"),
    c("self_propagating","adventices"),
    c("heading_date_global_a","date.épiaison.globale.a"),
    c("heading_note","note.épiaison"),
    
    c("spring_rainfall", "pluies.printemps"),
    c("spring_temperature", "températures.printemps"),
    c("spring_climatic_accident", "accidents.climatiques.printemps"),
    c("spring_climatic_notes", "notes.sur.le.climat.printemps"),
    c("spring_field_accident", "accidents.dans.le.champ.printemps"),
    c("spring_field_notes", "notes.sur.le.champ.printemps"),
    
    c("summer_observation_date", "date.observation.été"),
    c("summer_biomass", "biomasse"),
    c("heterogeneite", "hétérogénéité"),
    c("harvest_date", "date.récolte"),
    c("poids_battage", "poids.battage"),
    c("rdt_micro_parcelle", "rendement.micro.parcelle"),
    c("summer_special_remarks", "commentaires.été"),
    c("special_remarks_farmer","commentaires"),
    c("observation_date","date.observation"),
    
    c("summer_rainfall", "pluies.été"),
    c("summer_temperature", "températures.été"),
    c("summer_climatic_accident", "accidents.climatiques.été"),
    c("summer_climatic_notes", "notes.sur.le.climat.été"),
    c("summer_field_accident", "accidents.dans.le.champ.été"),
    c("summer_field_notes", "notes.sur.le.champ.été"),
    
    c("temperature", "températures"),
    c("rainfall"  , "pluies"),
    c("climate_accident" , "accidents.climatiques"),
    c("climate_notes","notes.climatiques"),
    c("field_accident","accidents.dans.le.champ"),
    
    c("tkw", "poids.de.mille.grains"),
    c("protein", "taux.de.proteine"),
    c("summer_verse", "verse"),
    c("plant_height", "hauteur"),
    c("spike_weight", "poids.de.l.epi"),
    c("spike_length","longueur.de.l.epi"),
    
    c("awns","barbe"),
    c("curve","courbure"),
    c("color","couleur"),
    c("crosses","croisements"),
    c("disease","maladie"),
    c("biomass","biomasse"),
    c("nbr_kernels","nbr.epillets"),
    c("nbr_missing_spikelet","nbr.epillets.manquants"),
    c("nbr_spike","nbr.épis"),
    c("nbr_sterile_spikelets","nbr.epillets.stériles"),
    c("measured_grain_weight","poids.grains.mesure"),
    c("estimated_nbr_grain_spike","nbr.estime.grain.par.epi"),
    c("LLSD","LLSD")
  )
  
  
  
  # 0. Clean the data set ----------
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
      a[which(a < 0)] = 0
      a[which(a > 500)] = NA
      d$data$"LLSD---LLSD" = a
    }
    
    
    # nbr_kernels---nbr.epillets
    if( length(grep("nbr_kernels---nbr_kernels", colnames(d$data))) > 0 ) {
      a = as.numeric(as.character(d$data$"nbr_kernels---nbr_kernels"))
      a[which(a <= 0)] = NA
      a[which(a > 60)] = NA
      d$data$"nbr_kernels---nbr_kernels" = a
    }
    
    #  spike_length---spike_length
    if( length(grep("spike_length---spike_length", colnames(d$data))) > 0 ) {
      a = as.numeric(as.character(d$data$"spike_length---spike_length"))
      a[which(a <= 0)] = NA
      a[which(a > 250)] = NA
      d$data$"spike_length---spike_length" = a
    }
    
    #  rdt
    if( length(grep("rdt", colnames(d$data))) > 0 ) {
      a = as.numeric(as.character(d$data$"rdt"))
      a[which(a <= 10)] = NA
      a[which(a > 100)] = NA
      d$data$"rdt" = a
    }
    
    # estimated_nbr_grain_spike
    if( length(grep("estimated_nbr_grain_spike---estimated_nbr_grain_spike", colnames(d$data))) > 0 ) {
      a = as.numeric(as.character(d$data$"estimated_nbr_grain_spike---estimated_nbr_grain_spike"))
      a[which(a <= 0)] = NA
      a[which(a > 100)] = NA
      d$data$"estimated_nbr_grain_spike---estimated_nbr_grain_spike" = a
    }
    
    return(d)
  }
  
  
  # 1. Statistical analysis on all data ---------- 
  
  # 1.1. Get the data and format it for PPBstats ----------
  message("
          -------------------------------------
          -------------------------------------
          1.1. get data
          -------------------------------------
          -------------------------------------")
  
  data = get.data(db_user = db_user, db_host = db_host, db_name = db_name, db_password = db_password, 
                  query.type = "data-classic", filter.on = "father-son", data.type ="relation" ,variable.in=vec_variables
  )
  
  #1.1.1. Transform some data
  if("spike_length"%in% vec_variables){
    # If there are measures on the field (F) but not on technical room (M) or the contrary, merge the 2 so that we don't loose information
    D=data$data$data
    D$'spike_length---spike_length' = D$"spike_length---spike_length_M"
    for (i in 1:nrow(D)){
      if(is.na(D[i,'spike_length---spike_length']) & !is.na(D[i,"spike_length---spike_length_F"])){
        D[i,'spike_length---spike_length'] = D[i,"spike_length---spike_length_F"]}
    }
    data$data$data=D
  }
  
  if("measured_grain_weight" %in% vec_variables){
    # If the number of kernels was not measured but we want to estimated it since the thousand kernel weight, total grain weight and number of spikes were measured
    D=data$data$data
    
    # Get the number of spikes 
    nBS=NULL
    for (i in 1:nrow(D)){
      nBS= c(nBS,ifelse(!is.na(D[i,"nbr_spike---nbr_spikes"]),D[i,"nbr_spike---nbr_spikes"],D[i,"nbr_spikes---nbr_spikes"]))
    }
    D$"nBS---nBS" = nBS
    # If it was not measured, its equal to the number of spikes on which the measures of spike weight were done
    for (i in 1:nrow(D)){ 
      if( !is.na(D[i,"measured_grain_weight---measured_grain_weight"]) & is.na(D[i,"nBS---nBS"]) ){
        nbr_ind = length(D[D$son %in% D[i,"son"] & D$block %in% D[i,"block"] & D$X %in% D[i,"X"] & D$Y %in% D[i,"Y"],"son"])
        D[i,"nBS---nBS"] = nbr_ind
      }
    }
    # Estimate the mean number of grain per spike
    D$'estimated_nbr_grain_spike---estimated_nbr_grain_spike' = if(!is.na(D[,"measured_grain_weight---measured_grain_weight"]) & !is.na(D[,"nBS---nBS"])){
      as.numeric(as.character(D[,"measured_grain_weight---measured_grain_weight"]))*1000/(as.numeric(as.character(D[,"nBS---nBS"]))*as.numeric(as.character(D[,"tkw---tkw"])))
    }
    data$data$data = D
  }
  
  
  
  #1.1.3. Correct some errors in data base
  if("tkw" %in% vec_variables){data$data$data[data$data$data$son %in% "Louesme-Blanc#VA_JUBA_2016_0001","tkw---tkw"][1] = 34.203}
  if("measured_grain_weight" %in% vec_variables){data$data$data[data$data$data$son %in% "Blanc-des-Flandres_MAV_2016_0001",
                                                                "measured_grain_weight---measured_grain_weight"][1] = 21.4}
  
  data$data = mag(data$data)
  data = translate.data(data, list_translation)
  data_stats = format.data(data, format = "PPBstats", fuse_g_and_s = TRUE)
  
  data_stats[data_stats$location %in% "JSG" & data_stats$year %in% "2016","block"] = 1
  data_stats[data_stats$location %in% "DAV" & data_stats$year %in% "2016"& data_stats$germplasm %in% "Peter-Jacoby", "block"] = 1
  
  
  vec_variables_trad = unlist(lapply(vec_variables, function(x){
    i = grep(x,unlist(lapply(list_translation, function(y) {return(y[1])})))
    return(list_translation[[i]][2])
  }))
  
  vec_variables = unlist(lapply(vec_variables_trad, function(x){
    return(paste(x,"---",x,sep=""))
  })
  )
  
  #data_stats[,vec_variables] = gsub(",",".",data_stats[,vec_variables])
  
  #1.1.2. Get mixtures data
  Mixtures_all = get.data(db_user = info_db$db_user, db_host = info_db$db_host, # db infos
                          db_name = info_db$db_name, db_password = info_db$db_password, # db infos
                          query.type = "data-mixture-1", # query for mixtures
                          filter.on = "father-son", # filters on father AND son
                          data.type = "relation", # data linked to relation between seed-lots
                          project.in="PPB-Mélange"
  )
  Mixtures_all$data$germplasm_son = gsub("^([^_]*)_.*$", "\\1", Mixtures_all$data$son) 
  Mixtures_all$data$germplasm_father = gsub("^([^_]*)_.*$", "\\1", Mixtures_all$data$father)
  Mixtures_all$data$year = gsub("^.*_([^_]*)_.*$","\\1", Mixtures_all$data$son)
  Mixtures_all$data$location = gsub("[^._]*_([^_]*)_.*$","\\1", Mixtures_all$data$son)
  
  
  # 1.2. model1 ----------
  message("
          -------------------------------------
          -------------------------------------
          1.2. model 1
          -------------------------------------
          -------------------------------------")
  
  fun_model1 = function(variable, data_stats) {
    out.model1 = model_1(data = data_stats, variable = variable, return.epsilon = TRUE, nb_iterations = 30000) # , nb_iterations = 1000)
    model.outputs = check_model_model_1(out.model1) 
    comp.mu = mean_comparisons_model_1(model.outputs, "mu", get.at.least.X.groups = 2)
    return(list("model.outputs" = model.outputs, "comp.par" = list("comp.mu" =comp.mu)))
  }
  
  res_model1 = mclapply(vec_variables, fun_model1, data_stats, mc.cores = length(vec_variables))
  names(res_model1) = vec_variables_trad
  
  # 1.3. model 2 ----------
  message("
          -------------------------------------
          -------------------------------------
          1.3. model 2
          -------------------------------------
          -------------------------------------")
  
  fun_model2 = function(variable, data_stats){
    out.model2 = model_2(data = data_stats, variable = variable, return.epsilon = TRUE, nb_iterations = 20000) # , nb_iterations = 1000)
    model.outputs = check_model_model_2(out.model2)
    
    para_ok = colnames(model.outputs$MCMC)
    test_a = length(grep("alpha\\[", para_ok )) > 0
    test_b = length(grep("beta\\[", para_ok )) > 0
    test_t = length(grep("theta\\[", para_ok )) > 0
    
    if( test_a ) { comp.alpha = mean_comparisons_model_2(model.outputs, "alpha", get.at.least.X.groups = 2) } else { comp.alpha = NULL }
    if( test_b ) { comp.beta = mean_comparisons_model_2(model.outputs, "beta", type = 2, threshold = 1) } else { comp.beta = NULL }
    if( test_t ) { comp.theta = mean_comparisons_model_2(model.outputs, "theta", get.at.least.X.groups = 2) } else { comp.theta = NULL }
    
    comp.par = list("comp.alpha" = comp.alpha, "comp.beta" = comp.beta, "comp.theta" = comp.theta)
    envs = colnames(out.model2$model2.presence.abscence.matrix)
    envs=envs[grep(year,envs)]
    if( test_a & test_b & test_t & length(envs)>0) { 
      pp = lapply(envs,function(x){predict_the_past_model_2(model.outputs,x)}) 
      names(pp)=envs
    } else { pp = NULL }
    predict.past = pp
    return(list("model.outputs" = model.outputs, "comp.par" = comp.par, "predict.past" = predict.past))
  }
  
  res_model2 = mclapply(vec_variables, fun_model2, data_stats, mc.cores = length(vec_variables))
  names(res_model2) = vec_variables_trad
  
  #1.4. model variance intra ---------
  message("
          -------------------------------------
          ------------------------------------
          1.4. model variance intra-population
          -------------------------------------
          -------------------------------------")
  
  fun_model3 = function(variable, data_stats){
    out.model_varintra = model_variance_intra(data = data_stats, variable = variable, return.sigma = TRUE, return.mu = FALSE, nb_iterations = 20000) 
    model.outputs = check_model_model_variance_intra(out.model_varintra)
    comp.sigma = mean_comparisons_model_varintra(model.outputs$MCMC, "sigma", get.at.least.X.groups = 2)
    return(list("model.outputs" = model.outputs, "comp.par" = list("comp.sigma" = comp.sigma)))
  }
  
  res_model_varintra = mclapply(vec_variables, fun_model3, data_stats, mc.cores = length(vec_variables))
  names(res_model_varintra) = vec_variables_trad
  
  # 2. Network data ----------
  message("
          -------------------------------------
          -------------------------------------
          2. Network data
          -------------------------------------
          -------------------------------------")
  
  data_network_year = get.data(db_user = db_user, db_host = db_host, db_name = db_name, 
                               db_password = db_password, query.type = "network", filter.on = "son", 
                               year.in = year)
  
  vec_person = sort(as.character(unique(data_network_year$data$network.info$person)))
  
  # 3. farmers'data ---------
  message("
          -------------------------------------
          -------------------------------------
          3. farmers'data
          -------------------------------------
          -------------------------------------")
  
  get_data_farmers = function(person){
    message(person)
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
  
  
  out_farmers_data = mclapply(vec_person, get_data_farmers, mc.cores = 10)
  names(out_farmers_data) = vec_person
  
  out_from_speed = list("year" = year, "vec_person" = vec_person, "res_model1" = res_model1, "res_model2" = res_model2, #"res_model_varintra" = res_model_varintra, 
                        "data_network_year" = data_network_year, "out_farmers_data" = out_farmers_data, "list_translation" = list_translation, "Mixtures_all" = Mixtures_all)
  
  return(out_from_speed)
}

