# 0. help -----------------------------------------------------------------
#' Function to get the "feedback file" containing the results of the year's trial for each farmer based on the analysis coming from \code{analyse_feedback_folder_1}
#' 
#' @param dir Directory where the element of each folder is created
#' 
#' @param person The farmer's name
#' 
#' @param out_analyse_feedback_folder_1 The outputs from \code{analyse_feedback_folder_1}
#
#' @details 
#' The function creates two folders :
#' \itemize{
#'  \item "tex.files" with the tex files used to create the pdf
#'  \item "feedback_folder" with, for each person, a folder with information coming from shinemas2R::get.pdf() see ?get.pdf for more details.
#' }
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 
#' 
feedback_folder_1 = function(
	person,
	out_analyse_feedback_folder_1)
	# go ----------
{
  # Set the right folder and create folders tex.files and feedback_folder ----------
  a = dir(dir)
  if( !file.exists(dir) ){ stop("directory ", dir, " does not exist.") }
  
  we_are_here = getwd()
  setwd(dir)
  if( !is.element("tex.files", dir()) ) { system("mkdir tex.files") ; message("The folder tex.files has been created") }
  if( !is.element("feedback_folder", dir()) ) { system("mkdir feedback_folder") ; message("The folder feedback_folder has been created") }

	year = out_analyse_feedback_folder_1$year
  res_model1 = out_analyse_feedback_folder_1$res_model1
  res_model2 = out_analyse_feedback_folder_1$res_model2
  data_network_year = out_analyse_feedback_folder_1$data_network_year
  data_all =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_all
  data_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_year
  data_S_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_S_year
  data_SR_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_SR_year
  data_PPB_mixture = out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_PPB_mixture
  Mixtures_all = out_analyse_feedback_folder_1$Mixtures_all
  
	# Créer title page
	a = paste(
		"	\\begin{titlepage}
		\\pagecolor{color1}
		\\noindent
		\\begin{center}
		\\begin{tabular}{cccc}
		\\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{/home/deap/Documents/Gaelle/scriptsR/dossiers_retour/test_dossier_retour/2.tex_files/sp4} &
		\\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{/home/deap/Documents/Gaelle/scriptsR/dossiers_retour/test_dossier_retour/2.tex_files/sp6} &
		\\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{/home/deap/Documents/Gaelle/scriptsR/dossiers_retour/test_dossier_retour/2.tex_files/sp7} &
		\\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{/home/deap/Documents/Gaelle/scriptsR/dossiers_retour/test_dossier_retour/2.tex_files/sp2}
		\\end{tabular}
		\\end{center}
		%\\[-1em]
		\\color{color2}
		\\makebox[0pt][l]{\\rule{1.3\\textwidth}{1pt}}
		\\par
		\\noindent
		\\textbf{\\textsf{Réseau Semences Paysannes}} \\\\ 
		\\textbf{\\textsf{Equipe DEAP, INRA Le Moulon}} \\\\
		\\textbf{\\textsf{CETAB}} \\\\
		\\textbf{\\textsf{Triptolème}} \\\\
		\\textbf{\\textsf{Pétanielle}} \\\\
		\\textbf{\\textsf{Touzelle}} \\\\
		\\textbf{\\textsf{ARDEAR Rhones-Alpes}} \\\\
		\\textbf{\\textsf{ARDEAR Centre}} \\\\
		\\textbf{\\textsf{Bergerie de Villarceaux}} \\\\
		\\textbf{\\textsf{Graines de Noé}} \\\\
		\\textbf{\\textsf{Kerna ùn Sohma}} \\\\
		\\textbf{\\textsf{Li mestère}} \\\\


		\\vfill
		\\noindent
		{\\Huge \\textbf{\\textsf{Programme de sélection décentralisée \\\\ et collaborative sur le blé tendre}}} \\\\
		~\\\\
		\\noindent{\\Large \\textbf{\\textsf{Résultats de la saison 2015-2016}}}
		\\vfill
		
		\\begin{flushright}
		\\noindent
		\\textbf{\\textsf{", person,"}}
		\\end{flushright}
		
		\\end{titlepage}
		
		\\nopagecolor % Use this to restore the color pages to white
    \\pagestyle{plain}
		"
	)
	
	
	p = paste(we_are_here, "/tex_files/titlepage_", person,".tex", sep = "")
	sink(p);	cat(a);	sink()
		
	OUT = list()
	
	# 0. Page de garde, contacts, table des matières ----------
	
	out = list("input" = paste("../tex_files/titlepage_", person, ".tex", sep = "")); OUT = c(OUT, out)
	
	
	# Contacts
	out = list("input" = "../tex_files/contacts.tex"); OUT = c(OUT, out)
	
	# Table of contents
	out = list("tableofcontents" = TRUE); OUT = c(OUT, out)
	
	
	# 1. Intro ----------
	
	out = list("input" = "../tex_files/intro.tex"); OUT = c(OUT, out)
	
	out = list("input" = "../tex_files/fiche_paysans_SP_cereales_v5.tex"); OUT = c(OUT, out)
	
	# 2. Partie sur la ferme ----------
	
	# les graph pour les fiches
	graph.fiche = function(OUT, data, variable) {
		
		if(variable == "note.globale.hiver---global"){ in.cap = "d'hiver" }
		if(variable == "note.globale.printemps---global"){ in.cap = "de printemps" }
		if(variable == "note.globale.ete---global"){ in.cap = "d'été" }
		
		colnames(data$data$data)[which(colnames(data$data$data) == variable )] = gsub("^([^---]*)---.*$", "\\1",	variable)
		p = shinemas2R::get.ggplot(data = data, ggplot.type = "data-interaction", x.axis = "year", 
															 in.col = "germplasm", vec_variables = gsub("^([^---]*)---.*$", "\\1",	variable), 
															 nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
		
		if( !is.null(p) ) {
		  out = list("figure" = list("caption" = paste("Evolution des notes globales", in.cap, "."), "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
		} else { 
			out = list("figure" = NULL)
		}
		OUT = c(OUT, out)
		
		return(OUT)
	}
	
	# Traduire les vec_variables
	traduction = function(tab,row_or_col)	{

		if( !is.null(tab$duplicated_infos)) {
				for (i in 1:length(tab$duplicated_infos)) {
					if (row_or_col == "row") {rownames(tab$duplicated_infos[[i]]$duplicated_infos_variables) = gsub("^([^---]*)---.*$", "\\1",rownames(tab$duplicated_infos[[i]]$duplicated_infos_variables))
					}else{colnames(tab$duplicated_infos[[i]]$duplicated_infos_variables) = gsub("^([^---]*)---.*$", "\\1",colnames(tab$duplicated_infos[[i]]$duplicated_infos_variables)) }
				}
		}
		
		if( !is.null(tab$not_duplicated_infos)) {
			for (i in 1:length(tab$not_duplicated_infos)) {
				if (row_or_col == "row") {rownames(tab$not_duplicated_infos[[i]]) = gsub("^([^---]*)---.*$", "\\1",rownames(tab$not_duplicated_infos[[i]]))
				}else{colnames(tab$not_duplicated_infos[[i]]) = gsub("^([^---]*)---.*$", "\\1",colnames(tab$not_duplicated_infos[[i]])) }
			}
		}
		
	return(tab)
	}
	
	out = list("chapter" = "Résultats dans la ferme"); OUT = c(OUT, out)
	
	out = list("text" = 
						 	"Dans ce chapitre, vous trouverez les résultats des données recueillies dans votre ferme.
						 Ces résultats vous permettent de répondre à la question : \\textbf{Quelles populations se comportent le mieux dans ma ferme?}.
						 "); OUT = c(OUT, out)
	
	# 2.1. Automne ----------
	out = list("section" = "Automne"); OUT = c(OUT, out)

	vec_variables = c(
	"topographie---notice.topographie.semis.2",
	"pratiques.semis---notice.pratiques.semis",
	"battance---notice.battance",
	"surface.micro.parcelle---notice.surface.micro.parcelle",
	"date.semis---date",
	"surface.champs---notice.surface.champ",
	"type.de.sol.info1---notice.type.sol.1",
	"type.de.sol.info2---notice.type.sol.2",
	"info.sur.le.champs---notice.info.champ",
	"espace.entre.micro.parcelles---espace.entre.micro.parcelles",
	"densite.semis---notice.densite.semis",
	"nom.champ---notice nom.champ",
	"labour.avant.semis---notice.labour.avant.semis",
	"drainage---notice.drainage",
	"précédent.cultural---notice.précédent.cultural",
	"remarques.automne---commentaires")

	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, col_to_display =NULL, 
									invert_row_col = TRUE, merge_g_and_s = TRUE, nb_duplicated_rows = 1, nb_row =NULL,
									nb_col = NULL)
	tab=traduction(tab,row_or_col = "row")

	# x = 
	out = list("table" = list("caption" = "Informations sur les pratiques culturales", "content" = tab)); OUT = c(OUT, out)
	
	vec_variables = 
		c("pluies.automne---pluies", 
			"températures.automne---température", 
			"notes.sur.le.climat.automne---notes.climatiques", 
			"accidents.climatiques.automne---accidents.climatiques")
	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
	tab= traduction(tab,"col")
	
	out = list("table" = list("caption" = "Informations sur le climat d'automne", "content" = tab)); OUT = c(OUT, out)
	
	
	# 2.2. Hiver ----------
	out = list("section" = "Notations d'hiver"); OUT = c(OUT, out)
	
	# 2.2.1. Evolution de la note globale ----------
	out = list("subsection" = "Note globale"); OUT = c(OUT, out)
	OUT = graph.fiche(OUT, data_all, "note.globale.hiver---global")
	
	
	# 2.2.2. Données hiver pour year ----------
	out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
	
	vec_variables = c("date.observation.hiver---date.observation", 
										"reprise---reprise", 
										"attitude des feuilles---attitude des feuilles", 
										"port.au.tallage---port.au.tallage", 
										"densité---densité", 
										"commentaires.hiver---post_winter_observation_notes")
	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
									nb_col = 5, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
	tab=traduction(tab,"col")
	out = list("table" = list("caption" = "Sommaire de la fiche hiver", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
	
	vec_variables = c("pluies.hiver---pluies", 
										"températures.hiver---températures",
										"accidents.climatiques.hiver---accidents.climatiques", 
										"notes.sur.le.climat.hiver---notes.climatiques", 
										"accidents.dans.le.champ.hiver---accidents.dans.le.champ", 
										"notes.sur.le.champ.hiver---commentaires")
	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
									nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
	tab=traduction(tab,"col")
	out = list("table" = list("caption" = "Informations sur le climat de l'hiver", "content" = tab)); OUT = c(OUT, out)
	
	# 2.3. Printemps ----------
	out = list("section" = "Notations de printemps"); OUT = c(OUT, out)
	
	# 2.3.1. Evolution de la note globale ----------
	out = list("subsection" = "Note globale"); OUT = c(OUT, out)
	OUT = graph.fiche(OUT, data_all, "note.globale.printemps---global")
	
	# 2.3.2. Données printemps pour year ----------
	out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)

	vec_variables = c(
	  "date.observation.printemps---date.observation",
	  "tallage---tallage",
	  "vigueur---vigueur",
	  "couleur.printemps---couleur.printemps",
	  "adventices.printemps---adventices",
	  "date.épiaison_60-100---date.épiaison_60-100",
	  "notes.maladies---notes.maladies", 
	  "commentaires.printemps---commentaires",
	  "note.épiaison---remarques.printemps_note")
	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
									nb_col = 6, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
	tab=traduction(tab,"col")
	out = list("table" = list("caption" = "Sommaire de la fiche printemps", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
	
	
	vec_variables = c("pluies.printemps---pluies", 
										"températures.printemps---températures", 
										"accidents.climatiques.printemps---accidents.climatiques.printemps", 
										"notes.sur.le.climat.printemps---notes.sur.le.climat.printemps", 
										"accidents.dans.le.champ.printemps", 
										"notes.sur.le.champ.printemps---notes.sur.le.champ.printemps")
	tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
									nb_col = 6, col_to_display = NULL, merge_g_and_s = TRUE)
	tab=traduction(tab,"col")
	out = list("table" = list("caption" = "Informations sur le climat du printemps", "content" = tab)); OUT = c(OUT, out)


# 2.4. Ete ----------
out = list("section" = "Notations d'été"); OUT = c(OUT, out)

# 2.3.1. Evolution de la note globale ----------
out = list("subsection" = "Note globale"); OUT = c(OUT, out)	
OUT = graph.fiche(OUT, data_all, "note.globale.ete---global")

# 2.3.2. Données ete pour year ----------
out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)

vec_variables = c("date.observation.été---date.observation", 
									"biomasse---biomasse", 
									"hétérogénéité---hétérogénéité", 
									"date.récolte---date.récolte", 
									"poids.battage---poids.battage", 
									"commentaires.été---commentaires")
tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
								nb_col = 5, nb_row = 7, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
tab=traduction(tab,"col")
out = list("table" = list("caption" = "Sommaire de la fiche été", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)

vec_variables = c("pluies.été---pluies",
									"températures.été---températures", 
									"accidents.climatiques.été---accidents.climatiques", 
									"notes.sur.le.climat.été---notes.climatiques", 
									"accidents.dans.le.champ.été---accidents.dans.le.champ", 
									"notes.sur.le.champ.été---notes.sur.le.champ.été")
tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
								nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
tab=traduction(tab,"col")
out = list("table" = list("caption" = "Informations sur le climat de l'été", "content" = tab)); OUT = c(OUT, out)


# 2.5. Mesure à la récolte ----------
out = list("section" = "Mesures à la récolte"); OUT = c(OUT, out)

# 2.5.1. Mesures sur les populations ----------
out = list("subsection" = "Mesures sur les populations"); OUT = c(OUT, out)

# 2.5.1.1. Poids de mille grains ----------
out = list("subsubsection" = "Le poids de mille grains"); OUT = c(OUT, out)

variable = "poids.de.mille.grains"
comp.mu = res_model1[[variable]]$comp.mu
p_interaction = PPBstats::get.ggplot(comp.mu, ggplot.type = "interaction")[person]
out = list("figure" = list("caption" = "
Comparaisons de moyennes pour le poids de mille grains au cours du temps. 
Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)

p_score = PPBstats::get.ggplot(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "
Scores des populations au cours du temps pour le poids de mille grains. 
Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
Un score maximal correspond au premier groupe de significativité. 
Un score minimal correspond au dernier groupe de significativité.
", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)

d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
if(!is.null(d)) {
  p_interaction_2 = PPBstats::get.ggplot(d, ggplot.type = "interaction")[person]
  out = list("figure" = list("caption" = "Evolution du poids de mille grains au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
}


out = list("text" = "Le tableau ci-dessous présente le poids de mille grains pour les populations récoltées cette année."); OUT = c(OUT, out)
tab = get.table(data = data_year, table.type = "mean", vec_variables = "poids.de.mille.grains---poids.de.mille.grains", 
								nb_col = 5, col_to_display = "germplasm", merge_g_and_s = TRUE)
tab=traduction(tab,"col")
out = list("table" = list("caption" = paste("Poids de mille grains des populations récoltées en ",year,sep=""), "content" = tab)); OUT = c(OUT, out)


# 2.5.1.2. Taux de protéine ----------
out = list("subsubsection" = "Le taux de protéine"); OUT = c(OUT, out)

variable = "taux.de.proteine"
comp.mu = res_model1[[variable]]$comp.mu
p_interaction = PPBstats::get.ggplot(comp.mu, ggplot.type = "interaction")[person]
out = list("figure" = list("caption" = "
                           Comparaisons de moyennes pour le taux de protéines au cours du temps. 
                           Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
                           Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
                           Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
                           ", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)

p_score = PPBstats::get.ggplot(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "
Scores des populations au cours du temps pour le taux de protéine. 
Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
Un score maximal correspond au premier groupe de significativité. 
Un score minimal correspond au dernier groupe de significativité.
", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)

d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
if(!is.null(d)) {
  p_interaction_2 = PPBstats::get.ggplot(d, ggplot.type = "interaction")[person]
  out = list("figure" = list("caption" = "Evolution du taux de protéine au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
}



# 2.5.1.3. Poids de mille grains en fonction du taux de protéine ----------
out = list("subsubsection" = "Le poids de mille grains en fonction du taux de protéine"); OUT = c(OUT, out)

p = shinemas2R::get.ggplot(data = data_all, ggplot.type = "data-biplot", in.col = "year", 
													 vec_variables = c("poids.de.mille.grains---poids.de.mille.grains", "taux.de.proteine---taux.de.proteine"), hide.labels.parts = c("person:year"))
out = list("figure" = list("caption" = "Relation entre le poids de mille grains et le taux de protéine", "content" = p, "width" = 1)); OUT = c(OUT, out)


# 2.5.1.4. Poids des épis ----------
out = list("subsubsection" = "Le poids des épis"); OUT = c(OUT, out)

variable = "poids.de.l.epi"
comp.mu = res_model1[[variable]]$comp.mu
p_interaction = PPBstats::get.ggplot(comp.mu, ggplot.type = "interaction")[person]
out = list("figure" = list("caption" = "
Comparaisons de moyennes pour le poids des épis au cours du temps. 
Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)

p_score = PPBstats::get.ggplot(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "
Scores des populations au cours du temps pour le poids des épis. 
Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
Un score maximal correspond au premier groupe de significativité. 
Un score minimal correspond au dernier groupe de significativité.
", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)

d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
if(!is.null(d)) {
  p_interaction_2 = PPBstats::get.ggplot(d, ggplot.type = "interaction")[person]
out = list("figure" = list("caption" = "Evolution du poids des épis au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
}


# 2.5.1.5. La hauteur et la verse ----------
out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)

p = shinemas2R::get.ggplot(data = data_all, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
													 vec_variables ="verse---verse", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
out = list("figure" = list("caption" = "Evolution de la verse au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

p = shinemas2R::get.ggplot(data = data_all, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
													 vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
out = list("figure" = list("caption" = "Evolution de la hauteur au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)



# 2.5.2. Etude de la sélection intra-population ----------

# 2.5.2.1. Poids de mille grains ----------
if (!is.null(data_S_year$data) & is.element("poids.de.mille.grains---poids.de.mille.grains",colnames(data_S_year$data$data))) {
  data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pS1 = PPBstats::get.ggplot(data= res_model1$poids.de.mille.grains$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                             nb_parameters_per_plot=8)
} else {pS1=NULL}

if (!is.null(data_SR_year$data)  & is.element("poids.de.mille.grains---poids.de.mille.grains",colnames(data_S_year$data$data))) {
  data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pSR1 = PPBstats::get.ggplot(data= res_model1$poids.de.mille.grains$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                           nb_parameters_per_plot=8)
} else {pSR1=NULL}


# 2.5.2.2. Protéine ----------
if (!is.null(data_S_year$data) & is.element("taux.de.proteine---taux.de.proteine",colnames(data_S_year$data$data))) {
  data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pS2 = PPBstats::get.ggplot(data= res_model1$taux.de.proteine$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                             nb_parameters_per_plot=8)
} else {pS2=NULL}

if (!is.null(data_SR_year$data) & is.element("taux.de.proteine---taux.de.proteine",colnames(data_S_year$data$data))) {
  data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pSR2 = PPBstats::get.ggplot(data= res_model1$taux.de.proteine$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                              nb_parameters_per_plot=8)
} else {pSR2=NULL}


# 2.5.2.3. Poids de l'épi ----------
if (!is.null(data_S_year$data) & is.element("poids.de.l.epi---poids.de.l.epi",colnames(data_S_year$data$data))) {
  data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pS3 = PPBstats::get.ggplot(data= res_model1$poids.de.l.epi$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                             nb_parameters_per_plot=8)
} else {pS3=NULL}

if (!is.null(data_SR_year$data) & is.element("poids.de.l.epi---poids.de.l.epi",colnames(data_S_year$data$data))) {
  data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
  pSR3 = PPBstats::get.ggplot(data= res_model1$poids.de.l.epi$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                              nb_parameters_per_plot=8)
} else {pSR3=NULL}


textS = list("text" = paste("
Vous avez la possibilité d'étudier votre sélection à l'intérieur des populations.\\\\
														
\\textbf{Comment lire les graphiques ?}
														
\\textbf{Le bouquet de sélection}: 
Ce graphique présente côte à côte les valeurs moyennes de votre bouquet de sélection (bouquetS) par rapport au vrac (vracS), c'est à dire la moyenne de ce qui n'a pas été sélectionné. 
La différence entre la moyenne du bouquet de sélection (bouquetS) et la moyenne du vrac (vracS) s'appelle le \\textbf{le différentiel de sélection}.
Cela permet de caractériser la manière dont vous avez sélectionné par rapport à la diversité disponible.
Si la sélection a été faite dans le cadre de l'essai sur les mélanges, la sélection correspondant à la modalité 1 (qui sera semée en pur à l'automne) est notée #VA 
tandis que la sélection correspondant à la modalité 2 (qui sera mélangées aux autres sélections pour reformer le mélange) est noté #JA.
",sep=""))


textSR = list("text" = paste("
\\textbf{La réponse à la sélection}: 
Ce graphique présente, en plus des differentiel de sélection faits en ", year, ", la réponse à la sélection. 
C'est à dire les épis issus de votre bouquet (bouquetR) et les épis issus du vrac (vracR). 
Cela permet de quantifier l'éfficacité de votre sélection. 
Par exemple si votre bouquet (bouquetS) en ", year," augmentait le poids de mille grains de 5 grammes par rapport au vrac (vracS), trois scénarios sont possibles:
														 
\\begin{itemize}

\\item bouquetR est supérieur de 5 grammes à vracR, alors la sélection a été totalement efficace : elle a été faite sur la partie génétique de la variation uniquement.

\\item bouquetR est supérieur entre 0 et 5 grammes à vracR, alors la sélection a été partiellement efficace ; elle a été faite sur une partie génétique et également une partie environnementale.

\\item bouquetR est supérieur de 0 grammes à vracR, alors la sélection n'a pas été effiace : elle a été faite sur la partie environmentale de la variation uniquement.

\\item bouquetR est inférieur à vracR : ce cas n'est théoriquement pas possible. Ce genre de résultats peut venir du manque de puissance du dispositif expérimental ou de l'interaction de la population avec l'année.

\\end{itemize}
",sep=""))


if( !is.null(pS1) | !is.null(pS2) | !is.null(pS3) | !is.null(pSR1) | !is.null(pSR2) | !is.null(pSR3) ){
	out = list("subsection" = "Etude de la réponse à la sélection"); OUT = c(OUT, out)
}
	

if( !is.null(pS1) | !is.null(pS2) | !is.null(pS3) ){
	out = list("subsubsection" = "Le differentiel de sélection"); OUT = c(OUT, out)
	
	out = textS; OUT = c(OUT, out)
	if( !is.null(pS1) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le poids de mille grains.", "content" = pS1, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
	if( !is.null(pS2) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le taux de protéine.", "content" = pS2, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
	if( !is.null(pS3) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le poids de l'épi. La barre autour de la moyenne représente la variation. Plus elle est importante, plus la variation est grande autour de la moyenne.", "content" = pS3, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
}

if( !is.null(pSR1) | !is.null(pSR2) | !is.null(pSR3) ){
	out = list("subsubsection" = "La réponse à la sélection"); OUT = c(OUT, out)
	
	out = textSR; OUT = c(OUT, out)
	if( !is.null(pSR1) ){ out = list("figure" = list("caption" = "Réponse à la sélection pour le poids de mille grains.", "content" = pSR1, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
	if( !is.null(pSR2) ){ out = list("figure" = list("caption" = "Réponse à la sélection pour le taux de protéine.", "content" = pSR2, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
	if( !is.null(pSR3) ){ out = list("figure" = list("caption" = "Réponse à la sélection pour le poids de l'épi. La barre autour de la moyenne représente la variation. Plus elle est importante, plus la variation est grande autour de la moyenne.", "content" = pSR3, "layout" = matrix(c(1), ncol = 1), "width" = 1))
	; OUT = c(OUT, out) }
}



# 3. Essai Mélanges -------
out = list("chapter" = "Résultats de l'essai mélanges"); OUT = c(OUT, out)
out = list("text" = "Cet essai, mis en place à l'automne 2015, vise à comparer les effets de différentes pratiques de sélection des mélanges sur leur comportement. 
					 Les pratiques testées sont : deux années de sélection dans les composantes avant de mélanger ; une sélection dans les composantes avant de mélanger puis une sélection
					 dans le mélange ; à partir du mélange créé sans sélection dans les composantes, deux années de sélection dans le mélange. Ces pratiques de sélection sont comparées
					 au mélange évoluant sans sélection massale. Lors de la saison 2015-2016, les paysans participant à l'essai ont semé leur(s) mélanges formés sans sélection dans
					 les composantes, ainsi que les composantes en pur. Les résultats obtenus cette année permettent de comparer le comportement des mélanges par rapport à leurs
					 composantes"); OUT = c(OUT, out)

# 3.1. Résultats sur la ferme -----
out = list("section" = "Résultats sur la ferme"); OUT = c(OUT, out)

# 3.1.1. Poids de mille grains -----
out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
variable = "poids.de.mille.grains"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "comp.in.farm", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison du poids de mille grains du mélange et de ses composantes. 
                                        Les populations qui partagent le même groupe pour une année donnée (représenté par une même lettre) ne sont pas significativement différentes.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.1.2. Taux de protéines -----
out = list("subsection" = "Taux de protéines"); OUT = c(OUT, out)

variable = "proteine"
p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, plot.type = "comp.in.farm", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison du poids de mille grains du mélange et de ses composantes. 
                                        Les populations qui partagent le même groupe pour une année donnée (représenté par une même lettre) ne sont pas significativement différentes.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.1.3. Poids de l'épi -----
out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
variable = "poids.de.l.epi"
p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, plot.type = "comp.in.farm", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison du poids de mille grains du mélange et de ses composantes. 
                                        Les populations qui partagent le même groupe pour une année donnée (représenté par une même lettre) ne sont pas significativement différentes.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.1.4. La hauteur et la verse ----------
# A faire !
# out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)
# 
# p = shinemas2R::get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
# 													 vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
# out = list("figure" = list("caption" = "Evolution de la hauteur au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
# 
# 
# p = shinemas2R::get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
# 													 vec_variables ="verse---verse", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
# out = list("figure" = list("caption" = "Evolution de la verse au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2. Résultats sur le réseau de fermes -----
out = list("section" = "Résultats sur le réseau de fermes"); OUT = c(OUT, out)
# Codes à tester !

# 3.2.1. Distribution de la différence entre le mélange et la moyenne de ses composantes sur le réseau -----
out = list("subsection" = "Distribution de la différence entre le mélange et la moyenne de ses composantes sur le réseau"); OUT = c(OUT, out)
out = list("text" = "Ces graphiques présentent le comportement des mélanges par rapport à la moyenne de leurs composantes respectives. 
           Un histogramme décallé vers la droite par rapport à 0 indique qu'une majorité des mélanges se sont mieux comportés que la moyenne de leurs composantes. 
           A l'inverse si l'histogramme est décallé vers la gauche la majorité des mélanges se sont moins bien comportés que la moyenne de leurs composantes. 
           La barre verticale indique la valeur moyenne de gain ou de la perte des mélanges par rapport à la moyenne de leurs composantes."); OUT = c(OUT, out)

# 3.2.1.1. poids de mille grains -----
out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
variable = "poids.de.mille.grains"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mix.gain.distribution", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Distribution de la différence de poids de mille grains entre les mélanges et la moyenne de leurs composantes sur le réseau.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2.1.2. Poids de l'épi -----
out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
variable = "poids.de.l.epi"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mix.gain.distribution", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Distribution de la différence de poids de l'épi entre les mélanges et la moyenne de leurs composantes sur le réseau.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2.2. Distribution des mélanges, de la moins bonne composante & la meilleure composante -----
out = list("subsection" = "Distributions des mélanges, de la moins bonne composante et la meilleure pour chaque mélange"); OUT = c(OUT, out)
out = list("text" = "Ces graphiques présentent la distribution de chacuns des mélanges, ainsi que de la moins bonne et la meilleure composante et la valeur moyenne des composantes. 
           Il permet de ...  [à compléter]
           
           "); OUT = c(OUT, out)

# 3.2.2.1. poids de mille grains -----
out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
variable = "poids.de.mille.grains"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mix.comp.distribution", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes ainsi que de la moyenne des composantes pour chaque mélange.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2.2.2. Poids de l'épi -----
out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
variable = "poids.de.l.epi"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mix.comp.distribution", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes ainsi que de la moyenne des composantes pour chaque mélange.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2.3. Comparaison de l'effet mélange par rapport à variété "pure" -----
out = list("subsection" = "Comparaison des performances moyennes des mélanges par rapport aux performances moyennes des composantes"); OUT = c(OUT, out)
out = list("text" = "On se pose la question de savoir s'il y a une différence significative entre la moyenne de tous les mélanges de l'essai et de toutes leurs composantes"); OUT = c(OUT, out)

# 3.2.3.1. poids de mille grains -----
out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
variable = "poids.de.mille.grains"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mixVScomp", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                                        Les moyennes sont significativement différentes si les lettres diffèrent.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)

# 3.2.3.2. Poids de l'épi -----
out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
variable = "poids.de.l.epi"
p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, plot.type = "mixVScomp", nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                                        Les moyennes sont significativement différentes si les lettres diffèrent.
                                        ", "content" = p_mélanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)



# 4. Le réseau de fermes ----------

# 4.1. Intro ----------
out = list("chapter" = "Resultats dans le réseau de fermes"); OUT = c(OUT, out)

out = list("text" = paste(
"Dans ce chapitre, vous trouverez les résultats des données recueillies dans les autres fermes du réseau.
Attention, ces résultats sont possibles uniquement si nous avons eu vos épis à mesurer.
	
Ces résultats vous permettent de répondre à la question : \\textbf{Quelles populations serait-il plus intéressant de tester chez moi pour les prochains semis?}
Ces résultats vous permettent de mobiliser la diversité évaluée dans le réseau de fermes.
Plutôt que de choisir des populations au hasard dans le réseau, nous vous indiquons les fermes sur lesquelles les populations se sont comportées (un peu) de la même manière que dans votre ferme cette année.
On ne propose que les fermes pour les années ", (as.numeric(year) - 1), " et ", year,".
	
Les différentes fermes sont nommées par les initiales des paysans suivies de l'année de récolte. Par exemple TDE:2014 pour la ferme TDE en 2014.
La carte ci dessous montre le nombre et la répartition des populations récoltées en ",year,".",sep="")); OUT = c(OUT, out)

p = shinemas2R::get.ggplot(data = data_network_year, ggplot.type = "network-reproduction-harvested", ggplot.display = "map", pie.size = .15)
out = list("figure" = list("caption" = paste("Répartition des fermes participant au projet en ", year," et nombre de populations récoltées."), "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)


# 4.2. Regroupement des fermes ----------
out = list("section" = "Regroupement des fermes"); OUT = c(OUT, out)

out = list("text" = "
Le graphique ci-dessous permet de regrouper les combinaisons ferme:année selon leurs similitudes. 
Plus les fermes sont proches sur le graphique, plus les populations cultivées en commun ont eu un comportement similaires dans ces fermes. 
Pour réaliser ce graphique, nous avons pris en compte le comportement des populations pour le poids de mille grains, le taux de protéine, la hauteur et le poids des épis.
           
Vous pouvez essayer des populations cultivées dans une ferme qui apparait proche de la votre. 
Les dossiers de chaque paysan(ne) est disponible à la demande."); OUT = c(OUT, out)


out = list(
  res_model2[["poids.de.mille.grains"]]$model.outputs,
  res_model2[["taux.de.proteine"]]$model.outputs,
  res_model2[["poids.de.l.epi"]]$model.outputs,
  res_model2[["hauteur"]]$model.outputs
)
names(out) = c("poids.de.mille.grains", "taux.de.proteine", "poids.de.l.epi", "hauteur")

           
clust = get.parameter.groups(out, parameter = "theta")
p_PCA = PPBstats::get.ggplot(clust, ggplot.type = "PCA")
out = list("figure" = list("caption" = paste("Analyse  en composantes principales sur les effets fermes:année"), "content" = p_PCA$ind, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)

clust_of_personyear = clust$clust[paste(person, year, sep = ":"),]$clust
farm_in_the_group = rownames(clust$clust)[which(clust$clust$clust == clust_of_personyear)]
farm_in_the_group = farm_in_the_group[-which(farm_in_the_group == paste(person, year, sep = ":"))]
toget = c(grep(year, farm_in_the_group), grep(as.character(as.numeric(year)-1), farm_in_the_group))
if( length(toget) > 0 ) { farm_in_the_group = farm_in_the_group[toget] }

out = list("text" = paste("Les fermes présentes dans le même groupe que votre ferme pour les années ", as.character(as.numeric(year)-1), " et ", year  ," sont : ", paste(farm_in_the_group, collapse = ", "))); OUT = c(OUT, out)

#comp.theta = res_model2$comp.par[[variable]]$comp.theta
#p_barplot_theta = PPBstats::get.ggplot(comp.theta, ggplot.type = "barplot")


# 4.3. Caractéristiques génétiques des populations dans le réseau ----------
out = list("section" = "Caractéristiques génétiques des populations dans le réseau"); OUT = c(OUT, out)

# 4.3.1. Effets génétiques des populations
out = list("subsection" = "Effets génétiques des populations"); OUT = c(OUT, out)

out = list("text" = paste("
Les effets génétiques correpondent à la valeur intrinsèque des populations.
Ces caractéristiques génétiques ont été estimées à partir du comportement des populations dans le réseau de fermes.
")); OUT = c(OUT, out)

variable = "poids.de.mille.grains"
comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
p_barplot_alpha = PPBstats::get.ggplot(comp.alpha, ggplot.type = "barplot")$alpha
p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le poids de mille grains", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)


variable = "taux.de.proteine"
comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
p_barplot_alpha = PPBstats::get.ggplot(comp.alpha, ggplot.type = "barplot")$alpha
p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le taux de protéine", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)


variable = "poids.de.l.epi"
comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
p_barplot_alpha = PPBstats::get.ggplot(comp.alpha, ggplot.type = "barplot")$alpha
p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le poids de l'épis", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)


# 4.3.2. Sensibilité des populations à l'interaction
out = list("subsection" = "Sensibilité des populations à l'interaction"); OUT = c(OUT, out)

out = list("text" = paste("
La sensibilité à l'interation des populations renseigne sur leur comportement en moyenne dans le réseau par rapport aux autres populations.
Moins elles sont sensibles à l’interaction, plus elles se comportent moyennement de la même manière dans les fermes par rapport aux autres populations.
")); OUT = c(OUT, out)

variable1 = "poids.de.mille.grains"
comp.beta = res_model2[[variable1]]$comp.par$comp.beta
tab1 = tail(comp.beta)

variable2 = "taux.de.proteine"
comp.beta = res_model2[[variable2]]$comp.par$comp.beta
tab2 = tail(comp.beta)

variable3 = "poids.de.l.epi"
comp.beta = res_model2[[variable3]]$comp.par$comp.beta
tab3 = tail(comp.beta)


tab = cbind.data.frame(rownames(tab1), rownames(tab2), rownames(tab3))
colnames(tab) = c(variable1, variable2, variable3)
out = list("table" = list("caption" = "Populations qui sont le moins sensible à l'interaction dans le réseau pour le poids de mille grains", "content" = tab)); OUT = c(OUT, out)



# 4.4. Prédire le passé ----------
out = list("section" = "Prédire le passé"); OUT = c(OUT, out)

out = list("text" = "
Ici nous vous proposons de prédire les valeurs qu’auraient eu certaines populations dans votre fermes cette année : on prédit le passé ! 
Cette information est issue des modèles statistiques que nous avons développés et est possible si nous avons reçu les épis cette année. 
"); OUT = c(OUT, out)

# Comme tous modèles, il donne une information avec une certaine confiance qui est donnée en pourcentage.

# Mettre l'intervalle de confiance dans notre cas: graph que pour les premiers ?!? A creuser
variable1 = "poids.de.mille.grains"
out = res_model2[[variable1]]$predict.past
out = out[grep(paste(person, year, sep = ":"), out$parameter),]
if( nrow(out) > 0 ) {
  out = out[order(out[,"50%"], decreasing = TRUE),]
  out = out[c(c(1:5),c((nrow(out) - 5):nrow(out))),]
  out1 = out
} else { out1 = NULL }

variable2 = "taux.de.proteine"
out = res_model2[[variable2]]$predict.past
out = out[grep(paste(person, year, sep = ":"), out$parameter),]
if( nrow(out) > 0 ) {
  out = out[order(out[,"50%"], decreasing = TRUE),]
  out = out[c(c(1:5),c((nrow(out) - 5):nrow(out))),]
  out2 = out
} else { out2 = NULL }

variable3 = "poids.de.l.epi"
out = res_m# Essai mélange
p_melanges = ggplot_mixture1(res_model1, data_PPB_mixture,variable, nb_parameters_per_plot = 15)[person]
out = list("figure" = list("caption" = "Comparaison du poids de mille grains du mélange et de ses composantes. 
													 Les populations qui partagent le même groupe pour une année donnée (représenté par une même lettre) ne sont pas significativement différentes.
													 La barre horizontale représente la moyenne des composantes
													 ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
OUT = c(OUT, out)

out = model2[[variable3]]$predict.past
out = out[grep(paste(person, year, sep = ":"), out$parameter),]
if( nrow(out) > 0 ) {
  out = out[order(out[,"50%"], decreasing = TRUE),]
  out = out[c(c(1:5),c((nrow(out) - 5):nrow(out))),]
  out3 = out
} else { out3 = NULL }

if( !is.null(out1) & !is.null(out2) & !is.null(out3) ) {
  tab = cbind.data.frame(out1$parameter, out1$`50%`, out2$parameter, out2$`50%`, out3$parameter, out3$`50%`)
  colnames(tab) = c(paste(c("germplasm", "valeur"), variable1), paste(c("germplasm", "valeur"), variable2), paste(c("germplasm", "valeur"), variable3))
  out = list("table" = list("caption" = "Prédiction du passé pour le poids de mille grains et le poids de l'épis.", "content" = tab)); OUT = c(OUT, out)
  
} else { 
  out = list("text" = "
Il n'est pas possible de prédire ces valeurs car nous n'avons aucune données péhnotypiques sur votre ferme pour cette année . 
"); OUT = c(OUT, out)
}


# /!\ Get pdf ----------
get.pdf(dir = paste(we_are_here, "/feedback_folder", sep = ""), 
        form.name = paste(person, year, sep = ":"), 
				LaTeX_head = "../2.tex_files/structure.tex", 
				LaTeX_body = OUT, 
				compile.tex = TRUE, 
				color1 = "mln-green", 
				color2 = "mln-brown"
				)

}




