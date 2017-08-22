# 0. help -----------------------------------------------------------------
#' Function to get the "feedback file" containing the results of the year's trial for each farmer based on the analysis coming from \code{analyse_feedback_folder_1}
#' 
#' @param dir Directory where folder for each person is created
#' 
#' @param person The farmer's name
#' 
#' @param out_analyse_feedback_folder_1 The outputs from \code{analyse_feedback_folder_1}
#' 
#' @param score Indicated whether to print the score graphics (TRUE) or not (FALSE)
#
#' @details 
#' The function creates two folders :
#' \itemize{
#'  \item "tex_files" with the tex files used to create the pdf
#'  \item "feedback_folder" with, for each person, a folder with information coming from shinemas2R::get.pdf() see ?get.pdf for more details.
#' }
#' 
#' @return Generate tex and pdf files
#' 
#' @author Pierre Rivière, Gaelle Van Frank
#' 
#' 
feedback_folder_1 = function(
  dir = ".",
  person,
  out_analyse_feedback_folder_1,
  score=F)
  # go ----------
{
  # Set the right folder and create folders tex_files and feedback_folder ----------
  a = dir(dir)
  if( !file.exists(dir) ){ stop("directory ", dir, " does not exist.") }
  
  
  setwd(dir)
  we_are_here = getwd()
  if( !is.element("tex_files", dir()) ) { system("mkdir tex_files") ; message("The folder tex_files has been created") }
  if( !is.element("feedback_folder", dir()) ) { system("mkdir feedback_folder") ; message("The folder feedback_folder has been created") }
  
  # Add info useful for feedback_folder_1
  p = system.file("extdata", "feedback_folder_1", package = "PPBformations")
  system(paste("cp ",p , "/* ", " ",we_are_here,"/tex_files", sep = ""))
  message("Several files used in the tex document have been copied to tex_files folder")
  
  # get info from out_analyse_feedback_folder_1
  year = out_analyse_feedback_folder_1$year
  res_model1 = out_analyse_feedback_folder_1$res_model1
  res_model2 = out_analyse_feedback_folder_1$res_model2
  res_model_varintra = out_analyse_feedback_folder_1$res_model_varintra
  data_network_year = out_analyse_feedback_folder_1$data_network_year
  data_all =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_all
  data_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_year
  data_S_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_S_year
  data_SR_year =  out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_SR_year
  data_PPB_mixture = out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_PPB_mixture
  Mixtures_all = out_analyse_feedback_folder_1$Mixtures_all
  
  levels(Mixtures_all$data$son) = c(levels(Mixtures_all$data$son) , "C70_ANB_2011_0001")
  Mixtures_all$data[Mixtures_all$data$son %in% "C70#S-crossés_ANB_2015_0001","son"] = as.factor("C70_ANB_2011_0001")
  Mixtures_all$data[Mixtures_all$data$germplasm_son %in% "C70#S-crossés","germplasm_son"] = "C70"
  
  # Créer title page
  a = paste(
    "	\\begin{titlepage}
    \\pagecolor{color1}
    \\noindent
    \\begin{center}
    \\begin{tabular}{cccc}
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp4} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp6} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp7} &
    \\includegraphics[width=.25\\textwidth, height=.25\\textwidth]{", we_are_here, "/tex_files/sp2}
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
    \\textbf{\\textsf{ARDEAR Rhone-Alpes}} \\\\
    \\textbf{\\textsf{ARDEAR Centre}} \\\\
    \\textbf{\\textsf{Bergerie de Villarceaux}} \\\\
    \\textbf{\\textsf{Graines de Noé}} \\\\
    \\textbf{\\textsf{Li mestère}} \\\\
    \\textbf{\\textsf{ADEAR 32}} \\\\
    \\textbf{\\textsf{ARDEAR NPDC}} \\\\
    
    
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
    ",
    sep=""
  )
  
  
  p = paste(we_are_here, "/tex_files/titlepage_", person,".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  OUT = list()
  
  # 0. Page de garde, contacts, table des matières ----------
  
  out = list("input" = paste("../tex_files/titlepage_", person, ".tex", sep = "")); OUT = c(OUT, out)
  
  
  # Contacts --------
  a=paste("	\\chapter*{Qui contacter ?}
          
          \\vfill
          
          \\noindent\\textbf{\\textsf{Correspondants nationaux :}} \\\\
          
          \\begin{wrapfigure}{l}{.20\\textwidth}
          \\begin{center} \\vspace{-20pt}
          \\includegraphics[width=.20\\textwidth]{",we_are_here,"tex_files/Logo-RSP.png}
          \\end{center} \\vspace{-20pt}
          \\end{wrapfigure}
          \\noindent
          ~\\\\
          Pierre Rivière \\href{mailto:pierre@semencespaysannes.org}{pierre@semencespaysannes.org} \\\\
          Patrick de Kochko \\href{mailto:patrick@semencespaysannes.org}{patrick@semencespaysannes.org} \\\\
          Réseau Semences Paysannes \\\\
          3, avenue de la Gare 47190 Aiguillon \\\\
          ~\\\\
          
          \\vfill
          
          \\begin{wrapfigure}{l}{.20\\textwidth}
          \\begin{center} \\vspace{-20pt}
          \\includegraphics[width=.20\\textwidth]{",we_are_here,"tex_files/Logo-UMRGV.jpg}
          \\end{center} \\vspace{-20pt}
          \\end{wrapfigure}
          \\noindent
          Isabelle Goldringer \\href{mailto:isabelle.goldringer@inra.fr}{isabelle.goldringer@inra.fr} \\\\
          Gaëlle van Frank \\href{mailto:gaelle.van-frank@inra.fr}{gaelle.van-frank@inra.fr} \\\\
          Equipe DEAP, INRA Le Moulon  \\\\
          Ferme du Moulon, 91190 Gif sur Yvette \\\\
          
          \\vfill
          
          \\newpage
          
          \\noindent\\textbf{\\textsf{Correspondants régionaux :}} \\\\
          
          \\begin{longtable}{p{.5\\textwidth}p{.5\\textwidth}}
          
          Julien Lacanette 									& \\\\
          CETAB 												& Triptolème \\\\
          3 avenue de la Gare, 47190 AIGUILLON 				& chez Carole Gruel, Launay 35330 BOVEL \\\\
          05.53.93.14.62										& \\\\
          \\href{mailto:cetab@laposte.net}{cetab@laposte.net} 	& \\href{mailto:Triptoleme.CA@gmail.com}{Triptoleme.CA@gmail.com} \\\\
          \\href{www.cetab.fr.nf}{www.cetab.fr.nf} 			& \\href{ www.semencespaysannes.org/qui_sommes-nous_286.php}{www.semencespaysannes.org/qui\\_sommes-nous\\_286.php}\\\\
          & \\\\
          & \\\\
          
          Lydie Carras & \\\\
          Pétanielle & Touzelle \\\\
          %& \\\\
          07 82 08 09 15 & \\\\
          \\href{mailto:lydiemsp@gmail.com}{lydiemsp@gmail.com} & \\\\
          & \\\\
          & \\\\
          
          Alexandre Hyacinthe  													& Sophie WOEHLING \\\\ 
          ARDEAR Rhones-Alpes 													& ARDEAR Centre \\\\
          58 rue Raulin 69007 LYON 												& Village d'entreprises de l'Arrou, 87A Route de Château-Renault, 41000 Blois \\\\
          04 72 41 79 22 															& 02 54 43 32 94 \\\\
          \\href{mailto:ardear.semences@wanadoo.fr}{ardear.semences@wanadoo.fr}	& \\href{mailto:ardearcentre.semencespaysannes@gmail.com}{ardearcentre.semencespaysannes@gmail.com} \\\\
          & \\\\
          & \\\\
          
          Bergerie de Villarceaux 																			& Graines de Noé \\\\
          Héloïse Boureau 																					& Hélène Montaz \\\\
          La Bergerie de Villarceaux, 95710 Chaussy 															& Technopole Agro-Environnement, Agronov RD-31, 21110 BRETENIERE \\\\
          01 34 67 91 23																						& 07 70 45 43 12 \\\\
          \\href{mailto:heloise.boureau@bergerie-villarceaux.org}{heloise.boureau@bergerie-villarceaux.org} 	& \\href{technique.grainesdenoe@gmail.com}{technique.grainesdenoe@gmail.com} \\\\
          \\href{www.bergerie-villarceaux.org}{www.bergerie-villarceaux.org} \\\\
          & \\\\
          & \\\\
          
          Kerna ùn Sohma & \\\\
          Anne Wanner & \\\\ 
          5 place de la gare – 68000 Colmar & \\\\
          03 89 24 43 19 & \\\\
          \\href{mailto:semencesalsaciennes@yahoo.fr}{semencesalsaciennes@yahoo.fr} & \\\\ 
          \\href{www.kernaunsohma.fr}{www.kernaunsohma.fr} & \\\\
          
          \\end{longtable}
          
          \\vfill
          
          %\\centering\\textsf{Fait à Aiguillon le~\\today}",sep="/")
  
  p = paste(we_are_here, "/tex_files/contacts", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/contacts.tex")
  OUT=c(OUT,out)
  
  # Table of contents
  out = list("tableofcontents" = TRUE); OUT = c(OUT, out)
  
  # 1. Intro ----------
  # 1.1 Pourquoi ce dossier -----
  a=paste("	\\chapter{Pourquoi ce dossier?}
          
          Ce dossier fait le bilan des premières années d'expérimentations qui se sont écoulées dans votre ferme et dans le réseau de fermes.
          Il permet de vous accompagner dans votre sélection d'un point de vue agronomique.
          Il n'est pas question ici de la qualité du blé, de son comportement en panification.
          Cet aspect qualité sera intégré dans le futur.
          
          \\warning{Ce dossier n'est qu'un modeste complément de l'expertise que vous avez développé en observant les populations chez vous, dans les champs et peut être au fournil.}
          
          Ce document est séparé en deux parties:
          
          \\begin{enumerate}
          
          \\item \\textbf{Une première partie traite des résultats dans votre ferme.} Cette partie permet de vous orienter pour répondre à la question : \\textbf{Quelles populations se comportent le mieux dans ma ferme?}
          
          Pour cela nous vous donnons les données recueillies pour chaque étape du cycle de la population (automne, hiver, printemps et été) ainsi que les mesures qui ont été effectuées au Moulon pour le poids de mille grains, le taux de protéine, le poids des épis, et dans les champs pour la hauteur et la verse.
          
          Pour les personnes concernées, nous vous apportons également des résultats sur vos bouquets de sélection, sur la réponse de ces bouquets à la sélection ainsi que sur l'essai mélange.
          
          \\item \\textbf{Une deuxième partie traite des résultats dans le réseau de fermes.} Cette partie permet de vous orienter pour répondre à la question : \\textbf{Quelles populations serait-il plus intéressant de tester chez moi pour les prochains semis?} Ces résultats vous permettent de mobiliser la diversité évaluée dans le réseau de fermes.
          
          \\begin{itemize}
          \\item Dans un premier temps, nous montrons les fermes dans lesquelles les populations se comportent le plus comme dans  votre ferme. Cela vous permet de vous mettre en lien avec cette ferme afin de récupérer quelques populations.
          
          \\item Dans un deuxième temps 
          
          \\item Ensuite, nous vous donnons les caractéristiques génétiques des populations, c'est à dire:
          \\begin{itemize}
          \\item Leurs effets génétiques (intrinsèque aux populations)
          \\item Leurs sensibilité à l'interaction. Moins elles sont sensibles à l'interaction, plus elles se comportent moyennement de la même manière dans les fermes par rapport aux autres populations.
          \\end{itemize}
          
          \\item Enfin, nous vous proposons de prédire les valeurs qu'auraient eu certaines populations dans vos fermes cette année : on prédit le passé!
          Cette information est issue des modèles statistiques que nous avons développés.
          Comme tous modèles, il donne une information avec une certaine confiance qui est donnée en pourcentage.
          \\end{itemize}
          
          \\end{enumerate}
          
          Les graphiques et les tableaux sont expliqués au fur et à mesure du document.
          En cas de soucis de compréhension, n'hésitez pas à nous contacter (\\href{mailto:pierre@semencespaysannes.org}{pierre@semencespaysannes.org}, 06 87 13 46 98). 
          Quand nous n'avons pas eu les données, il n'y a pas de résultats et la mention \"Pas de données\" apparaît.\\\\
          
          Au delà de cette diversité disponible dans votre ferme et dans le réseau, vous pouvez développer de nouvelles populations.
          Pour cela deux solutions sont à votre disposition\\footnote{Il en existe d'autres mais elles sont plus compliquées à mettre en oeuvre. On revient sur ce point lors des formations.} : 
          
          \\begin{itemize}
          \\item mélanger des populations existantes
          \\item faire des croisements. Dans ce cas, vous pouvez nous envoyer les parents que vous souhaitez croiser. L'équipe de recherche peut les croiser au Moulon. Elle pourra également venir faire des formations afin que vous puissiez réaliser vous même vos croisements dans votre ferme.
          \\end{itemize}
          
          ~\\\\  
          
          Selon vos souhaits et le nombre de populations que vous voulez semer, nous vous proposerons d'être ferme régionale ou ferme satellite.
          La figure ci-dessous rappelle la particularité de ces deux types de fermes.
          Avec les témoins : \\colorbox{black}{\\textcolor{white}{Rouge-du-Roc}}; \\colorbox{black}{\\textcolor{white}{C14}}; \\colorbox{black}{\\textcolor{white}{C21}}; \\colorbox{black}{\\textcolor{white}{Renan}}.
          Nous pouvons avoir les plans suivant (ces plans sont modulables selon vos contraintes ...):
          
          \\begin{center}
          \\begin{tabular}{c c}
          
          Fermes régionales & Fermes satellites \\\\
          \\hline	\\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FR.pdf} & \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FS_bis.pdf} \\\\
          
          4 témoins dans 2 blocs & pas de blocs; 1 témoin répété deux fois \\\\
          
          24 populations non répétées & 8 populations non répétées \\\\
          \\hline
          \\end{tabular}
          \\end{center}
          
          ~\\\\ 
          
          Nous avons joint à ce dossier trois fiches à remplir afin d'avoir 
          \\begin{itemize}
          \\item votre avis sur ce dossier. En effet, ce type de retour est en construction et nous avons besoin de vous pour l'améliorer.
          %\\item la liste des populations que vous souhaitez semer pour les prochains semis. Cela nous permettra de faciliter les échanges de semences et de vous proposer un plan
          \\item la liste éventuelle de parents que vous souhaiteriez croiser
          \\end{itemize}
          
          
          La fiche \\guill{Le rôle des paysans participant au projet de sélection collaborative sur les céréales} qui réapitule les différentes étapes du projet est présenté dans la partie 2 de ce document.
          
          \\vfill
          
          \\begin{flushright}
          Bonne lecture et bons semis!
          
          Le Réseau Semences Paysannes
          
          L'équipe DEAP de l'INRA du Moulon
          \\end{flushright}
          \\vfill
          
          
          \\newpage",sep="/")
  
  p = paste(we_are_here, "/tex_files/intro", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/intro.tex")
  OUT=c(OUT,out)
  
  #1.2 Fiche paysans --------
  a=paste("\\chapter{Le rôle des paysans participant au projet de sélection collaborative sur les céréales}
          
          \\begin{center}
          \\Large Version 5 du 30 septembre 2015
          \\end{center}
          
          
          \\section{Réunion de bilan de l'année écoulée et des prochains semis}
          Cette réunion a lieu dans la deuxième semaine de septembre à Paris. Pour le suivi de la dynamique,
          il est important que l'animateur et au moins un paysan y soient présents.
          
          \\section{Les semis : dispositifs expérimentaux}
          
          
          \\warning{Envoyez à votre animateur la liste des populations que vous souhaitez semer et il vous
          proposera un plan de semis. Ensuite, envoyer le plan définitif à votre animateur. Il vous
          enverra alors des étiquettes et des piquets afin de bien identifier vos micros-parcelles.}
          
          
          Pour le semis, deux choses sont importantes :
          
          \\begin{itemize}
          \\item semer le témoin qui vous sera envoyé si vous ne l'avez pas déjà. A part le témoin, vous
          semez ce que vous voulez ! Il est important de conserver les noms des sélections que l'on
          vous donne lorsque que vous faites des bouquets de sélection (sous la forme [nom de la
          variété]\\#[une lettre], par exemple Rouge-de-Bordeaux\\#R ou Blanc-de-la-Réole\\#E)
          
          \\item suivre un dispositif expérimental de type ferme régional \\yo{OU} satellites.
          Pour les fermes satellites, s'il y a peu de populations, on peut imaginer répéter le témoin une seule fois.
          \\end{itemize}
          
          Les témoins sont dans les cases noires. Les populations que vous choisissez sont dans les cases
          blanches. 
          La taille conseillée des micro-parcelles est entre 5 et 10m\\up{2}.
          \\\\
          
          
          \\begin{center}
          \\begin{tabular}{c c}
          Fermes régionales & Fermes satellites \\\\
          \\hline
          \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FR.pdf}  & \\includegraphics[width=.4\\textwidth]{",we_are_here,"tex_files/plan_FS_bis.pdf} 
          \\\\
          \\hline
          \\end{tabular}
          \\end{center}
          
          \\section{Suivi des populations avec les fiches}
          Votre animateur vous fera parvenir des fiches afin de suivre la culture. Ces fiches peuvent être
          issues du projet national ou issues de votre groupe.
          
          \\section{Envoi des épis récoltés par la poste pour qu'ils soient mesurés à l'INRA}
          
          A la récolte, votre animateur vous enverra un courrier avec deux types d'enveloppes :
          
          \\begin{itemize}
          \\item \\yo{« 50 épis au hasard »} pour y déposer 50 épis (ou moins s'il y a peu d'épis dans la micro-
          parcelle) pris au hasard dans la parcelle ;
          
          \\item \\yo{« bouquet d'épis sélectionnés »} si vous faites des bouquets de sélection. Ceci est optionnel
          mais peut être intéressant si vous souhaitez étudier vos sélections massales.
          Ces épis seront mesurés pour les barbes, la couleur, la courbure de l'épi, le poids de l'épi, le taux de
          protéine et le poids de mille grains. Une notice plus précise est fournie avec les sacs.
          \\end{itemize}
          
          \\warning{Il est important que ces sacs d'épis soient envoyés à l'INRA le plus vite possible !}
          
          Nous recevons en moyenne 700 sacs par an, les mesures doivent être terminées le 10 septembre,
          date limite pour faire les mesures de protéine. Si nous recevons tout fin août, ce n'est pas gérable ...
          
          ~\\\\
          
          \\yo{N'hésitez pas à nous contacter si vous souhaitez de plus amples informations :}
          
          Pierre Rivière, RSP ; \\url{pierre@semencespaysannes.org} ; 06 87 13 46 98
          
          \\newpage
          ", sep="/")
  
  p = paste(we_are_here, "/tex_files/fiche_paysans_SP_cereales_v5", ".tex", sep = "")
  sink(p);	cat(a);	sink()
  
  out=list("input" = "../tex_files/fiche_paysans_SP_cereales_v5.tex")
  OUT=c(OUT,out)
  
  # 2. Partie sur la ferme ----------
  
  # les graph pour les fiches
  graph.fiche = function(data, variable) {
    
    if(variable == "note.globale.hiver---global"){ in.cap = "d'hiver" }
    if(variable == "note.globale.printemps---note.globale.printemps"){ in.cap = "de printemps" }
    if(variable == "note.globale.ete---global"){ in.cap = "d'été" }
    
    if(variable == "all_notes"){ 
      in.cap = "d'hiver, de printemps et d'été"
      variable = c("note.globale.hiver---global","note.globale.printemps---note.globale.printemps","note.globale.ete---global")
			p = get_interaction_cycle(data,variable,equal.ylim = TRUE,nb_parameters_per_plot_in.col = 8)
			
    }else{
      p = get.ggplot(data = data, ggplot.type = "data-interaction", x.axis = "year", 
                     in.col = "germplasm", vec_variables = variable, 
                     nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
    }
    
    if( !is.null(p) ) {
      out = list("figure" = list("caption" = paste("Evolution des notes globales", in.cap, "."), "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1))
    } else { 
      out = list("figure" = NULL)
    }
    return(out)
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
  
  # 2.0. Populations présentes dans la ferme ----------
  out = list("section" = "Populations semées cette année dans votre ferme"); OUT = c(OUT, out)
  D=out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_year$data$data
  a = unique(D[!is.na(D$block) |!is.na(D$X) | !is.na(D$Y) | !is.na(D$"nom.champ---notice nom.champ") | !is.na(D$"poids.grains.mesure---poids.grains.mesure") | 
                 !is.na(D$"nbr_spikes---nbr.épiss") | !is.na(D$"poids.de.mille.grains---poids.de.mille.grains"),"son"])
  a = unlist(lapply(as.character(a),function(x){strsplit(x,'_')[[1]][1]}))
  b = unique(out_analyse_feedback_folder_1$out_farmers_data[[person]]$data_S_year$data$data$expe_name_2)
  b = unlist(lapply(as.character(b),function(x){strsplit(x,' | ')[[1]][3]}))
  b = unlist(lapply(as.character(b),function(x){strsplit(x,'_')[[1]][1]}))
  if(!is.null(b)){a = a[-grep(paste(b,collapse='|'),a)]}
  a=unique(a)
  a = paste(a, collapse=" ; ")
  
  out = list("text" = paste("Voici la liste des populations que vous avez semé cette année et pour lesquelles vous nous avez envoyé des informations ou des grains : \\textbf{", a,"}",sep="")); OUT = c(OUT, out)
  
  # 2.0.1. Evolution des notes globales au cours du cycle
  graph = graph.fiche(data_all, "all_notes")
  if(!is.null(graph$figure)){
    out = list("subsection" = "Evolution des notes globales de ces populations au cours de l'année"); OUT = c(OUT, out)
    OUT=c(OUT,graph)
  }
  
  
  
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
  
  
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur les pratiques culturales", "content" = tab)); OUT = c(OUT, out); comp = 1}
  
  vec_variables = 
    c("pluies.automne---pluies", 
      "températures.automne---température", 
      "notes.sur.le.climat.automne---notes.climatiques", 
      "accidents.climatiques.automne---accidents.climatiques")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab= traduction(tab,"col")
  
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat d'automne", "content" = tab)); OUT = c(OUT, out) ; comp = 1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  
  # 2.2. Hiver ----------
  out = list("section" = "Notations d'hiver"); OUT = c(OUT, out)
  comp=NULL
  
  # 2.2.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.hiver---global")
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)
    OUT = c(OUT,graph)
    comp=1
  }

  
  
  # 2.2.2. Données hiver pour year ----------
    vec_variables = c("date.observation.hiver---date.observation", 
                    "reprise---reprise", 
                    "attitude des feuilles---attitude des feuilles", 
                    "port.au.tallage---port.au.tallage", 
                    "densité---densité", 
                    "commentaires.hiver---post_winter_observation_notes")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Sommaire de la fiche hiver", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
    comp=1
  }
  
  vec_variables = c("pluies.hiver---pluies", 
                    "températures.hiver---températures",
                    "accidents.climatiques.hiver---accidents.climatiques", 
                    "notes.sur.le.climat.hiver---notes.climatiques", 
                    "accidents.dans.le.champ.hiver---accidents.dans.le.champ", 
                    "notes.sur.le.champ.hiver---commentaires")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat de l'hiver", "content" = tab)); OUT = c(OUT, out) ; comp = 1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.3. Printemps ----------
  out = list("section" = "Notations de printemps"); OUT = c(OUT, out)
  comp=NULL
  # 2.3.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.printemps---note.globale.printemps")
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)
    OUT = c(OUT,graph)
  }

  
  # 2.3.2. Données printemps pour year ----------
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
  if(!is.null(tab)){
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Sommaire de la fiche printemps", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)
    comp=1
  }
  
  
  vec_variables = c("pluies.printemps---pluies", 
                    "températures.printemps---températures", 
                    "accidents.climatiques.printemps---accidents.climatiques.printemps", 
                    "notes.sur.le.climat.printemps---notes.sur.le.climat.printemps", 
                    "accidents.dans.le.champ.printemps", 
                    "notes.sur.le.champ.printemps---notes.sur.le.champ.printemps")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 6, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Informations sur le climat du printemps", "content" = tab)); OUT = c(OUT, out); comp=1}
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.4. Ete ----------
  out = list("section" = "Notations d'été"); OUT = c(OUT, out)
  
  # 2.3.1. Evolution de la note globale ----------
  graph = graph.fiche(data_all, "note.globale.ete---global")
  if(!is.null(graph$figure)){
    out = list("subsection" = "Note globale"); OUT = c(OUT, out)	
    OUT = c(OUT,graph)
  }

  
  # 2.3.2. Données ete pour year ----------
  vec_variables = c("date.observation.été---date.observation", 
                    "biomasse---biomasse", 
                    "hétérogénéité---hétérogénéité", 
                    "date.récolte---date.récolte", 
                    "poids.battage---poids.battage", 
                    "commentaires.été---commentaires")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, nb_row = 7, col_to_display = c("germplasm", "block"), merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){out = list("table" = list("caption" = "Sommaire de la fiche été", "content" = tab, landscape = TRUE)); OUT = c(OUT, out)}
  
  vec_variables = c("pluies.été---pluies",
                    "températures.été---températures", 
                    "accidents.climatiques.été---accidents.climatiques", 
                    "notes.sur.le.climat.été---notes.climatiques", 
                    "accidents.dans.le.champ.été---accidents.dans.le.champ", 
                    "notes.sur.le.champ.été---notes.sur.le.champ.été")
  tab = get.table(data = data_year, table.type = "raw", vec_variables = vec_variables, 
                  nb_col = 5, col_to_display = NULL, merge_g_and_s = TRUE)
  tab=traduction(tab,"col")
  if(!is.null(tab)){ 
    out = list("subsection" = paste("Données détaillées pour", year)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Informations sur le climat de l'été", "content" = tab)); OUT = c(OUT, out)
    comp=1
  }
  if(is.null(comp)){out=list("text" = "Pas de données"); OUT=c(OUT,out)}
  
  # 2.5. Mesure à la récolte ----------
  out = list("section" = "Mesures à la récolte"); OUT = c(OUT, out)
  
  # 2.5.1. Mesures sur les populations ----------
  out = list("subsection" = "Mesures sur les populations"); OUT = c(OUT, out)
  out = list("text" = "Cette année nous sommes dans l'incapacité de vous donner des résultats concernant le taux de protéine car nous avons fait les analyses sur une nouvelle machine 
             et il nous faut faire des analyses complémentaires pour pouvoir calculer le taux de protéine des grains."); OUT = c(OUT, out)
  
  # 2.5.1.1. Poids de mille grains ----------
  out = list("subsubsection" = "Le poids de mille grains"); OUT = c(OUT, out)
  
  variable = "poids.de.mille.grains"
  if(variable %in% names(res_model1)){
    comp.mu = res_model1[[variable]]$comp.par$comp.mu
    attributes(comp.mu)$PPBstats.object = "mean_comparisons_model_1"  # à retirer quand résultats depuis nouveau package PPBstats
    p_interaction =plot.PPBstats(comp.mu, ggplot.type = "interaction", nb_parameters_per_plot = 10)[person]
    out = list("figure" = list("caption" = "
                               Comparaisons de moyennes pour le poids de mille grains au cours du temps. 
                               Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
                               Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
                               Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
                               ", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
    if (score == TRUE){
      p_score =plot.PPBstats(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
      out = list("figure" = list("caption" = "
                                 Scores des populations au cours du temps pour le poids de mille grains. 
                                 Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
                                 Un score maximal correspond au premier groupe de significativité. 
                                 Un score minimal correspond au dernier groupe de significativité.
                                 ", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
      
    }
    
    d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
    if(!is.null(d)) {
      attributes(d)$PPBstats.object = "check_model_model_1"  # à retirer quand résultats depuis nouveau package PPBstats
      p_interaction_2 =PPBstats::get_ggplot(d, ggplot.type = "interaction")[person]
      out = list("figure" = list("caption" = "Evolution du poids de mille grains au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    
    
    out = list("text" = "Le tableau ci-dessous présente le poids de mille grains pour les populations récoltées cette année."); OUT = c(OUT, out)
    tab = get.table(data = data_year, table.type = "mean", vec_variables = "poids.de.mille.grains---poids.de.mille.grains", 
                    nb_col = 5, col_to_display = "germplasm", merge_g_and_s = TRUE, order_var = "poids.de.mille.grains---poids.de.mille.grains")
    tab=traduction(tab,"col")
    tab$not_duplicated_infos$`set-1`$`poids.de.mille.grains` = round(as.numeric(as.character(tab$not_duplicated_infos$`set-1`$`poids.de.mille.grains`)),3)
    out = list("table" = list("caption" = paste("Poids de mille grains des populations récoltées en ",year,sep=""), "content" = tab)); OUT = c(OUT, out)
  }#else{out = list("text" = "No data."); OUT = c(OUT, out)}
  
  
  # 2.5.1.2. Taux de protéine ----------
  out = list("subsubsection" = "Le taux de protéine"); OUT = c(OUT, out)
  
  variable = "taux.de.proteine"
  if (variable %in% names(res_model1)){
    # 2.5.1.2. Taux de protéine ----------
    out = list("subsubsection" = "Le taux de protéine"); OUT = c(OUT, out)
    
    variable = "taux.de.proteine"
    comp.mu = res_model1[[variable]]$comp.par$comp.mu
    p_interaction =plot.PPBstats(comp.mu, ggplot.type = "interaction")[person]
    out = list("figure" = list("caption" = "
                               Comparaisons de moyennes pour le taux de protéines au cours du temps. 
                               Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
                               Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
                               Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
                               ", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
    if (score == TRUE) {
      p_score =plot.PPBstats(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
      out = list("figure" = list("caption" = "
                                 Scores des populations au cours du temps pour le taux de protéine. 
                                 Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
                                 Un score maximal correspond au premier groupe de significativité. 
                                 Un score minimal correspond au dernier groupe de significativité.
                                 ", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    
    
    d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
    if(!is.null(d)) {
      p_interaction_2 =plot.PPBstats(d, ggplot.type = "interaction")[person]
      out = list("figure" = list("caption" = "Evolution du taux de protéine au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    
    
    # 2.5.1.3. Poids de mille grains en fonction du taux de protéine ----------
    out = list("subsubsection" = "Le poids de mille grains en fonction du taux de protéine"); OUT = c(OUT, out)
    a=data_all$data$data
    prot_ok =  a[!is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & !is.na(a[,"taux.de.proteine---taux.de.proteine"]),c(1:40,grep("^taux.de.proteine---taux.de.proteine$",colnames(a)),grep("^poids.de.mille.grains---poids.de.mille.grains$",colnames(a)))]
    prot = a[is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & !is.na(a[,"taux.de.proteine---taux.de.proteine"]),c(1:40,grep("^taux.de.proteine---taux.de.proteine$",colnames(a)))]
    pmg = a[!is.na(a[,"poids.de.mille.grains---poids.de.mille.grains"]) & is.na(a[,"taux.de.proteine---taux.de.proteine"]),c("son","poids.de.mille.grains---poids.de.mille.grains")]
    to_add = merge(prot,pmg,by="son")
    a=rbind(prot_ok,to_add)
    data_all$data$data=a
    p = get.ggplot(data = data_all, ggplot.type = "data-biplot", in.col = "year", 
                   vec_variables = c("poids.de.mille.grains---poids.de.mille.grains", "taux.de.proteine---taux.de.proteine"), 
                   hide.labels.parts = c("person:year"))
    out = list("figure" = list("caption" = "Relation entre le poids de mille grains et le taux de protéine", "content" = p, "width" = 1)); OUT = c(OUT, out)
    
    
  }#else{# out = list("text" = "No data."); OUT = c(OUT, out)}

  # 2.5.1.4. Poids des épis ----------
  out = list("subsubsection" = "Le poids des épis"); OUT = c(OUT, out)
  
  variable = "poids.de.l.epi"
  if (variable %in% names(res_model1)){
    comp.mu = res_model1[[variable]]$comp.par$comp.mu
    p_interaction = plot.PPBstats(comp.mu, ggplot.type = "interaction", nb_parameters_per_plot = 15)[person]
    out = list("figure" = list("caption" = "
                               Comparaisons de moyennes pour le poids des épis au cours du temps. 
                               Les populations qui partagent le même groupe pour une année donnée (représenté par une barre) ne sont pas significativement différentes.
                               Le pourcentage de confiance dans cette information est indiqué en dessous des points. 
                               Imp veut dire impossible : nous n’avons pas pu faire de groupe car la variabilité due au sol était trop importante.
                               ", "content" = p_interaction, "layout" = matrix(c(1,2), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
    if(score == TRUE) {
      p_score =plot.PPBstats(comp.mu, ggplot.type = "score", nb_parameters_per_plot = 15)[person]
      out = list("figure" = list("caption" = "
                                 Scores des populations au cours du temps pour le poids des épis. 
                                 Un score élevé signifie que la population était dans un groupe de significativité avec une moyenne élevée. 
                                 Un score maximal correspond au premier groupe de significativité. 
                                 Un score minimal correspond au dernier groupe de significativité.
                                 ", "content" = p_score, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    
    
    d = res_model1[[variable]]$model.outputs$model1.data_env_whose_param_did_not_converge
    if(!is.null(d)) {
      attributes(d)$PPBstats.object = "check_model_model_1"  # à retirer quand résultats depuis nouveau package PPBstats
      p_interaction_2 =plot.PPBstats(d, ggplot.type = "interaction")[person]
      out = list("figure" = list("caption" = "Evolution du poids des épis au cours du temps sans analyses statistiques.", "content" = p_interaction_2, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    }
    }
  
  # 2.5.1.5. La hauteur et la verse ----------
  out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)
  
  p = get.ggplot(data = data_all, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
                 vec_variables ="verse---verse", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
  out = list("figure" = list("caption" = "Evolution de la verse au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  p = get.ggplot(data = data_all, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
                 vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
  out = list("figure" = list("caption" = "Evolution de la hauteur au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  
  # 2.5.1.6. La verse en fonction de la hauteur ----------
  out = list("subsubsection" = "La verse en fonction de la hauteur"); OUT = c(OUT, out)
  
  p = get.ggplot(data = data_all, ggplot.type = "data-biplot", in.col = "year", 
                 vec_variables = c("verse---verse", "hauteur---hauteur"), hide.labels.parts = c("person:year"))
  out = list("figure" = list("caption" = "Relation entre la verse et la hauteur", "content" = p, "width" = 1)); OUT = c(OUT, out)
  
  # 2.5.1.7. Le rendement ----------
  out = list("subsubsection" = "Le rendement"); OUT = c(OUT, out)
  
  
# !!!!!!!!! A FAIRE !!!!!!!  
  
  
  p = get.ggplot(data = data_all, ggplot.type = "data-biplot", in.col = "year", 
                 vec_variables = c("verse---verse", "hauteur---hauteur"), hide.labels.parts = c("person:year"))
  out = list("figure" = list("caption" = "Relation entre la verse et la hauteur", "content" = p, "width" = 1)); OUT = c(OUT, out)
  
  
  
  
  # 2.5.2. Etude de la sélection intra-population ----------
  
  # 2.5.2.1. Poids de mille grains ----------
  if (!is.null(data_S_year$data) & is.element("poids.de.mille.grains---poids.de.mille.grains",colnames(data_S_year$data$data))) {
    data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
    pS1 =plot.PPBstats(data= res_model1$poids.de.mille.grains$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                              nb_parameters_per_plot=8)
  } else {pS1=NULL}
  
  if (person != "ADP" & !is.null(data_SR_year$data)  & is.element("poids.de.mille.grains---poids.de.mille.grains", colnames(data_S_year$data$data))) {
    data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
    pSR1 =plot.PPBstats(data= res_model1$poids.de.mille.grains$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                               nb_parameters_per_plot=8)
  } else {pSR1=NULL}
  
  if ("taux.de.proteine" %in% names(res_model1)) {
    # 2.5.2.2. Protéine ----------
    if (!is.null(data_S_year$data) & is.element("taux.de.proteine---taux.de.proteine",colnames(data_S_year$data$data))) {
      data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
      pS2 =plot.PPBstats(data=res_model1$taux.de.proteine$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                                nb_parameters_per_plot=8)
    } else {pS2=NULL}
    
    if (!is.null(data_SR_year$data) & is.element("taux.de.proteine---taux.de.proteine",colnames(data_S_year$data$data))) {
      data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
      pSR2 =plot.PPBstats(data= res_model1$taux.de.proteine$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                                 nb_parameters_per_plot=8)
    } else {pSR2=NULL}
    
  }else{pS2=pSR2=NULL}
  
  
  # 2.5.2.3. Poids de l'épi ----------
  if (!is.null(data_S_year$data) & is.element("poids.de.l.epi---poids.de.l.epi",colnames(data_S_year$data$data))) {
    data_version = format.data(data_S_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
    pS3 =plot.PPBstats(data= res_model1$poids.de.l.epi$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                              nb_parameters_per_plot=8)
  } else {pS3=NULL}
  
  if (person != "ADP" & !is.null(data_SR_year$data) & is.element("poids.de.l.epi---poids.de.l.epi",colnames(data_S_year$data$data))) {
    data_version = format.data(data_SR_year, data.on = "son", fuse_g_and_s = TRUE, format = "PPBstats")
    pSR3 =plot.PPBstats(data= res_model1$poids.de.l.epi$comp.par$comp.mu, data_2=NULL, data_version = data_version, ggplot.type = "barplot", 
                               nb_parameters_per_plot=8)
  } else {pSR3=NULL}
  
  
  textS = list("text" = paste("
                              Vous avez la possibilité d'étudier votre sélection à l'intérieur des populations.\\\\
                              
                              \\textbf{Comment lire les graphiques ?}
                              
                              \\textbf{Le bouquet de sélection}: 
                              Ce graphique présente côte à côte les valeurs moyennes de votre bouquet de sélection (bouquetS) par rapport au vrac (vracS), c'est à dire la moyenne de ce qui n'a pas été sélectionné. 
                              La différence entre la moyenne du bouquet de sélection (bouquetS) et la moyenne du vrac (vracS) s'appelle le \\textbf{le différentiel de sélection}.
                              Cela permet de caractériser la manière dont vous avez sélectionné par rapport à la diversité disponible.
                              Si la sélection a été faite dans le cadre de l'essai sur les mélanges, la sélection correspondant à la modalité 1 (faite dans la population, qui sera semée en pur à l'automne) 
                              est notée #VA, la sélection correspondant à la modalité 2 (faite dans la population, qui sera mélangées aux autres sélections pour reformer le mélange) est noté #JA, tandis que
                              la sélection faite dans le mélange est notée #BA.
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
    if( !is.null(pS1) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le poids de mille grains. Le symbole au-dessus des populations représentent
                                                    la significativité de la différence de moyenne : le \".\" représente une faible significativité tandis que \"***\" représente une
                                                    forte significativité. S'il n'y a aucun symbole la différence n'est pas significative.", "content" = pS1, "layout" = matrix(c(1), ncol = 1), "width" = 1))
    ; OUT = c(OUT, out) }
    if( !is.null(pS2) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le taux de protéine. Le symbole au-dessus des populations représentent
                                                    la significativité de la différence de moyenne : le \".\" représente une faible significativité tandis que \"***\" représente une
                                                    forte significativité. S'il n'y a aucun symbole la différence n'est pas significative.", "content" = pS2, "layout" = matrix(c(1), ncol = 1), "width" = 1))
    ; OUT = c(OUT, out) }
    if( !is.null(pS3) ){ out = list("figure" = list("caption" = "Différentiel de sélection pour le poids de l'épi. Le symbole au-dessus des populations représentent
                                                    la significativité de la différence de moyenne : le \".\" représente une faible significativité tandis que \"***\" représente une
                                                    forte significativité. S'il n'y a aucun symbole la différence n'est pas significative.", "content" = pS3, "layout" = matrix(c(1), ncol = 1), "width" = 1))
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
             Les pratiques testées sont : 
             \\begin{itemize}
             \\item deux années de sélection dans les composantes avant de mélanger ; 
             \\item une sélection dans les composantes avant de mélanger puis une sélection dans le mélange ; 
             \\item à partir du mélange créé sans sélection dans les composantes, deux années de sélection dans le mélange.
             \\end{itemize}
             Ces pratiques de sélection sont comparées au mélange évoluant sans sélection massale. 
             Lors de la saison 2015-2016, les paysans participant à l'essai ont semé leurs mélanges formés sans sélection dans
             les composantes, ainsi que les composantes en pur. Les résultats obtenus cette année permettent de comparer le comportement des mélanges par rapport à leurs
             composantes"); OUT = c(OUT, out)
  
  # 3.1. Résultats sur la ferme -----
  out = list("section" = "Résultats sur la ferme"); OUT = c(OUT, out)
  if (is.null(data_PPB_mixture$data)) { 
    out = list("text" = "Vous n'avez pas mis en place cet essai sur votre ferme cette année."); OUT=c(OUT,out)
  }else{
    out = list("text" = "Les gaphiques suivant permettent de comparer la valeur du mélange à celles de ses composantes et à la valeur moyenne des composantes."); OUT=c(OUT,out)
    
    # 3.1.1. Poids de mille grains -----
    out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
    variable = "poids.de.mille.grains"
    if (variable %in% names(res_model1)){
      p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model = "model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 20)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$barplot)){out = list("figure" = list("caption" = "Comparaison du poids de mille grains du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ", "content" = p_melanges[[1]][[i]]$barplot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        }
        }
    
    variable = "taux.de.proteine"
    if (variable %in% names(res_model1)){
      # 3.1.2. Taux de protéines -----
      out = list("subsection" = "Taux de protéines"); OUT = c(OUT, out)
      
      p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, year=year, model="model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 17)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$barplot)){out = list("figure" = list("caption" = "Comparaison du taux de protéine du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ", "content" = p_melanges[[1]][[i]]$barplot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        }
      
        }
    
    
    # 3.1.3. Poids de l'épi -----
    out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
    variable = "poids.de.l.epi"
    if (variable %in% names(res_model1)){
      p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, year=year, model="model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 17)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$barplot)){out = list("figure" = list("caption" = "Comparaison du poids de l'épi du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ", "content" = p_melanges[[1]][[i]]$barplot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        }
        }
    
    # 3.1.4. Hauteur -----
    out = list("subsection" = "Hauteur"); OUT = c(OUT, out)
    variable = "hauteur"
    if (variable %in% names(res_model1)){
      p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, year=year, model="model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 17)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$barplot)){out = list("figure" = list("caption" = "Comparaison de la hauteur du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ", "content" = p_melanges[[1]][[i]]$barplot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        }
        }
    
    # 3.1.5. Longueur de l'épi -----
    out = list("subsection" = "Longueur de l'épi"); OUT = c(OUT, out)
    variable = "longueur.de.l.epi"
    if (variable %in% names(res_model1)){
      p_melanges = ggplot_mixture1(res_model1, Mixtures_all, variable, year=year, model="model_1", plot.type = "comp.in.farm", person, nb_parameters_per_plot = 17)
      for (i in 1:length(p_melanges[[1]])){
        if(!is.null(p_melanges[[1]][[i]]$barplot)){out = list("figure" = list("caption" = "Comparaison de la longueur de l'épi du mélange et de ses composantes. 
                                                                              Les populations qui partagent le même groupe (représenté par une même lettre) ne sont pas significativement différentes.
                                                                              ", "content" = p_melanges[[1]][[i]]$barplot, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)}
        }
        }
    
    
    # 3.1.4. La hauteur et la verse ----------
    # A faire !
    # out = list("subsubsection" = "La hauteur et la verse"); OUT = c(OUT, out)
    # 
    # p = get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
    # 													 vec_variables = "hauteur---hauteur", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
    # out = list("figure" = list("caption" = "Evolution de la hauteur au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    # 
    # 
    # p = get.ggplot(data = data_PPB_mixture, ggplot.type = "data-interaction", x.axis = "year", in.col = "germplasm", 
    # 													 vec_variables ="verse---verse", nb_parameters_per_plot_in.col = 5, merge_g_and_s = TRUE)
    # out = list("figure" = list("caption" = "Evolution de la verse au cours du temps", "content" = p, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
      }
  
  # 3.2. Résultats sur le réseau de fermes -----
  out = list("section" = "Résultats sur le réseau de fermes"); OUT = c(OUT, out)
  out = list("text" = "Dans cette partie sont présentés les résultats de l'essai mélange sur le réseau de ferme. 
             On s'intéresse dans un premier temps à la différence observée entre la valeur des mélanges et celle de la moyenne de leurs composantes respectives, 
             permettant de constater si les mélanges apportent en moyenne plutôt un gain ou une perte par rapport à la moyenne de leurs composantes. 
             Puis on s'intéresse à la variabilité observée, pour chacun des mélanges testés, de chacune des \"moins bonnes\" composantes, de la meilleure composante,
             de la valeur moyenne des composante et de celle des mélanges.
             Enfin on compare la valeur moyenne de l'ensemble des mélange à celle de l'ensemble des composantes pour voir si on détecte un effet du mélange."); OUT=c(OUT,out)
  
  
  # 3.2.1. Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau -----
  out = list("subsection" = "Distribution du gain du mélange par rapport à la moyenne de ses composantes sur le réseau"); OUT = c(OUT, out)
  out = list("text" = "Ces graphiques présentent le comportement des mélanges par rapport à la moyenne de leurs composantes respectives. 
             Un histogramme décallé vers la droite par rapport à 0 indique qu'une majorité des mélanges se sont mieux comportés que la moyenne de leurs composantes. 
             A l'inverse si l'histogramme est décallé vers la gauche la majorité des mélanges se sont moins bien comportés que la moyenne de leurs composantes."); OUT = c(OUT, out)
  
  # 3.2.1.1. poids de mille grains -----
  out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
  variable = "poids.de.mille.grains"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour poids de mille grains.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  
  
  
  # 3.2.1.2. Poids de l'épi -----
  out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
  variable = "poids.de.l.epi"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour le poids de l'épi.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.1.3. Hauteur -----
  out = list("subsection" = "Hauteur moyenne"); OUT = c(OUT, out)
  variable = "hauteur"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour la hauteur.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.1.4. Longueur de l'épi -----
  out = list("subsection" = "Longueur de l'épi"); OUT = c(OUT, out)
  variable = "longueur.de.l.epi"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour la longueur de l'épi.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.1.5. LLSD -----
  out = list("subsection" = "Distance dernière feuille - base de l'épi"); OUT = c(OUT, out)
  variable = "LLSD"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year,model="model_1", plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour la distance dernière feuille - base de l'épi.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.1.6. Nombre moyen de grains par épi  -----
  out = list("subsection" = "Nombre moyen de grains par épi"); OUT = c(OUT, out)
  variable = "nbr.grains.par.epi"
  if (!file.exists(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mix.gain.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Gain.distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution des rapports entre les comportement des mélanges et les comportements moyens
                                                      des leurs composantes respectives pour la distance dernière feuille - base de l'épi.
                                                      La ligne rouge verticale indique le gain moyen des mélanges par rapport à la moyenne de leurs composantes respectives 
                                                      tandis que la ligne pointillée noire est fixée sur un gain nul.", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  
  # 3.2.2. Distribution des mélanges, de la moins bonne composante & la meilleure composante -----
  out = list("subsection" = "Distributions des mélanges, de la moins bonne et la meilleure composante pour chaque mélange"); OUT = c(OUT, out)
  out = list("text" = "Ces graphiques présentent, pour chacun des mélanges testés cette année, le comportement du mélange, de sa moins bonne composantes,
             de sa meilleure composante et le comportement moyen de ses composantes. 
             On peut observer sur ces graphiques la variabilité de comportement des mélanges ainsi que celle de leurs moins bonne
             et meilleure composantes.
             "); OUT = c(OUT, out)
  
  # 3.2.2.1. poids de mille grains -----
  out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
  variable = "poids.de.mille.grains"
  if (!file.exists(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                      ainsi que de la moyenne des composantes pour chaque mélange pour le poids de mille grains.
                                                      Le X noir représente la valeur moyenne pour chaque type.
                                                      ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.2.2. Poids de l'épi -----
  out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
  variable = "poids.de.l.epi"
  if (!file.exists(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){ out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                       ainsi que de la moyenne des composantes pour chaque mélange pour le poids de l'épi.
                                                       Le X noir représente la valeur moyenne pour chaque type.
                                                       ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.2.3. Hauteur -----
  out = list("subsection" = "Hauteur moyenne"); OUT = c(OUT, out)
  variable = "hauteur"
  if (!file.exists(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year,model="model_1", plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                      ainsi que de la moyenne des composantes pour chaque mélange pour la hauteur (mm).
                                                      Le X noir représente la valeur moyenne pour chaque type.
                                                      ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.2.4. Longueur de l'épi -----
  out = list("subsection" = "Longueur de l'épi"); OUT = c(OUT, out)
  variable = "longueur.de.l.epi"
  if (!file.exists(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year,model="model_1", plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                      ainsi que de la moyenne des composantes pour chaque mélange pour la longueur de l'épi (mm).
                                                      Le X noir représente la valeur moyenne pour chaque type.
                                                      ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  # 3.2.2.5. LLSD -----
  out = list("subsection" = "Distance dernière feuille - base de l'épi"); OUT = c(OUT, out)
  variable = "LLSD"
  if (!file.exists(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))){
    p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year,model="model_1", plot.type = "mix.comp.distribution", person, nb_parameters_per_plot = 15)
    save(p_melanges,file=paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  }else{
    load(paste(we_are_here,"/figures/Distribution.",variable,".RData",sep=""))
  } 
  if(!is.null(p_melanges)){out = list("figure" = list("caption" = "Distribution sur le réseau des mélanges, des moins bonnes et meilleures composantes 
                                                      ainsi que de la moyenne des composantes pour chaque mélange pour la distance dernière feuille - base de l'épi (mm).
                                                      Le X noir représente la valeur moyenne pour chaque type.
                                                      ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 0.8)); OUT = c(OUT, out)}
  
  #  3.2.3. Comparaison de l'effet mélange par rapport à variété "pure" -----
  #  A virer...? 
  out = list("subsection" = "Comparaison de la performance moyenne des mélanges par rapport àa la performance moyenne des composantes"); OUT = c(OUT, out)
  out = list("text" = "On se pose la question de savoir s'il y a une différence significative entre la moyenne de tous les mélanges de l'essai 
             et la moyenne de toutes leurs composantes."); OUT = c(OUT, out)
  
  # 3.2.3.1. poids de mille grains -----
  out = list("subsection" = "Poids de mille grains"); OUT = c(OUT, out)
  variable = "poids.de.mille.grains"
  p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mixVScomp", person, nb_parameters_per_plot = 15)
  out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                             Les moyennes sont significativement différentes si les lettres diffèrent.
                             ", "content" = p_melanges$bp, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  # 3.2.3.2. Poids de l'épi -----
  out = list("subsection" = "Poids de l'épi"); OUT = c(OUT, out)
  variable = "poids.de.l.epi"
  p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year,model="model_1", plot.type = "mixVScomp", person, nb_parameters_per_plot = 15)
  out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                             Les moyennes sont significativement différentes si les lettres diffèrent.
                             ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  # 3.2.3.3. Hauteur -----
  out = list("subsection" = "Hauteur"); OUT = c(OUT, out)
  variable = "hauteur"
  p_melanges = ggplot_mixture1(res_model = res_model1, melanges_PPB_mixture = Mixtures_all, variable, year=year, model="model_1",plot.type = "mixVScomp", person, nb_parameters_per_plot = 15)
  out = list("figure" = list("caption" = "Comparaison entre la moyenne des mélanges et la moyenne des composantes sur le réseau.
                             Les moyennes sont significativement différentes si les lettres diffèrent.
                             ", "content" = p_melanges, "layout" = matrix(c(1,2,3), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  
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
  
  p = get.ggplot(data = data_network_year, ggplot.type = "network-reproduction-harvested", ggplot.display = "map", pie.size = .15)
  out = list("figure" = list("caption" = paste("Répartition des fermes participant au projet en ", year," et nombre de populations récoltées."), "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  
  # 4.2. Regroupement des fermes ----------
  out = list("section" = "Regroupement des fermes"); OUT = c(OUT, out)
  
  out = list("text" = "
             Le graphique ci-dessous permet de regrouper les combinaisons ferme:année selon leurs similitudes. 
             Plus les fermes sont proches sur le graphique, plus les populations cultivées en commun ont eu un comportement similaires dans ces fermes. 
             Pour réaliser ce graphique, nous avons pris en compte le comportement des populations pour le poids de mille grains, le taux de protéine, la hauteur et le poids des épis.
             
             Vous pouvez essayer des populations cultivées dans une ferme qui apparait proche de la votre. 
             Les dossiers de chaque paysan(ne) est disponible à la demande."); OUT = c(OUT, out)
  
  
  
  Model2 = lapply(res_model2,function(x){return(x$model.outputs)})
  
  clust = parameter_groups (Model2, parameter = "theta")
  p_PCA = plot.parameter_groups(clust)
  out = list("figure" = list("caption" = paste("Analyse  en composantes principales sur les effets fermes:année"), "content" = p_PCA$ind, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  
  clust_of_personyear = clust$clust$clust[paste(person, year, sep = ":"),"clust"]
  farm_in_the_group = rownames(clust$clust$clust)[which(clust$clust$clust$clust == clust_of_personyear)]
  farm_in_the_group = farm_in_the_group[-which(farm_in_the_group == paste(person, year, sep = ":"))]
  toget = c(grep(year, farm_in_the_group), grep(as.character(as.numeric(year)-1), farm_in_the_group))
  if( length(toget) > 0 ) { farm_in_the_group = farm_in_the_group[toget] }
  
  out = list("text" = paste("Les fermes présentes dans le même groupe que votre ferme pour les années ", as.character(as.numeric(year)-1), " et ", year  ," sont : ", paste(farm_in_the_group, collapse = ", "))); OUT = c(OUT, out)
  
  #comp.theta = res_model2$comp.par[[variable]]$comp.theta
  #p_barplot_theta =plot.PPBstats(comp.theta, ggplot.type = "barplot")
  
  
  # 4.3. Caractéristiques génétiques des populations dans le réseau ----------
  out = list("section" = "Caractéristiques génétiques des populations dans le réseau"); OUT = c(OUT, out)
  
  # 4.3.1. Effets génétiques des populations
  out = list("subsection" = "Effets génétiques des populations"); OUT = c(OUT, out)
  
  out = list("text" = paste("
                            Les effets génétiques correpondent à la valeur intrinsèque des populations.
                            Ces caractéristiques génétiques ont été estimées à partir du comportement des populations dans le réseau de fermes pour l'ensemble des années.
                            ")); OUT = c(OUT, out)
  
  variable = "poids.de.mille.grains"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    #p_barplot_alpha =plot.PPBstats(comp.alpha, ggplot.type = "barplot")$alpha
    #p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
    #out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le poids de mille grains", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
    out = list("text" = "Le tableau ci-dessous présente les effets génétiques minimum et maximum des populations dans le réseau pour le poids de mille grains."); OUT = c(OUT, out)
    tab = comp.alpha$mean.comparisons[,c("parameter","median","groups")]
    tab$parameter = ex_between(tab$parameter, "[", "]")
    colnames(tab) = c("Population","poids de mille grains moyen","groupe")
    attributes(tab)$invert = FALSE
    tab_tail = tail(tab)
    tab_head = head(tab)
    
    #Récupérer les résultats des 2 meilleures et 2 moins bonnes populations sur la ferme
    tab_pmg= res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons[[paste(person,year,sep=":")]]$mean.comparisons[,c("parameter","median","groups")]
    if(!is.null(tab_pmg)){
      tab_pmg$parameter = paste(ex_between(tab_pmg$parameter, "[", "]"),"*",sep=" ")
      colnames(tab_pmg) = c("Population","poids de mille grains moyen","groupe")
      tab_pmg$groupe = NA
      tab_pop_tail = tail(tab_pmg,n=2)
      tab_pop_head = head(tab_pmg,n=2)
      
      
      tab_tail=rbind(tab_tail,tab_pop_tail) ; tab_tail=tab_tail[order(tab_tail[,"poids de mille grains moyen"]),]
      tab_head=rbind(tab_head,tab_pop_head) ; tab_head=tab_head[order(tab_head[,"poids de mille grains moyen"]),]
      out = list("table" = list("caption" = "Effets génétiques minimums des populations dans le réseau pour le poids de mille grains.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des poids de mille grains les plus faible chez vous cette année (*)", "content" = tab_head)); OUT = c(OUT, out)
      out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le poids de mille grains.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des poids de mille grains les plus importants chez vous cette année (*)", "content" = tab_tail)); OUT = c(OUT, out)
      
    }else{
      out = list("table" = list("caption" = "Effets génétiques minimums des populations dans le réseau pour le poids de mille grains", "content" = tab_head)); OUT = c(OUT, out)
                              out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le poids de mille grains", "content" = tab_tail)); OUT = c(OUT, out)
                              
    }
  }
   
  
  variable = "taux.de.proteine"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    #p_barplot_alpha =plot.PPBstats(comp.alpha, ggplot.type = "barplot")$alpha
    #p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
    #out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le taux de protéine", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    
    out = list("text" = "Le tableau ci-dessous présente les effets génétiques des populations dans le réseau pour le taux de protéines."); OUT = c(OUT, out)
    tab = comp.alpha$mean.comparisons[,c("parameter","median","groups")]
    tab$parameter = ex_between(tab$parameter, "[", "]")
    colnames(tab) = c("Population","taux de protéine","groupe")
    attributes(tab)$invert = FALSE
    tab_tail = tail(tab)
    tab_head = head(tab)
    
    #Récupérer les résultats des 2 meilleures et 2 moins bonnes populations sur la ferme
    tab_prot = res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons[[paste(person,year,sep=":")]]$mean.comparisons[,c("parameter","median","groups")]
    if(!is.null(tab_prot)){
      tab_prot$parameter = paste(ex_between(tab_prot$parameter, "[", "]"),"*",sep=" ")
      colnames(tab_prot) = c("Population","taux de protéine","groupe")
      tab_prot$groupe = NA
      tab_pop_tail = tail(tab_prot,n=2)
      tab_pop_head = head(tab_prot,n=2)
      
      
      tab_tail=rbind(tab_tail,tab_pop_tail) ; tab_tail=tab_tail[order(tab_tail[,"taux de protéine"]),]
      tab_head=rbind(tab_head,tab_pop_head) ; tab_head=tab_head[order(tab_head[,"taux de protéine"]),]
      out = list("table" = list("caption" = "Effets génétiques minimums des populations dans le réseau pour le taux de protéine.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des taux de protéine les plus faible chez vous cette année (*)", "content" = tab_head)); OUT = c(OUT, out)
      out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le taux de protéine.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des taux de protéine les plus importants chez vous cette année (*)", "content" = tab_tail)); OUT = c(OUT, out)
      
    }else{
     out = list("table" = list("caption" = "Effets génétiques minimums des populations dans le réseau pour le taux de protéines", "content" = tab_head)); OUT = c(OUT, out)
     out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le taux de protéines", "content" = tab_tail)); OUT = c(OUT, out)
    }
  }
  
  
  variable = "poids.de.l.epi"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    #p_barplot_alpha =plot.PPBstats(comp.alpha, ggplot.type = "barplot")$alpha
    #p = p_barplot_alpha[c(1, length(p_barplot_alpha))]
    #out = list("figure" = list("caption" = "Effets génétiques minimum et maximum des populations dans le réseau pour le poids de l'épis", "content" = p, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    out = list("text" = "Le tableau ci-dessous présente les effets génétiques des populations dans le réseau pour le poids de l'épi."); OUT = c(OUT, out)
    tab = comp.alpha$mean.comparisons[,c("parameter","median","groups")]
    tab$parameter = ex_between(tab$parameter, "[", "]")
    colnames(tab) = c("Population","poids de l'épi","groupe")
    attributes(tab)$invert = FALSE
    tab_tail = tail(tab)
    tab_head = head(tab)
    
    #Récupérer les résultats des 2 meilleures et 2 moins bonnes populations sur la ferme
    tab_pe = res_model1[[variable]]$comp.par$comp.mu$data_mean_comparisons[[paste(person,year,sep=":")]]$mean.comparisons[,c("parameter","median","groups")]
    if(!is.null(tab_pe)){
      tab_pe$parameter = paste(ex_between(tab_pe$parameter, "[", "]"),"*",sep=" ")
      colnames(tab_pe) = c("Population","poids de l'épi","groupe")
      tab_pe$groupe = NA
      tab_pop_tail = tail(tab_pe,n=2)
      tab_pop_head = head(tab_pe,n=2)
      
      
      tab_tail=rbind(tab_tail,tab_pop_tail) ; tab_tail=tab_tail[order(tab_tail[,"poids de l'épi"]),]
      tab_head=rbind(tab_head,tab_pop_head) ; tab_head=tab_head[order(tab_head[,"poids de l'épi"]),]
      out = list("table" = list("caption" = "Effets génétiques (valeurs intrinsèques) minimums des populations dans le réseau pour le poids de l'épi.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des poids d'épis les plus faible chez vous cette année (*)", "content" = tab_head)); OUT = c(OUT, out)
      out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le poids de l'épi.
                              A titre comparatif sont reportées dans le tableau les deux populations ayant donné des poids d'épis les plus importants chez vous cette année (*)", "content" = tab_tail)); OUT = c(OUT, out)
      
    }else{
    out = list("table" = list("caption" = "Effets génétiques minimums des populations dans le réseau pour le poids de l'épi", "content" = tab_head)); OUT = c(OUT, out)
    out = list("table" = list("caption" = "Effets génétiques maximums des populations dans le réseau pour le poids de l'épi", "content" = tab_tail)); OUT = c(OUT, out)
    }
  }
  
  
  # 4.3.2. Sensibilité des populations à l'interaction
  out = list("subsection" = "Sensibilité des populations à l'interaction"); OUT = c(OUT, out)
  
  out = list("text" = paste("
                            La sensibilité à l'interation des populations renseigne sur leur comportement en moyenne dans le réseau par rapport aux autres populations.
                            Moins elles sont sensibles à l’interaction, plus elles se comportent moyennement de la même manière dans les fermes par rapport aux autres populations.
                            ")); OUT = c(OUT, out)
  
  tab=data.frame(rep(0,6))
  variable1 = "poids.de.mille.grains"
  if (variable1 %in% names(Model2)) {
    comp.beta = res_model2[[variable1]]$comp.par$comp.beta$mean.comparisons
    tab1 = tail(comp.beta)
    tab=cbind.data.frame(tab,rownames(tab1))
    colnames(tab)[ncol(tab)]=variable1
  }
  
  variable2 = "taux.de.proteine"
  if (variable2 %in% names(Model2)) {
    comp.beta = res_model2[[variable2]]$comp.par$comp.beta$mean.comparisons
    tab2 = tail(comp.beta)
    tab=cbind.data.frame(tab,rownames(tab2))
    colnames(tab)[ncol(tab)]=variable2
  }
  
  variable3 = "poids.de.l.epi"
  if (variable3 %in% names(Model2)) {
    comp.beta = res_model2[[variable3]]$comp.par$comp.beta$mean.comparisons
    tab3 = tail(comp.beta)
    tab=cbind.data.frame(tab,rownames(tab3))
    colnames(tab)[ncol(tab)]=variable3
  }
  tab=tab[,-1]
  
  tab = as.data.frame(apply(tab,2,function(x) {unlist(ex_between(x,"[","]"))}))
  attributes(tab)$invert = FALSE
  out = list("table" = list("caption" = paste("Populations qui sont le moins sensible à l'interaction dans le réseau pour le ",
                                              variable1,",",variable2,",",variable3,sep=" "), "content" = tab)); OUT = c(OUT, out)
  
  # 4.3.3. Effet génétique en fonction de la sensibilité à l'environnement
  out = list("subsection" = "Effet génétique en fonction de la sensibilité à l'environnement"); OUT = c(OUT, out)
  
  out = list("text" = paste("Sur les graphiques suivants sont placées les population selon leur effet génétique et leur sensibilité à l'interaction.
                            Les populations situées en bas à droite ont un effet génétique important et sont peu sensibles à l'interaction.
                            A l'inverse les populations en haut à gauche ont un effet génétique plus faibles que la moyenne des populations et sont sensibles à l'interaction.")); OUT = c(OUT, out)
  
  variable = "poids.de.mille.grains"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    comp.beta = res_model2[[variable]]$comp.par$comp.beta
    comp.beta$mean.comparisons = cbind(comp.beta$mean.comparisons,rownames(comp.beta$mean.comparisons))
    colnames(comp.beta$mean.comparisons) = c("median","parameter")
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 100)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le poids de mille grains de toutes les populations.
                               ", "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 90)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le poids de mille grains.
                               ", "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  }
  
  variable = "taux.de.proteine"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    comp.beta = res_model2[[variable]]$comp.par$comp.beta
    comp.beta$mean.comparisons = cbind(comp.beta$mean.comparisons,rownames(comp.beta$mean.comparisons))
    colnames(comp.beta$mean.comparisons) = c("median","parameter")
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 100)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le taux de protéine de toutes les populations.", 
                               "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 90)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le taux de protéine.", 
                               "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  }
  
  variable = "poids.de.l.epi"
  if (variable %in% names(Model2)) {
    comp.alpha = res_model2[[variable]]$comp.par$comp.alpha
    comp.beta = res_model2[[variable]]$comp.par$comp.beta
    comp.beta$mean.comparisons = cbind(comp.beta$mean.comparisons,rownames(comp.beta$mean.comparisons))
    colnames(comp.beta$mean.comparisons) = c("median","parameter")
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 90)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le poids de l'épi de toutes les populations.
                               ", "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
    p_alpha_beta =plot.PPBstats(data = comp.alpha, data_2 = comp.beta, data_version=NULL, ggplot.type = "biplot-alpha-beta", nb_parameters_per_plot = 90)
    out = list("figure" = list("caption" = "Effet génétique en fonction de la sensibilité à l'environnement pour le poids de l'épi.
                               ", "content" = p_alpha_beta, "layout" = matrix(c(1), ncol = 1), "width" = 1)); OUT = c(OUT, out)
  }
  
  # 4.4. Prédire le passé ----------
  out = list("section" = "Prédire le passé"); OUT = c(OUT, out)
  
  out = list("text" = "
             Ici nous vous proposons de prédire les valeurs qu’auraient eu certaines populations dans votre fermes cette année : on prédit le passé ! 
             Cette information est issue des modèles statistiques que nous avons développés et est possible si nous avons reçu les épis cette année. 
             "); OUT = c(OUT, out)
  
  # Comme tous modèles, il donne une information avec une certaine confiance qui est donnée en pourcentage.
  
  # Mettre l'intervalle de confiance dans notre cas: graph que pour les premiers ?!? A creuser
  name = NULL
  tab=rep(0,14)
  variable1 = "poids.de.mille.grains"
  if (variable1 %in% names(Model2) & !is.null(res_model2[[variable1]]$predict.past)) {
    name=c(name,"poids de mille grains")
    out = res_model2[[variable1]]$predict.past[[paste(person, year, sep = ":")]]$MCMC
    quantiles=NULL
    for(i in 1:ncol(out)){quantiles = rbind(quantiles,quantile( out[i,], probs=c(0, 0.05, 0.10, 0.50, 0.90, 0.95, 1)))}
    rownames(quantiles)=unlist(ex_between(names(out),"[",","))
    
    if( nrow(quantiles) > 0 ) {
      quantiles = quantiles[order(quantiles[,"50%"], decreasing = TRUE),]
      quantiles = quantiles[c(c(1:5),c((nrow(quantiles) - 5):nrow(quantiles))),]
      quantiles = cbind.data.frame(rownames(quantiles), quantiles$`50%`)
      tab_pop = tail(tab_pmg,n=3)[1:2] ; tab_pop$parameter = paste(ex_between(tab_pop$parameter, "[", "]"),"*",sep=" ") ; colnames(tab_pop)=colnames(quantiles)
      quantiles = rbind(quantiles,tab_pop) ; quantiles = quantiles[order(quantiles[,2]),]
      tab=cbind.data.frame(tab,quantiles)
      colnames(tab)[(ncol(tab)-1):ncol(tab)] = c(paste(c("germplasm", "valeur"), variable1))
    }
  } 
  
  variable2 = "taux.de.proteine"
  if (variable2 %in% names(Model2) & !is.null(res_model2[[variable2]]$predict.past)) {
    name=c(name,"taux de protéine")
    out = res_model2[[variable2]]$predict.past[[paste(person, year, sep = ":")]]$MCMC
    quantiles=NULL
    for(i in 1:ncol(out)){quantiles = rbind(quantiles,quantile( out[i,], probs=c(0, 0.05, 0.10, 0.50, 0.90, 0.95, 1)))}
    rownames(quantiles)=unlist(ex_between(names(out),"[",","))
    
    if( nrow(quantiles) > 0 ) {
      quantiles = quantiles[order(quantiles[,"50%"], decreasing = TRUE),]
      quantiles = quantiles[c(c(1:5),c((nrow(quantiles) - 5):nrow(quantiles))),]
      quantiles = cbind.data.frame(rownames(quantiles), quantiles$`50%`)
      tab_pop = tail(tab_prot,n=3)[1:2] ; tab_pop$parameter = paste(ex_between(tab_pop$parameter, "[", "]"),"*",sep=" ") ; colnames(tab_pop)=colnames(quantiles)
      quantiles = rbind(quantiles,tab_pop) ; quantiles = quantiles[order(quantiles[,2]),]
      tab=cbind.data.frame(tab,quantiles)
      colnames(tab)[(ncol(tab)-1):ncol(tab)] = c(paste(c("germplasm", "valeur"), variable2))
    }
  }
  
  variable3 = "poids.de.l.epi"
  if (variable3 %in% names(Model2) & !is.null(res_model2[[variable3]]$predict.past)) {
    name=c(name,"poids de l'épi")
    out = res_model2[[variable3]]$predict.past[[paste(person, year, sep = ":")]]$MCMC
    quantiles=NULL
    for(i in 1:ncol(out)){quantiles = rbind(quantiles,quantile( out[i,], probs=c(0, 0.05, 0.10, 0.50, 0.90, 0.95, 1)))}
    rownames(quantiles)=unlist(ex_between(names(out),"[",","))
    
    if( nrow(quantiles) > 0 ) {
      quantiles = quantiles[order(quantiles[,"50%"], decreasing = TRUE),]
      quantiles = quantiles[c(c(1:5),c((nrow(quantiles) - 5):nrow(quantiles))),]
      quantiles = cbind.data.frame(rownames(quantiles), quantiles$`50%`)
      tab_pop = tail(tab_pe,n=3)[1:2] ; tab_pop$parameter = paste(ex_between(tab_pop$parameter, "[", "]"),"*",sep=" ") ; colnames(tab_pop)=colnames(quantiles)
      quantiles = rbind(quantiles,tab_pop) ; quantiles = quantiles[order(quantiles[,2]),]
      tab=cbind.data.frame(tab,quantiles)
      colnames(tab)[(ncol(tab)-1):ncol(tab)] = c(paste(c("germplasm", "valeur"), variable3))
    }
  } 
  
  if (!is.null(dim(tab))){
    tab=tab[,-1]
    attributes(tab)$invert = FALSE
    out = list("table" = list("caption" = paste("Prédiction du passé pour les ",paste(name,collapse=", "),". A titre de comparaison, les meilleures populations pour ces variables cette année sur votre fermes sont notées avec *.",sep=""), "content" = tab)); OUT = c(OUT, out)  }else{
    out = list("text" = "
Il n'est pas possible de prédire ces valeurs car nous n'avons aucune données phénotypiques sur votre ferme pour cette année . 
"); OUT = c(OUT, out)
  }
  
  
  rm(list=setdiff(ls(), c("we_are_here","OUT","person","year")))
  
  # /!\ Get pdf ----------
  get.pdf(dir = paste(we_are_here, "/feedback_folder", sep = ""), 
          form.name = paste(person, year, sep = ":"), 
          LaTeX_head = "../tex_files/structure.tex", 
          LaTeX_body = OUT, 
          compile.tex = TRUE,
          color1 = "mln-green", 
          color2 = "mln-brown"
  )
  
}
