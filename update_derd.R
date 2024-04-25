# ============================================================================ #
# update_derd.R                                                                #
# ============================================================================ #
# UNDER LICENSE (GC Data Ecosystem, Government of Canada) #
# ------------------------------------------------------- #
# program contents                                        #
# 1. define() function                                    #
# 2. process() function                                   #
# 3. output() function                                    #
# 4. usage                                                #
# ------------------------------------------------------- #

# ------------------------------------------------------- #
# 1. define_en(input) and define_fr(input)                #
# ------------------------------------------------------- #
define_en <- function(input) {
 I <- data.frame(
  type = input$Type,
  subtype = input$SubType,
  entity = input$Label,
  description = input$Description,
  url = input$URL)

 I$category <- ""
 I$category[I$type == "Communities"] <- "Collaborative Initiatives"
 I$category[I$type == "Teams"] <- "Collaborative Initiatives"
 I$category[I$type == "Working Groups"] <- "Collaborative Initiatives"
 I$category[I$type == "Data Sources"] <- "Data Sources"
 I$category[I$type == "Knowledge Sharing Product"] <- "Educational Resources"
 I$category[I$type == "Learning Resources"] <- "Educational Resources"
 I$category[I$type == "Organizations"] <- "Educational Resources"
 I$category[I$type == "Committees"] <- "Governance bodies"
 I$category[I$type == "Policy Instruments"] <- "Policy instruments"
 I$category[I$type == "Projects and Initiatives"] <- "Programs and Projects"
 I$category[I$type == "Programs and Services"] <- "Programs and Projects"
 
 derd <- I
 return(derd)
}

define_fr <- function(input) {
 I <- data.frame(
  type = input$Type,
  subtype = input$SubType,
  entity = input$French.Entity.Full.Name,
  description = input$French.Description,
  url = input$French.URL)
 
 I$category <- ""
 I$category[I$type == "Teams"] <- "Initiatives collaboratives"
 I$category[I$type == "Communities"] <- "Initiatives collaboratives"
 I$category[I$type == "Working Groups"] <- "Initiatives collaboratives"
 I$category[I$type == "Policy Instruments"] <- "Instruments de politique"
 I$category[I$type == "Committees"] <- "Organes de gouvernance"
 I$category[I$type == "Projects and Initiatives"] <- "Programmes et Projets"
 I$category[I$type == "Programs and Services"] <- "Programmes et Projets"
 I$category[I$type == "Knowledge Sharing Product"] <- "Ressources éducatives"
 I$category[I$type == "Learning Resources"] <- "Ressources éducatives"
 I$category[I$type == "Organizations"] <- "Ressources éducatives"
 I$category[I$type == "Data Sources"] <- "Sources de données"
 
 I$type[I$type == "Committees"] <- "Comités"
 I$type[I$type == "Communities"] <- "Communautés"
 I$type[I$type == "Teams"] <- "Équipes"
 I$type[I$type == "Working Groups"] <- "Groupes de travail"
 I$type[I$type == "Policy Instruments"] <- "Instruments de politique"
 I$type[I$type == "Organizations"] <- "Organismes"
 I$type[I$type == "Programs and Services"] <- "Programmes et services"
 I$type[I$type == "Projects and Initiatives"] <- "Projets et initiatives"
 I$type[I$type == "Knowledge Sharing Product"] <- "Ressources éducatives"
 I$type[I$type == "Learning Resources"] <- "Ressources éducatives"
 I$type[I$type == "Data Sources"] <- "Sources de données"
 
 I$subtype[I$subtype == "Toolkits"] <- "Boîtes à outils"
 I$subtype[I$subtype == "Blogs"] <- "Blogs"
 I$subtype[I$subtype == "Newsletters"] <- "Bulletin"
 I$subtype[I$subtype == "Framework"] <- "Cadre"
 I$subtype[I$subtype == "Boards"] <- "Conseils d'administration"
 I$subtype[I$subtype == "Committees"] <- "Comités"
 I$subtype[I$subtype == "Communities"] <- "Communautés"
 I$subtype[I$subtype == "Councils"] <- "Conseils"
 I$subtype[I$subtype == "Courses"] <- "Cours"
 I$subtype[I$subtype == "Directives"] <- "Directives"
 I$subtype[I$subtype == "Documents"] <- "Documents"
 I$subtype[I$subtype == "Teams "] <- "Équipes"
 I$subtype[I$subtype == "Working Groups"] <- "Groupes de travail"
 I$subtype[I$subtype == "Legislation"] <- "Législation"
 I$subtype[I$subtype == "Guidelines"] <- "Lignes directrices"
 I$subtype[I$subtype == "Standards"] <- "Normes"
 I$subtype[I$subtype == "Podcasts"] <- "Podcasts"
 I$subtype[I$subtype == "Policies"] <- "Politiques"
 I$subtype[I$subtype == "Programs"] <- "Programmes"
 I$subtype[I$subtype == "Projects/initiatives"] <- "Projets et initiatives"
 I$subtype[I$subtype == "Resources"] <- "Ressources"
 I$subtype[I$subtype == "Data Sources"] <- "Sources de données"
 I$subtype[I$subtype == "Strategies"] <- "Stratégies"
 I$subtype[I$subtype == "Videos"] <- "Vidéos"

 I$subtype[I$subtype == "Learning provider"] <- "Fournisseur de formation"
 I$subtype[I$subtype == "Non-profit organization"] <- "Organisation à but non lucratif"
 I$subtype[I$subtype == "Other Government"] <- "Organization internationale"

 
 derd <- I
 return(derd)
}

# ------------------------------------------------------- #
# 2. process(derd_en) and process(derd_fr)                #
# ------------------------------------------------------- #
process <- function(derd) {
D <- as.data.frame(derd)

## replace apostrophe (`)
D$entity <- gsub("`", "'", D$entity)
D$description <- gsub("`", "'", D$description)

## replace apostrophe (â€™)
D$entity <- gsub("â€™", "'", D$entity)
D$description <- gsub("â€™", "'", D$description)

## replace hyphen (â€”)
D$entity <- gsub("â€”", "–", D$entity)
D$description <- gsub("â€”", "–", D$description)

## replace star (⍟)
D$entity <- gsub("⍟", "", D$entity)
D$description <- gsub("⍟", "", D$description)

## replace invalid character ( \v)
D$entity <- gsub(" \v", " ", D$entity)
D$description <- gsub("\v", " ", D$description)

## replace new lines
D$entity <- gsub("\n", " ", D$entity)
D$description <- gsub("\n", " ", D$description)

# replace vertical slash (|)
D$entity <- gsub("\\|", "/", D$entity)
D$description <- gsub("\\|", "/", D$description)

## replace and character slash (&)
D$entity <- gsub("&", " and ", D$entity)
D$description <- gsub("&", " and ", D$description)

## replace bullet (·)
D$entity <- gsub("·", "", D$entity)
D$description <- gsub("·", "", D$description)

## replace bullet (•)
D$entity <- gsub("•", "", D$entity)
D$description <- gsub("•", "", D$description)

## replace triple spaces
D$entity <- gsub("   ", " ", D$entity)
D$description <- gsub("   ", " ", D$description)

## replace double spaces
D$entity <- gsub("  ", " ", D$entity)
D$description <- gsub("  ", " ", D$description)

output <- c("")

output <- rbind(output,
paste(c(
"__NOTOC__",
"\n",
"\n"),
collapse = ""))

categories <- data.frame(table(D$category))[,1]
categories <- as.character(categories)

for (k in 1:length(categories)) {

output <- rbind(output,
paste(c(
"<div id = '",
gsub("é","e",gsub(" ","_",categories[k])),
"' name = '",
gsub("é","e",gsub(" ","_",categories[k])),
"'>",
"</div>",
"\n"),
collapse = ""))

output <- rbind(output,
paste(c(
"== ",
categories[k],
" ==",
"\n",
"\n"),
collapse = ""))

subtypes <- data.frame(table(D$subtype[D$category == categories[k]]))[,1]
subtypes <- as.character(subtypes)

output <- rbind(output,
paste(c(
"{{Collapsible list",
"\n",
"| title = <br />",
"\n",
"| bullets =",
"\n",
"\n",
"\n"),
collapse = ""))

for (j in 1:length(subtypes)) {

output <- rbind(output,
paste(c(
"| <br /><big>"),
collapse = ""))

if (subtypes[j] != categories[k]) {
output <- rbind(output,
paste(c(
"'''",
subtypes[j],
"'''"),
collapse = ""))
}

output <- rbind(output,
paste(c(
"</big>"),
"<ol>",
"\n",
collapse = ""))

entities <- data.frame(table(D$entity[D$subtype == subtypes[j]]))[,1]
entities <- as.character(entities)

output <- rbind(output,
paste(c(
"{{columns-list|colwidth=35em|",
"\n"),
collapse = ""))

for (i in 1:length(entities)) {

output <- rbind(output,
paste(c(
": '''",
"<html><a href=",
D$url[D$entity == entities[i]],
">",
D$entity[D$entity == entities[i]],
"</a></html>",
"''' - "),
collapse = ""))

output <- rbind(output,
paste(c(
D$description[D$entity == entities[i]],
"\n"),
collapse = ""))

i <- i + 1

}

output <- rbind(output,
paste(c(
"\n",
"\n",
"}}",
"\n"),
collapse = ""))

output <- rbind(output,
paste(c(
"</ol>",
"\n",
"\n"),
collapse = ""))

rm(i)
rm(entities)

j <- j + 1

}

output <- rbind(output,
paste(c(
"}}",
"\n",
"\n"),
collapse = ""))

rm(j)
rm(subtypes)

k <- k + 1

}

rm(k)
rm(categories)

return(output)
}

# ------------------------------------------------------- #
# 3. output_en(derd_output_en) and                        #
#    output_fr(derd_output_fr)                            #
# ------------------------------------------------------- #
output_en <- function(derd_output_en) {
cat(derd_output_en,
file = "~/code/derd-updater/derd_update_en.txt", sep = "")
}

output_fr <- function(derd_output_fr) {
cat(derd_output_fr,
file = "~/code/derd-updater/derd_update_fr.txt", sep = "")
}

# ------------------------------------------------------- #
# 4. use                                                  #
# ------------------------------------------------------- #
# 4.0. input read-in data
# ------------------------------------------------------- #
input <- read.csv(
"~/code/derd-updater/2024_04_16_gcderd.csv", header = TRUE)

# ------------------------------------------------------- #
# 4.1. define directory variables and features
# ------------------------------------------------------- #
derd_en <- define_en(input)
derd_fr <- define_fr(input)

# ------------------------------------------------------- #
# 4.2. process directory update
# ------------------------------------------------------- #
derd_output_en <- process(derd_en)
derd_output_fr <- process(derd_fr)

# ------------------------------------------------------- #
# 4.3. output directory update output to file
# ------------------------------------------------------- #
output_en(derd_output_en)
output_fr(derd_output_fr)

# ============================================================================ #
# updated on April 24, 2024                                                    #
# ============================================================================ #
