#########################
##### Final Soc 226 #####
#########################

#######################
##### Environment #####
#######################
library(igraph)
library(tidyverse)
library(dplyr)
library(plyr)
library(ggplot2)
library(doParallel)
library(foreach)
set.seed(1)
setwd("~/Documents/CompSoc_Grad/Fall20/Soc226/Final_Project")

#############################
##### Load & Clean Data #####
#############################

# Edges (general tech skill category, not specific software (no attributes))
# Tech skills & Occupations data
tech_job_csv <- read.csv("tech_skills.csv", header = TRUE)
tech_job_edges <- as.data.frame(tech_job_csv[,c(-1,-3,-4,-6)])

#remove duplicate edges for tech skill category
tech_job_edges <- unique(tech_job_edges[c("Title","Commodity.Title")])
rownames(tech_job_edges) <- 1:length(tech_job_edges$Title)
## Vertex attributes
tech.graph <- graph_from_data_frame(tech_job_edges, directed = FALSE)

#Industry, supply, occupation data
industry_supply_csv <- read.csv("industry_supply_job.csv", header = TRUE)
industry_supply_csv$Occupation <- as.character(industry_supply_csv$Occupation)
#check that this is in the right order
tech_graph_names <- as.data.frame(unique(tech_job_edges$Title))
colnames(tech_graph_names) <- c("Occupation")

industry_supply_frame <- tech_graph_names %>% left_join(industry_supply_csv, by = c("Occupation"))
industry_supply_frame$Projected.Growth..2018.2028. <- as.character(industry_supply_frame$Projected.Growth..2018.2028.)
industry_supply_frame <- industry_supply_frame[,c(-2)]
## Vertex names for subsetting only nodes with attribute data
att_node_names <- industry_supply_frame[which(!is.na(industry_supply_frame$Projected.Job.Openings..2018.2028.)),1]
## For imputation, not subset
industry_supply_frame$Projected.Growth..2018.2028. <- ifelse(industry_supply_frame$Projected.Growth..2018.2028. == "", "Average", industry_supply_frame$Projected.Growth..2018.2028.)
industry_supply_frame$Projected.Growth..2018.2028. <- ifelse(is.na(industry_supply_frame$Projected.Growth..2018.2028.), "Average", industry_supply_frame$Projected.Growth..2018.2028.)
industry_supply_frame$Industries <- as.character(industry_supply_frame$Industries)
industry_supply_frame$Industries <- ifelse(is.na(industry_supply_frame$Industries), "", industry_supply_frame$Industries)

# note that supply growth and job opening are both projected
colnames(industry_supply_frame) <- c("Occupation", "Supply Growth", "Job Openings", "Industries")

## no data for industry or supply data for 291 entries
## impute and try subset for homophily test;

### What to do with NA's in the graph representation
## weird problem with igraph interpretation of frame?

# Tech Skills Bipartite Plots ---------------------------------------------
# Jobs = FALSE, Skills = TRUE
V(tech.graph)$type <- V(tech.graph)$name %in% tech_job_edges[,2]
V(tech.graph)$color <- case_when(
  V(tech.graph)$type == FALSE ~ "blue",
  V(tech.graph)$type == TRUE ~ "red")

## Graph Visualization
#might need to change type back to logicals on this one
plot(tech.graph,
     layout = layout_as_bipartite(tech.graph, hgap = 3000, maxiter = 100),
     vertex.size=5,
     vertex.label=NA,
     vertex.label.cex=1, 
     vertex.color=V(tech.graph)$color,
     edge.arrow.size=0,
     edge.curved=.1,
     main = "Bipartite Graph of Technology Skills and Occupations")

plot(tech.graph,
     layout = layout.drl(tech.graph),
     vertex.size=5,
     vertex.label=NA,
     vertex.label.cex=1, 
     vertex.color=V(tech.graph)$color,
     edge.arrow.size=0,
     edge.curved=.1,
     main = "Bipartite Graph of Technology Skills and Occupations")

# Misc --------------------------------------------------------------------
# Reminder that skills are tech skills only here
V(tech.graph)$type <- case_when(
  V(tech.graph)$type == FALSE ~ "JOB",
  V(tech.graph)$type == TRUE ~ "SKILL")

vcount(tech.graph)
ecount(tech.graph)

## Create dataframe of occupations and all related skills
## for any non-network processing
job_list <- lapply(ego(tech.graph, nodes = which(V(tech.graph)$type == "JOB")), function(x) as.character(x$name))
head(job_list, n = 2)
job_frame <- as.data.frame(ldply(job_list, rbind))
# retrieve skills for a given occupations
job_frame[job_frame$`1` == "Computer User Support Specialists",]

## Create dataframe of skills and all related occupations
skill_list <- lapply(ego(tech.graph, nodes = which(V(tech.graph)$type == "SKILL")), function(x) as.character(x$name))
head(skill_list, n = 2)
skill_frame <- as.data.frame(ldply(skill_list, rbind))
# retrieve jobs for a given skill
skill_frame[skill_frame$`1` == "Backup or archival software",]

### Descriptives
# Occupations -- 974
sum(V(tech.graph)$type == "JOB")
# Skills -- 127
sum(V(tech.graph)$type == "SKILL")
# figure out how to subset metrics by attribute

##############################
##### Research Questions #####
##############################

# Initial Research Questions ----------------------------------------------

# Degree Analyses
V(tech.graph)$degree <- degree(tech.graph)
# Average skills/job
# 11.93943 (computer-based careers have up to half of all tech skills in network)
mean(degree(tech.graph, which(V(tech.graph)$type == "JOB")))
head(sort(degree(tech.graph, which(V(tech.graph)$type == "JOB")), decreasing = TRUE))
# Average jobs/skill
# 91.56693
mean(degree(tech.graph, which(V(tech.graph)$type == "SKILL")))
head(sort(degree(tech.graph, which(V(tech.graph)$type == "SKILL")), decreasing = TRUE), n = 30)

## Skill and occupation diversity -------------------------------------------------

## Which skills have the greatest diversity in its applications?
# Top 5: Spreadsheet software (881), Word processing software (828),
# Office suite software (781), Data base user interface and query software (736)
# Electronic mail software (651)

skill_tib <-tribble(
    ~Rank, ~Skill, ~Applicable_Occupations,
    1, "Spreadsheet software", 881,
    2, "Word processing software", 828,
    3, "Office suite software", 781,
    4, "Data base user interface software", 736,
    5, "Electronic mail software", 651
)

## Least?
# Several -- ie audit software, aviation ground support software, printer driver software
# Manufacturing execution system MES software -- all only apply to one occupation
least_skill_tib <-tribble(
  ~Rank, ~Skill, ~Applicable_Occupations,
  1, "Wireless software", 1,
  2, "Audit software", 1,
  3, "Contact center software", 1,
  4, "Aviation ground support software", 1,
  5, "Manufacturing execution system software", 1
)

## Which jobs (can) require the most skills?
# Top 5: Computer User Support Specialists (64), Network and Computer Systems Administrators (62),
# Computer Network Architects (61), Computer Systems Analysts (60)
# Computer Systems Engineers/Architects (58)

job_tib <-tribble(
  ~Rank, ~Occupation, ~Applicable_Skills,
  1, "Computer User Support Specialists", 64,
  2, "Network and Computer Systems Administrators", 62,
  3, "Computer Network Architects", 61,
  4, "Computer Systems Analysts", 60,
  5, "Computer Systems Engineers/Architects", 58
)

## What about after the computer people?
# General and Operations Managers (51), Management Analysts (51), -- a few tech jobs in between --
# Business Intelligence Analysts (48), Medical and Health Services Managers (40), 
# Sales Representatives, Wholesale and Manufacturing, Except Technical and Scientific Products (40)
job_tib <-tribble(
  ~Rank, ~Occupation, ~Applicable_Skills,
  1, "General and Operations Managers", 51,
  2, "Management Analysts", 51,
  3, "Business Intelligence Analysts", 48,
  4, "Medical and Health Services Managers", 40,
  5, "Sales Representatives", 40
)

## Least?
#Music Composers and Arrangers, Dishwashers, Musical Instrument Repairers and Tuners
# Food Cooking Machine Operators and Tenders
least_job_tib <-tribble(
  ~Rank, ~Occupation, ~Applicable_Skills,
  1, "Music Composers and Arrangers", 1,
  2, "Dishwashers", 1,
  3, "Musical Instrument Repairers and Tuners", 1,
  4, "Food Cooking Machine Operators and Tenders", 1,
  5, "Nurse Midwives", 2
)

# Projections -------------------------------------------------------------

## Skills used in the same jobs; jobs requiring the same skills
# First, we need make two projections of our bipartite graph
# One for skills, where edge weights represent number of shared job's requiring any 2 skills
# One for jobs, where edges represent the number of shared skills required by any 2 jobs

# change types back to logicals
V(tech.graph)$type <- case_when(
  V(tech.graph)$type == "JOB" ~ FALSE,
  V(tech.graph)$type == "SKILL" ~ TRUE)
V(tech.graph)$type <- as.logical(V(tech.graph)$type)
tech_projections <- bipartite.projection(tech.graph)
job_projection_tech <- tech_projections[[1]]
skill_projection_tech <- tech_projections[[2]]
# Now lets plot both of these (try a few for each)
plot(job_projection_tech,
     layout = layout_with_lgl(job_projection_tech),
     vertex.size = 5,
     vertex.label=NA,
     main="Bipartite Projections of Technology Skills onto Occupations")

plot(skill_projection_tech,
     layout = layout_with_lgl(skill_projection_tech),
     vertex.size = 5,
     vertex.label=NA,
     vertex.col = "red",
     main="Bipartite Projections of Technology Occupations onto Skills")

# Job Projection Network --------------------------------------------------

## Assign vertex attributes to job projection network

## Job Projection Descriptives
vcount(job_projection_tech)
ecount(job_projection_tech)
# super high graph density -- most jobs share at least one skill with
# almost every job
graph.density(job_projection_tech)

## set vertex attributes
V(job_projection_tech)$supply <- industry_supply_frame$`Supply Growth`
V(job_projection_tech)$job_openings <- industry_supply_frame$`Job Openings`

## try giving an empty list as an attribute
## industry read function
industry_attr <- vector("list", 974)
these_industries <- c()
make_industry_list <- function(x){
  if(x == ""){
    return(as.list(NA))
  }
  these_industries <- strsplit(x, " ?\\(\\d\\d?\\d%\\),? ?")
  industry_list <- as.list(these_industries[[1]])
  return(industry_list)
}
for(val in 1:length(industry_supply_frame$Occupation)){
  industry_attr[[val]] <- make_industry_list(industry_supply_frame[val,4])
}

V(job_projection_tech)$industries <- industry_attr

# Want to find the highest weighted edges and greatest total degrees
sort(E(job_projection_tech)$weight, decreasing = TRUE)
ends(job_projection_tech, which(E(job_projection_tech)$weight > 45), names = TRUE)

# Top 5: no surprise, the computer-based careers by far share the most required skills
# Management analysts, business intelligence analysts, info tech project managers,
# General and Operations Managers

# Huge number of edges with 1 as weight (36916). 1 skill shared between another job
# is extremely common, explains density. Let's see if this is a common skill and let's get
# the edge weight distribution
sort(E(job_projection_tech)$weight, decreasing = FALSE)
hist(E(job_projection_tech)$weight)

# Edge weight distribution
edge_weight_list <- rep(0, length(unique(E(job_projection_tech)$weight)))
names(edge_weight_list) <- unique(E(job_projection_tech)$weight)
for (edge in E(job_projection_tech)$weight){
  edge_weight_list[as.character(edge)] <- edge_weight_list[as.character(edge)] + 1
}
edge_weight_list <- edge_weight_list / ecount(job_projection_tech)
plot(as.integer(names(edge_weight_list)[order(as.integer(names(edge_weight_list)))]), edge_weight_list[order(as.integer(names(edge_weight_list)))], pch=19, xlab = "Edge Weight", ylab = "Frequency", main = "Edge Weight Distribution", col = "blue")

##5.244
mean(E(job_projection_tech)$weight)

# Degree Analyses ---------------------------------------------------------

### Which skills are fundamental in todays job market?
# Find out what skills are driving these low-edge-weight ties (36916 edges with weight 1)
# 36 / 127 skills account for this
## This shows us some underlying skills that are fundamental to todays labor market
## answers the question: which skills are shared by the jobs that are least in common
## Lots of ways to view this question, but this is just some quick insight
one_shared_skill <- ends(job_projection_tech, which(E(job_projection_tech)$weight == 1), names = TRUE)
one_shared_skill_dict <- c("")
names(one_shared_skill_dict) <- c("")
for (val in 1:length(one_shared_skill[,1])){
  this_skill <- shortest_paths(tech.graph, from = one_shared_skill[val,1], to = one_shared_skill[val,2])$vpath[[1]][[2]]$name
  if(!is.na(one_shared_skill_dict[this_skill])){
    one_shared_skill_dict[this_skill] <- as.integer(one_shared_skill_dict[this_skill]) + 1
  }else{
    one_shared_skill_dict[this_skill] <- 1
  }
}
## delete first blank element
one_shared_skill_dict <- one_shared_skill_dict[-1]
these_names <- names(one_shared_skill_dict)
one_shared_skill_dict <- as.integer(one_shared_skill_dict)
names(one_shared_skill_dict) <- these_names
sort(one_shared_skill_dict, decreasing = TRUE)

# Total degrees -- tells us who shares the most total skills with other jobs 
# (greatest flexibility in job-skill market)
# double counts skills when shared with unique jobs
V(job_projection_tech)$degree_weighted <- strength(job_projection_tech)
V(job_projection_tech)$degree <- degree(job_projection_tech)
#avg degree = 4911.01
mean(V(job_projection_tech)$degree_weighted)
#mean unweighted degree = 936.4579
mean(V(job_projection_tech)$degree)
head(sort(V(job_projection_tech)$degree_weighted, decreasing = TRUE), n = 10)
# Top 5: Computer User Support Specialists (10518), General and Operations Managers (10516),
# Software Quality Assurance Engineers and Testers (10027), Information Tech Project Managers (9990),
# Sales Representatives, Wholesale and Manufacturing, Except Technical and Scientific Products (9866)
vertex_attr(job_projection_tech, "name", index = which(V(job_projection_tech)$degree_weighted == 10516))

# Diameter = 3 (always able to avoid high weight edges because of density)
# Avg shortest path -- 1.037558
diameter(job_projection_tech)
get.edge.ids(job_projection_tech, c("Physicists", "Mine Shuttle Car Operators"))
edge_attr(job_projection_tech, name = "weight", index = 100883)

mean_distance(job_projection_tech)

## Degree distribution (looks like bell curve)
hist(V(job_projection_tech)$degree_weighted)
graph.strength.distribution <- function (graph, ..)
{
  if (!is.igraph(graph)) {
    stop("Not a graph object")
  }
  # graph.strength() instead of degree()
  cs <- graph.strength(graph)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$density
  res <- hi
  res
}
weighted_dist <- graph.strength.distribution(job_projection_tech)
plot(weighted_dist)
plot(degree.distribution(job_projection_tech))


# Community Detection (non-overlapping) -----------------------------------

# Community detection (think of explaination for overlapping/not)

### Hypothesis: nothing solid will show up because its too dense
### 1 way: try subsetting without the most common or least sparse skills
## might have to set number of communities to save computer
### Edge-betweenness-- too comp intensive
ncom <- 10

### Fast n' greedy
job_prj_coms_fg <- cluster_fast_greedy(job_projection_tech)
job_prj_coms_fg %>%
  sizes
modularity(job_projection_tech, job_prj_coms_fg$membership)
cut_at(job_prj_coms_fg, no = 2)
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(2)[cut_at(job_prj_coms_fg, no = 2)],
  mark.groups = cut_at(job_prj_coms_fg, no = 2) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "fastgreedy"
)

### Walktrap
set.seed(1)
job_prj_coms_wt <- walktrap.community(job_projection_tech, steps = 2)
job_prj_coms_wt %>%
  sizes
modularity(job_projection_tech, job_prj_coms_wt$membership)
cut_at(job_prj_coms_wt, no = ncom)
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(ncom)[cut_at(job_prj_coms_wt, no = ncom)],
  mark.groups = cut_at(job_prj_coms_wt, no = ncom) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "walktrap"
)

### InfoMap
set.seed(1)
job_prj_coms_im <- infomap.community(job_projection_tech, nb.trials = 100)
job_prj_coms_im %>%
  sizes
modularity(job_projection_tech, job_prj_coms_im$membership)
set.seed(1)
plot(job_prj_coms_im,
     job_projection_tech,
     vertex.label = NA,
     main = "InfoMap")

# comparing them all ----
par(mfrow = c(1,3))
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(ncom)[cut_at(job_prj_coms_fg, no = ncom)],
  mark.groups = cut_at(job_prj_coms_fg, no = ncom) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "fastgreedy"
)
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(ncom)[cut_at(job_prj_coms_wt, no = ncom)],
  mark.groups = cut_at(job_prj_coms_wt, no = ncom) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "walktrap"
)
set.seed(1)
plot(job_prj_coms_im,
     job_projection_tech,
     vertex.label = NA,
     main = "InfoMap")

modularity(job_projection_tech, cut_at(job_prj_coms_fg, no = ncom))
modularity(job_projection_tech, cut_at(job_prj_coms_wt, no = ncom))
modularity(job_projection_tech, job_prj_coms_im$membership)

par(mfrow = c(1,3))
set.seed(1)
ncom <- 6
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(ncom)[cut_at(job_prj_coms_fg, no = ncom)],
  mark.groups = cut_at(job_prj_coms_fg, no = ncom) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "fastgreedy"
)
set.seed(1)
plot(
  job_projection_tech,
  vertex.label = NA,
  vertex.color = rainbow(ncom)[cut_at(job_prj_coms_wt, no = ncom)],
  mark.groups = cut_at(job_prj_coms_wt, no = ncom) %>%
    setNames(names(V(job_projection_tech))) %>%
    split(names(.), .),
  main = "walktrap"
)

### community detection -- check if the jobs with the least and most tech skills are in the
### same community

### Question: how to make any sense out of communities?


# Supply homophily --------------------------------------------------------

## Supply homophily ###

assortativity_nominal(
  job_projection_tech,
  as.factor(V(job_projection_tech)$supply),
  directed = FALSE
)

assortativity(
  job_projection_tech,
  graph.strength(job_projection_tech),
  directed = FALSE
)

# chi squared contingency table (entries = cumalitive weights)
observed_counts <- as.list(c(rep(0,6)))
names(observed_counts) <- unique(V(job_projection_tech)$supply)
# observed counts -- 40 mins
for(edge in E(job_projection_tech)){
  node_one <- ends(job_projection_tech, es = edge, names = TRUE)[1,1]
  node_two <- ends(job_projection_tech, es = edge, names = TRUE)[1,2]
  supply_one <- V(job_projection_tech)$supply[which(V(job_projection_tech)$name == node_one)]
  supply_two <- V(job_projection_tech)$supply[which(V(job_projection_tech)$name == node_two)]
  if(supply_one == supply_two){
    observed_counts[[supply_one]] <- observed_counts[[supply_one]] + edge_attr(job_projection_tech, name = "weight", index = edge)
  }
}
# expected counts -- expected # of edges between * expected weight
expected_counts <- as.list(c(rep(0,6)))
names(expected_counts) <- unique(V(job_projection_tech)$supply)
mean_weight <- mean(E(job_projection_tech)$weight)
supply_sum <- 0
for(val in names(expected_counts)){
  supply_sum <- length(which(V(job_projection_tech)$supply == val))
  expected_counts[[val]] <- (supply_sum*(supply_sum - 1)/2)*graph.density(job_projection_tech)*mean_weight
}

contingency_table <- as.data.frame(observed_counts)
contingency_table <- rbind(contingency_table, as.data.frame(expected_counts))

chisq <- chisq.test(contingency_table)

positive_assort <- contingency_table[,c(-1,-4,-6)]
chisq_pos <- chisq.test(positive_assort)
negative_assort <- contingency_table[,c(-2,-3,-5)]
chisq_neg <- chisq.test(negative_assort)

## try above without "average" imputation; subset instead
# chi squared contingency table (entries = cumalitive weights)
job_proj_sub <- induced_subgraph(job_projection_tech, which(V(job_projection_tech)$name %in% att_node_names))

observed_counts_sub <- as.list(c(rep(0,6)))
names(observed_counts_sub) <- unique(V(job_proj_sub)$supply)
# observed counts -- ? mins
for(edge in E(job_proj_sub)){
  node_one <- ends(job_proj_sub, es = edge, names = TRUE)[1,1]
  node_two <- ends(job_proj_sub, es = edge, names = TRUE)[1,2]
  supply_one <- V(job_proj_sub)$supply[which(V(job_proj_sub)$name == node_one)]
  supply_two <- V(job_proj_sub)$supply[which(V(job_proj_sub)$name == node_two)]
  if(supply_one == supply_two){
    observed_counts_sub[[supply_one]] <- observed_counts_sub[[supply_one]] + edge_attr(job_proj_sub, name = "weight", index = edge)
  }
}
# expected counts -- expected # of edges between * expected weight
expected_counts_sub <- as.list(c(rep(0,6)))
names(expected_counts_sub) <- unique(V(job_proj_sub)$supply)
mean_weight_sub <- mean(E(job_proj_sub)$weight)
supply_sum_sub <- 0
for(val in names(expected_counts_sub)){
  supply_sum_sub <- length(which(V(job_proj_sub)$supply == val))
  expected_counts_sub[[val]] <- (supply_sum_sub*(supply_sum_sub - 1)/2)*graph.density(job_proj_sub)*mean_weight_sub
}

contingency_table_sub <- as.data.frame(observed_counts_sub)
contingency_table_sub <- rbind(contingency_table_sub, as.data.frame(expected_counts_sub))

chisq_sub <- chisq.test(contingency_table_sub)

positive_assort_sub <- contingency_table_sub[,c(-1,-4,-6)]
chisq_pos_sub <- chisq.test(positive_assort_sub)
negative_assort_sub <- contingency_table_sub[,c(-2,-3,-5)]
chisq_neg_sub <- chisq.test(negative_assort_sub)

## retry default nominal test for sharing at least one skill
## unweighted assortativity
assortativity_nominal(
  job_proj_sub,
  as.factor(V(job_proj_sub)$supply),
  directed = FALSE
)

# Skill Projection Network ------------------------------------------------

## Skill Projection Descriptives
vcount(skill_projection_tech)
ecount(skill_projection_tech)
# still very high density, but not nearly as high as the job net
graph.density(skill_projection_tech)
# Want to find the highest weighted edges and greatest total degrees
sort(E(skill_projection_tech)$weight, decreasing = TRUE)
ends(skill_projection_tech, which(E(skill_projection_tech)$weight > 576), names = TRUE)
# Top 5: Data base user interface and query software, spreadsheet software, office suite software,
# word processing software, electronic mail software, presentation software

# Total degrees -- tells us total number of jobs a skill shares with all other skills,
# double counts skills when shared with unique jobs
V(skill_projection_tech)$degree_weighted <- strength(skill_projection_tech)
#avg degree = 1590.157
mean(V(skill_projection_tech)$degree_weighted)
head(sort(V(skill_projection_tech)$degree_weighted, decreasing = TRUE), n = 10)
# Top 5: Spreadsheet software, Word processing software, Office suite software,
# Data base user interface and query software, Electronic mail software
# Software Quality Assurance Engineers and Testers (10027), Information Tech Project Managers (9990),
# Sales Representatives, Wholesale and Manufacturing, Except Technical and Scientific Products (9866)
vertex_attr(skill_projection_tech, "name", index = which(V(skill_projection_tech)$degree_weighted == 8568))

# Sample Projection -------------------------------------------------------

visual_net <- graph_from_data_frame(
  tribble(
    ~a, ~b,
    1, 2,
    3, 2,
    3, 4,
    5, 2,
    5, 4,
    5, 6
  ), directed = FALSE,
)
V(visual_net)$type <- c(1, 1, 1, 2, 2, 2)
V(visual_net)$color <- c("red", "red", "red", "blue", "blue", "blue")
plot_mat <- as.matrix(tribble(
  ~a, ~b,
  2, 6,
  2, 4,
  2, 2,
  4, 6,
  4, 4,
  4, 2
))
V(visual_net)$name <- (c("Receptionist", "Analyst", "Graphic Designer", "Word processing software", "Presentation Software", "Photoshop"))
plot(visual_net, vertex.label.dist = 3, layout = plot_mat, edge.width = 3)

visual_net_2 <- graph_from_data_frame(
  tribble(
    ~a, ~b,
    1, 2,
    2, 3,
  ), directed = FALSE,
)
V(visual_net_2)$type <- c(1, 1, 1)
V(visual_net_2)$color <- c("red", "red", "red")
plot_mat_2 <- as.matrix(tribble(
  ~a, ~b,
  2, 2,
  2, 4,
  2, 6
))
V(visual_net_2)$name <- (c("Graphic Designer", "Analyst", "Receptionist"))
plot(visual_net_2, vertex.label.dist = c(7, 4, 6), layout = plot_mat_2, curved = TRUE, edge.width = c(6,3), edge.color = "black", vertex.label.degree = -pi/16)

edge_table <-  as.data.frame(tribble(
  ~Occupation_2, ~Shared_Skills, ~Occupation_2, ~Edge_Weight,
  "Receptionist", "Word Processing", "Analyst", 1,
  "Receptionist", "Word Processing", "Graphic Designer", 1,
  "Analyst", "Word Processing, Presentation Software", "Graphic Designer", 2))



# Sample homophily --------------------------------------------------------

# Sample homophily
visual_net_3 <- graph_from_data_frame(
  tribble(
    ~a, ~b,
    1, 2,
    2, 3,
  ), directed = FALSE,
)
visual_net_3 <- delete_edges(visual_net_3, edges = E(visual_net_3)[1])
V(visual_net_3)$type <- c(1, 2, 2)
V(visual_net_3)$color <- c("blue", "red", "red")
plot_mat_3 <- as.matrix(tribble(
  ~a, ~b,
  2, 2,
  3, 3,
  4, 2
))
V(visual_net_3)$name <- (c("Decline", "Faster than average", "Faster than average"))
plot(visual_net_3, vertex.label.dist = c(4, 7, 7), layout = plot_mat_3, curved = TRUE, edge.color = "black", vertex.label.degree = -pi/16)

visual_net_4 <- graph_from_data_frame(
  tribble(
    ~a, ~b,
    1, 2,
    2, 3,
    1, 3
  ), directed = FALSE,
)
V(visual_net_4)$type <- c(1, 2, 2)
V(visual_net_4)$color <- c("blue", "red", "red")
plot_mat_4 <- as.matrix(tribble(
  ~a, ~b,
  2, 2,
  3, 3,
  4, 2
))
V(visual_net_4)$name <- (c("Decline", "Faster than average", "Faster than average"))
plot(visual_net_4, vertex.label.dist = c(4, 6, 6), layout = plot_mat_4, edge.width = c(1, 3, 1), curved = TRUE, edge.color = "black", vertex.label.degree = -pi/8, edge.label = c(1, 3, 1), edge.label.y = c(0.1, 0.1, -0.85), edge.label.x = c(-0.6,0.6,0), edge.label.cex = 2)

