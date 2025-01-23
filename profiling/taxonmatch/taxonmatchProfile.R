"
Profiling the Taxonmatch function from pdindicatoR
"
install.packages("profvis")
library("profvis")
library("ape")
library("rotl")
library("parallel")
library("MASS")
install.packages('foreach')
library("foreach")
install.packages("doParallel")
library("doParallel")
#Load in the test tree that is included in the package
tree_path <- system.file("extdata", "Fagales_species.nwk",
                         package = "pdindicatoR")
tree <- read.tree(tree_path)


profvis({
  taxonmatch <- function(tree) {
    tree_labels <- tree$tip.label
    ########################
    if (any(stringr::str_detect(tree_labels, "ott\\d+")) == FALSE) {
      taxa <- rotl::tnrs_match_names(tree_labels)
    } 
    ########################
    else {
      taxa <- data.frame(tree_labels)
      colnames(taxa)[1] <- "ott_id"
    }

    taxa[, "gbif_id"] <- NA
    i <- 1
    
    #######################################################
    for (id in taxa$ott_id) {
      if (is.na(id) == FALSE) { 
        tax_info <- rotl::taxonomy_taxon_info(id)
        for (source in tax_info[[1]]$tax_sources) { 
          if (grepl("gbif", source, fixed = TRUE)) {
            gbif <- stringr::str_split(source, ":")[[1]][2]
            taxa[i, ]$gbif_id <- gbif
          }
        }
      }
      i <- i + 1
    }
    #######################################################
    
    taxa$gbif_id <- as.integer(taxa$gbif_id)
    
    original_df <- data.frame(
      orig_tiplabel = unique(tree_labels),
      search_string = tolower(unique(tree_labels)))
    
    matched_result <- merge(taxa, original_df, by = "search_string", all.x = TRUE)
    return(matched_result)
  }
  matched <- taxonmatch(tree)
})

"
Two biggest computational bottlenecks
1) tax_info <- rotl::taxonomy_taxon_info(id) is the biggest bottleneck
2) taxa <- rotl::tnrs_match_names(tree_labels) is the second biggest bottleneck

(1) is a for loop that is applied over all ID's that are present in the extracted data.
This can potentially be simplified by substituting the for loop with an apply function

(2) is not a for loop, any additional improvement of the code speed can only be achieved 
through potential parallelization of the code?
"
parMatchNames <- function(num_cores){
  tip_labels<-tree$tip.label
  chunk_indices <- cut(seq_along(tip_labels), breaks = num_cores, labels = FALSE)
  split_list <- split(tip_labels, chunk_indices)
  matched <- parLapply(computeCluster, split_list, rotl::tnrs_match_names)
  return (matched)
}

print("Serial code timing")
system.time(rotl::tnrs_match_names(tree$tip.label))
"
   user  system elapsed 
   0.36    0.05    9.73 
"
computeCluster <- makeCluster(4, type="PSOCK")
clusterExport(computeCluster, list("tnrs_match_names"))
print("Parallellized code timing: 4 cores")
system.time(parMatchNames(4))
"
   user  system elapsed 
   0.00    0.00    6.64 
"

profvis({
  rotl::tnrs_match_names(tree$tip.label)
  parMatchNames <- function(num_cores){
    tip_labels<-tree$tip.label
    chunk_indices <- cut(seq_along(tip_labels), breaks = num_cores, labels = FALSE)
    split_list <- split(tip_labels, chunk_indices)
    matched <- parLapply(computeCluster, split_list, rotl::tnrs_match_names)
    return (matched)
  }
  computeCluster <- makeCluster(4, type="PSOCK")
  clusterExport(computeCluster, list("tnrs_match_names"))
  parMatchNames(4)
})

"
Using some simple parallellization with 4 cores we are able to reduce the time
from  6000ms to 4000ms. In addition to this memory usage appears to be significantly
reduced when utilizing the parallellized version
"



taxa <- rotl::tnrs_match_names(tree$tip.label)
taxa[, "gbif_id"] <- NA

profvis({
  getTaxonInfo <- function(taxa){
    for (id in taxa$ott_id) {
      if (is.na(id) == FALSE) { 
        tax_info <- rotl::taxonomy_taxon_info(id)
      }
    }
    tax_info
    
    ott_labels<-taxa$ott_id[!is.na(taxa$ott_id)]
    
    tax_info2<-lapply(ott_labels, rotl::taxonomy_taxon_info)
    
    chunk_indices <- cut(seq_along(ott_labels), breaks = 4, labels = FALSE)
    split_ott <- split(ott_labels, chunk_indices)
    
    computeCluster <- makeCluster(4, type="PSOCK")
    clusterExport(computeCluster, list("taxonomy_taxon_info", "split_ott"))
    
    tax_info3 <- parLapply(computeCluster, split_ott, taxonomy_taxon_info)
  }
  getTaxonInfo(taxa)
})

"
Substituting the for loop with an lapply function reduces computational cost by a
fraction, i.e. 61190 ms vs 61730 ms

Implementing parallelization offers a significant reduction in computational time,
i.e. 15640 ms vs [61960 ms(lapply), 61940 ms(current implementation)]
"
