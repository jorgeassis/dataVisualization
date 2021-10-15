## --------------------------------------------------
## --------------------------------------------------
##
## biodiversityDS.
## https://github.com/jorgeassis
##
## --------------------------------------------------
## --------------------------------------------------

distinctColors(3)

distinctColors <- function(n) {
  
  library(RColorBrewer)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  col_vector_long <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  
  if(n > 74) { col_vector <- sample(col_vector_long, n,replace=FALSE) }
  if(n <= 74) { col_vector <- sample(col_vector, n,replace=FALSE) }
  
  return(col_vector)
  
}
