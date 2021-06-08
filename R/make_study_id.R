make_study_id <- function(x){
  match(x, sort(unique(x)))
}