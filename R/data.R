#' @title Studies on Math Anxiety and Math Achievement
#' 
#' @description Results from 332 studies, including 747 effect sizes in total (Fisher-z), on the 
#'              relation between math anxiety and math achievement \insertCite{barroso2021meta}{blsmeta}.
#'   
#' @format A data frame with 747 rows and 11 variables:
#' 
#' * \code{es_id}: effect size id 
#' * \code{study_id}: study id, corresponding to the author variable.
#' * \code{author}: study authors
#' * \code{pub_year}: year of publication
#' * \code{continent}: 1 = North America; 2 = South America; 3 = Europe; 4 = Asia;
#'                     5 = Africa; 6 = Oceania (Australia and New Zealand); -999 not included
#' * \code{grade}: 1 = 1st - 2nd grade; 2 = 3rd - 5th grade; 3 = 6th - 8th grade; 
#'                 4 = 9th - 12th; 5 = postsecondary school (undergraduate and graduate students);
#'                 6 = non-student adults
#' * \code{low_ability}: low math ability. 1 = yes; 2 = no
#' * \code{teachers}: 1 = teacher sample; 2 = not teacher sample
#' * \code{ni}: sample size
#' * \code{yi}: effect size (Fisher-z)
#' * \code{vi}: sampling variance (SE^2)
#' @md
#' 
#' @details Further details can be found at \href{https://osf.io/5admx/}{https://osf.io/5admx/}.
#' 
#' @usage data("dat_barroso2020")
#' @references
#' \insertAllCited{}
"dat_barroso2020"