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
#' 
#' @references
#' \insertAllCited{}
"dat_barroso2020"


#' @title Studies on Age Differences in Executive Functioning
#' 
#' @description Results from 431 studies, including 1268 effect sizes (Hedge's g), on age differences in 
#' executive functioning \insertCite{maldonado2020age}{blsmeta}.
#' 
#' @format A data frame with 1268 rows and 13 variables:
#' 
#' * \code{es_id}: effect size id
#' * \code{study_id}: study id
#' * \code{author}: study authors
#' * \code{domain}: executive functioning domain
#' * \code{n1}: sample size in younger group
#' * \code{n2}: sample size in older group
#' * \code{n_total}: total sample size (n1 + n2)
#' * \code{mean_age1}: mean age of younger group
#' * \code{mean_age2}: mean age of older group
#' * \code{miyake}: framework put forward by Mijake and colleagues
#' * \code{task}: cognitive task administered
#' * \code{yi}: effect size (Hedge's g)
#' * \code{vi}: sampling variance (SE^2)
#' @md
#' 
#' @details Further details can be found at \href{https://osf.io/bcywg/}{https://osf.io/bcywg/}.
#' 
#' @usage data("dat_maldonado2018")
#' 
#' @references
#' \insertAllCited{}
"dat_maldonado2018"


#' @title Studies on the Color Red and Cognitive Performance
#' 
#' @description Results from 22 studies, including 67 effect sizes (SMD), on the effect of 
#' the color red on cognitive performance \insertCite{gnambs2020limited}{blsmeta}.
#' 
#' @format A data frame with 67 rows and 10 variables:
#' 
#' * \code{es_id}: effect size id
#' * \code{study_id}: study id
#' * \code{author}: study author
#' * \code{pub_year}: year of publication
#' * \code{country}: country where experiment was conducted
#' * \code{color}: control color
#' * \code{n}: sample size
#' * \code{design}: within or between subjects design
#' * \code{yi}: effect size (standardized mean difference)
#' * \code{vi}: sampling variance (SE^2)
#' @md
#' @details Further details can be found at \href{https://psyarxiv.com/a4qdv/}{https://psyarxiv.com/a4qdv/}.
#' 
#' @usage data("dat_gnambs2020")
#' 
#' @references
#' \insertAllCited{}
"dat_gnambs2020"


#' @title Studies on Out-Group Entitativity and Prejudice
#' 
#' @description Results from 21 studies, including 85 effect sizes (Fisher Z), on the effect of 
#' out-Group entitativity and prejudice \insertCite{agadullina2018people}{blsmeta}.
#' 
#' 
#' @format A data frame with 85 rows and 9 variables:
#' 
#' * \code{es_id}: effect size id
#' * \code{study_id}: study id
#' * \code{author}: study author
#' * \code{pub_year}: year of publication
#' * \code{n}: sample size
#' * \code{design}: within or between subjects design
#' * \code{ent_alpha}: entitativity score
#' * \code{yi}: effect size (Fisher z)
#' * \code{vi}: sampling variance (SE^2)
#' @md
#' 
#' @details Further details can be found at \href{https://osf.io/8dw5y/}{https://osf.io/8dw5y/}.
#' 
#' @usage data("dat_agadullina2018")
#' 
#' @references
#' \insertAllCited{}
"dat_agadullina2018" 

#' @title Studies on Social Identity Theory and Leadership: Leader Group Prototypicality
#' 
#' @description Results from 128 studies, including 251 effect sizes (Fisher Z), on the extent to which 
#' a leader is perceived to embody shared social identity \insertCite{steffens2021advancing}{blsmeta}.
#' 
#' @format A data frame with 251 rows and 10 variables:
#' 
#' * \code{es_id}: effect size id
#' * \code{study_id}: study id
#' * \code{author}: study author
#' * \code{n}: sample size
#' * \code{design}: 0 = experimental; 1 = correlational
#' * \code{published}: 0 = published; 1 = unpublished
#' * \code{proto_strength}: 0 = ad-hoc; 1 = natural
#' * \code{target.leader}: 0 = informal; 1 = formal
#' * \code{yi}: effect size (Fisher z)
#' * \code{vi}: sampling variance (SE^2)
#' @md
#' 
#' @details Further details can be found at \href{https://osf.io/hxgpw/}{https://osf.io/hxgpw/} 
#' 
#' @usage data("dat_steffens2020")
#' 
#' @references
#' \insertAllCited{}
"dat_steffens2020"