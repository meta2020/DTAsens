
#' @title Example data 1: IVD data
#'
#' @description {``To identify the most accurate methods for diagnosis of IVD-related bloodstream infection.''}
#'
#' @format A data frame with 33 rows and 5 variables:
#' \describe{
#'   \item{study}{study ID}
#'   \item{TP}{True positive}
#'   \item{FN}{False negative}
#'   \item{FP}{False positive}
#'   \item{TN}{True negative}
#' }
#'@source \url{http://www.ncbi.nlm.nih.gov/pubmed/9315770}

"IVD"

#' @title IVD data after logit transformation
#'
#' @description {Logit-transformed IVD data (after continuity correction)}
#'
#' @format A data frame with 33 rows and 7 variables:
#' \describe{
#'   \item{sens}{Sensitivity}
#'   \item{spec}{Sepcificity}
#'   \item{y1}{logit transformed sensitivity}
#'   \item{y2}{logit transformed specificity}
#'   \item{v1}{variance of logit transformed sensitivity}
#'   \item{v2}{variance of logit transformed specificity}
#'   \item{ldor.t}{t-statistic of log diagnostic odds ratio}
#' }
#'@source \url{http://www.ncbi.nlm.nih.gov/pubmed/9315770}
#'@seealso \code{\link{IVD}}

"IVD2"


#' @title Example data 2: Scheidler data
#'
#' @description {``To apply meta-analysis to compare the utility of
#' lymphangiography (LAG), computed tomography (CT), and magnetic resonance (MR) imaging
#' for the diagnosis of lymph node metastasis in patients with cervical cancer.''}
#'
#' @format A data frame with 44 rows and 5 variables:
#' \describe{
#'   \item{ID}{study ID}
#'   \item{author}{authors of studies}
#'   \item{year}{publication years}
#'   \item{method}{1 is CT, 2 is LAG, 3 is MR}
#'   \item{TP}{True positive}
#'   \item{FN}{False negative}
#'   \item{FP}{False positive}
#'   \item{TN}{True negative}
#' }
#'@source \url{http://annals.org/article.aspx?doi=10.7326/0003-4819-142-6-200503150-00011}

"Scheidler"


#' @title Scheidler data after logit transformation
#'
#' @description Logit-transformed Scheidler data (after continuity correction)
#'
#' @format A data frame with 44 rows and 7 variables:
#' \describe{
#'   \item{sens}{Sensitivity}
#'   \item{spec}{Sepcificity}
#'   \item{y1}{logit transformed sensitivity}
#'   \item{y2}{logit transformed specificity}
#'   \item{v1}{variance of logit transformed sensitivity}
#'   \item{v2}{variance of logit transformed specificity}
#'   \item{ldor.t}{t-statistic of log diagnostic odds ratio}

#' }
#'@source \url{http://annals.org/article.aspx?doi=10.7326/0003-4819-142-6-200503150-00011}
#'@seealso \code{\link{Scheidler}}

"Scheidler2"
