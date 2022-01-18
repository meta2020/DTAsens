
#' @title Example data 1: IVD data
#'
#' @description {``To identify the most accurate methods for diagnosis of IVD-related bloodstream infection.''}
#'
#' @format A data frame with 33 rows (studies) and 8 variables:
#' \describe{
#'   \item{study}{study ID}
#'   \item{TP}{True positive}
#'   \item{FN}{False negative}
#'   \item{FP}{False positive}
#'   \item{TN}{True negative}
#'   \item{cutoff.grp}{Cutoff value by group}
#'   \item{type}{Diagnostic type}
#'   \item{cutoff}{Cutoff information}
#'   #' }
#'@source \url{http://www.ncbi.nlm.nih.gov/pubmed/9315770}

"IVD"

#' @title IVD data after logit transformation
#'
#' @description {Logit-transformed IVD data (after continuity correction)}
#'
#' @format A data frame with 33 rows and 6 variables:
#' \describe{
#'   \item{sens}{Sensitivity}
#'   \item{spec}{Sepcificity}
#'   \item{y1}{logit transformed sensitivity}
#'   \item{y2}{logit transformed specificity}
#'   \item{v1}{variance of logit transformed sensitivity}
#'   \item{v2}{variance of logit transformed specificity}
#' }
#'@source \url{http://www.ncbi.nlm.nih.gov/pubmed/9315770}
#'@seealso \code{\link{IVD}}

"IVD2"


#' @title Example data 2: CD64 data
#'
#' @description {``To comprehensively and quantitatively summarize the accuracy of neutrophil CD64 in the early diagnosis of bacterial infection.''}
#'
#' @format A data frame with 27 rows (studies) and 11 variables:
#' \describe{
#'   \item{study}{study ID}
#'   \item{Author}{authors of studies}
#'   \item{TP}{True positive}
#'   \item{FN}{False negative}
#'   \item{FP}{False positive}
#'   \item{TN}{True negative}
#'   \item{cutoff}{Cutoff values}
#'   \item{unit}{Unit of cutoff}
#'   \item{unit.grp}{Unit group}
#'   \item{cutoff.grp}{Cutoff group}
#'   \item{subgroup}{Subgroup}
#' }
#'@source \url{https://pubmed.ncbi.nlm.nih.gov/22940278/}

"CD64"


#' @title CD64 data after logit transformation
#'
#' @description Logit-transformed CD64 data (after continuity correction)
#'
#' @format A data frame with 27 rows and 6 variables:
#' \describe{
#'   \item{sens}{Sensitivity}
#'   \item{spec}{Sepcificity}
#'   \item{y1}{logit transformed sensitivity}
#'   \item{y2}{logit transformed specificity}
#'   \item{v1}{variance of logit transformed sensitivity}
#'   \item{v2}{variance of logit transformed specificity}
#' }
#'@source \url{https://pubmed.ncbi.nlm.nih.gov/22940278/}
#'@seealso \code{\link{CD64}}

"CD64_2"


