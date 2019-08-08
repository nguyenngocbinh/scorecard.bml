
# hmeq <- read.csv('data/hmeq.csv') names(hmeq) <- tolower(names(hmeq)) save(hmeq, file =
# inst/extdata/hmeq.RData')
#' This is data to be included in my package
#' @name hmeq
#' @docType data
#' @usage data(hmeq)
#' @author Nguyen Ngoc Binh \email{nguyenngocbinhneu@@gmail.com}
#' @references \url{https://codeload.github.com/Carl-Lejerskar/HMEQ/zip/master}
#' @keywords data
#' \itemize{
#'   \item BAD. 1 = client defaulted on loan, 0 = loan repaid
#'   \item LOAN. Amount of the loan request
#'   \item MORTDUE. Amount due on existing mortgage
#'   \item VALUE. Value of current property
#'   \item REASON. DebtCon = debt consolidation HomeImp = home improvement
#'   \item JOB. Six occupational categories
#'   \item YOJ. Years at present job
#'   \item DEROG. Number of major derogatory reports
#'   \item DELINQ. Number of delinquent credit lines
#'   \item CLAGE. Age of oldest trade line in months
#'   \item NINQ. Number of recent credit lines
#'   \item CLNO. Number of credit lines
#'   \item DEBTINC. Debt-to-income ratio
#' }
#' @examples
#' data('hmeq')
