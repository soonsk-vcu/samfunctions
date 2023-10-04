#' make correlation table
#'
#' Returns a table where columns are results of cor.test using preferred alternative and method.Create a table of correlation tests against 1 dependent variable and all other columns of dataframe
#'
#' @param df dataframe with only numeric data
#' @param ind_index col index of dependent variable
#' @param alternative vector of alternative hypotheses for each independent variable
#' @param method default is pearson
#' @export
#' @author Samuel Soon
#'
#'


# Create a table of correlation test results against 1 dependent column and all other columns of dataframe
cortable <- function(df, ind_index, alternative=NULL, method="pearson"){

  # init alternative hypotheses if none provided
  if(is.null(alternative)){
    alternative <- rep("two.sided", ncol(df)-1)
  }

  # aux function for creating column for each correlation test
  corcol <- function(xind,yind,df, alternative, method){
    # init vectors
    x <- unlist(df[xind])
    y <- unlist(df[yind])
    # test
    l <- cor.test(x,y, method=method, alternative=alternative)
    # separate numeric and string entries for tidying
    numind <- l[sapply(l, is.numeric)]
    numdata <- round(unlist(l[sapply(l, is.numeric)]), 4)
    strdata <- unlist(l[!sapply(l, is.numeric)])
    # paste formatted entries with dependent variable name for clarity
    cortests <- c(numdata , strdata[-length(strdata)], "Dependent Variable" = colnames(df)[yind])
    cortests["method"] <- method
    cortests

  }

  # map corcol to each independent variable and gather into a table
  ret <- data.frame(mapply(corcol,x=(1:ncol(df))[-ind_index],
                           alternative = alternative,
                           MoreArgs = list(yind=ind_index,df=df, method=method)))

  colnames(ret) <- colnames(df)[-ind_index]

  ret

}
