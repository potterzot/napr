#Fun p-hacking with random data from the method man

#filename = '~/data/misc/an-even-bigger-fake-dataset.csv'
#df = data.frame(read.csv(filename))
#' A humorous (and hopefully not used!) p-hacking function.
#'
#' Originally from \href{http://www.twitter.com/methodsmanmd}.
#'
#check correlation between variables
phack <- function() {
  res <- data.frame(matrix(nrow=length(names(df)), ncol = 3))
  names(res) <- c("var1", "var2", "pval")
  for (vari in names(df)) {
    for (varj in names(df)) {
      if (vari!=varj) {
        r = stats::cor.test(df[,vari], df[,varj])
        if (r['p.value']<0.05) {
          res <- rbind(res, c(vari, varj, r['p.value']))
        }
      }
    }
  }
  res
}
