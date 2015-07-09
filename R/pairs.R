panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  require(stringr)
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- str_pad(format(r, digits = digits), width = digits + 2, pad = '0', side = 'right')
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}