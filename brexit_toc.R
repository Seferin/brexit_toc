#brexit_toc
#
#R Script by Sef James 15/11/018
#
#Source of withdrawal_agreement.txt is a pdf found here:
#https://ec.europa.eu/commission/sites/beta-political/files/draft_withdrawal_agreement_0.pdf

library(tidyverse)
library(tidytext)

withdrawal_agreement <- scan(file = "withdrawal_agreement.txt", what=list('character'), nlines = 10380, strip.white = TRUE, blank.lines.skip = TRUE, encoding = "UTF-8", sep = "\n")
withdrawal_agreement <- as.data.frame(withdrawal_agreement, row.names = NULL, optional = FALSE, cut.names = FALSE, col.names = c("Line"), fix.empty.names = TRUE, stringsAsFactors = FALSE)
withdrawal_agreement <- mutate(withdrawal_agreement, Page = cumsum(str_detect(Line, regex("& /en \\d+"))))
withdrawal_agreement <- mutate(withdrawal_agreement, Part_Number = cumsum(str_detect(Line, regex("^\\QPART \\E\\w+$"))))
withdrawal_agreement <- mutate(withdrawal_agreement, Part = str_extract(Line, regex("^\\QPART \\E\\w+$")))
withdrawal_agreement <- mutate(withdrawal_agreement, Article = str_extract(Line, regex("^\\QARTICLE \\E\\d+$")))
withdrawal_agreement <- mutate(withdrawal_agreement, Title = str_extract(Line, regex("^\\QTITLE \\E.+")))
withdrawal_agreement <- mutate(withdrawal_agreement, Chapter = str_extract(Line, regex("^\\QCHAPTER \\E.+")))
withdrawal_agreement <- mutate(withdrawal_agreement, Linenumber = row_number())

#Attempt to construct named parts, articles, titles, chapters by concatenating str_extract and subsequent line number
vars <- c("Part", "Article", "Title", "Chapter")
for(i in vars){
  idx <- which(!is.na(withdrawal_agreement[,i]))
  idy <- idx + 1
  withdrawal_agreement[idx,i] <- paste(withdrawal_agreement[idx,"Line"],withdrawal_agreement[idy,"Line"], sep = ": ")
}

#Combine into toc vector
withdrawal_agreement$toc <- NA
for(i in vars){
  idx <- which(!is.na(withdrawal_agreement[,i]))
  withdrawal_agreement[idx,"toc"] <- withdrawal_agreement[idx,i]
}
 
#toc is very long, so now combine into toc_concise without the Article sub-headings
vars <- c("Part", "Title", "Chapter")
withdrawal_agreement$toc_concise <- NA
for(i in vars){
  idx <- which(!is.na(withdrawal_agreement[,i]))
  withdrawal_agreement[idx,"toc_concise"] <- withdrawal_agreement[idx,i]
}

#Write long and short toc to txt files
vars <- c("toc", "toc_concise")
for(i in vars){
  Table_of_Contents <- withdrawal_agreement[which(!is.na(withdrawal_agreement[,i])),c(i, "Page")]
  Table_of_Contents$Page <- paste("p.", Table_of_Contents$Page)
  names(Table_of_Contents) <- c("Table of Contents", "Page")
  rownames(Table_of_Contents) <- NULL
  sink(file = paste0(i,".txt"), append = FALSE, type = c("output", "message"),
       split = FALSE)
  print(Table_of_Contents, row.names = F, right = F)
  sink(file = NULL)
}