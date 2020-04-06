

require(tidyverse)
library(purrr)

# ClientID: 559052943659-39i8ju2mmn0vtjrpnmiigtqrq4vqg5ah.apps.googleusercontent.com
# Secret: tznZ9j-BlPaiBFsLY9zASPAd
#NEW
#348639780589-7hqvgfjcmsbm3u03e1lr146p77bu1a0s.apps.googleusercontent.com
#_QRDstLrI1yVSkStx6vYPbOp


GAR_CLIENT_JSON="C:/Users/montino/Downloads/client_secret_348639780589-7hqvgfjcmsbm3u03e1lr146p77bu1a0s.apps.googleusercontent.com.json"

# Authorize Google Analytics. This should launch a browser and require you to
# log in.
googleAuthR::gar_set_client(GAR_CLIENT_JSON)
library(googleAnalyticsR)
ga_auth()

accounts <- ga_account_list()
uniqueAccounts <- distinct(accounts, accounts$accountId, accounts$accountName)

View(uniqueAccounts)
userList <- list()
for (a in seq_along(uniqueAccounts$`accounts$accountId`)) {
  if (uniqueAccounts[a,1,1] != "61448901") {
    cat(uniqueAccounts[a,1,1],";", uniqueAccounts[a,2,1], "\n");
    userList <- ga_users_list(uniqueAccounts[a,1,1], webPropertyId = NULL, viewId = NULL);
    write_excel_csv(userList,path = "C:/Users/montino/Downloads/users.csv" ,append = TRUE)
  }
}

#writeClipboard(unique(userList$userRef.email))



#REMOVAL

library(readxl)
removal <- read_excel("C:/Users/montino/Downloads/removal.xlsx", col_types = c("text", "text"))
View(removal)
d <- 1;
errorLog <- ""
for (c in removal$email) {
  cat(c," - ",removal[d,2,1],"\n")
  res <- try(ga_users_delete(c,removal[d,2,1]))
  if(inherits(res, "try-error"))
  {
    #error handling code, maybe just skip this iteration using
    #errorLog <- paste (errorLog, "ERROR;",c,";",removal[d,2,1],"\n")
    
    res <- try(ga_users_delete(c,removal[d,2,1]))
    if(inherits(res, "try-error"))
    {
      #error handling code, maybe just skip this iteration using
      errorLog <- paste (errorLog, "ERROR;",c,";",removal[d,2,1],"\n")
      cat("ERROR;",c,";",removal[d,2,1],"\n")
      next
    } else {
    }
    
    d <- (d+1)
    next
  } else {
    d <- (d+1)
  }
  
}

