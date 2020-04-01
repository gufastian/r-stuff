
library(googleAnalyticsR)
require(tidyverse)


# Authorize Google Analytics. This should launch a browser and require you to
# log in.
ga_auth()

accounts <- ga_account_list()
uniqueAccounts <- distinct(accounts, accounts$accountId, accounts$accountName)

View(uniqueAccounts)

for (a in seq_along(uniqueAccounts$`accounts$accountId`)) {
  cat(uniqueAccounts[a,1,1],";", uniqueAccounts[a,2,1], "\n");
}


userList <- ga_users_list("73667627", webPropertyId = NULL, viewId = NULL); writeClipboard(unique(userList$userRef.email))