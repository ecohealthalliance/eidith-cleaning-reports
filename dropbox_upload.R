#!/usr/bin/env Rscript

library(rdrop2)
library(cyphr)
library(sodium)
library(base64enc)


if(Sys.getenv("CIRCLE_BRANCH") == "master") {
  token <- decrypt(readRDS("token.rds.encrypted"), key = key_sodium(base64decode(Sys.getenv("SODIUM_KEY"))))
  to_upload <- list.files("outputs", pattern = "(html|xlsx)$", full.names = TRUE)
  for (f in to_upload) {
    drop_upload(f, path  = "shared/eidith-cleaning-reports-outputs/", dtoken = token, autorename = FALSE)
  }
}


# db_token <- readRDS("token.rds")
# key <-sodium::keygen()
# key64 <- base64enc::base64encode(key)
# zz <- cyphr::encrypt(saveRDS(db_token, "token.rds.encrypted"), key =  cyphr::key_sodium(base64decode(key64)))
# key64
