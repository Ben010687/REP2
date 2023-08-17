 
library(usethis)
library(gitcreds)

usethis::create_github_token()
gitcreds::gitcreds_set()



datos = rnorm(2000)
hist(datos)
summary(datos)

x = y + 1
