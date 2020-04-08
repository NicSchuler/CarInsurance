# install.packages("xts")
# install.packages("sp")
# install.packages("zoo")
# install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type="source")


library(CASdatasets)

data("pg15pricing")
data("pg17trainclaim")
data("pg17trainpol")

save(pg17trainpol, file="pg17trainpol.Rdata")
save(pg17trainclaim, file="pg17trainclaim.Rdata")
