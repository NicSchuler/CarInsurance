# install.packages("xts")
# install.packages("sp")
# install.packages("zoo")
# install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type="source")


library(CASdatasets)

data("pg17testyear1")
data("pg17testyear2")
data("pg17testyear3")
data("pg17testyear4")
data("pg17trainclaim")
data("pg17trainpol")

save(pg17testyear1, file="../Data/pg17testyear1.Rdata")
save(pg17testyear2, file="../Data/pg17testyear2.Rdata")
save(pg17testyear3, file="../Data/pg17testyear3.Rdata")
save(pg17testyear4, file="../Data/pg17testyear4.Rdata")

