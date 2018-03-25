.First <-
function () 
{
    cat("Calling .First() in .Rprofile\n")
    options(repos = "https://cran.rstudio.com/")
}
.Last <-
function () 
{
    cat("Calling .Last() in .Rprofile\n")
}
