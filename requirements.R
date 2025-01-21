
pNames <- readLines(file("requirements.txt"))
isInstalled<-pNames%in%installed.packages()

checkRequirements<- function(requirementsFile="requirements.txt",path=getwd()){
  #Read in the different required packages
  packageNames <- readLines(file(file.path(path, requirementsFile)), warn = FALSE)
  #Check if all the listed packages are installed on the system
  isInstalled <- packageNames %in% installed.packages()
  #Filter out the FALSE values to obtain the indices of non installed packages
  notInstalled <- which(!isInstalled)
  #If any package is not installed the previous line returns a list of nonzero length
  if (length(notInstalled)>0){
    print("The following packages were not installed:")
    for (idx in notInstalled){
      print(packageNames[idx])
    }
    return (list(packageNames, packageNames[notInstalled]))
  }
  else {
    print("All packages in the requirements file are installed")
    return (packageNames)
  }
}

loadRequirements <- function(requirementsFile="requirements.txt",path=getwd()){
  #Check if everything is installed
  packages <- checkRequirements(requirementsFile = requirementsFile, path=path)
  #If checkRequirements returns something with a length greater than 1 this indicates
  #missing packages as we get a list of installed and non installed back
  if (length(packages)>1){
    choice <- readline("Do you wish to install the missing packages?[y/n]: ")
    if (choice=="y" | choice=="Y" | choice=="yes" | choice=="Yes"){
      print("Installing missing packages")
      #Install all packages that are present in the missing list
      lapply(packages[2], install.packages, character.only=TRUE)
      #Load all packages mentioned in requirementsFile
      lapply(packages, library, character.only=TRUE)
    }
    else {
      stop("Exiting loadRequirements function")
    }
  }
  else {
    print("All required packages are installed")
    lapply(packages, library, character.only=TRUE)
  }
}

loadRequirements()
