library(dplyr)
library(nlme)
library(car)
library(randtests)
library(lmtest)
library(strucchange)
library(tseries)
library(stats)

#HELLWIG
#corrVector - wektor korelacji Y z X, corrMatrix - macierz korelacji miedzy X
hellwig = function(corrYX = NULL, corrX = NULL) {
  
  validateVector = function(vector) {
    if (length(vector) == 0) {
      stop("Vector cannot be empty")
    }
    
    if (!is.numeric(vector)) {
      stop ("Vector must contain numeric values")
    }
    
    numberOfRows = nrow(vector)
    numberOfColumns = ncol(vector)
    hasDimmensions = as.logical(numberOfRows) && as.logical(numberOfColumns)
    
    if (hasDimmensions && !is.na(hasDimmensions)) {
      if (numberOfRows != 1 || numberOfColumns != 1) {
        warning("Matrix was passed as a value instead of vector and will be transformed into a vector")
      }
    }
  }
  
  validateDiagonalMatrix = function(matrix) {
    if (length(matrix) == 0) {
      stop("Matrix cannot be empty")
    }
    
    if (!is.numeric(matrix)) {
      stop ("Matrix must contain numeric values")
    }
    
    numberOfRows = nrow(matrix)
    numberOfColumns = ncol(matrix)
    hasDimmensions = as.logical(numberOfRows) && as.logical(numberOfColumns)
    
    if (is.na(hasDimmensions)) {
      stop("Value passed as a function argument is not a matrix")
    }
    
    if (numberOfRows != numberOfColumns) {
      stop("Number of columns and rows must be equal in square matrix")
    }
  }
  
  createCorrDataList = function(vector, matrix) {
    data = list();
    
    data[['corrVector']] = as.vector(vector)
    data[['corrMatrix']] = as.matrix(matrix)
    
    return(data)
  }
  
  extractDataFromMatrix = function(matrix) {
    vector = matrix[1, -1]
    matrix = matrix[-1, -1]
    data = createCorrDataList(vector, matrix)
    
    return(data)
  }
  
  prepareData = function(corrYX = NULL, corrX = NULL) {
    if(is.null(corrYX)) {
      stop("There are no data passed as an argument")
    } else if(is.null(corrX)) {
      matrix = as.matrix(corrYX)
      validateDiagonalMatrix(matrix)
      
      data = extractDataFromMatrix(matrix)
      
      return(data)
    } else {
      vector = as.vector(corrYX)
      validateVector(corrYX)
      matrix = as.matrix(corrX)
      validateDiagonalMatrix(corrX)
      if(length(vector) != ncol(matrix)) {
        stop("Number of variables in correlation vector is different than number of variables in matrix")
      }
      
      data = createCorrDataList(vector, matrix)
      
      return(data)
    }
  }
  
  #Generowanie wszystkich kombinacji zmiennych
  generateVariablesCombinations = function(numberOfVariables) {
    combinations = list();
    
    for (combinationSize in 1:numberOfVariables) {
      combination = combn(numberOfVariables, combinationSize)
      numberOfCombinations = ncol(combination)
      
      for(combinationIndex in 1:numberOfCombinations) {
        combinations = c(combinations, list(combination[,combinationIndex]))
      }
    }
    
    return(combinations)
  }
  
  #Indywidualna pojemno?????? no???nik???w informacji (hkj)
  calculateIndividualCapacity = function(corrVector, corrMatrix) {
    return((corrVector^2)/sum(abs(corrMatrix)))
  }
  
  #Pojemno?????? integralna kombinacji no???nik???w informacji (Hk)
  calculateIntegralCapacity = function(corrVector, corrMatrix, combination) {
    integralCapacity = 0
    combinationSize = length(combination)
    
    for(combinationElement in 1:combinationSize) {
      variableNumber = combination[combinationElement]
      corrY = corrVector[variableNumber]
      corrX = corrMatrix[variableNumber, combination]
      
      individalCapacity = calculateIndividualCapacity(corrY, corrX)
      
      integralCapacity = integralCapacity + individalCapacity
    }
    
    
    return (integralCapacity)
  }
  
  calculateIntegralCapacities = function(corrVector, corrMatrix, combinations) {
    integralCapacities = NULL
    numberOfCombinations = length(combinations)
    
    for(combinationNumber in 1:numberOfCombinations) {
      combination = combinations[[combinationNumber]]
      
      #Pojemno?????? integralna kombinacji no???nik???w informacji (Hk)
      integralCapacity = calculateIntegralCapacity(corrVector, corrMatrix, combination)
      
      integralCapacities = c(integralCapacities, integralCapacity)
    }
    
    return(integralCapacities)
  }
  
  findBestCombinationIndex =  function(integralCapacities) {
    return(which(integralCapacities == max(integralCapacities)))
  }
  
  createBestCombinationData = function(corrMatrix, combinations, integralCapacities) {
    data = list()
    bestCombinationIndex = findBestCombinationIndex(integralCapacities)
    bestCombination = combinations[[bestCombinationIndex]]
    
    if(is.null(colnames(corrMatrix))) {
      data[["combination"]] = bestCombination
    } else {
      corrMatrixVariablesNames = colnames(corrMatrix)
      
      data[["combination"]] = corrMatrixVariablesNames[bestCombination]
    }
    
    data[["integralCapacity"]] = integralCapacities[bestCombinationIndex]
    
    return(data)
  }
  
  result = tryCatch({
    data = prepareData(corrYX, corrX)
    
    corrVector = data$corrVector
    corrMatrix = data$corrMatrix
    
    numberOfVariables = length(corrVector)
    
    #Generowanie kombinacji zmiennych
    combinations = generateVariablesCombinations(numberOfVariables)
    
    #Obliczanie pojemno???ci integralnej dla ka???dej kombinacji
    integralCapacities = calculateIntegralCapacities(corrVector, corrMatrix, combinations)
    
    bestCombination = createBestCombinationData(corrMatrix, combinations, integralCapacities)
    
    return(bestCombination)
    
  }, error = function(err) {
    message(err)
    
    return (NA)
  }, warning = function(warn) {
    message(warn)
    
    return (NULL)
  })
}

library(readxl)
dane <- read_excel("C:/Justyna/Zadania/Mgr 1/sem III/SWAP/Projekt/Do_hellwiga.xlsx", 
                          col_types = c("skip", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
View(dane)


R0 = c(cor(dane$x1, dane[2:6]))
R = cor(dane[2:6])
hellwig(R0, R)
