EP1_8994136 <- function(arqinput, arqoutput) {

  tempo_inicial <- Sys.time()
  library(GA)
  matrizAdj <- as.matrix(read.table(arqinput, sep=",", header=TRUE))

  # Função que calcula distancia da rota
  distancia_rota <- function(rota, distMatrix) {
    rota <- c(rota, rota[1])
    route <- embed(rota, 2)[,2:1]
    sum(distMatrix[route])
  }
  
  # Funcao de fitness a ser maximizada
  fitness_func <- function(rota, ...) 1/distancia_rota(rota, ...)
  
  # Chamada da funcao do pacote GA
  resultados <- ga(type = "permutation", 
                   fitness = fitness_func, 
                   distMatrix = matrizAdj,
                   lower = 1, 
                   upper = nrow(matrizAdj), 
                   popSize = 50, 
                   maxiter = 5000,
                   run = 500, 
                   pmutation = 0.2)
  
  calcula_melhor_solucao <- function(resultados){
    if (length(resultados@bestSol) == 0) {
      matriz <- resultados@solution
      melhor_solucao <- resultados@solution[which.max(resultados@fitnessValue),]
      return (melhor_solucao)
    } else {
      return(resultados@bestSol)
    }
  }
  # Obtem valores a serem impressos a partir da solucao fornecida
  tempo_execucao <- Sys.time()-tempo_inicial
  melhor_solucao <- calcula_melhor_solucao(resultados)
  custo <- distancia_rota(melhor_solucao, matrizAdj)
  vetor_resultados <- cat(cat(custo, ",", tempo_execucao, sep = ""), melhor_solucao, sep = ",")
 
  #cria vetor para escrever no arquivo de saida
  vetor_pt_1 <- paste(custo, tempo_execucao,  sep=",")
  vetor_pt_2 <- paste(melhor_solucao, collapse=',')
  vetor_resultados <- paste(vetor_pt_1, vetor_pt_2, sep=',')
  
  # Imprime resultados
  cat("Custo: ", custo)
  cat("Tempo de execução: ", tempo_execucao)
  cat("Rota: ", melhor_solucao)
  
  # Registra resultados no arquivo de saida
  writeLines(vetor_resultados, arqoutput)
}
