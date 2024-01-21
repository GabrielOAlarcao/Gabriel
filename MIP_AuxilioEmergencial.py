# Trabalho final da matéria Análise de Matriz Insumo-Produto
# Estimação dos efeitos do Auxílio Emergencial

## Choques baseados no artigo
# Sanches, Marina; Cardomingo, Matias; Carvalho, Laura (2021). Quão mais fundo poderia ter sido esse poço? Analisando o efeito estabilizador do Auxílio Emergencial em 2020 (Nota de Política Econômica nº 007). MADE/USP.


import numpy as np
import pandas as pd
import dataframe_image as dfi
import matplotlib.pyplot as plt

def LeMatrizTRU(Arquivo, NomePasta, LinhaInicial, ColunaInicial, Linhas, Colunas):
  Sheet = pd.read_excel(Arquivo, sheet_name = NomePasta, header = None, index_col = None)
  Matriz = Sheet.values[LinhaInicial:LinhaInicial+Linhas, ColunaInicial:ColunaInicial+Colunas]
  return Matriz

def LeNomeSetores(Arquivo, NomePasta, LinhaInicial, ColunaInicial, Linhas, Colunas):
  dSheet = pd.read_excel(Arquivo, sheet_name=NomePasta, header= None, index_col = None)
  NomeColunas=[]
  for i in range(Colunas):
    NomeColunas.append(dSheet.values[LinhaInicial, ColunaInicial+i])
  
  return NomeColunas

def LeNomeProdutos(Arquivo, NomePasta, LinhaInicial, ColunaInicial, Linhas, Colunas):
  dSheet = pd.read_excel(Arquivo, sheet_name=NomePasta, header= None, index_col = None)

  NomeLinhas=[]
  for i in range(Linhas):
    NomeLinhas.append(dSheet.values[LinhaInicial+i, ColunaInicial])
  
  return NomeLinhas

def CarregaTabelaRecursos(Caminho, Setores, AnoMIP, Produtos, ColunasOfertas):
  ArquivoTabelaRecursos = str(Setores)+"_tab1_"+str(AnoMIP)+".xls"
  Producao = LeMatrizTRU(Caminho+ArquivoTabelaRecursos, "producao",
                         5, 2, Produtos, Setores)
  Producao = Producao.T
  Oferta = LeMatrizTRU(Caminho+ArquivoTabelaRecursos, "oferta",
                         5, 2, Produtos, ColunasOfertas)
  vImportacao = LeMatrizTRU(Caminho+ArquivoTabelaRecursos, "importacao",
                         5, 2, Produtos, 1)
  NomeSetores = LeNomeSetores(Caminho+ArquivoTabelaRecursos, "producao", 3, 2, Produtos, Setores)
  NomeProdutos = LeNomeProdutos(Caminho+ArquivoTabelaRecursos, "producao", 5, 1, Produtos, Setores)
  return Producao, Oferta, vImportacao, NomeSetores, NomeProdutos

def CarregaTabelaUsos(Caminho, Setores, AnoMIP, Produtos, ColunasDemandaFinal):
  ArquivoTabelaUsos = str(Setores)+"_tab2_"+str(AnoMIP)+".xls"
  ConsumoIntermediario = LeMatrizTRU(Caminho+ArquivoTabelaUsos, "CI",
                         5, 2, Produtos, Setores)
  DemandaFinal = LeMatrizTRU(Caminho+ArquivoTabelaUsos, "demanda",
                         5, 2, Produtos, ColunasDemandaFinal)
  VA = LeMatrizTRU(Caminho+ArquivoTabelaUsos, "VA",
                         5, 1, LinhasVA, Setores)
  return ConsumoIntermediario, DemandaFinal, VA

def CalculaMatrizDistribuicao(MatrizEntrada):
  TotalProduto = np.sum(MatrizEntrada, axis=1)
  MatrizSaida = MatrizEntrada / TotalProduto[:,None]
  MatrizSaida = np.nan_to_num(MatrizSaida, nan=0,posinf=0, neginf=0)
  return MatrizSaida

def CalculaDistribuicao(VetorEntrada, Distribuicao):
  MatrizSaida = VetorEntrada[:,None] * Distribuicao 
  return MatrizSaida

def CalculaDistribuicaoMargem (VetorEntrada, Distribuicao, Margem):
  PropMargem = VetorEntrada[Margem[0]:Margem[1]+1, None] / np.sum(VetorEntrada[Margem[0]:Margem[1]+1, None])
  MatrizSaida = VetorEntrada[:,None] * Distribuicao
  MargemDistribuida = np.sum(MatrizSaida[0:Margem[0],:], axis = 0) + np.sum(MatrizSaida[Margem[1]+1:,:], axis = 0)
  MatrizSaida[Margem[0]:Margem[1]+1,:] = MargemDistribuida[None,:] * PropMargem * (-1.)
  return MatrizSaida

def AgregaLinhas(Matriz, Setores):
  vMatrizTotal = np.sum(Matriz, axis=0)
  vConsumoIntermediario = vMatrizTotal[None, 0:Setores]
  vDemandaFinal = vMatrizTotal[None, Setores:]
  return vConsumoIntermediario, vDemandaFinal  


#Definição de Parâmetros
AnoMIP = 2018
Produtos = 128
Setores = 68

ColunasOfertas = 7
ColunaMargemComercio = 1
ColunaMargemTransporte =2
ColunaII = 3
ColunaIPI = 4
ColunaICMS = 5
ColunaOI = 6

ColunasDemandaFinal = 6
ColunaExportacao = 0
ColunaGoverno = 1
ColunaISFLF = 2
ColunaFamilias = 3
ColunaFBCF = 4
ColunaEstoque = 5
LinhaVBP = 12
LinhaOcupacoes = 13
LinhasVA = 14
LinhaRenumeracoes = 1
Caminho = "/content/drive/MyDrive/Colab Notebooks/"
  
  #Carrega Tabela de Recursos
Producao,Oferta,vImportacao, NomeSetores, NomeProdutos  = CarregaTabelaRecursos(Caminho, Setores, AnoMIP, Produtos, ColunasOfertas)

  #Carrega Tabela de Usos
ConsumoIntermediario,DemandaFinal,VA = CarregaTabelaUsos(Caminho, Setores, AnoMIP, Produtos, ColunasDemandaFinal)

DemandaFinalSemEstoque = np.copy(DemandaFinal)
DemandaFinalSemEstoque[:,ColunaEstoque]= 0.0
ConsumoTotalSemEstoque = np.concatenate((ConsumoIntermediario, DemandaFinalSemEstoque), axis = 1)
  #Matriz de distribuição sem variação de estoque
Distribuicao = CalculaMatrizDistribuicao(ConsumoTotalSemEstoque)
  #Margens e impostos
MargemComercio   = CalculaDistribuicaoMargem(Oferta[:, ColunaMargemComercio], Distribuicao, [92,93])
MargemTransporte = CalculaDistribuicaoMargem(Oferta[:, ColunaMargemTransporte], Distribuicao, [94,97])
ValorIPI = CalculaDistribuicao(Oferta[:, ColunaIPI], Distribuicao)
ValorICMS = CalculaDistribuicao(Oferta[:, ColunaICMS], Distribuicao)
ValorOI = CalculaDistribuicao(Oferta[:, ColunaOI], Distribuicao)

DemandaFinalSemExportacao = np.copy(DemandaFinalSemEstoque)
DemandaFinalSemExportacao[:,ColunaExportacao]=0.0
ConsumoTotalSemExportacao = np.concatenate((ConsumoIntermediario, DemandaFinalSemExportacao), axis = 1)
DistribuicaoImportacao = CalculaMatrizDistribuicao(ConsumoTotalSemExportacao)
  # gera matriz de distribuição sem exportação
ValorII = CalculaDistribuicao(Oferta[:, ColunaII], DistribuicaoImportacao)
mImportacao = CalculaDistribuicao(vImportacao.reshape(-1), DistribuicaoImportacao)
ConsumoTotalPC = np.concatenate((ConsumoIntermediario, DemandaFinal), axis=1)
ConsumoTotalPB = ConsumoTotalPC - MargemComercio - MargemTransporte -ValorIPI - ValorICMS - ValorOI - mImportacao - ValorII

mU = ConsumoTotalPB[:,0:Setores]
mE = ConsumoTotalPB[:,Setores:]

                          
VBP = VA[LinhaVBP,:].reshape(1,Setores)
Ocupacoes = VA[LinhaOcupacoes,:]


mX = np.copy(VBP)
mX_Chapeu = np.diagflat(1/mX)
mB = np.dot(mU, mX_Chapeu)

mV = Producao
mQ = np.sum(mV, axis=0)
mQChapeu = np.diagflat(1/mQ)

mD = np.dot(mV, mQChapeu)

mA = np.dot(mD, mB).astype(float)
mY = np.dot(mD, mE).astype(float)
mZ = np.dot(mD, mU).astype(float)
mI = np.eye(Setores)
mLeontief = np.linalg.inv(mI - mA)

# ConsumoIntermediário (ci) e Demanda Final (df)
vMConsumoIntermediario, vMDemandaFinal = AgregaLinhas(mImportacao, Setores)
ValorIIci, ValorIIdf = AgregaLinhas(ValorII, Setores)
ValorICMSci, ValorICMSdf = AgregaLinhas(ValorICMS, Setores)
ValorIPIci, ValorIPIdf = AgregaLinhas(ValorIPI, Setores)
ValorOIci, ValorOIdf = AgregaLinhas(ValorOI, Setores)

mTConsumoIntermediario = np.concatenate((ValorIIci,ValorIPIci,ValorICMSci,ValorOIci), axis=0)
mTDemandaFinal = np.concatenate((ValorIIdf,ValorIPIdf,ValorICMSdf,ValorOIdf), axis=0)

#Checagem do equilíbrio
DemandaTotal = np.sum(mZ, axis=1) + np.sum(mY, axis=1)
OfertaTotal = np.sum(mZ, axis=0) + vMConsumoIntermediario + np.sum(mTConsumoIntermediario, axis=0) + VA[0,:]
CheckTotalVBP = np.sum(OfertaTotal) - np.sum(VBP)

## 
OfertaNacional = np.sum(mZ, axis = 0, keepdims=True)
OfertaTributos = np.sum(mTConsumoIntermediario, axis = 0)
OfertaConsumoIntermediario = OfertaNacional + vMConsumoIntermediario + OfertaTributos
OfertaConsumoIntermediario = np.concatenate((mZ, OfertaNacional, vMConsumoIntermediario, mTConsumoIntermediario, OfertaConsumoIntermediario, VA), axis = 0)
TotalDemandaConsumoIntermediario = np.sum(OfertaConsumoIntermediario, axis=1, keepdims=True)
TotalLinhaDemandaNacional = np.sum(mY, axis=0, keepdims=True)
Demanda = np.concatenate((mY, TotalLinhaDemandaNacional, vMDemandaFinal, mTDemandaFinal), axis = 0)
TotalLinhaDemandaFinal = np.sum(DemandaFinal, axis=0, keepdims=True)
DemandaFinal = np.concatenate((Demanda, TotalLinhaDemandaFinal, np.zeros([14,6], dtype=float)), axis=0)
TotalDemandaFinal = np.sum(DemandaFinal, axis=1, keepdims=True)
DemandaTotal = TotalDemandaConsumoIntermediario + TotalDemandaFinal

MIP = np.concatenate((OfertaConsumoIntermediario,TotalDemandaConsumoIntermediario, DemandaFinal, TotalDemandaFinal, DemandaTotal), axis=1)


### Modelo fechado
DemandaFamilias = mY[:, ColunaFamilias].reshape(Setores,1)
RendaFamilias = VA[LinhaRenumeracoes,:].reshape(1,Setores)
nAux = np.zeros([1,1], dtype= float)
nAux[0,0] = np.sum(DemandaFamilias) 
ZBarra = np.concatenate((mZ, RendaFamilias), axis =0)
vAux = np.concatenate((DemandaFamilias, nAux), axis = 0)
ZBarra = np.concatenate((ZBarra,vAux), axis = 1)
nAux[0,0] = np.sum(VBP)
VBPBarra = np.concatenate((VBP, nAux), axis =1)
mABarra = np.zeros([Setores+1, Setores+1], dtype=float)
mABarra[:,:] = ZBarra[:,:]/ VBPBarra[0,:]
IBarra = np.eye(Setores+1)
LeontiefBarra = np.linalg.inv(IBarra - mABarra)


# Cálculo do choque no consumo das famílias sem o Auxílio Emergencial
choque = -0.11


nAux = np.zeros([1,1], dtype=float)
DeltaYBarra = np.concatenate(((choque*mY[:,ColunaFamilias]).reshape(Setores,1), nAux), axis=0)
DeltaXBarra = np.dot(LeontiefBarra, DeltaYBarra)
DeltaVBPBarra = np.sum(DeltaXBarra)

VariacaoVBPBarra = round((DeltaVBPBarra / np.sum(VBP) *100),2)
print("o VBP endógeno vai variar em ",VariacaoVBPBarra," %")


# Cálculo do choque no consumo das famílias devido ao Auxílio Emergencial
choque_AE = -0.055

nAux = np.zeros([1,1], dtype=float)
DeltaYBarra = np.concatenate(((choque_AE*mY[:,ColunaFamilias]).reshape(Setores,1), nAux), axis=0)
DeltaXBarra = np.dot(LeontiefBarra, DeltaYBarra)
DeltaVBPBarra = np.sum(DeltaXBarra)

VariacaoVBPBarra_AE = round((DeltaVBPBarra / np.sum(VBP) *100), 2)
print("o VBP endógeno vai variar em ",VariacaoVBPBarra_AE," %")

##
data = {'Choque no Consumo (%)': [choque*100,choque_AE*100],  'Variação VBP (%)': [VariacaoVBPBarra, VariacaoVBPBarra_AE]}

EfeitoAE = pd.DataFrame(data, index= ["Sem Auxílio Emergencial", "Com Auxílio Emergencial"])
dfi.export(EfeitoAE, '/content/drive/MyDrive/Colab Notebooks/EfeitoAE.png', table_conversion='matplotlib')


# Multiplicadores de Renda
vRequisitosRenda = VA[LinhaRenumeracoes,:] / VBP
mRequisitosRendaDiagonal = np.diagflat(vRequisitosRenda)
GeradorRenda = np.dot(mRequisitosRendaDiagonal, mLeontief)
MultSimplesRenda = np.sum(GeradorRenda, axis=0)
MultiplicadorRendaI = MultSimplesRenda / vRequisitosRenda

GeradorRendaModeloFechado = np.dot(mRequisitosRendaDiagonal, LeontiefBarra[0:Setores, 0:Setores])
MultTotalRenda = np.sum(GeradorRendaModeloFechado, axis=0)
MultiplicadorRendaII = MultTotalRenda / vRequisitosRenda

EfeitoInduzidoRenda = MultTotalRenda - MultSimplesRenda
EfeitoDiretoRenda = vRequisitosRenda
EfeitoIndiretoRenda = MultSimplesRenda - EfeitoDiretoRenda

## Nomes dos setores
NomeSetores = [e[5:] for e in NomeSetores]


MultiplicadoresRenda = np.vstack((MultTotalRenda, MultSimplesRenda ,EfeitoInduzidoRenda,EfeitoDiretoRenda , EfeitoIndiretoRenda, MultiplicadorRendaII )).T
MultiplicadoresRenda = np.around(MultiplicadoresRenda.astype(np.double),3) 
MultiplicadoresRenda = pd.DataFrame(data=MultiplicadoresRenda, index = NomeSetores, columns=["Total", "Direto+Indireto", "Induzido","Direto","Indireto", "MRII"])
