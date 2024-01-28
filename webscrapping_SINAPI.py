from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time
import urllib
import shutil
import wget
import numpy as np
import pandas as pd
import patoolib
from glob import glob
from pyunpack import Archive
from datetime import datetime
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from urllib.request import Request, urlopen
import os
import zipfile
import tarfile
from pdfminer.high_level import extract_text

## Caminho para baixar os arquivos do SINAPI
path = "define your path in here"

def extract_last_part(link):
    # Extract the last part of the link (assuming links are separated by '/')
    return link.split('/')[-1]

def download_files_from_links(all_links, download_path):
    for sub_list in all_links:
        for link in sub_list:
            file_name = extract_last_part(link)
            
            if file_name not in os.listdir(download_path):
                try:
                    print('\n########## Arquivo', file_name, '##########')
                    print('Download através do wget.')
                    wget.download(url=link, out=os.path.join(download_path, file_name))

                    count_time = 0
                    while (file_name not in os.listdir(download_path)) and (count_time < download_limit_time):
                        time.sleep(1)
                        count_time += 1

                    time.sleep(2)                
                except:
                    print('\nFalha ao tentar baixar o arquivo através do wget. Abrindo tentativa de download via navegador Chrome.')
                    chromeOptions = webdriver.ChromeOptions()
                    prefs = {"download.default_directory" : download_path}
                    chromeOptions.add_experimental_option("prefs", prefs)
                    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=chromeOptions)
                    driver.get(link)

                    count_time = 0
                    while (file_name not in os.listdir(download_path)) and (count_time < download_limit_time):
                        time.sleep(1)
                        count_time += 1

                    time.sleep(1)                
                    driver.close()

                print('\n')
                if file_name in os.listdir(download_path):
                    print('Arquivo ' + file_name + ' baixado com sucesso!')
                else:
                    print('Erro ao tentar baixar o arquivo ' + file_name + '. Tente rodar novamente o programa deixando a opção "Apagar todos os dados antigos?" como "False"')
                print('\n')


def extract_compressed_files(path, extracted_files=set()):
    for root, dirs, files in os.walk(path):
        for file in files:
            file_path = os.path.join(root, file)

            # Check if the file is a ZIP archive
            if zipfile.is_zipfile(file_path):
                if file_path not in extracted_files:
                    try:
                        with zipfile.ZipFile(file_path, 'r') as zip_ref:
                            zip_ref.extractall(root)
                        print(f"Extracted {file} in {root}")

                        # Add the file to the set of extracted files
                        extracted_files.add(file_path)

                        # Recursively extract files from the nested ZIP archive
                        extract_compressed_files(os.path.join(root, file.split('.')[0]), extracted_files)

                    except (zipfile.BadZipFile, EOFError) as e:
                        print(f"Error extracting {file}: {e}")

            # Check if the file is a TAR archive
            elif tarfile.is_tarfile(file_path):
                if file_path not in extracted_files:
                    try:
                        with tarfile.open(file_path, 'r') as tar_ref:
                            tar_ref.extractall(root)
                        print(f"Extracted {file} in {root}")

                        # Add the file to the set of extracted files
                        extracted_files.add(file_path)

                        # Recursively extract files from the nested TAR archive
                        extract_compressed_files(os.path.join(root, file.split('.')[0]), extracted_files)

                    except (tarfile.TarError, EOFError) as e:
                        print(f"Error extracting {file}: {e}")
                        

def filter_files_by_keywords(path, extension, keywords):
    # Search for files with the specified extension in the path
    files = glob(os.path.join(path, f'*.{extension}'))

    # Filter files that contain any of the keywords in their names
    filtered_files = [file for file in files if all(keyword in os.path.basename(file) for keyword in keywords)]

    return filtered_files  
  

def read_xls_files(file_paths):
    data_frames = []
    for file_path in file_paths:
        # Read the Excel file into a DataFrame
        try:
            df = pd.read_excel(file_path, engine='xlrd')
            mes = datetime.strptime(df.iloc[1,0][-7:], '%m/%Y')
            df = df.iloc[5:, :]
            df.columns = df.iloc[0]
            df = df.iloc[1:]
            df['data'] = mes
            df['UF'] = file_path.split("_")[-3]
            df = df[df['DESCRICAO DO INSUMO'].str.contains("TIJOLO", case = False) | df['DESCRICAO DO INSUMO'].str.contains("ACARTONADO", case = False)]
            
            data_frames.append(df)
            print(f"Successfully read: {file_path}")
        except Exception as e:
            print(f"Error reading {file_path}: {e}")
    
    concatenated_df = pd.concat(data_frames, ignore_index=True)
    concatenated_df['UNIDADE DE MEDIDA'] = concatenated_df['UNIDADE DE MEDIDA'].fillna(concatenated_df['UNIDADE'])
    concatenated_df.drop(columns=['UNIDADE'], inplace=True)
    
    concatenated_df['PRECO MEDIANO R$'] = concatenated_df['PRECO MEDIANO R$'].fillna(concatenated_df['  PRECO MEDIANO R$'])
    concatenated_df.drop(columns=['  PRECO MEDIANO R$'], inplace=True)
    
    concatenated_df.dropna(subset = ['DESCRICAO DO INSUMO'])

    return concatenated_df  
                    
# Set up the web driver (you need to download the appropriate driver for your browser)
# For example, for Chrome, you can use chromedriver: https://sites.google.com/chromium.org/driver/
n_ufs = list(range(638, 665))

categorias = ["categoria_" + str(number) for number in n_ufs]
button_categorias = ["btncategoria_" + str(number) for number in n_ufs]

driver = webdriver.Chrome()
url = "https://www.caixa.gov.br/site/Paginas/downloads.aspx"
driver.get(url)


all_links = []

for categoria, button_categoria in zip(categorias, button_categorias):
    print(f"Processing: {categoria}, {button_categoria}")
    # Find the button by ID
    button = driver.find_element(By.ID, button_categoria)

    # Click the button
    button.click()

    # Wait for a short period (adjust the time based on your specific case)
    time.sleep(3)  # You may need to adjust this time

    # Find the category element
    category_elements = driver.find_elements(By.ID, categoria)

    category_links = []

    # Iterate over each category element
    for category_element in category_elements:
        # Find all links within the category
        links = category_element.find_elements(By.TAG_NAME, "a")

        # Print the href attribute of each link ending with ".zip"
        for link in links:
            href = link.get_attribute("href")
            if href and href.endswith(".zip"):
                category_links.append(href)

    all_links.append(category_links)

# After finishing, close the browser
driver.quit()

links_list = [pd.Series(link) for link in all_links]

# Concatenate Series into a DataFrame
df = pd.concat(links_list, axis=0)

df = pd.DataFrame(df)

df.columns = ['links']

df.to_parquet("path to save", index = False)


## Lendo o arquivo com os links
all_links = pd.read_parquet("path to save").values.tolist()

## Baixando os dados 
download_files_from_links(all_links, path)


# Extraindo os dados dos arquivos zipados
extract_compressed_files(path)


# Leitura dos dados dentro da pasta
# Selecionar os arquivos "SINAPI_Preco_Ref_Insumos"
## Dados zipados
extension = 'xls'  # Change to 'pdf' if needed
keywords = ['SINAPI_Preco_Ref_Insumos','_Desonerado']

filtered_files = filter_files_by_keywords(path, extension, keywords)
filtered_files = [os.path.join(path, file_name.replace('\\', '/')) for file_name in filtered_files]

df = read_xls_files(filtered_files)

df.to_csv("path to save", index = False)

