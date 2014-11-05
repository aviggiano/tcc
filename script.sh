#!/usr/bin/env bash
sudo su
yes | yum install R 				# instala o pacote R no linux
yes | yum install git 			# instala o git para carregar os métodos de recomendação
ssh-keygen -t rsa 		# gera a chave publica para se poder carregar o repositorio do sistema
cat ~/.ssh/id_rsa.pub	# depois, deve-se adicionar essa chave publica nas configurações do Github
git clone git@github.com:aviggiano/tcc	# clona o repositorio
cd tcc && Rscript recsys/run_tests.R 		# executa o script de avaliacao
