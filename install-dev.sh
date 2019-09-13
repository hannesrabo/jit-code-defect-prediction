#!/bin/bash

# Installing python
if [ "$(uname)" == "Darwin" ]; then
    sudo brew install python3 python3-pip python3-venv jupyter
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    sudo apt install python3 python3-pip python3-venv jupyter
fi

# Creating a virtual environment.
mkdir env-jit && python3 -m venv env-jit
source env-jit/bin/activate

# Setting up dependencies
pip3 install -r requirements.txt

ipython kernel install --user --name=env-jit  
