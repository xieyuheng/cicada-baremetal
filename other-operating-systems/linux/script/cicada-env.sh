#!/bin/bash

# this script file sets and modifies some environment variables needed by cicada
# you should change the value of this variables
# especially ``cicada_path''

# the following notes are for new linux users

# 1. you can use the following command to active this modifications 
#      source cicada-env.sh 
#    then you can run cicada in your terminal

# 2. you can simply put the following line into your ``.bashrc''
#    then you open a new terminal
#    ``.bashrc'' is readed
#    during the initialization of ``bash''

# 3. in archlinux, you can put this file in ``/etc/profile.d/''
#    but note that
#    ``/etc/profile.d/*.sh'' are readed
#    only during the initialization of ``bash --login''



export cicada_path=$HOME/cicada/other-operating-systems/linux
export cicada_core_path=$cicada_path/core
export cicada_core=english.ccd
export PATH=$PATH:$cicada_path/play
