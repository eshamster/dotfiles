#!/bin/bash

function link_file {
    dir="~/${2:-}/"
    if [ ! -d ~/${2:-} ]; then
        echo "Target directory '${dir}' is not existed" >&2
        exit 1
    fi
    ln -s $(cd $(dirname ${0}) ; pwd)/$1 ~/${2:-}/ -v
}

link_file emacs/init.el .emacs.d
