#!/bin/bash

WORK_DIR=${HOME}/temp/install
EMACS_HOME=${HOME}/.emacs.d
SITE_LISP=${EMACS_HOME}/site-lisp
HYPER_SPEC=${EMACS_HOME}/docs/HyperSpec

if [ ! -d ${WORK_DIR} ]; then
    mkdir -p ${WORK_DIR} -v
fi

function src_install {
    ./configure
    if [ $? -ne 0 ]; then
        echo "--- ERROR: ./configure ---"
        exit 0
    fi
    make
    if [ $? -ne 0 ]; then
        echo "--- ERROR: make ---"
        exit 0
    fi
    sudo make install
    if [ $? -ne 0 ]; then
        echo "--- ERROR: make install ---"
        exit 0
    fi
}

sudo yum -y install libcurl-devel zlib-devel perl-ExtUtils-MakeMaker

# ----- git ----- #
# install the version that magit can use (> 1.9.4)

git_ver=`git --version | cut -d " " -f 3`
target_git_ver="2.7.0"
if [ "${git_ver}" != "${target_git_ver}" ]; then
    (cd ${WORK_DIR}
     wget https://www.kernel.org/pub/software/scm/git/git-${target_git_ver}.tar.gz
     tar zxf git-${target_git_ver}.tar.gz
     cd git-${target_git_ver}
     src_install)
fi

# ----- Roswell ----- #

which bc >& /dev/null
if [ $? -ne 0 ]; then
    sudo yum install bc -y
fi

# check autoconf versoin
autoconf_ver=`autoconf --version | head -n 1 | cut -d " " -f 4`
if [ `echo "${autoconf_ver} < 2.65" | bc` -eq 1 ]; then
    (cd ${WORK_DIR}
     if [ ! -d autoconf-2.69 ]; then
         wget http://ftp.gnu.org/gnu/autoconf/autoconf-2.69.tar.gz
         tar zxf autoconf-2.69.tar.gz
     fi
     cd autoconf-2.69
     src_install)
fi

if [ ! -d "${WORK_DIR}/roswell" ]; then 
    (cd ${WORK_DIR}
     git clone -b release https://github.com/roswell/roswell.git
     cd roswell
     sh bootstrap
     src_install
     ros run -q)
fi

# ----- HyperSpec ----- #

sudo yum -y install w3m

if [ ! -d ${HYPER_SPEC} ]; then
    mkdir ${HYPER_SPEC} -vp
    rm -rfv ${HYPER_SPEC}
    (cd ${WORK_DIR}
     wget ftp://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz
     tar zxf HyperSpec-7-0.tar.gz
     mv HyperSpec ${HYPER_SPEC} -v)
fi

# ----- Others ----- #
if [ ! -d ${SITE_LISP} ]; then
    mkdir ${SITE_LISP} -v
fi

if [ ! -f "${SITE_LISP}/slime-repl-ansi-color.el" ]; then
    (cd ${SITE_LISP}
     wget https://raw.githubusercontent.com/deadtrickster/slime-repl-ansi-color/master/slime-repl-ansi-color.el)
fi
