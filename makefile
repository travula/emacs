CODE_DIR=build/code
DOC_DIR=build/docs
VER_FILE=${CODE_DIR}/VERSION
EMACS=emacs-25.2


all:  build

build: init write-version
	${EMACS}  --script elisp/publish.el


init:
	mkdir -p ${CODE_DIR}

write-version:
	 # allow these to fail since the parent folder may not have a git
	 # repo.
	\rm -rf ${VER_FILE}
	echo -n "git remote origin url: " > ${VER_FILE}
	- echo `git config --get remote.origin.url` >> ${VER_FILE}
	echo -n "built from commit    : " >> ${VER_FILE}
	- echo `git rev-parse HEAD` >> ${VER_FILE}
	echo -n "commit date          : " >> ${VER_FILE}
	- echo `git log -1 --format=%cd` >> ${VER_FILE}
	echo -n "commit message       : " >> ${VER_FILE}
	- echo `git log --pretty=format:'%s' -n 1` >> ${VER_FILE}


