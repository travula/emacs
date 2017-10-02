TOP=${CURDIR}
BUILD_DIR=${TOP}/build
CODE_DIR=${BUILD_DIR}/code
DOC_DIR=${BUILD_DIR}/docs
VER_FILE=${BUILD_DIR}/VERSION
EMACS=emacs-25.2
# note that emacs-25.2 comes with org-8.2.10

build: init write-version
	${EMACS}  --script ${TOP}/elisp/publish.el


init:
	mkdir -p ${CODE_DIR}

write-version: emacs-version
	 # allow these to fail since the parent folder may not have a git
	 # repo.
	echo -n "git remote origin url: " >> ${VER_FILE}
	- echo `git config --get remote.origin.url` >> ${VER_FILE}
	echo -n "built from commit    : " >> ${VER_FILE}
	- echo `git rev-parse HEAD` >> ${VER_FILE}
	echo -n "commit date          : " >> ${VER_FILE}
	- echo `git log -1 --format=%cd` >> ${VER_FILE}
	echo -n "commit message       : " >> ${VER_FILE}
	- echo `git log --pretty=format:'%s' -n 1` >> ${VER_FILE}


emacs-version:
	\rm -rf ${VER_FILE}
	echo -n "built using          : " >> ${VER_FILE}
	- echo `${EMACS} --version | head -1` >> ${VER_FILE}
	echo -n "and org version      : " >> ${VER_FILE}
	- echo `${EMACS} -q --batch --eval '(princ (org-version))'` >> ${VER_FILE}


install:
	${EMACS} -q --batch -l ${CODE_DIR}/install-packages.el

emacs:
	${EMACS} -q  -l ${CODE_DIR}/init.el
