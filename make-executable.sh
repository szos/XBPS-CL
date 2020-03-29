#!/bin/sh

LISP=$(which sbcl)
exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31mSBCL not installed, exiting\e[0m"
    exit 1
else
    /bin/echo -e "\e[1;32mSBCL is installed, proceeding\e[0m"
fi
sbcl --eval "(progn (handler-case (in-package :ql) (PACKAGE-DOES-NOT-EXIST () (exit :code 1))) (sb-ext:exit :code 0))" > /dev/null 2>&1

exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31mQuicklisp not installed, exiting\e[0m"
    exit 1
else
    /bin/echo -e "\e[1;32mQuicklisp is installed, proceeding\e[0m"
fi

sbcl --eval "(push #p\"./\" asdf:*central-registry*)" --eval "(asdf:operate 'asdf:load-op 'xbps)" --eval "(xbps::make-xbps-graphical-executable)"

# sbcl --eval "(push #p\"./\" asdf:*central-registry*)" --eval "(ql:quickload :xbps)" --eval "(xbps::make-xbps-graphical-executable)"

exit="$?"

if [ ! "$exit" -eq 0 ]; then
    /bin/echo -e "\e[1;31m:XBPS not found, aborting\e[0m"
    exit 1
fi
