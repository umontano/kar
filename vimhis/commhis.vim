
24,29s/\ \ \ \ //g
history
history20
24,29s/^/\ \ \ \ /g
!cp pyphone.py  pychange.py
! ls *.py
set no wrap
set rnu
!cp pyphone.py pychange.py
set nu rnu nowrap
!cp -v copyRCLONE.sh  out.txt
marks
17,$>
17,$>>
17,$<<
%s/t('/t('__/gc
set nu renu nowrap
%s/',end=/__',end=/gc
help q:
>
%s/')\n\ \{4\}
%s/'+l/_',l/gc
%s/_PUT//gc
%s/PASSED__
%s/PASSED__//gv
%s/PASSED__//gic
!cp copyRCLONE.sh  out.txt
!python3 pychange.py out.txt
w .viminfo
e .viminfo
!rclone copy --update --verbose  pychange.py  m:z/Dropbox/y/u/
set 
set wrap
set nowrap
set nu
28,54 dd
27,54y
e JORNADA.rlstq
27,54d
54
e 4fd0b098.bmk
w NEOV_RPLUG.TXT
set nowrap incsearch hlsearch
e cregexmakefile.c
e ~/uuu/pcxOLDcinput.c
bdelete %
set makeprg=make
e GNUmakefile
e substitute_single_backslash_by_double.c
w submakefile
e submakefile
make -f submakefile
b6
b5
w BACKUPpatternmkefile
b3
w BACKUPsubsmakefile
e BACKUPsubsmakefile
b4
b BACKUPsubsmakefile
make -f  BACKUPsubsmakefile
.!history | tail -11
!history | tail -11
w ~/storage/shared/cred.sh
q!
e ~/b/github_url.txt
e ~/b/rsync_exclude.txt
!aaaa
!bbbb
!cccc
echom ~/p/psychometric/cfa_2models_evaluation_lavaan_function_CBQ.R | echom ~/p/psychometric/part3cfa_cbq.R 
echom ~/p/psychometric/xREMOVEEScbq.txt |echom ~/p/psychometric/xSHORTENED_FACTORS.spec |echom ~/p/psychometric/xSHORTENED_FORM_SCALES.spec | echom 'aaa'
e ~/b/rsync_include.txt
h titlestring
h statusline
h status-line
%s/\V.sh/.sh\n/ge
%s/\V.sh/.sh\n/gc
set incsearch hlsearch 
set list
set incsearch hlsearch list laststatus=2 nu rnu nowrap
e ~/xxxx.xxx
e pyphone.py
set  makeprg=clear;sh\ %
make
set incsearch hlsearch nu rnu
set incsearch hlsearch nu rnu nowrap
syntax enable
e ~/b/gitsh/
e ~/p/psychometric/kodigit.txt
e ~/u/gitsh/from_wd_push.sh
wq
e xrres.txt
/\$ grep/;/sed /s/\$ //ge
e xxxx.xxx
bdelete
w xxxx.xxx
e ~/b/rsync_files.txt
buffers
ter
e ~/.bash_history
.;+2d
h yank
h ranges
/grep //grep /d
/\V#!\/bin\/sh//R CMD BATCH/;/sed/-1yank
/grep //sed /P
/sed /P
/sed /-1p
/\V_factors=3//_factors=3/yank
/\V#!/bin/sh/;/sed /s/\Vxrres.txt/x15.txt/ge
/\V\#!\/bin\/sh/;/sed /s/\Vxrres.txt/x15.txt/ge
/\#\!\/bin\/sh/;/R CMD//R CMD/yank
e GNUmakefile.sh
/\V\/bin\/sh/;/grep //grep /s/\Vx15.txt/x03.txt/ge
b#
/bin//grep /;/grep /normal I# 
/grep //gep /;$s/\# //ge
/grep //grep /;$s/\# //ge
/bin//grep /;/grep /-2normal I# 
/bin//grep /;/grep /-1normal I# 
/bin//R /;/grep /-1normal I# 
/bin//R /;/ CMD /s/\# //ge
b2
b1
e part3cfa_cbq.R
term
h term_sendkeys()
:qa
ww
set nu rnu
%s/
%s/matrices_number/number_of_imputations/ge
%s/SURG/SU/ge
%s/CE/EC/ge
%s/AN/NA/gc
r !perintf "\#"; for i in `seq 42`; do printf =; done
r !printf "\#"; for i in `seq 42`; do printf =; done
qa
q
/\Vp.mat = cor_test_mat/;/\v^)/-1 <<<
/\Vp.mat = cor_test_mat/;/\v^\)/-1 <<<
/\Vp.mat = cor_test_mat/;/\v^\)/-1 >>
w
set incsearch hlsearch laststatus=0  nu rnu nowrap
w | !git add . && git commit -m 'vi edit' && git push origin main

