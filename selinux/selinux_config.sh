#!/bin/sh
/usr/sbin/selinux-activate
semanage port -a -t idia_web_port_t -p tcp 3000
setsebool -P allow_execstack 1
setsebool -P allow_execmem 1
setsebool -P global_ssp 1
setsebool allow_daemons_use_tty on
