<?php
system("rm -f *.rel");
foreach(glob("*.asm") as $f) system("sdasz80 -o $f");
