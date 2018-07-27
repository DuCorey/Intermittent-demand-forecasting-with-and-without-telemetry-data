#!/usr/bin/bash

rsync -azvhu /media/storage/Documents/AL/ bruno:/home/corey/AL/
rsync -azvhu bruno:/home/corey/AL/code bruno:/home/corey/AL/results /media/storage/Documents/AL/

