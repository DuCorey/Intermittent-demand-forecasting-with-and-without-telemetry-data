#!/usr/bin/bash

rsync -azvhu /media/storage/Documents/AL/ bruno:/home/corey/AL/
rsync -azvhu bruno:/home/corey/AL/ /media/storage/Documents/AL/
