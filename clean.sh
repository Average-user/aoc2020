#!/bin/bash
rm *.hi
rm *.o
ls | grep -v "\." | xargs rm
