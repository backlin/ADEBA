#!/bin/sh

ls -t adeba_*.tar.gz | head -1 | xargs R CMD install
