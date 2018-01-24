#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  .paket/paket.exe $@
else
  # use mono
  mono .paket/paket.exe $@
fi